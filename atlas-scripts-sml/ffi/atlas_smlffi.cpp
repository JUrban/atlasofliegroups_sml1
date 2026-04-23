#include <exception>
#include <memory>
#include <sstream>
#include <string>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>

#include "Atlas.h"
#include "matreduc.h"
#include "lattice.h"
#include "lietype.h"
#include "prerootdata.h"
#include "rootdata.h"
#include "innerclass.h"
#include "realredgp.h"
#include "repr.h"
#include "blocks.h"
#include "K_repr.h"
#include "alcoves.h"
#include "bitmap.h"
#include "dynkin.h"

static thread_local std::string g_last_error;
static thread_local std::string g_last_result;

namespace atlas {
namespace interpreter {
void check_interrupt() {}
} // namespace interpreter
} // namespace atlas

extern "C" const char* atlas_last_error()
{
  return g_last_error.c_str();
}

static const char* store_result(std::string s)
{
  g_last_result = std::move(s);
  return g_last_result.c_str();
}

static atlas::int_Matrix identity_matrix(unsigned int n)
{
  atlas::int_Matrix m(n, n);
  for (unsigned int i = 0; i < n; ++i)
    for (unsigned int j = 0; j < n; ++j)
      m(i, j) = (i == j) ? 1 : 0;
  return m;
}

static atlas::RatWeight ratweight_from_int32(const int32_t* nums, std::size_t n, int denom);

static bool parse_int_list(const char* text, std::vector<int>& out)
{
  out.clear();
  if (text == nullptr)
  {
    g_last_error = "parse_int_list: null text";
    return false;
  }
  std::istringstream in(text);
  int v;
  while (in >> v)
    out.push_back(v);
  if (!in.eof() && in.fail())
  {
    g_last_error = "parse_int_list: parse error";
    return false;
  }
  return true;
}

static bool parse_int_matrix_text(const char* text, atlas::int_Matrix& out)
{
  std::vector<int> xs;
  if (!parse_int_list(text, xs))
    return false;
  if (xs.size() < 2)
  {
    g_last_error = "parse_int_matrix_text: truncated header";
    return false;
  }
  const int n_rows = xs[0];
  const int n_cols = xs[1];
  if (n_rows < 0 || n_cols < 0)
  {
    g_last_error = "parse_int_matrix_text: negative dimensions";
    return false;
  }
  const std::size_t need = static_cast<std::size_t>(2 + n_rows * n_cols);
  if (xs.size() != need)
  {
    g_last_error = "parse_int_matrix_text: entry count mismatch";
    return false;
  }
  out = atlas::int_Matrix(static_cast<unsigned int>(n_rows), static_cast<unsigned int>(n_cols));
  std::size_t k = 2;
  for (int i = 0; i < n_rows; ++i)
    for (int j = 0; j < n_cols; ++j)
      out(i, j) = xs[k++];
  return true;
}

static std::string int_matrix_to_text(const atlas::int_Matrix& m)
{
  std::ostringstream out;
  out << m.n_rows() << ' ' << m.n_columns();
  for (unsigned int i = 0; i < m.n_rows(); ++i)
    for (unsigned int j = 0; j < m.n_columns(); ++j)
      out << ' ' << m(i, j);
  return out.str();
}

extern "C" const char* atlas_intmat_cartan_matrix_type_text(const char* mat_text)
{
  try
  {
    atlas::int_Matrix cm;
    if (!parse_int_matrix_text(mat_text, cm))
      return store_result("-1");
    if (cm.n_rows() != cm.n_columns())
    {
      g_last_error = "atlas_intmat_cartan_matrix_type_text: non-square matrix";
      return store_result("-1");
    }

    atlas::Permutation pi;
    atlas::lietype::LieType lt = atlas::dynkin::Lie_type(cm, pi);

    std::ostringstream out;
    out << lt.size();
    for (const auto& sf : lt)
      out << ' ' << sf.type() << ' ' << sf.rank();

    out << " | " << pi.size();
    for (unsigned int i = 0; i < pi.size(); ++i)
      out << ' ' << static_cast<unsigned int>(pi[i]);

    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

namespace {
struct GroupHandle
{
  atlas::lietype::LieType lt;
  atlas::lietype::InnerClassType ict;
  atlas::prerootdata::PreRootDatum prd;
  atlas::WeightInvolution di;
  atlas::innerclass::InnerClass ic;
  atlas::realredgp::RealReductiveGroup G;
  std::unique_ptr<atlas::repr::Rep_table> rt;

  GroupHandle(char type_letter,
              unsigned int rank,
              char inner_class_letter,
              atlas::RealFormNbr rf)
    : lt()
    , ict()
    , prd([&] {
        lt.push_back(atlas::lietype::SimpleLieType(type_letter, rank));
        return atlas::prerootdata::PreRootDatum(lt, /*prefer_co=*/false);
      }())
    , di([&] {
        ict.push_back(inner_class_letter);
        return atlas::lietype::involution(lt, ict);
      }())
    , ic(prd, di)
    , G(ic, rf)
    , rt()
  {
    rt = std::make_unique<atlas::repr::Rep_table>(G);
  }

  GroupHandle(atlas::prerootdata::PreRootDatum&& prd_in,
              atlas::WeightInvolution&& di_in,
              atlas::RealFormNbr rf,
              const atlas::RatCoweight& coch,
              atlas::TorusPart tp)
    : lt()
    , ict()
    , prd(std::move(prd_in))
    , di(std::move(di_in))
    , ic(prd, di)
    , G(ic, rf, coch, tp)
    , rt()
  {
    // Ensure involution table knows Cartan class for identity (see synthetic real form wrapper).
    atlas::TwistedInvolution tw_id;
    atlas::CartanNbr cn = ic.class_number(tw_id);
    ic.generate_Cartan_orbit(cn);
    const atlas::BitMap& b = ic.Cartan_ordering().below(cn);
    for (auto it = b.begin(); it(); ++it)
      ic.generate_Cartan_orbit(*it);

    rt = std::make_unique<atlas::repr::Rep_table>(G);
  }
};

struct ParamHandle
{
  GroupHandle* group; // non-owning; user must keep group alive
  atlas::repr::StandardRepr sr;

  ParamHandle(GroupHandle* group, atlas::repr::StandardRepr&& sr)
    : group(group), sr(std::move(sr))
  {}
};

struct ParamListHandle
{
  GroupHandle* group; // non-owning
  std::vector<std::pair<atlas::repr::StandardRepr, int>> terms;
  long start_pos; // for block() results; -1 if not applicable

  explicit ParamListHandle(GroupHandle* group)
    : group(group), terms(), start_pos(-1)
  {}
};

struct KTypePolHandle
{
  GroupHandle* group; // non-owning
  atlas::K_repr::K_type_pol poly;

  KTypePolHandle(GroupHandle* group, atlas::K_repr::K_type_pol&& poly)
    : group(group), poly(std::move(poly))
  {}
};

struct KTypeHandle
{
  GroupHandle* group; // non-owning; user must keep group alive
  atlas::K_repr::K_type t;

  KTypeHandle(GroupHandle* group, atlas::K_repr::K_type&& t)
    : group(group), t(std::move(t))
  {}
};

struct AdaptedBasisHandle
{
  atlas::int_Matrix basis;
  std::vector<int> diagonal;
};

struct EchelonHandle
{
  atlas::int_Matrix M;
  atlas::int_Matrix C;
  std::vector<int> pivots;
  int eps;
};

struct DiagonalizeHandle
{
  std::vector<int> diagonal;
  atlas::int_Matrix row;
  atlas::int_Matrix col;
};

struct RootDatumHandle
{
  atlas::prerootdata::PreRootDatum prd;
  atlas::rootdata::RootDatum rd;

  explicit RootDatumHandle(atlas::prerootdata::PreRootDatum&& prd)
    : prd(std::move(prd)), rd(this->prd)
  {}
};
} // namespace

extern "C" const char* atlas_rootdatum_root_coradical_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_root_coradical_text: null handle";
      return store_result("-1");
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const unsigned int r = h->rd.rank();

    atlas::int_Matrix m(r, r);
    unsigned int col = 0;
    for (auto it = h->rd.beginSimpleRoot(); it != h->rd.endSimpleRoot(); ++it, ++col)
      for (unsigned int i = 0; i < r; ++i)
        m(i, col) = (*it)[i];
    for (auto it = h->rd.beginCoradical(); it != h->rd.endCoradical(); ++it, ++col)
      for (unsigned int i = 0; i < r; ++i)
        m(i, col) = (*it)[i];

    if (col != r)
    {
      g_last_error = "atlas_rootdatum_root_coradical_text: internal size mismatch";
      return store_result("-1");
    }
    return store_result(int_matrix_to_text(m));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_rootdatum_coroot_radical_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_coroot_radical_text: null handle";
      return store_result("-1");
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const unsigned int r = h->rd.rank();

    atlas::int_Matrix m(r, r);
    unsigned int col = 0;
    for (auto it = h->rd.beginSimpleCoroot(); it != h->rd.endSimpleCoroot(); ++it, ++col)
      for (unsigned int i = 0; i < r; ++i)
        m(i, col) = (*it)[i];
    for (auto it = h->rd.beginRadical(); it != h->rd.endRadical(); ++it, ++col)
      for (unsigned int i = 0; i < r; ++i)
        m(i, col) = (*it)[i];

    if (col != r)
    {
      g_last_error = "atlas_rootdatum_coroot_radical_text: internal size mismatch";
      return store_result("-1");
    }
    return store_result(int_matrix_to_text(m));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void* atlas_param_finals(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_finals: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_finals: null group pointer in param";
      return nullptr;
    }

    atlas::repr::Rep_context rc(p->group->G);
    auto finals = rc.finals_for(p->sr);

    auto* out = new ParamListHandle(p->group);
    for (auto it = finals.begin(); not finals.at_end(it); ++it)
      out->terms.emplace_back(it->first, it->second);

    return static_cast<void*>(out);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_block_survivors(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_block_survivors: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_block_survivors: null group/Rep_table";
      return nullptr;
    }

    auto& G = p->group->G;
    auto& rt = *p->group->rt;
    atlas::repr::Rep_context rc(G);

    auto sr = p->sr; // local copy; lookup_full_block normalises in-place
    if (!rc.is_standard(sr))
    {
      g_last_error = "atlas_param_block_survivors: parameter is not standard";
      return nullptr;
    }

    atlas::BlockElt start;
    atlas::repr::block_modifier bm;
    auto& block = rt.lookup_full_block(sr, start, bm);
    const auto& gamma = sr.gamma();

    const atlas::RankFlags singular = block.singular(bm, gamma);
    long start_pos = -1;

    auto* out = new ParamListHandle(p->group);
    out->terms.reserve(block.size());
    for (atlas::BlockElt z = 0; z < block.size(); ++z)
      if (block.survives(z, singular))
      {
        if (z == start)
          start_pos = static_cast<long>(out->terms.size());
        out->terms.emplace_back(rc.sr(block.representative(z), bm, gamma), 1);
      }
    out->start_pos = start_pos;
    return static_cast<void*>(out);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" long atlas_paramlist_size(void* list_handle)
{
  try
  {
    if (list_handle == nullptr)
    {
      g_last_error = "atlas_paramlist_size: null list handle";
      return -1;
    }
    const auto* h = static_cast<const ParamListHandle*>(list_handle);
    return static_cast<long>(h->terms.size());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" long atlas_paramlist_start_pos(void* list_handle)
{
  try
  {
    if (list_handle == nullptr)
    {
      g_last_error = "atlas_paramlist_start_pos: null list handle";
      return -1;
    }
    const auto* h = static_cast<const ParamListHandle*>(list_handle);
    return static_cast<long>(h->start_pos);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" long atlas_paramlist_mult(void* list_handle, long index)
{
  try
  {
    if (list_handle == nullptr)
    {
      g_last_error = "atlas_paramlist_mult: null list handle";
      return 0;
    }
    const auto* h = static_cast<const ParamListHandle*>(list_handle);
    if (index < 0 || static_cast<std::size_t>(index) >= h->terms.size())
    {
      g_last_error = "atlas_paramlist_mult: index out of range";
      return 0;
    }
    return static_cast<long>(h->terms[static_cast<std::size_t>(index)].second);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" void* atlas_paramlist_get_param_clone(void* list_handle, long index)
{
  try
  {
    if (list_handle == nullptr)
    {
      g_last_error = "atlas_paramlist_get_param_clone: null list handle";
      return nullptr;
    }
    const auto* h = static_cast<const ParamListHandle*>(list_handle);
    if (h->group == nullptr)
    {
      g_last_error = "atlas_paramlist_get_param_clone: null group pointer";
      return nullptr;
    }
    if (index < 0 || static_cast<std::size_t>(index) >= h->terms.size())
    {
      g_last_error = "atlas_paramlist_get_param_clone: index out of range";
      return nullptr;
    }

    const auto& sr = h->terms[static_cast<std::size_t>(index)].first;
    return static_cast<void*>(
      new ParamHandle(h->group, atlas::repr::StandardRepr(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void atlas_paramlist_free(void* list_handle)
{
  try
  {
    if (list_handle == nullptr)
      return;
    delete static_cast<ParamListHandle*>(list_handle);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
  }
}

extern "C" const char* atlas_intmat_find_solution_text(const char* mat_text, const char* vec_text)
{
  try
  {
    std::vector<int> mat;
    std::vector<int> vec;
    if (!parse_int_list(mat_text, mat))
      return store_result("-1");
    if (!parse_int_list(vec_text, vec))
      return store_result("-1");
    if (mat.size() < 2 || vec.size() < 1)
    {
      g_last_error = "atlas_intmat_find_solution_text: truncated input";
      return store_result("-1");
    }
    const int n_rows = mat[0];
    const int n_cols = mat[1];
    if (n_rows < 0 || n_cols < 0)
    {
      g_last_error = "atlas_intmat_find_solution_text: negative dimensions";
      return store_result("-1");
    }
    const std::size_t need = static_cast<std::size_t>(2 + n_rows * n_cols);
    if (mat.size() != need)
    {
      g_last_error = "atlas_intmat_find_solution_text: matrix entry count mismatch";
      return store_result("-1");
    }
    if (vec[0] != n_rows)
    {
      g_last_error = "atlas_intmat_find_solution_text: vector length mismatch";
      return store_result("-1");
    }
    if (vec.size() != static_cast<std::size_t>(1 + n_rows))
    {
      g_last_error = "atlas_intmat_find_solution_text: vector entry count mismatch";
      return store_result("-1");
    }

    atlas::int_Matrix A(static_cast<unsigned int>(n_rows), static_cast<unsigned int>(n_cols));
    std::size_t k = 2;
    for (int i = 0; i < n_rows; ++i)
      for (int j = 0; j < n_cols; ++j)
        A(i, j) = mat[k++];

    atlas::int_Vector b(static_cast<unsigned int>(n_rows));
    for (int i = 0; i < n_rows; ++i)
      b[i] = vec[1 + i];

    if (!atlas::matreduc::has_solution(A, b))
      return store_result("0");

    atlas::int_Vector x = atlas::matreduc::find_solution(A, b);
    std::ostringstream out;
    out << n_cols;
    for (int j = 0; j < n_cols; ++j)
      out << ' ' << x[j];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_kernel_text(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return store_result("-1");
    atlas::int_Matrix k = atlas::lattice::kernel(std::move(m));
    return store_result(int_matrix_to_text(k));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_eigen_lattice_text(const char* mat_text, int eigen_value)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return store_result("-1");
    atlas::int_Matrix k = atlas::lattice::eigen_lattice(std::move(m), eigen_value);
    return store_result(int_matrix_to_text(k));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_smith_basis_text(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return store_result("-1");
    std::vector<int> diag;
    atlas::int_Matrix basis = atlas::matreduc::Smith_basis(m, diag);
    return store_result(int_matrix_to_text(basis));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_smith_diag_text(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return store_result("-1");
    std::vector<int> diag;
    (void)atlas::matreduc::Smith_basis(m, diag);
    std::ostringstream out;
    out << diag.size();
    for (int d : diag)
      out << ' ' << d;
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void* atlas_intmat_diagonalize(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return nullptr;
    auto* h = new DiagonalizeHandle();
    h->diagonal = atlas::matreduc::diagonalise(m, h->row, h->col);
    return static_cast<void*>(h);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_intmat_diagonalize_diag_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_diagonalize_diag_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const DiagonalizeHandle*>(handle);
    std::ostringstream out;
    out << h->diagonal.size();
    for (int d : h->diagonal)
      out << ' ' << d;
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_diagonalize_row_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_diagonalize_row_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const DiagonalizeHandle*>(handle);
    return store_result(int_matrix_to_text(h->row));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_diagonalize_col_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_diagonalize_col_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const DiagonalizeHandle*>(handle);
    return store_result(int_matrix_to_text(h->col));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void atlas_intmat_diagonalize_free(void* handle)
{
  try
  {
    delete static_cast<DiagonalizeHandle*>(handle);
  }
  catch (...)
  {
  }
}

extern "C" void* atlas_intmat_adapted_basis(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return nullptr;
    auto* h = new AdaptedBasisHandle();
    h->basis = atlas::matreduc::adapted_basis(std::move(m), h->diagonal);
    return static_cast<void*>(h);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_intmat_adapted_basis_matrix_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_adapted_basis_matrix_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const AdaptedBasisHandle*>(handle);
    return store_result(int_matrix_to_text(h->basis));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_adapted_basis_diag_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_adapted_basis_diag_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const AdaptedBasisHandle*>(handle);
    std::ostringstream out;
    out << h->diagonal.size();
    for (const int d : h->diagonal)
      out << ' ' << d;
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void atlas_intmat_adapted_basis_free(void* handle)
{
  try
  {
    if (handle == nullptr)
      return;
    delete static_cast<AdaptedBasisHandle*>(handle);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
  }
}

extern "C" const char* atlas_intmat_in_lattice_basis_text(const char* a_text, const char* m_text)
{
  try
  {
    atlas::int_Matrix A;
    atlas::int_Matrix M;
    if (!parse_int_matrix_text(a_text, A))
      return store_result("-1");
    if (!parse_int_matrix_text(m_text, M))
      return store_result("-1");
    if (A.n_rows() != M.n_rows())
    {
      g_last_error = "atlas_intmat_in_lattice_basis_text: row dimension mismatch";
      return store_result("-1");
    }

    const unsigned int n = A.n_rows();
    const unsigned int m = A.n_columns();
    const unsigned int r = M.n_columns();

    atlas::int_Matrix C(m, r);
    for (unsigned int j = 0; j < r; ++j)
    {
      atlas::int_Vector b(n);
      for (unsigned int i = 0; i < n; ++i)
        b[i] = M(i, j);
      if (!atlas::matreduc::has_solution(A, b))
      {
        g_last_error = "atlas_intmat_in_lattice_basis_text: column not in lattice span";
        return store_result("-1");
      }
      atlas::int_Vector x = atlas::matreduc::find_solution(A, std::move(b));
      for (unsigned int i = 0; i < m; ++i)
        C(i, j) = x[i];
    }

    return store_result(int_matrix_to_text(C));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void* atlas_group_new_F4_s()
{
  try
  {
    auto* h = new GroupHandle('F', 4, 's', atlas::RealFormNbr(0));
    return static_cast<void*>(h);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void atlas_group_free(void* handle)
{
  try
  {
    delete static_cast<GroupHandle*>(handle);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
  }
}

extern "C" long atlas_group_kgb_size(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_kgb_size: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return static_cast<long>(h->G.KGB_size());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" long atlas_group_rank(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_rank: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    atlas::repr::Rep_context rc(h->G);
    return static_cast<long>(rc.rank());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_group_is_split(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_is_split: null handle";
      return 0;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return h->G.isSplit() ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_group_is_compact(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_is_compact: null handle";
      return 0;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return h->G.isCompact() ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" long atlas_group_component_rank(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_component_rank: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return static_cast<long>(h->G.component_rank());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_group_rho_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_rho_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    atlas::repr::Rep_context rc(h->G);
    const auto& rd = rc.root_datum();
    const auto rank = rc.rank();
    atlas::RatWeight rho = atlas::rootdata::rho(rd);
    rho.normalize();

    std::ostringstream out;
    out << rho.denominator();
    const auto& num = rho.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_group_simple_coroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_simple_coroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    atlas::repr::Rep_context rc(h->G);
    const auto& rd = rc.root_datum();
    const auto ss_rank = rd.semisimple_rank();
    const auto rank = rd.rank();

    std::ostringstream out;
    out << ss_rank << ' ' << rank;
    for (unsigned int s = 0; s < ss_rank; ++s)
    {
      const auto& cor = rd.simpleCoroot(static_cast<atlas::weyl::Generator>(s));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << cor[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_group_posroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_posroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    atlas::repr::Rep_context rc(h->G);
    const auto& rd = rc.root_datum();
    const auto rank = rd.rank();
    const auto n = rd.numPosRoots();

    std::ostringstream out;
    out << n << ' ' << rank;
    for (unsigned int j = 0; j < n; ++j)
    {
      const auto& r = rd.posRoot(static_cast<atlas::RootNbr>(j));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_group_semisimple_rank(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_semisimple_rank: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return static_cast<int>(h->G.semisimple_rank());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_group_real_form_number(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_real_form_number: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return static_cast<int>(h->G.realForm());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

static const char* ratvec_to_text(const atlas::RatWeight& v)
{
  std::ostringstream out;
  out << v.denominator();
  const auto& num = v.numerator();
  for (std::size_t i = 0; i < num.size(); ++i)
    out << ' ' << static_cast<long long>(num[i]);
  return store_result(out.str());
}

static const char* ratcoweight_to_text(const atlas::RatCoweight& v)
{
  std::ostringstream out;
  out << v.denominator();
  const auto& num = v.numerator();
  for (std::size_t i = 0; i < num.size(); ++i)
    out << ' ' << static_cast<long long>(num[i]);
  return store_result(out.str());
}

extern "C" int atlas_kgb_status(void* group_handle, int s, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_status: null group handle";
      return -1;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    const auto& rd = g->G.root_datum();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_status: invalid KGB index";
      return -1;
    }
    if (s < 0 || static_cast<unsigned int>(s) >= rd.semisimple_rank())
    {
      g_last_error = "atlas_kgb_status: invalid generator index";
      return -1;
    }

    const atlas::KGBElt xv = static_cast<atlas::KGBElt>(x);
    const atlas::weyl::Generator sv = static_cast<atlas::weyl::Generator>(s);
    unsigned stat = kgb.status(sv, xv);
    if (stat == 0u && !kgb.isDescent(sv, xv))
      stat = 4u; // complex ascent
    return static_cast<int>(stat);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_kgb_cross(void* group_handle, int s, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_cross: null group handle";
      return -1;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    const auto& rd = g->G.root_datum();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_cross: invalid KGB index";
      return -1;
    }
    if (s < 0 || static_cast<unsigned int>(s) >= rd.semisimple_rank())
    {
      g_last_error = "atlas_kgb_cross: invalid generator index";
      return -1;
    }

    const atlas::KGBElt xv = static_cast<atlas::KGBElt>(x);
    const atlas::weyl::Generator sv = static_cast<atlas::weyl::Generator>(s);
    return static_cast<int>(kgb.cross(sv, xv));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_kgb_cross_word_text(void* group_handle, int x, const char* word_text)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_cross_word_text: null group handle";
      return -1;
    }
    if (word_text == nullptr)
    {
      g_last_error = "atlas_kgb_cross_word_text: null word_text";
      return -1;
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    const auto& rd = g->G.root_datum();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_cross_word_text: invalid KGB index";
      return -1;
    }

    std::vector<int> xs;
    if (!parse_int_list(word_text, xs))
      return -1;
    if (xs.empty())
    {
      g_last_error = "atlas_kgb_cross_word_text: empty word";
      return -1;
    }
    const int k = xs[0];
    if (k < 0)
    {
      g_last_error = "atlas_kgb_cross_word_text: negative length";
      return -1;
    }
    if (xs.size() != static_cast<std::size_t>(1 + k))
    {
      g_last_error = "atlas_kgb_cross_word_text: wrong arity";
      return -1;
    }

    atlas::WeylWord ww;
    ww.reserve(static_cast<std::size_t>(k));
    for (int i = 0; i < k; ++i)
    {
      const int s = xs[1 + i];
      if (s < 0 || static_cast<unsigned int>(s) >= rd.semisimple_rank())
      {
        g_last_error = "atlas_kgb_cross_word_text: invalid generator index";
        return -1;
      }
      ww.push_back(static_cast<atlas::weyl::Generator>(s));
    }

    const atlas::KGBElt xv = static_cast<atlas::KGBElt>(x);
    // Match `basic.at`: `cross(WeylElt w, KGBElt x)` applies the word in
    // reverse order (`w.word ~`).
    return static_cast<int>(kgb.cross(ww, xv));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_kgb_cayley(void* group_handle, int s, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_cayley: null group handle";
      return -1;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    const auto& rd = g->G.root_datum();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_cayley: invalid KGB index";
      return -1;
    }
    if (s < 0 || static_cast<unsigned int>(s) >= rd.semisimple_rank())
    {
      g_last_error = "atlas_kgb_cayley: invalid generator index";
      return -1;
    }

    const atlas::KGBElt xv = static_cast<atlas::KGBElt>(x);
    const atlas::weyl::Generator sv = static_cast<atlas::weyl::Generator>(s);
    atlas::KGBElt y = kgb.any_Cayley(sv, xv);
    if (y == atlas::UndefKGB)
      y = xv; // match atlas interpreter behavior: leave unchanged if undefined
    return static_cast<int>(y);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" int atlas_kgb_length(void* group_handle, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_length: null group handle";
      return -1;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_length: invalid KGB index";
      return -1;
    }
    return static_cast<int>(kgb.length(static_cast<atlas::KGBElt>(x)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_kgb_torus_factor_text(void* group_handle, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_torus_factor_text: null group handle";
      return nullptr;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& kgb = g->G.kgb();
    if (x < 0 || static_cast<unsigned int>(x) >= kgb.size())
    {
      g_last_error = "atlas_kgb_torus_factor_text: invalid KGB index";
      return nullptr;
    }
    atlas::RatCoweight tf = kgb.torus_factor(static_cast<atlas::KGBElt>(x));
    tf.normalize();
    return ratcoweight_to_text(tf);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_rootdatum_new_simple(char type_letter, int rank, int prefer_coroots)
{
  try
  {
    if (rank <= 0)
    {
      g_last_error = "atlas_rootdatum_new_simple: rank must be positive";
      return nullptr;
    }
    atlas::lietype::LieType lt;
    lt.push_back(atlas::lietype::SimpleLieType(type_letter, static_cast<unsigned int>(rank)));
    atlas::prerootdata::PreRootDatum prd(lt, prefer_coroots != 0);
    return static_cast<void*>(new RootDatumHandle(std::move(prd)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_rootdatum_new_from_simple_mats_text(const char* simple_roots_text,
                                                          const char* simple_coroots_text,
                                                          int prefer_coroots)
{
  try
  {
    if (simple_roots_text == nullptr || simple_coroots_text == nullptr)
    {
      g_last_error = "atlas_rootdatum_new_from_simple_mats_text: null input";
      return nullptr;
    }
    atlas::int_Matrix roots;
    atlas::int_Matrix coroots;
    if (!parse_int_matrix_text(simple_roots_text, roots))
      return nullptr;
    if (!parse_int_matrix_text(simple_coroots_text, coroots))
      return nullptr;
    atlas::prerootdata::PreRootDatum prd(roots, coroots, prefer_coroots != 0);
    prd.test_Cartan_matrix();
    return static_cast<void*>(new RootDatumHandle(std::move(prd)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void atlas_rootdatum_free(void* handle)
{
  try
  {
    delete static_cast<RootDatumHandle*>(handle);
  }
  catch (...)
  {
  }
}

extern "C" void* atlas_rootdatum_dual(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_dual: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    atlas::prerootdata::PreRootDatum prd_dual(h->prd, atlas::tags::DualTag{});
    return static_cast<void*>(new RootDatumHandle(std::move(prd_dual)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" long atlas_rootdatum_rank(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_rank: null handle";
      return -1;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    return static_cast<long>(h->rd.rank());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_rootdatum_rho_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_rho_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();
    atlas::RatWeight rho = atlas::rootdata::rho(h->rd);
    rho.normalize();
    std::ostringstream out;
    out << rho.denominator();
    const auto& num = rho.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_make_dominant_ratweight_text(void* handle, const char* ratweight_text)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_make_dominant_ratweight_text: null handle";
      return store_result("-1");
    }
    if (ratweight_text == nullptr)
    {
      g_last_error = "atlas_rootdatum_make_dominant_ratweight_text: null input";
      return store_result("-1");
    }

    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();

    std::vector<int> xs;
    if (!parse_int_list(ratweight_text, xs))
      return store_result("-1");
    if (xs.size() != static_cast<std::size_t>(1 + rank))
    {
      g_last_error = "atlas_rootdatum_make_dominant_ratweight_text: wrong arity";
      return store_result("-1");
    }
    const int denom = xs[0];
    if (denom == 0)
    {
      g_last_error = "atlas_rootdatum_make_dominant_ratweight_text: zero denominator";
      return store_result("-1");
    }
    std::vector<int32_t> nums;
    nums.reserve(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const int v = xs[1 + i];
      if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
      {
        g_last_error = "atlas_rootdatum_make_dominant_ratweight_text: numerator out of int32 range";
        return store_result("-1");
      }
      nums.push_back(static_cast<int32_t>(v));
    }

    atlas::RatWeight w = ratweight_from_int32(nums.data(), rank, denom);
    h->rd.make_dominant(w.numerator());
    w.normalize();

    std::ostringstream out;
    out << w.denominator();
    const auto& num = w.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_rootdatum_simple_roots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_simple_roots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto ss_rank = h->rd.semisimple_rank();
    const auto rank = h->rd.rank();

    std::ostringstream out;
    out << ss_rank << ' ' << rank;
    for (unsigned int s = 0; s < ss_rank; ++s)
    {
      const auto& r = h->rd.simpleRoot(static_cast<atlas::weyl::Generator>(s));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_simple_coroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_simple_coroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto ss_rank = h->rd.semisimple_rank();
    const auto rank = h->rd.rank();

    std::ostringstream out;
    out << ss_rank << ' ' << rank;
    for (unsigned int s = 0; s < ss_rank; ++s)
    {
      const auto& cor = h->rd.simpleCoroot(static_cast<atlas::weyl::Generator>(s));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << cor[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_posroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_posroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();
    const auto n = h->rd.numPosRoots();

    std::ostringstream out;
    out << n << ' ' << rank;
    for (unsigned int j = 0; j < n; ++j)
    {
      const auto& r = h->rd.posRoot(static_cast<atlas::RootNbr>(j));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_poscoroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_poscoroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();
    const auto n = h->rd.numPosRoots();

    std::ostringstream out;
    out << n << ' ' << rank;
    for (unsigned int j = 0; j < n; ++j)
    {
      const auto& r = h->rd.posCoroot(static_cast<atlas::RootNbr>(j));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_simple_factors_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_simple_factors_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const atlas::lietype::LieType lt = h->rd.type();
    std::ostringstream out;
    out << lt.size();
    for (const auto& slt : lt)
      out << ' ' << slt.type() << ' ' << slt.rank();
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_roots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_roots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();
    const auto n = h->rd.numRoots();

    std::ostringstream out;
    out << n << ' ' << rank;
    for (unsigned int j = 0; j < n; ++j)
    {
      const auto& r = h->rd.root(static_cast<atlas::RootNbr>(j));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_coroots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_coroots_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();
    const auto n = h->rd.numRoots();

    std::ostringstream out;
    out << n << ' ' << rank;
    for (unsigned int j = 0; j < n; ++j)
    {
      const auto& r = h->rd.coroot(static_cast<atlas::RootNbr>(j));
      for (unsigned int i = 0; i < rank; ++i)
        out << ' ' << r[i];
    }
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_rootdatum_FPP_orbit_numers_text(void* handle, const char* ratweight_text)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_rootdatum_FPP_orbit_numers_text: null handle";
      return store_result("-1");
    }
    if (ratweight_text == nullptr)
    {
      g_last_error = "atlas_rootdatum_FPP_orbit_numers_text: null input";
      return store_result("-1");
    }

    auto* h = static_cast<RootDatumHandle*>(handle);
    const auto rank = h->rd.rank();

    std::vector<int> xs;
    if (!parse_int_list(ratweight_text, xs))
      return store_result("-1");
    if (xs.size() != static_cast<std::size_t>(1 + rank))
    {
      g_last_error = "atlas_rootdatum_FPP_orbit_numers_text: wrong arity";
      return store_result("-1");
    }
    const int denom = xs[0];
    if (denom == 0)
    {
      g_last_error = "atlas_rootdatum_FPP_orbit_numers_text: zero denominator";
      return store_result("-1");
    }
    std::vector<int32_t> nums;
    nums.reserve(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const int v = xs[1 + i];
      if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
      {
        g_last_error = "atlas_rootdatum_FPP_orbit_numers_text: numerator out of int32 range";
        return store_result("-1");
      }
      nums.push_back(static_cast<int32_t>(v));
    }

    atlas::RatWeight gamma = ratweight_from_int32(nums.data(), rank, denom);
    gamma.normalize();

    atlas::weyl::WeylGroup W(h->rd.Cartan_matrix());
    auto list = atlas::weyl::FPP_orbit_numers(h->rd, W, gamma);

    atlas::int_Matrix out(static_cast<unsigned int>(list.size()), rank);
    unsigned int r = 0;
    for (auto&& v : list)
    {
      for (unsigned int i = 0; i < rank; ++i)
        out(r, i) = v[i];
      ++r;
    }
    return store_result(int_matrix_to_text(out));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" long atlas_kgb_size_F4_s()
{
  try
  {
    using namespace atlas;

    GroupHandle h('F', 4, 's', atlas::RealFormNbr(0));
    return static_cast<long>(h.G.KGB_size());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" void* atlas_group_new_simple(char type_letter,
                                       int rank,
                                       char inner_class_letter,
                                       int rf)
{
  try
  {
    if (rank <= 0)
    {
      g_last_error = "atlas_group_new_simple: rank must be positive";
      return nullptr;
    }
    if (rf < 0)
    {
      g_last_error = "atlas_group_new_simple: real form number must be >= 0";
      return nullptr;
    }

    // IMPORTANT: validate the real form number against the inner class.
    // Some invalid values can crash the Atlas library before throwing.
    atlas::lietype::LieType lt;
    lt.push_back(atlas::lietype::SimpleLieType(type_letter, static_cast<unsigned int>(rank)));
    atlas::prerootdata::PreRootDatum prd(lt, /*prefer_co=*/false);
    atlas::lietype::InnerClassType ict;
    ict.push_back(inner_class_letter);
    atlas::WeightInvolution di = atlas::lietype::involution(lt, ict);
    atlas::innerclass::InnerClass ic(prd, di);

    const auto nrf = static_cast<long>(ic.numRealForms());
    if (rf >= nrf)
    {
      g_last_error = "atlas_group_new_simple: real form number out of range";
      return nullptr;
    }

    auto* h = new GroupHandle(type_letter,
                              static_cast<unsigned int>(rank),
                              inner_class_letter,
                              atlas::RealFormNbr(static_cast<unsigned short>(rf)));
    return static_cast<void*>(h);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" long atlas_group_num_real_forms(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_num_real_forms: null handle";
      return -1;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    return static_cast<long>(h->ic.numRealForms());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_group_make_dominant_ratweight_text(void* group_handle, const char* ratweight_text)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_group_make_dominant_ratweight_text: null group handle";
      return store_result("-1");
    }
    if (ratweight_text == nullptr)
    {
      g_last_error = "atlas_group_make_dominant_ratweight_text: null input";
      return store_result("-1");
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();

    std::vector<int> xs;
    if (!parse_int_list(ratweight_text, xs))
      return store_result("-1");
    if (xs.size() != static_cast<std::size_t>(1 + rank))
    {
      g_last_error = "atlas_group_make_dominant_ratweight_text: wrong arity";
      return store_result("-1");
    }
    const int denom = xs[0];
    if (denom == 0)
    {
      g_last_error = "atlas_group_make_dominant_ratweight_text: zero denominator";
      return store_result("-1");
    }
    std::vector<int32_t> nums;
    nums.reserve(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const int v = xs[1 + i];
      if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
      {
        g_last_error = "atlas_group_make_dominant_ratweight_text: numerator out of int32 range";
        return store_result("-1");
      }
      nums.push_back(static_cast<int32_t>(v));
    }

    atlas::RatWeight w = ratweight_from_int32(nums.data(), rank, denom);
    rc.root_datum().make_dominant(w.numerator());
    w.normalize();

    std::ostringstream out;
    out << w.denominator();
    const auto& num = w.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_group_from_dominant_ratweight_text(void* group_handle, const char* ratweight_text)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_group_from_dominant_ratweight_text: null group handle";
      return store_result("-1");
    }
    if (ratweight_text == nullptr)
    {
      g_last_error = "atlas_group_from_dominant_ratweight_text: null input";
      return store_result("-1");
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();
    const auto& rd = rc.root_datum();

    std::vector<int> xs;
    if (!parse_int_list(ratweight_text, xs))
      return store_result("-1");
    if (xs.size() != static_cast<std::size_t>(1 + rank))
    {
      g_last_error = "atlas_group_from_dominant_ratweight_text: wrong arity";
      return store_result("-1");
    }
    const int denom = xs[0];
    if (denom == 0)
    {
      g_last_error = "atlas_group_from_dominant_ratweight_text: zero denominator";
      return store_result("-1");
    }

    atlas::matrix::Vector<atlas::arithmetic::Numer_t> num(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const int v = xs[1 + i];
      num[i] = static_cast<atlas::arithmetic::Numer_t>(v);
    }

    // factor_dominant modifies `num` in-place and returns a WeylWord that
    // converts the dominant representative back to the original weight.
    atlas::WeylWord witness = rd.factor_dominant(num);

    // Rebuild dominant RatWeight with the original denominator.
    atlas::RatWeight dom(num, denom);
    dom.normalize();

    std::ostringstream out;
    out << witness.size();
    for (auto s : witness)
      out << ' ' << static_cast<int>(s);
    out << '\n';
    out << dom.denominator();
    const auto& domNum = dom.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << domNum[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_group_weyl_word_act_ratweight_text(void* group_handle,
                                                               const char* word_text,
                                                               const char* ratweight_text)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: null group handle";
      return store_result("-1");
    }
    if (word_text == nullptr)
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: null word_text";
      return store_result("-1");
    }
    if (ratweight_text == nullptr)
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: null ratweight_text";
      return store_result("-1");
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();
    const auto& rd = rc.root_datum();

    std::vector<int> ws;
    if (!parse_int_list(word_text, ws))
      return store_result("-1");
    if (ws.empty())
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: empty word";
      return store_result("-1");
    }
    const int k = ws[0];
    if (k < 0)
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: negative word length";
      return store_result("-1");
    }
    if (ws.size() != static_cast<std::size_t>(1 + k))
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: wrong word arity";
      return store_result("-1");
    }

    atlas::WeylWord ww;
    ww.reserve(static_cast<std::size_t>(k));
    for (int i = 0; i < k; ++i)
    {
      const int s = ws[1 + i];
      if (s < 0 || static_cast<unsigned int>(s) >= rd.semisimple_rank())
      {
        g_last_error = "atlas_group_weyl_word_act_ratweight_text: invalid generator index";
        return store_result("-1");
      }
      ww.push_back(static_cast<atlas::weyl::Generator>(s));
    }

    std::vector<int> xs;
    if (!parse_int_list(ratweight_text, xs))
      return store_result("-1");
    if (xs.size() != static_cast<std::size_t>(1 + rank))
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: wrong ratweight arity";
      return store_result("-1");
    }
    const int denom = xs[0];
    if (denom == 0)
    {
      g_last_error = "atlas_group_weyl_word_act_ratweight_text: zero denominator";
      return store_result("-1");
    }
    std::vector<int32_t> nums;
    nums.reserve(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const int v = xs[1 + i];
      if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
      {
        g_last_error = "atlas_group_weyl_word_act_ratweight_text: numerator out of int32 range";
        return store_result("-1");
      }
      nums.push_back(static_cast<int32_t>(v));
    }

    atlas::RatWeight v = ratweight_from_int32(nums.data(), rank, denom);
    rd.act(ww, v);
    v.normalize();

    std::ostringstream out;
    out << v.denominator();
    const auto& num = v.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_group_distinguished_involution_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_distinguished_involution_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    const atlas::WeightInvolution& delta = h->ic.distinguished();
    return store_result(int_matrix_to_text(delta));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_group_rootdatum_new(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_rootdatum_new: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    auto prd = h->prd; // copy
    return new RootDatumHandle(std::move(prd));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_group_kgb_involution_matrix_text(void* handle, int x)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_group_kgb_involution_matrix_text: null handle";
      return nullptr;
    }
    auto* h = static_cast<GroupHandle*>(handle);
    if (x < 0 || static_cast<unsigned int>(x) >= h->G.KGB_size())
    {
      g_last_error = "atlas_group_kgb_involution_matrix_text: invalid KGB index";
      return nullptr;
    }
    atlas::repr::Rep_context rc(h->G);
    const auto rank = rc.rank();
    const auto& m = rc.kgb().involution_matrix(static_cast<atlas::KGBElt>(x));

    std::ostringstream out;
    out << rank;
    for (unsigned int i = 0; i < rank; ++i)
      for (unsigned int j = 0; j < rank; ++j)
        out << ' ' << m(i, j);
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_group_kgb_involution_is_minus_identity(void* group_handle, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_group_kgb_involution_is_minus_identity: null group handle";
      return 0;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    if (x < 0 || static_cast<unsigned int>(x) >= g->G.KGB_size())
    {
      g_last_error = "atlas_group_kgb_involution_is_minus_identity: invalid KGB index";
      return 0;
    }
    const auto& kgb = g->G.kgb();
    const auto& m = kgb.involution_matrix(static_cast<atlas::KGBElt>(x));
    const auto r = m.n_rows();
    if (r != m.n_columns())
      return 0;
    for (unsigned int i = 0; i < r; ++i)
      for (unsigned int j = 0; j < r; ++j)
      {
        const int expected = (i == j) ? -1 : 0;
        if (m(i, j) != expected)
          return 0;
      }
    return 1;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" void* atlas_param_trivial(void* group_handle)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_param_trivial: null group handle";
      return nullptr;
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);

    const auto rank = rc.rank();
    atlas::Weight lambda_rho(rank, 0);
    atlas::RatWeight nu = atlas::rootdata::rho(rc.root_datum());

    const atlas::KGBElt x_open = static_cast<atlas::KGBElt>(g->G.KGB_size() - 1);
    atlas::repr::StandardRepr sr = rc.sr(x_open, lambda_rho, nu);

    return static_cast<void*>(new ParamHandle(g, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void atlas_param_free(void* param_handle)
{
  try
  {
    delete static_cast<ParamHandle*>(param_handle);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
  }
}

extern "C" void* atlas_param_clone(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_clone: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_clone: null group pointer in param";
      return nullptr;
    }
    atlas::repr::StandardRepr sr = p->sr;
    return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_normalise(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_normalise: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_normalise: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    atlas::repr::StandardRepr sr = p->sr;
    rc.normalise(sr);
    return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_kgb_all_lambda_differential_0_text(void* group_handle, int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_kgb_all_lambda_differential_0_text: null group handle";
      return nullptr;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    if (x < 0 || static_cast<unsigned int>(x) >= g->G.KGB_size())
    {
      g_last_error = "atlas_kgb_all_lambda_differential_0_text: invalid KGB index";
      return nullptr;
    }

    atlas::repr::Rep_context rc(g->G);
    const unsigned int rank = rc.rank();

    const auto& theta = rc.kgb().involution_matrix(static_cast<atlas::KGBElt>(x));
    atlas::int_Matrix th1 = identity_matrix(rank); // (1+theta)
    th1 += theta;
    atlas::int_Matrix thm = identity_matrix(rank); // (1-theta)
    thm -= theta;

    // columns of K span ker(1+theta)
    const atlas::int_Matrix K = atlas::lattice::kernel(th1);
    const unsigned int m = K.n_columns();

    // C = in_lattice_basis(K, 1-theta): solve K * c_j = (1-theta).col(j)
    atlas::int_Matrix C(m, rank);
    for (unsigned int j = 0; j < rank; ++j)
    {
      atlas::int_Vector b(rank);
      for (unsigned int i = 0; i < rank; ++i)
        b[i] = thm(i, j);
      if (!atlas::matreduc::has_solution(K, b))
      {
        g_last_error = "atlas_kgb_all_lambda_differential_0_text: internal error: (1-theta) not in ker(1+theta)";
        return nullptr;
      }
      atlas::int_Vector sol = atlas::matreduc::find_solution(K, std::move(b));
      for (unsigned int i = 0; i < m; ++i)
        C(i, j) = sol[i];
    }

    std::vector<int> diagonal;
    const atlas::int_Matrix A = atlas::matreduc::adapted_basis(C, diagonal);

    std::vector<unsigned int> gens;
    gens.reserve(diagonal.size());
    for (unsigned int j = 0; j < diagonal.size(); ++j)
      if (diagonal[j] == 2)
        gens.push_back(j);

    const unsigned int k = static_cast<unsigned int>(gens.size());

    atlas::int_Matrix basis(rank, k);
    for (unsigned int t = 0; t < k; ++t)
    {
      const unsigned int j = gens[t];
      for (unsigned int i = 0; i < rank; ++i)
      {
        long long acc = 0;
        for (unsigned int u = 0; u < m; ++u)
          acc += static_cast<long long>(K(i, u)) * static_cast<long long>(A(u, j));
        basis(i, t) = static_cast<int>(acc);
      }
    }

    // enumerate all {0,1}-combinations of basis columns
    std::vector<atlas::int_Vector> out;
    out.reserve(static_cast<std::size_t>(1) << k);
    out.emplace_back(rank);
    for (unsigned int i = 0; i < rank; ++i)
      out.back()[i] = 0;

    for (unsigned int col = 0; col < k; ++col)
    {
      const std::size_t cur = out.size();
      out.reserve(cur * 2);
      for (std::size_t idx = 0; idx < cur; ++idx)
      {
        atlas::int_Vector v = out[idx];
        for (unsigned int i = 0; i < rank; ++i)
          v[i] += basis(i, col);
        out.push_back(std::move(v));
      }
    }

    std::ostringstream s;
    s << out.size() << ' ' << rank;
    for (const auto& v : out)
      for (unsigned int i = 0; i < rank; ++i)
        s << ' ' << v[i];
    return store_result(s.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_param_lambda_text(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_lambda_text: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_lambda_text: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    const auto& rd = rc.root_datum();
    const auto rank = rc.rank();
    atlas::RatWeight rho = atlas::rootdata::rho(rd);

    const atlas::Weight lambda_rho = rc.lambda_rho(p->sr);
    atlas::RatWeight lambda(lambda_rho, 1);
    lambda += rho;
    lambda.normalize();

    std::ostringstream out;
    out << lambda.denominator();
    const auto& num = lambda.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_param_nu_text(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_nu_text: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_nu_text: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    const auto rank = rc.rank();
    atlas::RatWeight nu = rc.nu(p->sr);
    nu.normalize();

    std::ostringstream out;
    out << nu.denominator();
    const auto& num = nu.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

static atlas::K_repr::K_type_pol full_deform_stdrep(atlas::repr::Rep_table& rt,
                                                    const atlas::repr::StandardRepr& sr_in)
{
  atlas::repr::K_type_nr_poly result;
  auto finals = rt.finals_for(sr_in);
  for (auto it = finals.begin(); not finals.at_end(it); ++it)
  {
    atlas::repr::StandardRepr sr = it->first;
    const int mult = it->second;
    for (auto&& term : rt.full_deformation(sr))
    {
      atlas::Split_integer c = term.second;
      c *= mult;
      result.add_term(term.first, c);
    }
  }
  return atlas::repr::export_K_type_pol(rt, result);
}

extern "C" long atlas_param_height(void* param_handle)
{
  try
  {
    if (param_handle == nullptr)
    {
      g_last_error = "atlas_param_height: null param handle";
      return -1;
    }
    auto* p = static_cast<ParamHandle*>(param_handle);
    return static_cast<long>(p->sr.height());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_param_gamma_text(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_gamma_text: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_gamma_text: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    const auto rank = rc.rank();
    const auto& gamma = p->sr.gamma();

    std::ostringstream out;
    out << gamma.denominator();
    const auto& num = gamma.numerator();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << num[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_cross(void* p_handle, int s)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_cross: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_cross: null group pointer in param";
      return nullptr;
    }

    atlas::repr::Rep_context rc(p->group->G);
    const unsigned int r =
      atlas::rootdata::integrality_rank(rc.root_datum(), p->sr.gamma());
    if (s < 0 || static_cast<unsigned int>(s) >= r)
    {
      std::ostringstream out;
      out << "atlas_param_cross: illegal simple reflection: " << s << ", should be <" << r;
      g_last_error = out.str();
      return nullptr;
    }

    atlas::repr::StandardRepr sr = rc.cross(static_cast<atlas::weyl::Generator>(s), p->sr);
    return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_cayley(void* p_handle, int s)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_cayley: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_cayley: null group pointer in param";
      return nullptr;
    }

    atlas::repr::Rep_context rc(p->group->G);
    const unsigned int r =
      atlas::rootdata::integrality_rank(rc.root_datum(), p->sr.gamma());
    if (s < 0 || static_cast<unsigned int>(s) >= r)
    {
      std::ostringstream out;
      out << "atlas_param_cayley: illegal simple reflection: " << s << ", should be <" << r;
      g_last_error = out.str();
      return nullptr;
    }

    try
    {
      atlas::repr::StandardRepr sr = rc.Cayley(static_cast<atlas::weyl::Generator>(s), p->sr);
      return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
    }
    catch (atlas::error::Cayley_error&)
    {
      atlas::repr::StandardRepr sr = p->sr;
      return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
    }
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_scale(void* p_handle, int num, int den)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_scale: null param handle";
      return nullptr;
    }
    if (den == 0)
    {
      g_last_error = "atlas_param_scale: zero denominator";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_scale: null group pointer in param";
      return nullptr;
    }

    atlas::repr::Rep_context rc(p->group->G);
    atlas::RatNum f(num, den);
    f.normalize();
    atlas::repr::StandardRepr sr = rc.scale(p->sr, f);
    return static_cast<void*>(new ParamHandle(p->group, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_param_reducibility_points_text(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_reducibility_points_text: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_reducibility_points_text: null group pointer in param";
      return nullptr;
    }

    atlas::repr::Rep_context rc(p->group->G);
    atlas::RatNumList rp = rc.reducibility_points(p->sr);

    std::ostringstream out;
    out << rp.size();
    for (const auto& r : rp)
      out << ' ' << r.numerator() << ' ' << r.denominator();
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_param_is_standard(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_is_standard: null param handle";
      return 0;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_is_standard: null group pointer in param";
      return 0;
    }
    atlas::repr::Rep_context rc(p->group->G);
    return rc.is_standard(p->sr) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_param_is_final(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_is_final: null param handle";
      return 0;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_is_final: null group pointer in param";
      return 0;
    }
    atlas::repr::Rep_context rc(p->group->G);
    return rc.is_final(p->sr) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" void* atlas_param_twist(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_twist: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_twist: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    atlas::repr::StandardRepr sr2 = rc.inner_twisted(p->sr);
    return static_cast<void*>(new ParamHandle(p->group, std::move(sr2)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_param_equivalent(void* a_handle, void* b_handle)
{
  try
  {
    if (a_handle == nullptr || b_handle == nullptr)
    {
      g_last_error = "atlas_param_equivalent: null param handle";
      return 0;
    }
    const auto* a = static_cast<const ParamHandle*>(a_handle);
    const auto* b = static_cast<const ParamHandle*>(b_handle);
    if (a->group == nullptr || b->group == nullptr)
    {
      g_last_error = "atlas_param_equivalent: null group pointer in param";
      return 0;
    }
    if (a->group != b->group)
    {
      g_last_error = "atlas_param_equivalent: params belong to different groups";
      return 0;
    }
    atlas::repr::Rep_context rc(a->group->G);
    return rc.equivalent(a->sr, b->sr) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_param_is_hermitian(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_is_hermitian: null param handle";
      return 0;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_is_hermitian: null group pointer in param";
      return 0;
    }
    atlas::repr::Rep_context rc(p->group->G);
    atlas::repr::StandardRepr tw = rc.inner_twisted(p->sr);
    return rc.equivalent(tw, p->sr) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" long atlas_param_x(void* param_handle)
{
  try
  {
    if (param_handle == nullptr)
    {
      g_last_error = "atlas_param_x: null param handle";
      return -1;
    }
    auto* p = static_cast<ParamHandle*>(param_handle);
    return static_cast<long>(p->sr.x());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" void* atlas_param_full_deform(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_full_deform: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_full_deform: null group/Rep_table";
      return nullptr;
    }
    auto& rt = *p->group->rt;

    atlas::K_repr::K_type_pol poly = full_deform_stdrep(rt, p->sr);
    return static_cast<void*>(new KTypePolHandle(p->group, std::move(poly)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_param_c_form_irreducible(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_c_form_irreducible: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_c_form_irreducible: null group/Rep_table";
      return nullptr;
    }
    auto& rt = *p->group->rt;

    if (!rt.is_standard(p->sr))
    {
      g_last_error = "atlas_param_c_form_irreducible: parameter not standard";
      return nullptr;
    }
    if (!rt.is_final(p->sr))
    {
      g_last_error = "atlas_param_c_form_irreducible: parameter not final (needed for KL_sum_at_s)";
      return nullptr;
    }

    const unsigned int ori_p = rt.orientation_number(p->sr);
    atlas::repr::SR_poly kl = rt.KL_column_at_s(p->sr);

    atlas::K_repr::K_type_pol result;
    for (const auto& term : kl)
    {
      atlas::Split_integer c = term.second;
      const auto& q = term.first;
      const unsigned int ori_q = rt.orientation_number(q);
      const unsigned int d = (ori_p - ori_q) & 3u;
      if (d == 2u)
        c = c.times_s();
      else if (d != 0u)
      {
        g_last_error = "atlas_param_c_form_irreducible: odd orientation difference";
        return nullptr;
      }

      atlas::repr::StandardRepr qc = atlas::weyl::alcove_center(rt, q);
      atlas::K_repr::K_type_pol cfq = full_deform_stdrep(rt, qc);
      result.add_multiple(std::move(cfq), c);
    }

    return static_cast<void*>(new KTypePolHandle(p->group, std::move(result)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_ktypepol_is_typewise_pure(void* kt_handle)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_is_typewise_pure: null handle";
      return 0;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    for (const auto& term : kt->poly)
    {
      const auto& c = term.second;
      if (!(c.e() == 0 || c.s() == 0))
        return 0;
    }
    return 1;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_ktypepol_is_pure(void* kt_handle)
{
  return atlas_ktypepol_is_typewise_pure(kt_handle);
}

extern "C" int atlas_ktypepol_impure_height(void* kt_handle)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_impure_height: null handle";
      return -2;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);

    int best = -1;
    for (const auto& term : kt->poly)
    {
      const auto& c = term.second;
      if (c.e() != 0 && c.s() != 0)
      {
        const int h = static_cast<int>(term.first.height());
        if (best < 0 || h < best)
          best = h;
      }
    }
    return best;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -2;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -2;
  }
}

extern "C" void atlas_ktypepol_free(void* kt_handle)
{
  try
  {
    delete static_cast<KTypePolHandle*>(kt_handle);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
  }
}

extern "C" void atlas_ktype_free(void* t_handle)
{
  try
  {
    delete static_cast<KTypeHandle*>(t_handle);
  }
  catch (...)
  {
  }
}

extern "C" void* atlas_param_K_type(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_K_type: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_K_type: null group pointer in param";
      return nullptr;
    }
    atlas::repr::Rep_context rc(p->group->G);
    atlas::K_repr::K_type t = rc.sr_K(p->sr);
    return static_cast<void*>(new KTypeHandle(p->group, std::move(t)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_ktype_parameter(void* t_handle)
{
  try
  {
    if (t_handle == nullptr)
    {
      g_last_error = "atlas_ktype_parameter: null K_type handle";
      return nullptr;
    }
    const auto* t = static_cast<const KTypeHandle*>(t_handle);
    if (t->group == nullptr)
    {
      g_last_error = "atlas_ktype_parameter: null group pointer in K_type";
      return nullptr;
    }
    atlas::repr::Rep_context rc(t->group->G);
    atlas::repr::StandardRepr sr = rc.sr(t->t);
    return static_cast<void*>(new ParamHandle(t->group, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_ktype_is_final(void* t_handle)
{
  try
  {
    if (t_handle == nullptr)
    {
      g_last_error = "atlas_ktype_is_final: null K_type handle";
      return 0;
    }
    const auto* t = static_cast<const KTypeHandle*>(t_handle);
    if (t->group == nullptr)
    {
      g_last_error = "atlas_ktype_is_final: null group pointer in K_type";
      return 0;
    }
    atlas::repr::Rep_context rc(t->group->G);
    return rc.is_final(t->t) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_ktype_x(void* t_handle)
{
  try
  {
    if (t_handle == nullptr)
    {
      g_last_error = "atlas_ktype_x: null K_type handle";
      return -1;
    }
    const auto* t = static_cast<const KTypeHandle*>(t_handle);
    return static_cast<int>(t->t.x());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_ktype_lambda_rho_text(void* t_handle)
{
  try
  {
    if (t_handle == nullptr)
    {
      g_last_error = "atlas_ktype_lambda_rho_text: null K_type handle";
      return store_result("-1");
    }
    const auto* t = static_cast<const KTypeHandle*>(t_handle);
    if (t->group == nullptr)
    {
      g_last_error = "atlas_ktype_lambda_rho_text: null group pointer in K_type";
      return store_result("-1");
    }
    const auto rank = t->group->G.rank();
    std::ostringstream out;
    out << rank;
    const auto& w = t->t.lambda_rho();
    for (std::size_t i = 0; i < rank; ++i)
      out << ' ' << w[i];
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void* atlas_ktype_new_from_x_lambda_rho_text(void* group_handle, int x, const char* lambda_rho_text)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_ktype_new_from_x_lambda_rho_text: null group handle";
      return nullptr;
    }
    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();

    std::vector<int> xs;
    if (!parse_int_list(lambda_rho_text, xs))
    {
      g_last_error = "atlas_ktype_new_from_x_lambda_rho_text: failed to parse lambda_rho text";
      return nullptr;
    }

    std::vector<int> nums;
    if (xs.size() == rank)
      nums = xs;
    else if (xs.size() == rank + 1 && xs[0] == static_cast<int>(rank))
      nums = std::vector<int>(xs.begin() + 1, xs.end());
    else
    {
      std::ostringstream msg;
      msg << "atlas_ktype_new_from_x_lambda_rho_text: expected " << rank
          << " ints (optionally preceded by rank), got " << xs.size();
      g_last_error = msg.str();
      return nullptr;
    }

    atlas::Weight w(static_cast<unsigned int>(rank));
    for (unsigned int i = 0; i < rank; ++i)
      w[i] = nums[i];

    atlas::K_repr::K_type t = rc.sr_K(static_cast<atlas::KGBElt>(x), std::move(w));
    return static_cast<void*>(new KTypeHandle(g, std::move(t)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_ktype_K_type_formula(void* t_handle, int cutoff)
{
  try
  {
    if (t_handle == nullptr)
    {
      g_last_error = "atlas_ktype_K_type_formula: null K_type handle";
      return nullptr;
    }
    if (cutoff < 0)
    {
      g_last_error = "atlas_ktype_K_type_formula: negative cutoff";
      return nullptr;
    }
    const auto* t = static_cast<const KTypeHandle*>(t_handle);
    if (t->group == nullptr)
    {
      g_last_error = "atlas_ktype_K_type_formula: null group pointer in K_type";
      return nullptr;
    }

    atlas::repr::Rep_context rc(t->group->G);
    atlas::K_repr::K_type t2 = t->t;
    atlas::K_repr::KT_pol kt_int = rc.K_type_formula(t2, static_cast<atlas::repr::level>(cutoff));

    atlas::K_repr::K_type_pol poly;
    for (const auto& term : kt_int)
      poly.add_term(term.first, atlas::Split_integer(term.second));

    return static_cast<void*>(new KTypePolHandle(t->group, std::move(poly)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" long atlas_ktypepol_num_terms(void* kt_handle)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_num_terms: null handle";
      return -1;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    // `Free_Abelian_light::size()` is only an *upper bound* (it counts storage,
    // including zero coefficients). We want the number of nonzero terms.
    return static_cast<long>(kt->poly.count_terms());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" const char* atlas_ktypepol_term_text(void* kt_handle, long index)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_term_text: null handle";
      return store_result("-1");
    }
    if (index < 0)
    {
      g_last_error = "atlas_ktypepol_term_text: negative index";
      return store_result("-1");
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    const std::size_t i = static_cast<std::size_t>(index);
    const std::size_t n_terms = kt->poly.count_terms();
    if (i >= n_terms)
    {
      g_last_error = "atlas_ktypepol_term_text: index out of range";
      return store_result("-1");
    }

    std::size_t cur = 0;
    for (const auto& term : kt->poly)
    {
      if (cur == i)
      {
        const auto& t = term.first;
        const auto& c = term.second;
        std::ostringstream out;
        out << c.e() << ' ' << c.s();
        out << ' ' << static_cast<long>(t.x());
        out << ' ' << static_cast<long>(t.height());
        const auto& lam = t.lambda_rho();
        for (std::size_t j = 0; j < lam.size(); ++j)
          out << ' ' << lam[j];
        return store_result(out.str());
      }
      ++cur;
    }

    g_last_error = "atlas_ktypepol_term_text: internal iteration failure";
    return store_result("-1");
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" void* atlas_ktypepol_to_ht(void* kt_handle, int cutoff)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_to_ht: null handle";
      return nullptr;
    }
    if (cutoff < 0)
    {
      g_last_error = "atlas_ktypepol_to_ht: negative cutoff";
      return nullptr;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    atlas::K_repr::K_type_pol result;
    for (const auto& term : kt->poly)
    {
      if (static_cast<int>(term.first.height()) <= cutoff)
        result.add_term(term.first, term.second);
    }
    return static_cast<void*>(new KTypePolHandle(kt->group, std::move(result)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_ktypepol_clone(void* kt_handle)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_clone: null handle";
      return nullptr;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    atlas::K_repr::K_type_pol poly = kt->poly.copy();
    return static_cast<void*>(new KTypePolHandle(kt->group, std::move(poly)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_ktypepol_add(void* a_handle, void* b_handle)
{
  try
  {
    if (a_handle == nullptr || b_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_add: null handle";
      return nullptr;
    }
    const auto* a = static_cast<const KTypePolHandle*>(a_handle);
    const auto* b = static_cast<const KTypePolHandle*>(b_handle);
    if (a->group != b->group)
    {
      g_last_error = "atlas_ktypepol_add: polynomials belong to different groups";
      return nullptr;
    }

    atlas::K_repr::K_type_pol result = a->poly.copy();
    result.add_multiple(b->poly, atlas::arithmetic::Split_integer(1, 0));
    return static_cast<void*>(new KTypePolHandle(a->group, std::move(result)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_ktypepol_scale_split(void* kt_handle, int e, int s)
{
  try
  {
    if (kt_handle == nullptr)
    {
      g_last_error = "atlas_ktypepol_scale_split: null handle";
      return nullptr;
    }
    const auto* kt = static_cast<const KTypePolHandle*>(kt_handle);
    atlas::arithmetic::Split_integer c(e, s);
    atlas::K_repr::K_type_pol result(kt->poly.cmp());
    result.add_multiple(kt->poly, c);
    return static_cast<void*>(new KTypePolHandle(kt->group, std::move(result)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_intmat_echelon(const char* mat_text)
{
  try
  {
    atlas::int_Matrix m;
    if (!parse_int_matrix_text(mat_text, m))
      return nullptr;

    atlas::int_Matrix col;
    bool flip = false;
    atlas::bitmap::BitMap piv = atlas::matreduc::column_echelon(m, col, flip);

    auto* h = new EchelonHandle();
    h->M = std::move(m);
    h->C = std::move(col);
    h->pivots.clear();
    h->pivots.reserve(piv.size());
    for (atlas::bitmap::BitMap::iterator it = piv.begin(); it(); ++it)
      h->pivots.push_back(static_cast<int>(*it));
    h->eps = flip ? -1 : 1;
    return static_cast<void*>(h);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" const char* atlas_intmat_echelon_M_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_echelon_M_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const EchelonHandle*>(handle);
    return store_result(int_matrix_to_text(h->M));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_echelon_C_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_echelon_C_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const EchelonHandle*>(handle);
    return store_result(int_matrix_to_text(h->C));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" const char* atlas_intmat_echelon_pivots_text(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_echelon_pivots_text: null handle";
      return store_result("-1");
    }
    const auto* h = static_cast<const EchelonHandle*>(handle);
    std::ostringstream out;
    out << h->pivots.size();
    for (int i : h->pivots)
      out << ' ' << i;
    return store_result(out.str());
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return store_result("-1");
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return store_result("-1");
  }
}

extern "C" int atlas_intmat_echelon_eps(void* handle)
{
  try
  {
    if (handle == nullptr)
    {
      g_last_error = "atlas_intmat_echelon_eps: null handle";
      return 0;
    }
    const auto* h = static_cast<const EchelonHandle*>(handle);
    return h->eps;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" void atlas_intmat_echelon_free(void* handle)
{
  try
  {
    delete static_cast<EchelonHandle*>(handle);
  }
  catch (...)
  {
  }
}

// Forward declaration: defined later alongside `atlas_param_is_unitary`.
static atlas::arithmetic::RatNum mu_ktype_convert_cform_hermitian(const atlas::repr::Rep_table& rt,
                                                                  const atlas::K_repr::K_type& t);

extern "C" void* atlas_param_hermitian_form_irreducible(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_hermitian_form_irreducible: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_hermitian_form_irreducible: null group/Rep_table";
      return nullptr;
    }

    // Only defined (for now) in the same equal-rank path used by `atlas_param_is_unitary`.
    atlas::repr::Rep_table& rt = *p->group->rt;
    const atlas::repr::StandardRepr& sr = p->sr;

    atlas::repr::StandardRepr tw = rt.inner_twisted(sr);
    if (!rt.equivalent(tw, sr))
    {
      g_last_error = "atlas_param_hermitian_form_irreducible: parameter not hermitian";
      return nullptr;
    }

    if (!rt.is_standard(sr))
    {
      g_last_error = "atlas_param_hermitian_form_irreducible: parameter not standard";
      return nullptr;
    }
    if (!rt.is_final(sr))
    {
      g_last_error = "atlas_param_hermitian_form_irreducible: parameter not final";
      return nullptr;
    }

    const unsigned int ori_p = rt.orientation_number(sr);
    atlas::repr::SR_poly kl = rt.KL_column_at_s(sr);

    atlas::K_repr::K_type_pol c_form;
    for (const auto& term : kl)
    {
      atlas::Split_integer c = term.second;
      const auto& q = term.first;
      const unsigned int ori_q = rt.orientation_number(q);
      const unsigned int d = (ori_p - ori_q) & 3u;
      if (d == 2u)
        c = c.times_s();
      else if (d != 0u)
      {
        g_last_error = "atlas_param_hermitian_form_irreducible: odd orientation difference";
        return nullptr;
      }

      atlas::repr::StandardRepr qc = atlas::weyl::alcove_center(rt, q);
      atlas::K_repr::K_type_pol cfq = full_deform_stdrep(rt, qc);
      c_form.add_multiple(std::move(cfq), c);
    }

    // convert_cform_hermitian: multiply each term by s^(mu(t)-mu(t0)).
    if (c_form.is_zero())
      return new KTypePolHandle(p->group, atlas::K_repr::K_type_pol{});

    const atlas::arithmetic::RatNum mu0 =
      mu_ktype_convert_cform_hermitian(rt, c_form.front().first);

    atlas::K_repr::K_type_pol herm_form;
    for (const auto& term : c_form)
    {
      atlas::Split_integer c = term.second;
      atlas::arithmetic::RatNum diff =
        mu_ktype_convert_cform_hermitian(rt, term.first) - mu0;
      diff.normalize();
      if (diff.denominator() != 1)
      {
        g_last_error = "atlas_param_hermitian_form_irreducible: mu difference is not integral";
        return nullptr;
      }
      if ((diff.numerator() & 1LL) != 0)
        c = c.times_s();
      herm_form.add_term(term.first, c);
    }

    return new KTypePolHandle(p->group, std::move(herm_form));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_param_is_unitary_c_form(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_is_unitary_c_form: null param handle";
      return 0;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_is_unitary_c_form: null group/Rep_table";
      return 0;
    }

    auto& rt = *p->group->rt;

    atlas::repr::StandardRepr tw = rt.inner_twisted(p->sr);
    if (!rt.equivalent(tw, p->sr))
      return 0; // not hermitian => not unitary

    if (!rt.is_standard(p->sr))
    {
      g_last_error = "atlas_param_is_unitary_c_form: parameter not standard";
      return 0;
    }
    if (!rt.is_final(p->sr))
    {
      g_last_error = "atlas_param_is_unitary_c_form: parameter not final";
      return 0;
    }

    const unsigned int ori_p = rt.orientation_number(p->sr);
    atlas::repr::SR_poly kl = rt.KL_column_at_s(p->sr);

    atlas::K_repr::K_type_pol result;
    for (const auto& term : kl)
    {
      atlas::Split_integer c = term.second;
      const auto& q = term.first;
      const unsigned int ori_q = rt.orientation_number(q);
      const unsigned int d = (ori_p - ori_q) & 3u;
      if (d == 2u)
        c = c.times_s();
      else if (d != 0u)
      {
        g_last_error = "atlas_param_is_unitary_c_form: odd orientation difference";
        return 0;
      }

      atlas::repr::StandardRepr qc = atlas::weyl::alcove_center(rt, q);
      atlas::K_repr::K_type_pol cfq = full_deform_stdrep(rt, qc);
      result.add_multiple(std::move(cfq), c);
    }

    for (const auto& t : result)
    {
      const auto& c = t.second;
      if (!(c.e() == 0 || c.s() == 0))
        return 0;
    }
    return 1;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

static atlas::arithmetic::RatNum mu_ktype_simple(const atlas::repr::Rep_table& rt,
                                                 const atlas::K_repr::K_type& t)
{
  using atlas::arithmetic::RatNum;

  const auto& rd = rt.root_datum();
  const auto& kgb = rt.kgb();
  const auto& theta = kgb.involution_matrix(t.x());

  atlas::Weight lambda_plus_rho = t.lambda_rho();
  lambda_plus_rho += rd.twoRho(); // lambda - rho + 2rho = lambda + rho

  atlas::Weight one_plus_theta_lambda_plus_rho = theta * lambda_plus_rho;
  one_plus_theta_lambda_plus_rho += lambda_plus_rho;

  atlas::RatCoweight torus_factor = kgb.torus_factor(t.x());
  atlas::RatCoweight rho_check = atlas::rootdata::rho_check(rd);
  atlas::RatCoweight tf_plus_rho_check = torus_factor + rho_check;

  RatNum dot = tf_plus_rho_check.dot_Q(one_plus_theta_lambda_plus_rho);
  dot /= 2;
  dot.normalize();
  return dot;
}

static atlas::RatCoweight half_sum_poscoroots(const atlas::RootDatum& rd,
                                              const atlas::RootNbrSet& pos_roots)
{
  atlas::Coweight sum(rd.rank());
  for (auto it = pos_roots.begin(); it(); ++it)
    sum += rd.coroot(*it);
  atlas::RatCoweight half(sum, 2);
  half.normalize();
  return half;
}

static atlas::RatCoweight rho_check_r_x(const atlas::repr::Rep_table& rt,
                                        atlas::KGBElt x)
{
  const auto& rd = rt.root_datum();
  const auto& i_tab = rt.involution_table();
  const auto inv = rt.kgb().inv_nr(x);
  atlas::RootNbrSet real_pos = i_tab.real_roots(inv) & rd.posroot_set();
  return half_sum_poscoroots(rd, real_pos);
}

static atlas::RatCoweight rho_check_i_x(const atlas::repr::Rep_table& rt,
                                        atlas::KGBElt x)
{
  const auto& rd = rt.root_datum();
  const auto& i_tab = rt.involution_table();
  const auto inv = rt.kgb().inv_nr(x);
  atlas::RootNbrSet imag_pos = i_tab.imaginary_roots(inv) & rd.posroot_set();
  return half_sum_poscoroots(rd, imag_pos);
}

static int dim_u_cap_p_theta_stable_parabolic(const atlas::repr::Rep_table& rt,
                                              atlas::KGBElt x)
{
  const auto& rd = rt.root_datum();
  const auto& kgb = rt.kgb();
  const auto& i_tab = rt.involution_table();

  const auto inv = kgb.inv_nr(x);
  const auto& theta = kgb.involution_matrix(x);

  // Weight defining the theta-stable parabolic: (1+theta)*rho.
  atlas::RatWeight rho = atlas::rootdata::rho(rd);
  atlas::RatWeight lambda = theta * rho + rho;
  lambda.normalize(); // (possibly) reduce denominator

  // Use the numerator as an integral representative; scaling doesn't change
  // the associated parabolic.
  atlas::matrix::Vector<atlas::arithmetic::Numer_t> lambda_dom = lambda.numerator();
  atlas::WeylWord w = rd.factor_dominant(lambda_dom);

  // Parabolic (S, cross(x,w)) where S are those simple coroots vanishing on
  // the dominant representative of lambda.
  atlas::KGBElt x_par = kgb.cross(x, w);
  const auto inv_par = kgb.inv_nr(x_par);

  std::vector<bool> in_Levi(rd.semisimple_rank(), false);
  for (atlas::weyl::Generator s = 0; s < rd.semisimple_rank(); ++s)
  {
    atlas::arithmetic::Numer_t eval = 0;
    const auto& alpha_v = rd.simpleCoroot(s);
    for (unsigned i = 0; i < rd.rank(); ++i)
      eval += static_cast<atlas::arithmetic::Numer_t>(alpha_v[i]) * lambda_dom[i];
    if (eval == 0)
      in_Levi[s] = true;
  }

  // Standard Levi coweight H: sum of fundamental_coweight(t) for t not in S.
  atlas::RatCoweight H(rd.rank());
  for (atlas::weyl::Generator s = 0; s < rd.semisimple_rank(); ++s)
    if (!in_Levi[s])
      H += rd.fundamental_coweight(s);
  H.normalize();

  // coweight used for compact/noncompact test on imaginary roots
  atlas::RatCoweight compact_test = rho_check_i_x(rt, x_par) + kgb.torus_factor(x_par);
  compact_test.normalize();

  const auto complex_set = i_tab.complex_roots(inv_par);
  const auto imaginary_set = i_tab.imaginary_roots(inv_par);

  int complex_count = 0;
  int noncompact_imag_count = 0;

  const atlas::RootNbrSet posroots = rd.posroot_set();
  for (auto it = posroots.begin(); it(); ++it)
  {
    const atlas::RootNbr alpha = *it;
    const auto dot = H.dot_Q(rd.root(alpha));
    if (dot.numerator() == 0)
      continue; // alpha in Levi => not in nilradical

    if (complex_set.isMember(alpha))
    {
      ++complex_count;
      continue;
    }

    if (imaginary_set.isMember(alpha))
    {
      auto eval = compact_test.dot_Q(rd.root(alpha));
      eval.normalize();
      if (eval.denominator() != 1)
        throw std::runtime_error("dim_u_cap_p: non-integral compact_test evaluation");
      if ((eval.numerator() & 1LL) != 0)
        ++noncompact_imag_count;
    }
  }

  if ((complex_count & 1) != 0)
    throw std::runtime_error("dim_u_cap_p: odd number of complex nilradical roots");

  return noncompact_imag_count + (complex_count / 2);
}

static int dim_u_cap_p_x_cached(const atlas::repr::Rep_table& rt, atlas::KGBElt x)
{
  // Cache per (Rep_table*, x) to avoid repeated parabolic/root scans.
  struct CacheKey
  {
    const atlas::repr::Rep_table* rt;
    atlas::KGBElt x;
    bool operator==(const CacheKey& o) const { return rt == o.rt && x == o.x; }
  };
  struct CacheKeyHash
  {
    std::size_t operator()(const CacheKey& k) const
    {
      return (reinterpret_cast<std::size_t>(k.rt) >> 4) ^ (static_cast<std::size_t>(k.x) * 1315423911u);
    }
  };

  static thread_local std::unordered_map<CacheKey, int, CacheKeyHash> cache;

  CacheKey key{&rt, x};
  auto it = cache.find(key);
  if (it != cache.end())
    return it->second;

  int v = dim_u_cap_p_theta_stable_parabolic(rt, x);
  cache.emplace(key, v);
  return v;
}

static atlas::arithmetic::RatNum mu_ktype_convert_cform_hermitian(const atlas::repr::Rep_table& rt,
                                                                  const atlas::K_repr::K_type& t)
{
  using atlas::arithmetic::RatNum;

  const auto& rd = rt.root_datum();
  const auto& kgb = rt.kgb();

  // In convert_c_form.at:
  //   mu_terms(KType tp,mat delta): lambda_rho_term = (g_l-rho_check_r(x))*tp.lambda_rho
  // with g_l = E.g-E.l = torus_factor(x)+rho_check(rd) (independent of delta).
  // For the split F4 verifier, delta is distinguished (and effectively trivial),
  // so the tau term vanishes; we keep the remaining two contributions:
  //   mu(tp,delta) = (torus_factor+rho_check-rho_check_r)*lambda_rho + dim_u_cap_p(x)
  atlas::RatCoweight g_l = kgb.torus_factor(t.x()) + atlas::rootdata::rho_check(rd);
  atlas::RatCoweight rho_r = rho_check_r_x(rt, t.x());
  atlas::RatCoweight coeff = g_l - rho_r;

  RatNum mu = coeff.dot_Q(t.lambda_rho());
  mu += dim_u_cap_p_x_cached(rt, t.x());
  mu.normalize();
  return mu;
}

static int atlas_param_is_unitary_impl(atlas::repr::Rep_table& rt, const atlas::repr::StandardRepr& sr)
{
  try
  {
    atlas::repr::StandardRepr tw = rt.inner_twisted(sr);
    if (!rt.equivalent(tw, sr))
      return 0; // not hermitian => not unitary

    if (!rt.is_standard(sr))
    {
      g_last_error = "atlas_param_is_unitary: parameter not standard";
      return 0;
    }
    if (!rt.is_final(sr))
    {
      g_last_error = "atlas_param_is_unitary: parameter not final";
      return 0;
    }

    // Equal-rank case: hermitian_form_irreducible(p) = c_form_irreducible(p)
    // converted by convert_cform_hermitian. For now we implement this path,
    // which is the one used by the F4_s verifier.
    const unsigned int ori_p = rt.orientation_number(sr);
    atlas::repr::SR_poly kl = rt.KL_column_at_s(sr);

    atlas::K_repr::K_type_pol c_form;
    for (const auto& term : kl)
    {
      atlas::Split_integer c = term.second;
      const auto& q = term.first;
      const unsigned int ori_q = rt.orientation_number(q);
      const unsigned int d = (ori_p - ori_q) & 3u;
      if (d == 2u)
        c = c.times_s();
      else if (d != 0u)
      {
        g_last_error = "atlas_param_is_unitary: odd orientation difference";
        return 0;
      }

      atlas::repr::StandardRepr qc = atlas::weyl::alcove_center(rt, q);
      atlas::K_repr::K_type_pol cfq = full_deform_stdrep(rt, qc);
      c_form.add_multiple(std::move(cfq), c);
    }

    if (c_form.is_zero())
      return 1; // vacuously pure

    // Early exit: mixed split coefficients in the c-form remain mixed under the
    // subsequent `convert_cform_hermitian` parity adjustments (multiplication by
    // `s` only swaps the integer and `s` parts). So if the c-form is already
    // not typewise pure, the parameter cannot be unitary.
    for (const auto& t : c_form)
    {
      const auto& c = t.second;
      if (!(c.e() == 0 || c.s() == 0))
        return 0;
    }

    // convert_cform_hermitian: multiply each term by s^(mu(t)-mu(t0)).
    // Since s^n only depends on parity, we just test odd/even differences.
    const atlas::arithmetic::RatNum mu0 =
      mu_ktype_convert_cform_hermitian(rt, c_form.front().first);

    atlas::K_repr::K_type_pol herm_form;
    for (const auto& term : c_form)
    {
      atlas::Split_integer c = term.second;
      atlas::arithmetic::RatNum diff =
        mu_ktype_convert_cform_hermitian(rt, term.first) - mu0;
      diff.normalize();
      if (diff.denominator() != 1)
      {
        g_last_error = "atlas_param_is_unitary: mu difference is not integral";
        return 0;
      }
      if ((diff.numerator() & 1LL) != 0)
        c = c.times_s();
      herm_form.add_term(term.first, c);
    }

    bool all_int = true;
    bool all_s = true;
    for (const auto& t : herm_form)
    {
      const auto& c = t.second;
      if (!(c.e() == 0 || c.s() == 0))
        return 0; // not even typewise pure
      if (c.s() != 0)
        all_int = false;
      if (c.e() != 0)
        all_s = false;
    }
    return (all_int || all_s) ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" int atlas_param_is_unitary(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_is_unitary: null param handle";
      return 0;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_is_unitary: null group/Rep_table";
      return 0;
    }

    return atlas_param_is_unitary_impl(*p->group->rt, p->sr);
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

namespace {

static std::string lie_type_to_short_string(const atlas::lietype::LieType& lt)
{
  std::ostringstream out;
  bool first = true;
  for (const auto& sf : lt)
  {
    if (!first)
      out << " x ";
    first = false;
    out << sf.type() << sf.rank();
  }
  return out.str();
}

static std::string subset_to_string(const std::vector<atlas::weyl::Generator>& S)
{
  std::ostringstream out;
  out << '[';
  for (std::size_t i = 0; i < S.size(); ++i)
  {
    if (i)
      out << ',';
    out << static_cast<unsigned int>(S[i]);
  }
  out << ']';
  return out.str();
}

static std::vector<atlas::KGBElt> down_neighbors(const atlas::kgb::KGB& kgb,
                                                 const std::vector<atlas::weyl::Generator>& S,
                                                 atlas::KGBElt x)
{
  std::vector<atlas::KGBElt> out;
  out.reserve(S.size() * 2);
  for (auto s : S)
  {
    const auto stat = kgb.status(s, x);
    if (stat == atlas::gradings::Status::Complex)
    {
      if (kgb.isDescent(s, x))
        out.push_back(kgb.cross(s, x)); // C-
      // C+ contributes nothing
    }
    else if (stat == atlas::gradings::Status::Real)
    {
      atlas::KGBElt y = kgb.any_Cayley(s, x);
      out.push_back(y);
      out.push_back(kgb.cross(s, y));
    }
    // ImaginaryCompact / ImaginaryNoncompact contribute nothing
  }
  return out;
}

static std::vector<atlas::KGBElt> ascents(const atlas::kgb::KGB& kgb,
                                         const std::vector<atlas::weyl::Generator>& S,
                                         atlas::KGBElt x)
{
  std::vector<atlas::KGBElt> out;
  out.reserve(S.size());
  for (auto s : S)
  {
    const auto stat = kgb.status(s, x);
    if (stat == atlas::gradings::Status::ImaginaryNoncompact)
      out.push_back(kgb.any_Cayley(s, x)); // nc ascent
    else if (stat == atlas::gradings::Status::Complex && !kgb.isDescent(s, x))
      out.push_back(kgb.cross(s, x)); // C+ ascent
  }
  return out;
}

static atlas::KGBElt maximal_for_S(const atlas::kgb::KGB& kgb,
                                  const std::vector<atlas::weyl::Generator>& S,
                                  atlas::KGBElt x)
{
  while (true)
  {
    auto ups = ascents(kgb, S, x);
    if (ups.empty())
      return x;
    x = ups.front();
  }
}

static std::vector<atlas::KGBElt> equivalence_class_of(const atlas::kgb::KGB& kgb,
                                                       const std::vector<atlas::weyl::Generator>& S,
                                                       atlas::KGBElt x_max)
{
  const std::size_t n = kgb.size();
  std::vector<uint8_t> seen(n, 0);
  std::vector<atlas::KGBElt> todo;
  todo.reserve(n);

  seen[static_cast<std::size_t>(x_max)] = 1;
  todo.push_back(x_max);

  for (std::size_t i = 0; i < todo.size(); ++i)
  {
    atlas::KGBElt x = todo[i];
    auto downs = down_neighbors(kgb, S, x);
    for (auto y : downs)
    {
      const std::size_t idx = static_cast<std::size_t>(y);
      if (idx < n && !seen[idx])
      {
        seen[idx] = 1;
        todo.push_back(y);
      }
    }
  }
  return todo;
}

static atlas::KGBElt class_min_by_length_then_number(const atlas::kgb::KGB& kgb,
                                                     const std::vector<atlas::KGBElt>& klass)
{
  atlas::KGBElt best = klass.front();
  unsigned int best_len = kgb.length(best);
  for (auto x : klass)
  {
    unsigned int len = kgb.length(x);
    if (len < best_len || (len == best_len && x < best))
    {
      best = x;
      best_len = len;
    }
  }
  return best;
}

static bool has_theta_stable_Levi(const atlas::RootDatum& rd,
                                  const atlas::WeightInvolution& theta,
                                  const std::vector<atlas::weyl::Generator>& S)
{
  std::vector<uint8_t> inS(rd.semisimple_rank(), 0);
  for (auto s : S)
    inS[static_cast<std::size_t>(s)] = 1;

  atlas::RatCoweight H(rd.rank());
  for (atlas::weyl::Generator t = 0; t < rd.semisimple_rank(); ++t)
    if (!inS[static_cast<std::size_t>(t)])
      H += rd.fundamental_coweight(t);
  H.normalize();

  for (auto s : S)
  {
    atlas::Weight alpha = rd.simpleRoot(s);
    atlas::Weight theta_alpha = theta * alpha;
    auto eval = H.dot_Q(theta_alpha);
    eval.normalize();
    if (eval.numerator() != 0)
      return false;
  }
  return true;
}

static bool is_dominant_for_G(const atlas::RootDatum& rdG, const atlas::RatWeight& v)
{
  const auto& num = v.numerator();
  for (atlas::weyl::Generator s = 0; s < rdG.semisimple_rank(); ++s)
  {
    const auto& alpha_v = rdG.simpleCoroot(s);
    atlas::arithmetic::Numer_t dot = 0;
    for (unsigned int i = 0; i < rdG.rank(); ++i)
      dot += static_cast<atlas::arithmetic::Numer_t>(alpha_v[i]) * num[i];
    if (dot < 0)
      return false;
  }
  return true;
}

static bool map_word_to_Levi(const atlas::WeylWord& wwG,
                             const std::vector<atlas::weyl::Generator>& S,
                             atlas::WeylWord& out)
{
  std::vector<int> map(64, -1);
  for (std::size_t i = 0; i < S.size(); ++i)
  {
    const auto s = static_cast<unsigned int>(S[i]);
    if (s >= map.size())
      map.resize(s + 1, -1);
    map[s] = static_cast<int>(i);
  }

  out.clear();
  out.reserve(wwG.size());
  for (auto s : wwG)
  {
    const auto si = static_cast<unsigned int>(s);
    if (si >= map.size() || map[si] < 0)
      return false;
    out.push_back(static_cast<atlas::weyl::Generator>(map[si]));
  }
  return true;
}

static atlas::WeylWord map_word_from_Levi(const atlas::WeylWord& wwL,
                                         const std::vector<atlas::weyl::Generator>& S)
{
  atlas::WeylWord wwG;
  wwG.reserve(wwL.size());
  for (auto sL : wwL)
    wwG.push_back(S[static_cast<std::size_t>(sL)]);
  return wwG;
}

} // namespace

extern "C" const char* atlas_param_good_range_induced_from_first_text(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_good_range_induced_from_first_text: null param handle";
      return nullptr;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr || p->group->rt == nullptr)
    {
      g_last_error = "atlas_param_good_range_induced_from_first_text: null group/Rep_table";
      return nullptr;
    }

    auto& G = p->group->G;
    auto& rtG = *p->group->rt;
    const auto& rdG = rtG.root_datum();
    const auto& kgbG = rtG.kgb();
    const atlas::KGBElt xG = p->sr.x();

    atlas::repr::Rep_context rcG(G);
    const atlas::RatWeight lambda_p = rcG.lambda(p->sr);
    const atlas::RatWeight nu_p = rcG.nu(p->sr);
    const atlas::RatWeight rhoG = atlas::rootdata::rho(rdG);

    const auto r = static_cast<unsigned int>(rdG.semisimple_rank());
    if (r > 20)
    {
      g_last_error = "atlas_param_good_range_induced_from_first_text: semisimple rank too large";
      return nullptr;
    }

    // Enumerate all subsets S of simple roots (sufficient for G2; exponential in rank).
    const std::size_t max_mask = static_cast<std::size_t>(1u) << r;
    for (std::size_t mask = 0; mask < max_mask; ++mask)
    {
      std::vector<atlas::weyl::Generator> S;
      for (unsigned int s = 0; s < r; ++s)
        if ((mask >> s) & 1u)
          S.push_back(static_cast<atlas::weyl::Generator>(s));

      // For induction from a proper Levi, S must be nonempty; but keep full S
      // since the .at script reports that case separately.
      if (S.empty())
        continue;

      const atlas::KGBElt x_max = maximal_for_S(kgbG, S, xG);
      const auto klass = equivalence_class_of(kgbG, S, x_max);
      const atlas::KGBElt x_min = class_min_by_length_then_number(kgbG, klass);

      if (kgbG.length(x_min) != 0)
        continue; // not closed => not theta-stable

      const auto& theta_xmax = kgbG.involution_matrix(x_max);
      if (!has_theta_stable_Levi(rdG, theta_xmax, S))
        continue;

      // Build Levi root datum (same ambient coordinates).
      atlas::int_Matrix sr_mat(rdG.rank(), static_cast<unsigned int>(S.size()));
      atlas::int_Matrix scr_mat(rdG.rank(), static_cast<unsigned int>(S.size()));
      for (std::size_t j = 0; j < S.size(); ++j)
      {
        const auto s = S[j];
        const auto& alpha = rdG.simpleRoot(s);
        const auto& alpha_v = rdG.simpleCoroot(s);
        for (unsigned int i = 0; i < rdG.rank(); ++i)
        {
          sr_mat(i, static_cast<unsigned int>(j)) = alpha[i];
          scr_mat(i, static_cast<unsigned int>(j)) = alpha_v[i];
        }
      }

      atlas::prerootdata::PreRootDatum prdL(sr_mat, scr_mat, /*prefer_co=*/false);
      const auto& theta0 = kgbG.involution_matrix(x_min); // should be distinguished
      atlas::innerclass::InnerClass icL(prdL, theta0);

      // Make sure involution table knows about the Cartan class at identity.
      atlas::TwistedInvolution tw_id; // identity
      atlas::CartanNbr cn = icL.class_number(tw_id);
      icL.generate_Cartan_orbit(cn);
      const atlas::BitMap& b = icL.Cartan_ordering().below(cn);
      for (auto it = b.begin(); it(); ++it)
        icL.generate_Cartan_orbit(*it);

      atlas::RatCoweight torus_factor = kgbG.torus_factor(x_min);
      // Project to theta-fixed subspace: (1+theta)/2.
      {
        auto& num = torus_factor.numerator();
        num += theta0.right_prod(num);
        torus_factor /= 2;
        torus_factor.normalize();
      }

      atlas::RatCoweight coch(0);
      atlas::RealFormNbr rf = atlas::innerclass::real_form_of(icL, tw_id, torus_factor, coch);
      atlas::TorusPart tp = atlas::realredgp::minimal_torus_part(icL, rf, coch, tw_id, torus_factor);

      atlas::realredgp::RealReductiveGroup L(icL, rf, coch, tp);
      atlas::repr::Rep_table rtL(L);
      atlas::repr::Rep_context rcL(L);
      const auto& kgbL = rtL.kgb();
      const auto& rdL = rtL.root_datum();
      const atlas::RatWeight rhoL = atlas::rootdata::rho(rdL);

      // Compute x_L = inverse_embed_KGB(x_G,L) by mapping the twisted involution word.
      atlas::WeylWord wwG = G.Weyl_group().word(kgbG.involution(xG).w());
      atlas::WeylWord wwL;
      if (!map_word_to_Levi(wwG, S, wwL))
        continue;

      atlas::TwistedInvolution twL = L.Weyl_group().element(wwL);
      atlas::TitsElt aL(icL.Tits_group(), kgbG.torus_part(xG), twL);
      atlas::KGBElt xL = kgbL.lookup(aL);
      if (xL == atlas::UndefKGB)
        continue;

      // Build p_L = parameter(x_L, lambda(p)-rho(G)+rho(L), nu(p)).
      atlas::RatWeight lambda_L = lambda_p - rhoG + rhoL;
      lambda_L.normalize();

      atlas::RatWeight lam_minus_rho = lambda_L - rhoL;
      lam_minus_rho.normalize();
      if (lam_minus_rho.denominator() != 1)
        continue; // not a valid parameter for L

      atlas::Weight lambda_rho_L(rdL.rank());
      for (unsigned int i = 0; i < rdL.rank(); ++i)
        lambda_rho_L[i] = static_cast<int>(lam_minus_rho.numerator()[i]);

      atlas::repr::StandardRepr srL = rcL.sr(xL, lambda_rho_L, nu_p);
      if (!rtL.is_final(srL))
        continue;

      // Weakly good: v = infchar(p_L) + rho(G)-rho(L) is dominant for G.
      atlas::RatWeight v = srL.gamma() + (rhoG - rhoL);
      v.normalize();
      if (!is_dominant_for_G(rdG, v))
        continue;

      // Induce back: construct x_G' by embedding x_L using the mapped word.
      atlas::WeylWord wwG_back = map_word_from_Levi(L.Weyl_group().word(kgbL.involution(xL).w()), S);
      atlas::TwistedInvolution twG_back = G.Weyl_group().element(wwG_back);
      atlas::TitsElt aG(G.innerClass().Tits_group(), kgbL.torus_part(xL), twG_back);
      atlas::KGBElt xG_back = kgbG.lookup(aG);
      if (xG_back == atlas::UndefKGB)
        continue;

      // Induced parameter has the same (lambda,nu) as p; check it finalizes to p.
      atlas::RatWeight lam_minus_rho_G = lambda_p - rhoG;
      lam_minus_rho_G.normalize();
      if (lam_minus_rho_G.denominator() != 1)
        continue;
      atlas::Weight lambda_rho_G(rdG.rank());
      for (unsigned int i = 0; i < rdG.rank(); ++i)
        lambda_rho_G[i] = static_cast<int>(lam_minus_rho_G.numerator()[i]);

      atlas::repr::StandardRepr srG_back = rcG.sr(xG_back, lambda_rho_G, nu_p);
      auto finals = rtG.finals_for(srG_back);
      auto it = finals.begin();
      if (finals.at_end(it))
        continue;
      const auto only = *it;
      ++it;
      if (!finals.at_end(it))
        continue;
      if (!(only.first == p->sr && only.second == 1))
        continue;

      const int same = (S.size() == static_cast<std::size_t>(rdG.semisimple_rank())) ? 1 : 0;
      const int unitary = atlas_param_is_unitary_impl(rtL, srL);

      std::ostringstream desc;
      desc << "type=" << lie_type_to_short_string(rdL.type()) << " rf=" << rf << " S=" << subset_to_string(S);
      std::ostringstream out;
      out << "1|" << same << "|" << unitary << "|" << desc.str();
      return store_result(out.str());
    }

    return store_result("0|||");
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" void* atlas_group_new_levi_of_parabolic(void* group_handle,
                                                   const char* S_text,
                                                   int x)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_group_new_levi_of_parabolic: null group handle";
      return nullptr;
    }
    if (S_text == nullptr)
    {
      g_last_error = "atlas_group_new_levi_of_parabolic: null S_text";
      return nullptr;
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    const auto& rdG = g->G.root_datum();
    const auto& kgbG = g->G.kgb();

    if (x < 0 || static_cast<unsigned int>(x) >= kgbG.size())
    {
      g_last_error = "atlas_group_new_levi_of_parabolic: invalid KGB index";
      return nullptr;
    }

    std::vector<int> S_int;
    if (!parse_int_list(S_text, S_int))
      return nullptr;

    const auto ss_rank = static_cast<unsigned int>(rdG.semisimple_rank());
    std::vector<uint8_t> seen(ss_rank, 0);
    std::vector<atlas::weyl::Generator> S;
    S.reserve(S_int.size());
    for (int s : S_int)
    {
      if (s < 0 || static_cast<unsigned int>(s) >= ss_rank)
      {
        g_last_error = "atlas_group_new_levi_of_parabolic: generator index out of range";
        return nullptr;
      }
      if (seen[static_cast<unsigned int>(s)] != 0)
        continue;
      seen[static_cast<unsigned int>(s)] = 1;
      S.push_back(static_cast<atlas::weyl::Generator>(s));
    }

    atlas::KGBElt x_min = static_cast<atlas::KGBElt>(x);
    while (true)
    {
      auto downs = down_neighbors(kgbG, S, x_min);
      if (downs.empty())
        break;
      x_min = downs.front();
    }

    // Check theta-stability of the Levi: for t not in S, H=fund_coweight sum,
    // require H*(theta*alpha_s)=0 for all s in S.
    const auto& theta = kgbG.involution_matrix(x_min);
    if (!has_theta_stable_Levi(rdG, theta, S))
    {
      g_last_error = "atlas_group_new_levi_of_parabolic: Levi factor is not theta-stable";
      return nullptr;
    }

    // Build Levi root datum as in basic.at sub_datum(rd,S): keep ambient rank.
    atlas::int_Matrix sr_mat(rdG.rank(), static_cast<unsigned int>(S.size()));
    atlas::int_Matrix scr_mat(rdG.rank(), static_cast<unsigned int>(S.size()));
    for (std::size_t j = 0; j < S.size(); ++j)
    {
      const auto s = S[j];
      const auto& alpha = rdG.simpleRoot(s);
      const auto& alpha_v = rdG.simpleCoroot(s);
      for (unsigned int i = 0; i < rdG.rank(); ++i)
      {
        sr_mat(i, static_cast<unsigned int>(j)) = alpha[i];
        scr_mat(i, static_cast<unsigned int>(j)) = alpha_v[i];
      }
    }

    atlas::prerootdata::PreRootDatum prdL(sr_mat, scr_mat, /*prefer_co=*/false);

    atlas::RatCoweight torus_factor = kgbG.torus_factor(x_min);
    // Project torus_factor to theta-fixed: (1+theta)/2.
    {
      auto& num = torus_factor.numerator();
      num += theta.right_prod(num);
      torus_factor /= 2;
      torus_factor.normalize();
    }

    // Determine which (weak) real form in the Levi inner class this describes.
    atlas::innerclass::InnerClass icTmp(prdL, theta);
    atlas::TwistedInvolution tw_id;
    atlas::CartanNbr cn = icTmp.class_number(tw_id);
    icTmp.generate_Cartan_orbit(cn);
    const atlas::BitMap& b = icTmp.Cartan_ordering().below(cn);
    for (auto it = b.begin(); it(); ++it)
      icTmp.generate_Cartan_orbit(*it);

    atlas::RatCoweight coch(0);
    atlas::RealFormNbr rf = atlas::innerclass::real_form_of(icTmp, tw_id, torus_factor, coch);
    atlas::TorusPart tp =
      atlas::realredgp::minimal_torus_part(icTmp, rf, coch, tw_id, torus_factor);

    return static_cast<void*>(
      new GroupHandle(std::move(prdL), atlas::WeightInvolution(theta), rf, coch, tp));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

static atlas::RatWeight ratweight_from_int32(const int32_t* nums,
                                             std::size_t n,
                                             int denom)
{
  using Numer = atlas::arithmetic::Numer_t;
  atlas::matrix::Vector<Numer> v(n);
  for (std::size_t i = 0; i < n; ++i)
    v[i] = static_cast<Numer>(nums[i]);
  return atlas::RatWeight(v, static_cast<Numer>(denom));
}

extern "C" void* atlas_param_new_from_lambda_nu(void* group_handle,
                                               int x,
                                               int lambda_denom,
                                               const int32_t* lambda_nums,
                                               int nu_denom,
                                               const int32_t* nu_nums)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_param_new_from_lambda_nu: null group handle";
      return nullptr;
    }
    if (lambda_nums == nullptr || nu_nums == nullptr)
    {
      g_last_error = "atlas_param_new_from_lambda_nu: null vector pointer";
      return nullptr;
    }
    if (lambda_denom == 0 || nu_denom == 0)
    {
      g_last_error = "atlas_param_new_from_lambda_nu: zero denominator";
      return nullptr;
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();

    if (x < 0 || static_cast<unsigned int>(x) >= g->G.KGB_size())
    {
      g_last_error = "atlas_param_new_from_lambda_nu: invalid KGB index";
      return nullptr;
    }

    atlas::RatWeight lambda = ratweight_from_int32(lambda_nums, rank, lambda_denom);
    atlas::RatWeight nu = ratweight_from_int32(nu_nums, rank, nu_denom);

    atlas::RatWeight rho = atlas::rootdata::rho(rc.root_datum());
    atlas::RatWeight lam_minus_rho = lambda - rho;
    lam_minus_rho.normalize();

    if (lam_minus_rho.denominator() != 1)
    {
      g_last_error = "atlas_param_new_from_lambda_nu: lambda-rho not integral";
      return nullptr;
    }

    const auto& num = lam_minus_rho.numerator();
    atlas::Weight lambda_rho(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const auto v = num[i];
      if (v < std::numeric_limits<int>::min() || v > std::numeric_limits<int>::max())
      {
        g_last_error = "atlas_param_new_from_lambda_nu: lambda-rho entry out of int range";
        return nullptr;
      }
      lambda_rho[i] = static_cast<int>(v);
    }

    atlas::repr::StandardRepr sr =
      rc.sr(static_cast<atlas::KGBElt>(x), lambda_rho, nu);

    return static_cast<void*>(new ParamHandle(g, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

extern "C" int atlas_param_equal(void* a_handle, void* b_handle)
{
  try
  {
    if (a_handle == nullptr || b_handle == nullptr)
    {
      g_last_error = "atlas_param_equal: null param handle";
      return 0;
    }
    const auto* a = static_cast<const ParamHandle*>(a_handle);
    const auto* b = static_cast<const ParamHandle*>(b_handle);
    return a->sr == b->sr ? 1 : 0;
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return 0;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return 0;
  }
}

extern "C" long atlas_param_hash(void* p_handle, long modulus)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_hash: null param handle";
      return -1;
    }
    if (modulus <= 0)
    {
      g_last_error = "atlas_param_hash: modulus must be positive";
      return -1;
    }
    const auto* p = static_cast<const ParamHandle*>(p_handle);
    const auto m = static_cast<std::size_t>(modulus);
    return static_cast<long>(p->sr.hashCode(m));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return -1;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return -1;
  }
}

extern "C" void* atlas_param_contragredient(void* p_handle)
{
  try
  {
    if (p_handle == nullptr)
    {
      g_last_error = "atlas_param_contragredient: null param handle";
      return nullptr;
    }

    const auto* p = static_cast<const ParamHandle*>(p_handle);
    if (p->group == nullptr)
    {
      g_last_error = "atlas_param_contragredient: null group pointer in param";
      return nullptr;
    }

    auto* g = p->group;
    atlas::repr::Rep_context rc(g->G);
    const auto& rd = rc.root_datum();
    const auto& W = rc.Weyl_group();
    const auto w0 = W.longest();
    const auto ww = W.word(w0);

    atlas::RatWeight rho = atlas::rootdata::rho(rd);

    const atlas::Weight lambda_rho = rc.lambda_rho(p->sr);
    atlas::RatWeight lambda(lambda_rho, 1);
    lambda += rho;

    atlas::RatWeight nu = rc.nu(p->sr);

    rd.act(ww, lambda);
    rd.act(ww, nu);
    lambda.negate();
    nu.negate();

    atlas::RatWeight lam_minus_rho = lambda - rho;
    lam_minus_rho.normalize();
    if (lam_minus_rho.denominator() != 1)
    {
      g_last_error = "atlas_param_contragredient: lambda-rho not integral";
      return nullptr;
    }

    const auto& num = lam_minus_rho.numerator();
    const auto rank = rc.rank();
    atlas::Weight lambda_rho2(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const auto v = num[i];
      if (v < std::numeric_limits<int>::min() || v > std::numeric_limits<int>::max())
      {
        g_last_error = "atlas_param_contragredient: lambda-rho entry out of int range";
        return nullptr;
      }
      lambda_rho2[i] = static_cast<int>(v);
    }

    const atlas::KGBElt x2 = rc.kgb().cross(ww, p->sr.x());
    atlas::repr::StandardRepr sr2 = rc.sr(x2, lambda_rho2, nu);
    return static_cast<void*>(new ParamHandle(g, std::move(sr2)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}

static bool parse_int32_list(const char* text, std::size_t n, std::vector<int32_t>& out)
{
  if (text == nullptr)
  {
    g_last_error = "parse_int32_list: null text";
    return false;
  }

  std::istringstream in(text);
  out.clear();
  out.reserve(n);
  for (std::size_t i = 0; i < n; ++i)
  {
    long long v = 0;
    if (!(in >> v))
    {
      g_last_error = "parse_int32_list: too few integers";
      return false;
    }
    if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
    {
      g_last_error = "parse_int32_list: integer out of int32 range";
      return false;
    }
    out.push_back(static_cast<int32_t>(v));
  }
  // ensure there are no extra tokens (beyond whitespace)
  long long extra = 0;
  if (in >> extra)
  {
    g_last_error = "parse_int32_list: too many integers";
    return false;
  }
  return true;
}

extern "C" void* atlas_param_new_from_lambda_nu_text(void* group_handle,
                                                    int x,
                                                    const char* lambda_nums_text,
                                                    int lambda_denom,
                                                    const char* nu_nums_text,
                                                    int nu_denom)
{
  try
  {
    if (group_handle == nullptr)
    {
      g_last_error = "atlas_param_new_from_lambda_nu_text: null group handle";
      return nullptr;
    }
    if (lambda_denom == 0 || nu_denom == 0)
    {
      g_last_error = "atlas_param_new_from_lambda_nu_text: zero denominator";
      return nullptr;
    }

    auto* g = static_cast<GroupHandle*>(group_handle);
    atlas::repr::Rep_context rc(g->G);
    const auto rank = rc.rank();

    if (x < 0 || static_cast<unsigned int>(x) >= g->G.KGB_size())
    {
      g_last_error = "atlas_param_new_from_lambda_nu_text: invalid KGB index";
      return nullptr;
    }

    std::vector<int32_t> lambda_nums;
    std::vector<int32_t> nu_nums;
    if (!parse_int32_list(lambda_nums_text, rank, lambda_nums))
      return nullptr;
    if (!parse_int32_list(nu_nums_text, rank, nu_nums))
      return nullptr;

    atlas::RatWeight lambda = ratweight_from_int32(lambda_nums.data(), rank, lambda_denom);
    atlas::RatWeight nu = ratweight_from_int32(nu_nums.data(), rank, nu_denom);

    atlas::RatWeight rho = atlas::rootdata::rho(rc.root_datum());
    atlas::RatWeight lam_minus_rho = lambda - rho;
    lam_minus_rho.normalize();

    if (lam_minus_rho.denominator() != 1)
    {
      g_last_error = "atlas_param_new_from_lambda_nu_text: lambda-rho not integral";
      return nullptr;
    }

    const auto& num = lam_minus_rho.numerator();
    atlas::Weight lambda_rho(rank);
    for (std::size_t i = 0; i < rank; ++i)
    {
      const auto v = num[i];
      if (v < std::numeric_limits<int>::min() || v > std::numeric_limits<int>::max())
      {
        g_last_error = "atlas_param_new_from_lambda_nu_text: lambda-rho entry out of int range";
        return nullptr;
      }
      lambda_rho[i] = static_cast<int>(v);
    }

    atlas::repr::StandardRepr sr =
      rc.sr(static_cast<atlas::KGBElt>(x), lambda_rho, nu);

    return static_cast<void*>(new ParamHandle(g, std::move(sr)));
  }
  catch (const std::exception& e)
  {
    g_last_error = e.what();
    return nullptr;
  }
  catch (...)
  {
    g_last_error = "unknown C++ exception";
    return nullptr;
  }
}
