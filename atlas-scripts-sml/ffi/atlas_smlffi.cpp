#include <exception>
#include <memory>
#include <sstream>
#include <string>
#include <cstdint>
#include <limits>
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

  explicit ParamListHandle(GroupHandle* group)
    : group(group), terms()
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
    return static_cast<long>(kt->poly.size());
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
    if (i >= kt->poly.size())
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
