#include <exception>
#include <memory>
#include <sstream>
#include <string>
#include <cstdint>
#include <limits>
#include <vector>

#include "Atlas.h"
#include "lietype.h"
#include "prerootdata.h"
#include "rootdata.h"
#include "innerclass.h"
#include "realredgp.h"
#include "repr.h"

static thread_local std::string g_last_error;

extern "C" const char* atlas_last_error()
{
  return g_last_error.c_str();
}

static atlas::int_Matrix identity_matrix(unsigned int n)
{
  atlas::int_Matrix m(n, n);
  for (unsigned int i = 0; i < n; ++i)
    for (unsigned int j = 0; j < n; ++j)
      m(i, j) = (i == j) ? 1 : 0;
  return m;
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
  {}
};

struct ParamHandle
{
  GroupHandle* group; // non-owning; user must keep group alive
  atlas::repr::StandardRepr sr;

  ParamHandle(GroupHandle* group, atlas::repr::StandardRepr&& sr)
    : group(group), sr(std::move(sr))
  {}
};
} // namespace

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
