#include <exception>
#include <memory>
#include <string>

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
