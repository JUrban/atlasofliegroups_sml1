#include <exception>
#include <memory>
#include <string>

#include "Atlas.h"
#include "lietype.h"
#include "prerootdata.h"
#include "rootdata.h"
#include "innerclass.h"
#include "realredgp.h"

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
struct F4SplitHandle
{
  atlas::lietype::LieType lt;
  atlas::prerootdata::PreRootDatum prd;
  atlas::rootdata::RootDatum rd;
  atlas::rootdata::RootDatum drd;
  atlas::WeightInvolution di;
  atlas::innerclass::InnerClass ic;
  atlas::realredgp::RealReductiveGroup G;

  F4SplitHandle()
    : lt()
    , prd([&] {
        lt.push_back(atlas::lietype::SimpleLieType('F', 4));
        return atlas::prerootdata::PreRootDatum(lt, /*prefer_co=*/false);
      }())
    , rd(prd)
    , drd(rd, atlas::tags::DualTag{})
    , di(identity_matrix(lt.rank()))
    , ic(rd, drd, di)
    , G(ic, ic.quasisplit())
  {}
};
} // namespace

extern "C" void* atlas_group_new_F4_s()
{
  try
  {
    auto* h = new F4SplitHandle();
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
    delete static_cast<F4SplitHandle*>(handle);
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
    auto* h = static_cast<F4SplitHandle*>(handle);
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

    F4SplitHandle h;
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
