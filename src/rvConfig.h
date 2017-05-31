//===- rvConfig.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg, simon
//

#ifndef _RVCONFIG_H
#define	_RVCONFIG_H

// variables defined by corresponding compiler
// gcc     = __GNUC__
// icc     = __INTEL_COMPILER
// msvc    = _MSC_VER
// llvm    = __llvm__
// borland = __BORLANDC__

// variables defined by corresponding operating system
// windows (general) = _WIN32
// windows 64bit     = _WIN64
// linux             = __linux
// mac os            = __APPLE__ & __MACH__ (icc & gcc)

// http://predef.sourceforge.net/


//----------------------------------------------------------------------------//
// RegionVectorizer configuration defines
// These should be set by the build script
//----------------------------------------------------------------------------//

// Enable silent mode (no output at all)
// (default: deactivated)
//#define RV_SILENT_MODE

// Ignore alignment analysis and only generate aligned loads/stores.
// This may lead to segmentation faults.
// (default: deactivated)
//#define RV_FORCE_ALIGNED_MEMOPS

// Ignore alignment analysis and only generate unaligned loads/stores.
// (default: deactivated)
//#define RV_FORCE_UNALIGNED_MEMOPS



//----------------------------------------------------------------------------//
// debug flags
//----------------------------------------------------------------------------//

// debug macros
// do while and ((void)0) are used to enforce semicolon
// DEBUG_RV_VISIBLE allows arbitrary code that does not have its own scope
// NOTE: a boolean 'mVerbose' has to be in scope in order to use this ;)
#ifdef RV_DEBUG
#   define DEBUG_RV_NO_VERBOSE(x) do { x } while (0)
#   define DEBUG_RV(x) if (true) { x }
#   define DEBUG_RV_VISIBLE(x) x ((void)0)
#   define IF_VERBOSE if (true)
#   define IF_DEBUG if (true)
#else
#   define DEBUG_RV_NO_VERBOSE(x) ((void)0)
#   define DEBUG_RV(x) ((void)0)
#   define DEBUG_RV_VISIBLE(x) ((void)0)
#   define IF_VERBOSE if (false)
#   define IF_DEBUG if (false)
#endif

// VectorizationAnalysis should remain independent of RVInfo, so the verbose flag is
// stored separately
#ifdef RV_DEBUG
#   define DEBUG_VA_NO_VERBOSE(x) do { x } while (0)
// #   define DEBUG_VA(x) if (true) { x } // be quiet
#   define DEBUG_VA(x) ((void)0)
#   define DEBUG_VA_VISIBLE(x) x ((void)0)
#else
#   define DEBUG_VA_NO_VERBOSE(x) ((void)0)
#   define DEBUG_VA(x) ((void)0)
#   define DEBUG_VA_VISIBLE(x) ((void)0)
#endif

//----------------------------------------------------------------------------//
// Misc
//----------------------------------------------------------------------------//

// Use this macro to silence warnings for variables that are
// only used in assertions.
#define RV_UNUSED(x) ((void)(x))


// use ShuffleInsts to broadcast operands
// #define RV_USE_SHUFFLE_BROADCAST


#endif // _RVCONFIG_H
