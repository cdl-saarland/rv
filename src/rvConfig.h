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
// debug flags
//----------------------------------------------------------------------------//

#ifdef RV_DEBUG
#   define IF_VERBOSE if (true)
#   define IF_DEBUG if (true)
#else
#   define IF_VERBOSE if (false)
#   define IF_DEBUG if (false)
#endif

//----------------------------------------------------------------------------//
// Misc
//----------------------------------------------------------------------------//

// Use this macro to silence warnings for variables that are
// only used in assertions.
#define RV_UNUSED(x) ((void)(x))


#endif // _RVCONFIG_H
