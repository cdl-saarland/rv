//===- src/rvConfig.h - some more helper functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

// TODO refactor & deprecate

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
#  define IF_VERBOSE if (true)
#  define IF_DEBUG if (true)
#else
#  define IF_VERBOSE if (false)
#  define IF_DEBUG if (false)
#endif

//----------------------------------------------------------------------------//
// Misc
//----------------------------------------------------------------------------//

// Use this macro to silence warnings for variables that are
// only used in assertions.
#define RV_UNUSED(x) ((void)(x))


#endif // _RVCONFIG_H

namespace rv {

template<typename N>
extern N
GetValue(const char * name, N defVal);

}
