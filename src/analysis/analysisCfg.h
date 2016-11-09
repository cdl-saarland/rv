//===- analysisCfg.h  -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//

#ifndef SRC_ANALYSIS_ANALYSISCFG_H_
#define SRC_ANALYSIS_ANALYSISCFG_H_

// #define PF_VERBOSE

// #define VA_VERIFY
// Debug output. Generate paths in new disjoint paths test.
// Enables the legacy(== reference) code paths based on path enumeration and runs the new algorithm against them

#ifdef VA_VERIFY
#define WFV_ENABLE_LEGACY_API
// enables the path enumeration API
// do not use this API for any new code!
#endif
#endif /* SRC_ANALYSIS_ANALYSISCFG_H_ */
