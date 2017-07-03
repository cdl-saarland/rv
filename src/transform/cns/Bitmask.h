/*
 * Bitmask.h
 *
 *  Created on: 30.04.2010
 */

#ifndef BITMASK_HPP_
#define BITMASK_HPP_

#include <iostream>
#include <vector>

namespace rv {

typedef std::vector<bool> BoolVector;

/*
 * bit operations
 */
inline BoolVector AND(BoolVector a, BoolVector b) {
  BoolVector mask(a.size(), false);

  for (uint i = 0; i < a.size(); ++i) {
    mask[i] = a[i] && b[i];
  }

  return mask;
}

inline BoolVector OR(BoolVector a, BoolVector b) {
  BoolVector mask(a.size(), false);

  for (uint i = 0; i < a.size(); ++i) {
    mask[i] = a[i] || b[i];
  }

  return mask;
}

inline BoolVector NOT(BoolVector a) {
  BoolVector mask(a.size(), false);

  for (uint i = 0; i < a.size(); ++i) {
    mask[i] = !a[i];
  }

  return mask;
}

/*
 * quantors
 */
inline bool EXISTS(BoolVector a) {
  for (uint i = 0; i < a.size(); ++i) {
    if (a[i])
      return true;
  }

  return false;
}

inline bool NONE(BoolVector a) { return !EXISTS(a); }

inline bool ALL(BoolVector a) { return !EXISTS(NOT(a)); }

inline void dumpVector(BoolVector mask) {
  for (uint i = 0; i < mask.size(); ++i) {
    if (mask[i])
      std::cerr << "1";
    else
      std::cerr << "0";
  }
  std::cerr << "\n";
}
}

#endif /* BITMASK_HPP_ */
