/*
 * stringUtils.h
 *
 *  Created on: Nov 28, 2012
 *      Author: Simon Moll
 */

#ifndef STRINGUTILS_H_
#define STRINGUTILS_H_

#include <sstream>

template<typename T>
inline std::string str(const T & v)
{
	std::stringstream o; o << v; return o.str();
}


#endif /* STRINGUTILS_H_ */
