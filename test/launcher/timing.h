/*
 * timing.hpp
 *
 *  Created on: Nov 10, 2015
 *      Author: simon
 */

#ifndef TESTS_LAUNCHER_TIMING_H_
#define TESTS_LAUNCHER_TIMING_H_


typedef unsigned int uint;

#define MEASURE_WALL_TIME

#ifdef MEASURE_WALL_TIME
#include <chrono>

static
std::chrono::time_point<std::chrono::high_resolution_clock>
GetTime() {
	return std::chrono::high_resolution_clock::now();
}


static
size_t
GetTimeDiff(std::chrono::time_point<std::chrono::high_resolution_clock> start, std::chrono::time_point<std::chrono::high_resolution_clock> end) {
	auto startNano = std::chrono::time_point_cast<std::chrono::nanoseconds>(start);
	auto startTime = startNano.time_since_epoch().count();
	auto endNano = std::chrono::time_point_cast<std::chrono::nanoseconds>(end);
	auto endTime = endNano.time_since_epoch().count();
	return endTime - startTime;
}


#else
#include <ctime>
#include <cassert>

static clock_t
GetTime() {
	clock_t now = clock();
	if (now == ((clock_t) -1)) abort();
	return now;
}

static
size_t
GetTimeDiff(clock_t start, clock_t end) {
	return (end - start);
}

#endif



#endif /* TESTS_LAUNCHER_TIMING_H_ */
