/*
# C-Code for timing
# (c) 2011 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
# Created: 2011-12-11
# Last changed:  2011-12-11
*/

#define _TIMING_C_SRC

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

#include "timing.h"

#ifndef STANDALONE
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#endif


#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__) 

#include <windows.h>
#include <Winbase.h>  // for GetTickCount64

#else

#include <stdio.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

#endif



/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__) 

#else

#define THOUSAND 1000
#define MILLION 1000000
#define BILLION 1000000000

#endif


/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__) 

#else

timer_t time_ID;
struct itimerspec hightimer;
struct itimerspec lowtimer;

#endif

/*****************************************************************************/
/**                                                                         **/
/**                   PROTOTYPYPES OF LOCAL FUNCTIONS                       **/
/**                                                                         **/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

// unsigned long long TickOrigin;
// int TicksPerSec;
// int TicksPaused;

/*****************************************************************************/
/**                                                                         **/
/**                          GLOBAL VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

static unsigned long long TickOrigin;
static int TicksPerSec;
static int TicksPaused;

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__) 



/*
unsigned long long
get_tick_count(void)
{
#if WINVER >= 0x0600
    return GetTickCount64();
#else
    return GetTickCount();
#endif
}
*/

void initTicks(){
  LARGE_INTEGER li;
  QueryPerformanceFrequency(&li);
  TicksPerSec = li.QuadPart;
  QueryPerformanceCounter(&li);
  TickOrigin = li.QuadPart;
}

void resetTicks(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  TickOrigin = li.QuadPart;
  //TickOrigin = get_tick_count();
}

void doneTicks(){
}

int getTicksPerSec(){
  LARGE_INTEGER li;
  QueryPerformanceFrequency(&li);
  TicksPerSec = li.QuadPart;
  return TicksPerSec;
}

int getNewTicks(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  //return get_tick_count() - TickOrigin;
  return li.QuadPart - TickOrigin;
}

double getNewSecs(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  //return get_tick_count() - TickOrigin;
  return (li.QuadPart - TickOrigin)/TicksPerSec;
}

void pauseTicks(){
  TicksPaused = getNewTicks();
}
void resumeTicks(){
  resetTicks();
  TickOrigin -= TicksPaused;
}


#else



void resetTicks(){
	hightimer.it_interval.tv_sec = 0;
	hightimer.it_interval.tv_nsec = 0;
	hightimer.it_value.tv_sec = MILLION; /* a large number */
	hightimer.it_value.tv_nsec = 0;
	timer_settime(time_ID, 0, &hightimer, NULL);
}


void initTicks(){
	if (timer_create(CLOCK_REALTIME, NULL, &time_ID) != 0) {
	  error("Could not create a timer based on CLOCK_REALTIME");
	}
	resetTicks();
}

void doneTicks(){
	if (timer_delete(time_ID) != 0) {
	  error("Could not delete a timer based on CLOCK_REALTIME");
	}
}


int getTicksPerSec(){
  return MILLION;
}

int getNewTicks(){
  timer_gettime(time_ID, &lowtimer);
  return MILLION*(hightimer.it_value.tv_sec - lowtimer.it_value.tv_sec) + (hightimer.it_value.tv_nsec - lowtimer.it_value.tv_nsec)/THOUSAND;
}

double getNewSecs(){
  timer_gettime(time_ID, &lowtimer);
  return (hightimer.it_value.tv_sec - lowtimer.it_value.tv_sec) + (hightimer.it_value.tv_nsec - lowtimer.it_value.tv_nsec)/BILLION;
}

void pauseTicks(){
  timer_gettime(time_ID, &lowtimer);
  doneTicks();
}
void resumeTicks(){
  if (timer_create(CLOCK_REALTIME, NULL, &time_ID) != 0) {
    error("Could not create a timer based on CLOCK_REALTIME");
  }
  timer_settime(time_ID, 0, &lowtimer, NULL);
}

#endif



/*
double PCFreq = 0.0;
__int64 CounterStart = 0;

void StartCounter()
{
    LARGE_INTEGER li;
    if(!QueryPerformanceFrequency(&li))
        cout << "QueryPerformanceFrequency failed!\n";

    PCFreq = double(li.QuadPart)/1000.0;

    QueryPerformanceCounter(&li);
    CounterStart = li.QuadPart;
}
double GetCounter()
{
    LARGE_INTEGER li;
    QueryPerformanceCounter(&li);
    return double(li.QuadPart-CounterStart)/PCFreq;
}
*/

/*****************************************************************************/
/**                                                                         **/
/**                           LOCAL FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

// static


/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/
