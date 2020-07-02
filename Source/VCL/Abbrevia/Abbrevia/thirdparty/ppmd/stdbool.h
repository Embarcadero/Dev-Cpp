#ifndef _STDBOOL_H
#define _STDBOOL_H

#if !defined(__BORLANDC__)
#include <stdint.h>
typedef int8_t _Bool; 
#endif

/* ISO C Standard: 7.16 Boolean type */
#define bool _Bool
#define true 1
#define false 0
#define __bool_true_false_are_defined 1

#endif
