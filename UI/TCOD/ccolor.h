#ifndef __HTCOD_COLOR_H
#define __HTCOD_COLOR_H

#include "libtcod/libtcod.h"

void TCOD_color_RGB_ptr(uint8, uint8, uint8, TCOD_color_t*);
void TCOD_color_HSV_ptr(float, float, float, TCOD_color_t*);

bool TCOD_color_equals_ptr(TCOD_color_t*, TCOD_color_t*);
void TCOD_color_add_ptr(TCOD_color_t*, TCOD_color_t*, TCOD_color_t*);
void TCOD_color_subtract_ptr(TCOD_color_t*, TCOD_color_t*, TCOD_color_t*);
void TCOD_color_multiply_ptr(TCOD_color_t*, TCOD_color_t*, TCOD_color_t*);
void TCOD_color_multiply_scalar_ptr(TCOD_color_t*, float, TCOD_color_t*);
void TCOD_color_lerp_ptr(TCOD_color_t*, TCOD_color_t*, float, TCOD_color_t*);

void TCOD_color_get_HSV_ptr(TCOD_color_t* c,float * h, float * s, float * v);
#endif
