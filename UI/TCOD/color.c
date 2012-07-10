#include "color.h"

void TCOD_color_RGB_ptr(uint8 r, uint8 g, uint8 b, TCOD_color_t* out) {
    *out = TCOD_color_RGB(r, g, b);
}

void TCOD_color_HSV_ptr(float h, float s, float v, TCOD_color_t* out) {
    *out = TCOD_color_HSV(h, s, v);
}

bool TCOD_color_equals_ptr(TCOD_color_t* c1, TCOD_color_t* c2) {
    return TCOD_color_equals(*c1, *c2);
}

void TCOD_color_add_ptr(TCOD_color_t* c1, TCOD_color_t* c2, TCOD_color_t* out) {
    *out = TCOD_color_add(*c1, *c2);
}

void TCOD_color_subtract_ptr(TCOD_color_t* c1, TCOD_color_t* c2, TCOD_color_t* out) {
    *out = TCOD_color_subtract(*c1, *c2);
}

void TCOD_color_multiply_ptr(TCOD_color_t* c1, TCOD_color_t* c2, TCOD_color_t* out) {
    *out = TCOD_color_multiply(*c1, *c2);
}

void TCOD_color_multiply_scalar_ptr(TCOD_color_t* c, float s, TCOD_color_t* out) {
    *out = TCOD_color_multiply_scalar(*c, s);
}

void TCOD_color_lerp_ptr(TCOD_color_t* c1, TCOD_color_t* c2, float l, TCOD_color_t* out) {
    *out = TCOD_color_lerp(*c1, *c2, l);
}

void TCOD_color_get_HSV_ptr(TCOD_color_t* c, float* h, float* s, float* v) {
    TCOD_color_get_HSV(*c, h, s, v);
}
