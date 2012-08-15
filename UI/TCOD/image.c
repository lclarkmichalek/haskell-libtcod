#include "image.h"

void TCOD_image_get_pixel_ptr(TCOD_image_t img, int x, int y, TCOD_color_t* col) {
    *col = TCOD_image_get_pixel(img, x, y);
}

void TCOD_image_get_mipmap_ptr(TCOD_image_t img, float x0, float y0, float x1, float y1, TCOD_color_t* col) {
    col* = TCOD_image_get_mipmap(img, x0, y0, x1, y1);
}

void TCOD_image_clear_ptr(TCOD_image_t img, TCOD_color_t* col) {
    TCOD_image_clear(img, *col);
}

void TCOD_image_put_pixel_ptr(TCOD_image_t img, int x, int y, TCOD_color_t* col) {
    TCOD_image_put_pixel(img, x, y, *col);
}

void TCOD_image_set_key_color_ptr(TCOD_image_t img, TCOD_color_t* col) {
    TCOD_image_set_key_color(img, *col);
}
