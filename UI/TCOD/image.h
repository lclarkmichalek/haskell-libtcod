#ifndef __HTCOD_IMAGE_H
#define __HTCOD_IMAGE_H

#include "libtcod/libtcod.h"

void TCOD_image_get_pixel_ptr(TCOD_image_t, int, int, TCOD_color_t*);
void TCOD_image_get_mipmap_ptr(TCOD_image_t, float, float, float, float, TCOD_color_t*);
void TCOD_image_clear_ptr(TCOD_image_t, TCOD_color_t*);
void TCOD_image_put_pixel_ptr(TCOD_image_t, int, int, TCOD_color_t*);
void TCOD_image_set_key_color_ptr(TCOD_image_t, TCOD_color_t*);

#endif
