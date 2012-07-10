#ifndef __HTCOD_CONSOLE_H
#define __HTCOD_CONSOLE_H

#include <stdlib.h>

#include "libtcod/libtcod.h"

void TCOD_console_delete_ptr(TCOD_console_t* c);

void TCOD_console_set_default_background_ptr(TCOD_console_t, TCOD_color_t*);
void TCOD_console_set_default_foreground_ptr(TCOD_console_t, TCOD_color_t*);

#endif
