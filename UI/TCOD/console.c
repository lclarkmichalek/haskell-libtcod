#include "console.h"

void TCOD_console_delete_ptr(TCOD_console_t* c) {
    TCOD_console_delete(*c);
    free(c);
}

void TCOD_console_set_default_background_ptr(TCOD_console_t con, TCOD_color_t* col) {
    TCOD_console_set_default_background(con, *col);
}

void TCOD_console_set_default_foreground_ptr(TCOD_console_t con, TCOD_color_t* col) {
    TCOD_console_set_default_foreground(con, *col);
}