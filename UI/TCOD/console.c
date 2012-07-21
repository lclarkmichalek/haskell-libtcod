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

void TCOD_console_set_char_background_ptr(TCOD_console_t con, int x, int y,
                                          TCOD_color_t* col,
                                          TCOD_bkgnd_flag_t bf) {
    TCOD_console_set_char_background(con, x, y, *col, bf);
}

void TCOD_console_set_char_foreground_ptr(TCOD_console_t con, int x, int y,
                                          TCOD_color_t* col) {
    TCOD_console_set_char_foreground(con, x, y, *col);
}


void TCOD_console_put_char_ex_ptr(TCOD_console_t con, int x, int y, int c,
                                  TCOD_color_t* fore, TCOD_color_t* back) {
    TCOD_console_put_char_ex(con, x, y, c, *fore, *back);
}

void TCOD_console_get_default_background_ptr(TCOD_console_t con, TCOD_color_t* col) {
    *col = TCOD_console_get_default_background(con);
}

void TCOD_console_get_default_foreground_ptr(TCOD_console_t con, TCOD_color_t* col) {
    *col = TCOD_console_get_default_foreground(con);
}

void TCOD_console_get_char_background_ptr(TCOD_console_t con, int x, int y,
                                          TCOD_color_t* col) {
    *col = TCOD_console_get_char_background(con, x, y);
}

void TCOD_console_get_char_foreground_ptr(TCOD_console_t con, int x, int y,
                                          TCOD_color_t* col) {
    *col = TCOD_console_get_char_foreground(con, x, y);
}

void TCOD_console_set_fade_ptr(uint8 f, TCOD_color_t* col) {
    TCOD_console_set_fade(f, *col);
}

void TCOD_console_get_fading_color_ptr(TCOD_color_t* col) {
    *col = TCOD_console_get_fading_color();
}

void TCOD_console_wait_for_keypress_ptr(bool flush, TCOD_key_t* k) {
    *k = TCOD_console_wait_for_keypress(flush);
}

void TCOD_console_check_for_keypress_ptr(int flags, TCOD_key_t* k) {
    *k = TCOD_console_check_for_keypress(flags);
}

void TCOD_console_set_key_color_ptr(TCOD_console_t con, TCOD_color_t* col) {
    TCOD_console_set_key_color(con, *col);
}
