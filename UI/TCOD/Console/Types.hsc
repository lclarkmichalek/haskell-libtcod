{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module UI.TCOD.Console.Types where

import Foreign.C.Types
import Foreign.Storable

#include "libtcod/libtcod.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype Keycode = Keycode CInt
                deriving (Show, Eq)

data KeyEvent = KeyEvent
                { eventCode :: Keycode
                , eventCharacter :: Maybe Char
                , eventPressed :: Bool
                , lAlt, lCtrl, rAlt, rCtrl, shift :: Bool
                } deriving (Show, Eq)

instance Storable KeyEvent where
  sizeOf _ = #{size TCOD_key_t}
  alignment _ = #{alignment TCOD_key_t}
  peek ptr = do
    code <- #{peek TCOD_key_t, vk} ptr
    char <- #{peek TCOD_key_t, c} ptr
    pressed <- #{peek TCOD_key_t, pressed} ptr
    lalt <- #{peek TCOD_key_t, lalt} ptr
    lctrl <- #{peek TCOD_key_t, lctrl} ptr
    ralt <- #{peek TCOD_key_t, ralt} ptr
    rctrl <- #{peek TCOD_key_t, lctrl} ptr
    shift <- #{peek TCOD_key_t, shift} ptr
    let char' = if (Keycode code) == kChar
                then Just char
                else Nothing
    return $ KeyEvent (Keycode code) char' pressed lalt lctrl ralt rctrl shift

  poke ptr (KeyEvent (Keycode code) mChar pressed lalt lctrl ralt rctrl shift) =
    do
      #{poke TCOD_key_t, vk} ptr code
      case mChar of
        Just char -> #{poke TCOD_key_t, c} ptr char
        Nothing -> #{poke TCOD_key_t, c} ptr '\0'
      #{poke TCOD_key_t, pressed} ptr pressed
      #{poke TCOD_key_t, lalt} ptr lalt
      #{poke TCOD_key_t, lctrl} ptr lctrl
      #{poke TCOD_key_t, ralt} ptr ralt
      #{poke TCOD_key_t, rctrl} ptr rctrl
      #{poke TCOD_key_t, shift} ptr shift

#{enum Keycode, Keycode,
 kNone = TCODK_NONE,
 kEscape = TCODK_ESCAPE,
 kBackspace = TCODK_BACKSPACE,
 kTab = TCODK_TAB,
 kEnter = TCODK_ENTER,
 kShift = TCODK_SHIFT,
 kControl = TCODK_CONTROL,
 kAlt = TCODK_ALT,
 kPause = TCODK_PAUSE,
 kCapslock = TCODK_CAPSLOCK,
 kPageup = TCODK_PAGEUP,
 kPagedown = TCODK_PAGEDOWN,
 kEnd = TCODK_END,
 kHome = TCODK_HOME,
 kUp = TCODK_UP,
 kLeft = TCODK_LEFT,
 kRight = TCODK_RIGHT,
 kDown = TCODK_DOWN,
 kPrintscreen = TCODK_PRINTSCREEN,
 kInsert = TCODK_INSERT,
 kDelete = TCODK_DELETE,
 kLwin = TCODK_LWIN,
 kRwin = TCODK_RWIN,
 kApps = TCODK_APPS,
 k0 = TCODK_0,
 k1 = TCODK_1,
 k2 = TCODK_2,
 k3 = TCODK_3,
 k4 = TCODK_4,
 k5 = TCODK_5,
 k6 = TCODK_6,
 k7 = TCODK_7,
 k8 = TCODK_8,
 k9 = TCODK_9,
 kKp0 = TCODK_KP0,
 kKp1 = TCODK_KP1,
 kKp2 = TCODK_KP2,
 kKp3 = TCODK_KP3,
 kKp4 = TCODK_KP4,
 kKp5 = TCODK_KP5,
 kKp6 = TCODK_KP6,
 kKp7 = TCODK_KP7,
 kKp8 = TCODK_KP8,
 kKp9 = TCODK_KP9,
 kKpadd = TCODK_KPADD,
 kKpsub = TCODK_KPSUB,
 kKpdiv = TCODK_KPDIV,
 kKpmul = TCODK_KPMUL,
 kKpdec = TCODK_KPDEC,
 kKpenter = TCODK_KPENTER,
 kF1 = TCODK_F1,
 kF2 = TCODK_F2,
 kF3 = TCODK_F3,
 kF4 = TCODK_F4,
 kF5 = TCODK_F5,
 kF6 = TCODK_F6,
 kF7 = TCODK_F7,
 kF8 = TCODK_F8,
 kF9 = TCODK_F9,
 kF10 = TCODK_F10,
 kF11 = TCODK_F11,
 kF12 = TCODK_F12,
 kNumlock = TCODK_NUMLOCK,
 kScrolllock = TCODK_SCROLLLOCK,
 kSpace = TCODK_SPACE,
 kChar = TCODK_CHAR
 }

newtype Character = Character CInt
                  deriving (Show, Eq)

#{enum Character, Character,
  cHline = TCOD_CHAR_HLINE,
  cVline = TCOD_CHAR_VLINE,
  cNe = TCOD_CHAR_NE,
  cNw = TCOD_CHAR_NW,
  cSe = TCOD_CHAR_SE,
  cSw = TCOD_CHAR_SW,
  cTeew = TCOD_CHAR_TEEW,
  cTeee = TCOD_CHAR_TEEE,
  cTeen = TCOD_CHAR_TEEN,
  cTees = TCOD_CHAR_TEES,
  cCross = TCOD_CHAR_CROSS,
  cDhline = TCOD_CHAR_DHLINE,
  cDvline = TCOD_CHAR_DVLINE,
  cDne = TCOD_CHAR_DNE,
  cDnw = TCOD_CHAR_DNW,
  cDse = TCOD_CHAR_DSE,
  cDsw = TCOD_CHAR_DSW,
  cDteew = TCOD_CHAR_DTEEW,
  cDteee = TCOD_CHAR_DTEEE,
  cDteen = TCOD_CHAR_DTEEN,
  cDtees = TCOD_CHAR_DTEES,
  cDcross = TCOD_CHAR_DCROSS,
  cBlock1 = TCOD_CHAR_BLOCK1,
  cBlock2 = TCOD_CHAR_BLOCK2,
  cBlock3 = TCOD_CHAR_BLOCK3,
  cArrowN = TCOD_CHAR_ARROW_N,
  cArrowS = TCOD_CHAR_ARROW_S,
  cArrowE = TCOD_CHAR_ARROW_E,
  cArrowW = TCOD_CHAR_ARROW_W,
  cArrow2N = TCOD_CHAR_ARROW2_N,
  cArrow2S = TCOD_CHAR_ARROW2_S,
  cArrow2E = TCOD_CHAR_ARROW2_E,
  cArrow2W = TCOD_CHAR_ARROW2_W,
  cDarrowH = TCOD_CHAR_DARROW_H,
  cDarrowV = TCOD_CHAR_DARROW_V,
  cCheckboxUnset = TCOD_CHAR_CHECKBOX_UNSET,
  cCheckboxSet = TCOD_CHAR_CHECKBOX_SET,
  cRadioUnset = TCOD_CHAR_RADIO_UNSET,
  cRadioSet = TCOD_CHAR_RADIO_SET,
  cSubpNw = TCOD_CHAR_SUBP_NW,
  cSubpNe = TCOD_CHAR_SUBP_NE,
  cSubpN = TCOD_CHAR_SUBP_N,
  cSubpSe = TCOD_CHAR_SUBP_SE,
  cSubpDiag = TCOD_CHAR_SUBP_DIAG,
  cSubpE = TCOD_CHAR_SUBP_E,
  cSubpSw = TCOD_CHAR_SUBP_SW,
  cSmilie = TCOD_CHAR_SMILIE,
  cSmilieInv = TCOD_CHAR_SMILIE_INV,
  cHeart = TCOD_CHAR_HEART,
  cDiamond = TCOD_CHAR_DIAMOND,
  cClub = TCOD_CHAR_CLUB,
  cSpade = TCOD_CHAR_SPADE,
  cBullet = TCOD_CHAR_BULLET,
  cBulletInv = TCOD_CHAR_BULLET_INV,
  cMale = TCOD_CHAR_MALE,
  cFemale = TCOD_CHAR_FEMALE,
  cNote = TCOD_CHAR_NOTE,
  cNoteDouble = TCOD_CHAR_NOTE_DOUBLE,
  cLight = TCOD_CHAR_LIGHT,
  cExclamDouble = TCOD_CHAR_EXCLAM_DOUBLE,
  cPilcrow = TCOD_CHAR_PILCROW,
  cSection = TCOD_CHAR_SECTION,
  cPound = TCOD_CHAR_POUND,
  cMultiplication = TCOD_CHAR_MULTIPLICATION,
  cFunction = TCOD_CHAR_FUNCTION,
  cReserved = TCOD_CHAR_RESERVED,
  cHalf = TCOD_CHAR_HALF,
  cOneQuarter = TCOD_CHAR_ONE_QUARTER,
  cCopyright = TCOD_CHAR_COPYRIGHT,
  cCent = TCOD_CHAR_CENT,
  cYen = TCOD_CHAR_YEN,
  cCurrency = TCOD_CHAR_CURRENCY,
  cThreeQuarters = TCOD_CHAR_THREE_QUARTERS,
  cDivision = TCOD_CHAR_DIVISION,
  cGrade = TCOD_CHAR_GRADE,
  cUmlaut = TCOD_CHAR_UMLAUT,
  cPow1 = TCOD_CHAR_POW1,
  cPow3 = TCOD_CHAR_POW3,
  cPow2 = TCOD_CHAR_POW2,
  cBulletSquare = TCOD_CHAR_BULLET_SQUARE
}

newtype ColCtrl = ColCtrl CInt
                deriving (Show, Eq)

#{enum ColCtrl, ColCtrl,
  col1 = TCOD_COLCTRL_1,
  col2 = TCOD_COLCTRL_2,
  col3 = TCOD_COLCTRL_3,
  col4 = TCOD_COLCTRL_4,
  col5 = TCOD_COLCTRL_5,
  colNumber = TCOD_COLCTRL_NUMBER,
  colForeRGB = TCOD_COLCTRL_FORE_RGB,
  colBackRGB = TCOD_COLCTRL_BACK_RGB
  }

newtype BackgroundFlag = BackgroundFlag CInt
                       deriving (Show, Eq)

#{enum BackgroundFlag, BackgroundFlag,
  backNone = TCOD_BKGND_NONE,
  backSet = TCOD_BKGND_SET,
  backMultiply = TCOD_BKGND_MULTIPLY,
  backLighten = TCOD_BKGND_LIGHTEN,
  backDarken = TCOD_BKGND_DARKEN,
  backScreen = TCOD_BKGND_SCREEN,
  backColorDodge = TCOD_BKGND_COLOR_DODGE,
  backColorBurn = TCOD_BKGND_COLOR_BURN,
  backAdd = TCOD_BKGND_ADD,
  backAdda = TCOD_BKGND_ADDA,
  backBurn = TCOD_BKGND_BURN,
  backOverlay = TCOD_BKGND_OVERLAY,
  backAlph = TCOD_BKGND_ALPH,
  backDefault = TCOD_BKGND_DEFAULT
 }

newtype KeyStatus = KeyStatus CInt
                  deriving (Show, Eq)

#{enum KeyStatus, KeyStatus,
  keyPressed = TCOD_KEY_PRESSED,
  keyReleased = TCOD_KEY_RELEASED
  }
-- The font flags, as defined in libtcod/console_types.h. Used to
-- define the font layout, and the font type.
newtype FontFlag = FontFlag CInt
                 deriving (Eq, Show)

#{enum FontFlag, FontFlag
 , fontLayoutASCIICol = TCOD_FONT_LAYOUT_ASCII_INCOL
 , fontLayoutASCIIRow = TCOD_FONT_LAYOUT_ASCII_INROW
 , fontTypeGreyscale = TCOD_FONT_TYPE_GREYSCALE
 , fontLayoutTCOD = TCOD_FONT_LAYOUT_TCOD
 }

{- The renderer enum type, defined in libtcod/console_types.h. Defines
   the renderer to be used. If the hardware does not support a
   render, libtcod will work it's way down the list of renders till
   it finds one that is supported.

   Using GLSL is recommended, and can have a 9x increase in speed on
   recent cards. -}
newtype Renderer = Renderer CInt
                 deriving (Eq, Show)

#{enum Renderer, Renderer,
  renderGLSL = TCOD_RENDERER_GLSL,
  renderOpenGL = TCOD_RENDERER_OPENGL,
  renderSDL = TCOD_RENDERER_SDL,
  renderNone = TCOD_NB_RENDERERS
 }

newtype Alignment = Alignment CInt
                  deriving (Eq, Show)

#{enum Alignment, Alignment,
  alignLeft = TCOD_LEFT,
  alignRight = TCOD_RIGHT,
  alignCenter = TCOD_CENTER
  }