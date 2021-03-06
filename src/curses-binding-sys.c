/*****************************************************************************
**                                                                          **
**                        UNIX Terminal Control Package                     **
**                             (n)curses Binding                            **
**                                                                          **
** ************************************************************************ **
**                                                                          **
**  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              **
**  All rights reserved.                                                    **
**                                                                          **
**  Original Contributors:                                                  **
**  * Richard Wai (ANNEXI-STRAYLINE)                                        **
**                                                                          **
**                                                                          **
**  Redistribution and use in source and binary forms, with or without      **
**  modification, are permitted provided that the following conditions are  **
**  met:                                                                    **
**      * Redistributions of source code must retain the above copyright    **
**        notice, this list of conditions and the following disclaimer.     **
**                                                                          **
**      * Redistributions in binary form must reproduce the above copyright **
**        notice, this list of conditions and the following disclaimer in   **
**        the documentation and/or other materials provided with the        **
**        distribution.                                                     **
**                                                                          **
**      * Neither the name of ANNEXI-STRAYLINE nor the names of its         **
**        contributors may be used to endorse or promote products derived   **
**        from this software without specific prior written permission.     **
**                                                                          **
**  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     **
**  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       **
**  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A **
**  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ANNEXI-STRAYLINE   **
**  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR  **
**  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF    **
**  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR         **
**  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,   **
**  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR **
**  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  **
**  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                              **
**                                                                          **
*****************************************************************************/

#include <unistd.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#ifdef __NADACURSES_WIDE_SUPPORT
#include <locale.h>
#endif

#if defined (__NADACURSES_HOST_OS_LINUX) || \
    defined (__NADACURSES_HOST_OS_FREE_BSD)
#include <sys/ioctl.h>
#include <ncurses.h>

#elif defined (__NADACURSES_HOST_OS_SOLARIS) 
#include <unistd.h>
#include <termio.h>
#include <ncurses/ncurses.h>

#else
#error "Host OS not supported."
#endif



#define BOOL_FALSE 0
#define BOOL_TRUE  1

/**************/
/** Internal **/
/**************/
static inline void internal_set_attrs
(chtype * mod,
 unsigned bold,
 unsigned standout,
 unsigned dim,
 unsigned uline,
 unsigned invert,
 unsigned blink)
{
     if ( bold )
          *mod |= A_BOLD;

     if ( standout )
          *mod |= A_STANDOUT;

     if ( dim )
          *mod |= A_DIM;

     if ( uline )
          *mod |= A_UNDERLINE;

     if ( invert )
          *mod |= A_REVERSE;

     if ( blink )
          *mod |= A_BLINK;
}

static inline void internal_set_attrs_color
(chtype * mod,
 unsigned bold,
 unsigned standout,
 unsigned dim,
 unsigned uline,
 unsigned invert,
 unsigned blink,
 
 short pair)
{
     internal_set_attrs ( mod, bold, standout, dim, uline, invert, blink );
     *mod |= COLOR_PAIR(pair);
}


static inline bool internal_do_wborder
(WINDOW * win,
 
 unsigned bold,
 unsigned standout,
 unsigned dim,
 unsigned uline,
 unsigned invert,
 unsigned blink,

 chtype * ls,
 chtype * rs,
 chtype * ts,
 chtype * bs,
 chtype * tl,
 chtype * tr,
 chtype * bl,
 chtype * br)
{
     int retval;
     
     internal_set_attrs ( ls, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( rs, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( ts, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( bs, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( tl, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( tr, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( bl, bold, standout, dim, uline, invert, blink );
     internal_set_attrs ( br, bold, standout, dim, uline, invert, blink );

     retval = wborder ( win, *ls, *rs, *ts, *bs, *tl, *tr, *bl, *br );

     if ( retval == OK )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}


static inline bool internal_do_wborder_color
(WINDOW * win,
 
 unsigned bold,
 unsigned standout,
 unsigned dim,
 unsigned uline,
 unsigned invert,
 unsigned blink,

 short pair,

 chtype * ls,
 chtype * rs,
 chtype * ts,
 chtype * bs,
 chtype * tl,
 chtype * tr,
 chtype * bl,
 chtype * br)
{
     int retval;
     
     internal_set_attrs_color
          ( ls, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( rs, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( ts, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( bs, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( tl, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( tr, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( bl, bold, standout, dim, uline, invert, blink, pair );
     internal_set_attrs_color
          ( br, bold, standout, dim, uline, invert, blink, pair );

     retval = wborder ( win, *ls, *rs, *ts, *bs, *tl, *tr, *bl, *br );

     if ( retval == OK )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}


/******************/
/* Curses.Binding */
/******************/

/* function Sys_errno return int                    */
/*   with                                           */
/*   Import        => True,                         */
/*   Convention    => C,                            */
/*   External_Name => "__binding_curses_sys_errno"; */
int __binding_curses_sys_errno ( void )
{
     return (errno);
}


/* procedure CURSES_CURSES_init_lib                */
/*   with                                          */
/*   Import => True,                               */
/*   Convention => C,                              */
/*   External_Name => "__binding_curses_init_lib"; */

void __binding_curses_init_lib ( void )
{
#ifdef __NADACURSES_WIDE_SUPPORT
     setlocale ( LC_ALL, "" );
#endif
     return;
}


/****************************/
/* Curses.Binding.Terminals */
/****************************/

/* function CURSES_KEY_xyz_INIT return CURSES_Character       */
/*   with                                                     */
/*   Import        => True,                                   */
/*   Convention    => C,                                      */
/*   External_Name => "__binding_curses_key_xyz_init";        */

int __binding_curses_key_nokey_init     ( void ) { return (int)(ERR);          }
int __binding_curses_key_down_init      ( void ) { return (int)(KEY_DOWN);     }
int __binding_curses_key_up_init        ( void ) { return (int)(KEY_UP);       }
int __binding_curses_key_left_init      ( void ) { return (int)(KEY_LEFT);     }
int __binding_curses_key_right_init     ( void ) { return (int)(KEY_RIGHT);    }
int __binding_curses_key_home_init      ( void ) { return (int)(KEY_HOME);     }
int __binding_curses_key_end_init       ( void ) { return (int)(KEY_END);      }
int __binding_curses_key_insert_init    ( void ) { return (int)(KEY_IC);       }
int __binding_curses_key_delete_init    ( void ) { return (int)(KEY_DC);       }
int __binding_curses_key_backspace_init ( void ) { return (int)(KEY_BACKSPACE);}
int __binding_curses_key_enter_init     ( void ) { return (int)(KEY_ENTER);    }
int __binding_curses_key_pagedn_init    ( void ) { return (int)(KEY_NPAGE);    }
int __binding_curses_key_pageup_init    ( void ) { return (int)(KEY_PPAGE);    }
int __binding_curses_key_f0_init        ( void ) { return (int)(KEY_F0);       }
int __binding_curses_key_resize_init    ( void ) { return (int)(KEY_RESIZE);   }

/* function Curses.Binding.CURSES_set_term (Handle: Terminal_Handle) */
/*   return Terminal_Handle                                          */
/*     with                                                          */
/*     Import        => True,                                        */
/*     Convention    => C,                                           */
/*     External_Name => "__binding_curses_set_term";                 */

SCREEN * __binding_curses_set_term ( SCREEN * new )
{
     return ( set_term ( new ) );
}


/* function Curses.Binding.Terminals.CURSES_newterm           */
/*     (model: in char_array; outfd, infd: in System.Address) */
/*      return Terminal_Handle                                */
/*      with                                                  */
/*      Import        => True,                                */
/*      Convention    => C,                                   */
/*      External_Name => "__binding_curses_newterm";          */

SCREEN * __binding_curses_newterm
(char * type, FILE * outfd, FILE * infd)
{
     char * type_filter = type;

     if (type_filter != NULL)
     {
          if ( (type_filter[0] == '\0' || type_filter[0] == ' ') )
               type_filter = NULL;
     }

     return ( newterm ( type_filter, outfd, infd ) );
}


/* procedure CURSES_initterm return Surface_Handle */
/*   with                                          */
/*   Import        => True,                        */
/*   Convention    => C,                           */
/*   External_Name => "__binding_curses_initterm"; */

void __binding_curses_meta_initterm ( void )
{


     
     /* Cancel the window resize signal (SIGWINCH) */
#ifdef SIGWINCH
     struct sigaction ignore_action;
     memset ( &ignore_action, 0, sizeof (struct sigaction) );
     ignore_action.sa_handler = SIG_IGN;

     sigaction ( SIGWINCH, (const struct sigaction *)&ignore_action, NULL );

#else
#warning "SIGWINCH Signal not defined"
     
#endif

     noecho    ( );
     raw       ( );
     nonl      ( );
     
     timeout ( 0 );

     return;
}


/* procedure Curses.Binding.Terminals.CURSES_endwin  */
/*      with                                         */
/*      Import        => True,                       */
/*      Convention    => C,                          */
/*      External_Name => "__binding_curses_endwin";  */

int __binding_curses_endwin ( void )
{
     endwin ( );
}


/* procedure Curses.Binding.CURSES_delscreen (sp: in Terminal_Handle) */
/*    with                                                            */
/*    Import        => True,                                          */
/*    Convention    => C,                                             */
/*    External_Name => "__binding_curses_delscreen";                  */

void __binding_curses_delscreen ( SCREEN *sp )
{
     delscreen ( sp );

     return;
}


/* procedure CURSES_doupdate_meta (crow, ccol: in int) */
/*   with                                              */
/*   Import        => True,                            */
/*   Convention    => C,                               */
/*   External_Name => "__binding_curses_doupdate";     */

void __binding_curses_doupdate_meta ( int crow, int ccol )
{
     int srow, scol;

     getsyx ( srow, scol );
     setsyx ( crow, ccol );
     doupdate ( );

     setsyx ( srow, scol );
     return;
}


/* procedure CURSES_clear                       */
/*   with                                       */
/*   Import        => True,                     */
/*   Convention    => C,                        */
/*   External_Name => "__binding_curses_clear"; */
void __binding_curses_clear ( void )
{
     clear ();
     refresh (); /* This is needed to make the pad-based rendering work */
     return;
}


/* procedure CURSES_query_extents                       */
/*   (line          : in     File_Handle;               */
/*    ws_row, ws_col:    out unsigned_short);           */
/*   with                                               */
/*   Import        => True,                             */
/*   Convention    => C,                                */
/*   External_Name => "__binding_curses_query_extents"; */

void __binding_curses_query_extents
(
     FILE           * line,
     unsigned short * ws_row,
     unsigned short * ws_col
)
{
     int retval;
     
     struct winsize ws;
     retval = ioctl ( fileno ( line ), TIOCGWINSZ, &ws );

     if (retval < 0)
     {
          *ws_row = -1;
          *ws_col = -1;
          return;
     }

     *ws_row = ws.ws_row;
     *ws_col = ws.ws_col;
     return;
}


/* function CURSES_curs_set (visibility: int) return bool */
/*   with                                                 */
/*   Import        => True,                               */
/*   Convention    => C,                                  */
/*   External_Name => "__binding_curses_curs_set";        */

bool __binding_curses_curs_set (int visibility)
{
     int retval;

     retval = curs_set ( visibility );

     if (retval != ERR)
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
     
     return curs_set ( visibility );
}


/* function CURSES_getch return CURSES_Character */
/*   with                                        */
/*   Import        => True,                      */
/*   Convention    => C,                         */
/*   External_Name => "__binding_curses_getch";  */

int __binding_curses_wgetch ( WINDOW * win )
{
     return ( wgetch ( win ) );
}


/* function CURSES_ungetch (ch: CURSES_Character) return int */
/*   with                                                    */
/*   Import        => True,                                  */
/*   Convention    => C,                                     */
/*   External_Name => "__binding_curses_ungetch";            */

int __binding_curses_ungetch ( int ch )
{
     int retval;
     
     retval = ungetch ( ch );

     return ( (retval == ERR) ? -1 : 0 );
     
}


/************************/
/* Curses.Binding.Color */
/************************/

/* function Curses.Binding.Color.CURSES_start_color return bool */
/*   with                                                       */
/*   Import        => True,                                     */
/*   Convention    => C,                                        */
/*   External_Name => "__binding_curses_start_color";           */

unsigned char __binding_curses_start_color ( void )
{
     int retval;

     retval = start_color ( );

     /* According to the manpage */
     /* SVr4 specifies only "an integer value other than ERR" upon */
     /* successful completion */
     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
}


/* function Curses.Binding.Color.CURSES_init_pair               */
/*   (pair, f, b: in short) return bool                         */
/*   with                                                       */
/*   Import        => True,                                     */
/*   Convention    => C,                                        */
/*   External_Name => "__binding_curses_init_pair";             */

unsigned char __binding_curses_init_pair (short pair, short f, short b)
{
     int retval;

     retval = init_pair ( pair, f, b );

     /* According to the manpage */
     /* SVr4 specifies only "an integer value other than ERR" upon */
     /* successful completion */
     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
}


/* function Curses.Binding.Color.CURSES_init_color   */
/*   (color, r, g, b: in short) return bool          */
/*   with                                            */
/*   Import        => True,                          */
/*   Convention    => C,                             */
/*   External_Name => "__binding_curses_init_color"; */

unsigned char __binding_curses_init_color
(short color, short r, short g, short b)
{
     int retval;

     retval = init_color (color, r, g, b);

     /* According to the manpage */
     /* SVr4 specifies only "an integer value other than ERR" upon */
     /* successful completion */
     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
}


/* function Curses.Binding.Color.CURSES_has_colors return bool */
/*   with                                                      */
/*   Import        => True,                                    */
/*   Convention    => C,                                       */
/*   External_Name => "__binding_curses_has_colors";           */

unsigned char __binding_curses_has_colors ( void )
{
     bool retval;

     retval = has_colors ( );

     if (retval == TRUE)
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}

/* function Curses.Binding.Color.CURSES_can_change_color return bool */
/*   with                                                            */
/*   Import        => True,                                          */
/*   Convention    => C,                                             */
/*   External_Name => "__binding_curses_can_change_color";           */

unsigned char __binding_curses_can_change_color ( void )
{
     bool retval;

     retval = can_change_color ( );

     if (retval == TRUE)
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}


/* function CURSES_use_default_colors return bool            */
/*   with                                                    */
/*   Import        => True,                                  */
/*   Convention    => C,                                     */
/*   External_Name => "__binding_curses_use_default_colors"; */

unsigned char __binding_curses_use_default_colors ( void )
{
     int retval;

     retval = use_default_colors ();

     if ( retval == ERR )
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function Curses.Binding.Color.CURSES_color_content    */
/*   (color: in short; r, g, b: out short) return bool   */
/*   with                                                */
/*   Import        => True,                              */
/*   Convention    => C,                                 */
/*   External_Name => "__binding_curses_color_content";  */

unsigned char __binding_curses_color_content
(short color, short * r, short * g, short * b)
{
     int retval;

     retval = color_content ( color, r, g, b );

     /* According to the manpage */
     /* SVr4 specifies only "an integer value other than ERR" upon */
     /* successful completion */
     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function CURSES_pair_content (pair: in short; f, b: out short) */
/*                              return bool                       */
/*   with                                                         */
/*   Import        => True,                                       */
/*   Convention    => C,                                          */
/*   External_Name => "__binding_curses_pair_content";            */

unsigned char __binding_curses_pair_content
(short pair, short *f, short * b)
{
     int retval;

     retval = pair_content ( pair, f, b );

     /* According to the manpage */
     /* SVr4 specifies only "an integer value other than ERR" upon */
     /* successful completion */
     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
}


/* function Curses.Binding.Color.CURSES_max_colors return short */
/*   with                                                       */
/*   Import        => True,                                     */
/*   Convention    => C,                                        */
/*   External_Name => "__binding_curses_max_colors";            */

short __binding_curses_max_colors ( void )
{
     return ( (short)(COLORS) );
}


/* function Curses.Binding.Color.CURSES_max_pairs return short */
/*   with                                                      */
/*   Import        => True,                                    */
/*   Convention    => C,                                       */
/*   External_Name => "__binding_curses_max_pairs";            */

short __binding_curses_max_pairs ( void )
{
     return ( (short)(COLOR_PAIRS) );
}


/* function CURSES_wcolor_set (win: Surface_Handle; pair: short) */
/*                            return bool;                       */
/*   with                                                        */
/*   Import        => True,                                      */
/*   Convention    => C,                                         */
/*   External_Name => "__binding_curses_wcolor_set";             */

bool __binding_curses_wcolor_set ( WINDOW * win, short pair )
{
     int retval;

     retval = wcolor_set ( win, pair, NULL );

     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
}


/* function CURSES_meta_wbkgd_color (win   : in Surface_Handle;          */
/*                                   blank : in char;                    */
/*                                   bold, standout, dim, uline, invert, */
/*                                   blink : in unsigned;                */
/*                                   pair  : in short)                   */
/*                                  return bool                          */
/*   with                                                                */
/*   Import        => True,                                              */
/*   Convention    => C,                                                 */
/*   External_Name => "__binding_curses_meta_wbkgd_color";               */

bool __binding_curses_meta_wbkgd_color
(
     WINDOW * win, char blank, unsigned bold, unsigned standout, unsigned dim,
     unsigned uline, unsigned invert, unsigned blink, short pair
)
{
     int    retval;

     chtype set_char = (chtype)blank;

     internal_set_attrs_color
     ( &set_char, bold, standout, dim, uline, invert, blink, pair );

     retval = wbkgd ( win, set_char );

     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function  CURSES_newwin (nlines, ncols, begin_y, begin_x: in int) */
/*                         return Surface_Handle                     */
/*   with                                                            */
/*   Import        => True,                                          */
/*   Convention    => C,                                             */
/*   External_Name => "__binding_curses_newwin";                     */

WINDOW * __binding_curses_newwin ( int nlines, int ncols, int begin_y, int begin_x )
{
     return ( newwin ( nlines, ncols, begin_y, begin_x ) );
}


/* function  CURSES_newpad (nlines, ncols: in int) */
/*                         return Surface_Handle   */
/*   with                                          */
/*   Import        => True,                        */
/*   Convention    => C,                           */
/*   External_Name => "__binding_curses_newpad";   */

WINDOW * __binding_curses_newpad ( int nlines, int ncols )
{
     return ( newpad ( nlines, ncols ) );
}


/* procedure CURSES_meta_setup_win (win: in Surface_Handle) */
/*   with                                                   */
/*   Import        => True,                                 */
/*   Convention    => C,                                    */
/*   External_Name => "__binding_curses_meta_setup_win";    */

void __binding_curses_meta_setup_win ( WINDOW * win )
{
     intrflush ( win, FALSE );
     meta      ( win, TRUE  );
     keypad    ( win, TRUE  );
     nodelay   ( win, TRUE  );

     return;
}

/* procedure CURSES_delwin (win: in Surface_Handle) */
/*   with                                           */
/*   Import        => True,                         */
/*   Convention    => C,                            */
/*   External_Name => "__binding_curses_delwin";    */

void __binding_curses_delwin ( WINDOW * win )
{
     delwin ( win );
     return;
}


/* procedure CURSES_getmaxyx (win       : in     Surface_Handle;  */
/*                            maxy, maxx:    out int)             */
/*   with                                                         */
/*   Import        => True,                                       */
/*   Convention    => C,                                          */
/*   External_Name => "__binding_curses_getmaxyx";                */

void __binding_curses_getmaxyx ( WINDOW * win, int * maxy, int * maxx )
{
     int tmp_maxy, tmp_maxx;

     getmaxyx ( win, tmp_maxy, tmp_maxx );

     *maxy = tmp_maxy;
     *maxx = tmp_maxx;

     return;
}


/* procedure CURSES_wmove (win : in Surface_Handle; */
/*                         y, x: in int)            */
/*   with                                           */
/*   Import        => True,                         */
/*   Convention    => C,                            */
/*   External_Name => "__binding_curses_wmove";     */

void __binding_curses_wmove ( WINDOW * win, int y, int x )
{
     wmove ( win, y, x );
}


/* procedure CURSES_meta_wattrset                                 */ 
/*   (win                  : in Surface_Handle;                   */
/*    bold, standout, dim,                                        */
/*    uline, invert, blink : in unsigned)                         */
/* with                                                           */
/*   Import        => True,                                       */
/*   Convention    => C,                                          */
/*   External_Name => "__binding_curses_meta_wattrset";           */

void __binding_curses_meta_wattrset
(
     WINDOW * win,
     unsigned bold, unsigned standout, unsigned dim,
     unsigned uline, unsigned invert, unsigned blink
)
{
     int attrs = A_NORMAL;

     if ( bold )
          attrs |= A_BOLD;

     if ( standout )
          attrs |= A_STANDOUT;

     if ( dim )
          attrs |= A_DIM;

     if ( uline )
          attrs |= A_UNDERLINE;

     if ( invert )
          attrs |= A_REVERSE;

     if ( blink )
          attrs |= A_BLINK;

     wattrset ( win, attrs );

     return;
               
}


/* procedure CURSES_meta_wclrch (win: in Surface_Handle) */
/*   with                                                */
/*   Import        => True,                              */
/*   Convention    => C,                                 */
/*   External_Name => "__binding_curses_meta_wclrch";    */

void __binding_curses_meta_wclrch ( WINDOW * win )
{
     chtype curch;

     curch = winch ( win );

     curch ^= curch & A_CHARTEXT;
     curch |= ((const chtype)(' ')) & A_CHARTEXT;

     waddch ( win, (const chtype)curch );

     return;
     
}


/* function CURSES_waddstr (win: Surface_Handle; str: char_array) return int */
/*   with                                                                    */
/*   Import        => True,                                                  */
/*   Convention    => C,                                                     */
/*   External_Name => "__binding_curses_waddstr";                            */

int __binding_curses_waddstr ( WINDOW * win, const char * str )
{
     int retval;

     retval = waddstr ( win, str );

     return ( (retval == ERR) ? -1 : 0 );
}


/* function CURSES_winnstr (win: in     Surface_Handle; */
/*                          str: in out char_array;     */
/*                          len: in     int)            */
/*                         return int                   */
/*   with                                               */
/*   Import        => True,                             */
/*   Convention    => C,                                */
/*   External_Name => "__binding_curses_winnstr";       */
int __binding_curses_winnstr ( WINDOW * win, char * str, int len )
{
     return ( winnstr ( win, str, len ) );
}


/* procedure CURSES_wmvinch (win                 : in     Surface_Handle; */
/*                           y, x                : in     int;            */
/*                           ch                  :    out char;           */
/*                                                                        */
/*                           bold, standout, dim,                         */
/*                           uline, invert, blink: out    unsigned;       */
/*                                                                        */
/*                           color_pair          : out short)             */
/*   with                                                                 */
/*   Import        => True,                                               */
/*   Convention    => C,                                                  */
/*   External_Name => "__binding_curses_mvwinch";                         */
void __binding_curses_mvwinch
(
     WINDOW * win,
     int y, int x,

     char * ch,

     unsigned * bold,  unsigned * standout, unsigned * dim,
     unsigned * uline, unsigned * invert,   unsigned * blink,

     short * color_pair
)
{
     chtype c;
     int attrs;

     c = mvwinch ( win, y, x );

     *ch         = (char )(c & A_CHARTEXT);
     *color_pair = (short)(c & A_COLOR);
     attrs       = (int  )(c & A_ATTRIBUTES);
     
     *bold     = (attrs & A_BOLD     ) ? 1 : 0;
     *standout = (attrs & A_STANDOUT ) ? 1 : 0;
     *dim      = (attrs & A_DIM      ) ? 1 : 0;
     *uline    = (attrs & A_UNDERLINE) ? 1 : 0;
     *invert   = (attrs & A_REVERSE  ) ? 1 : 0;
     *blink    = (attrs & A_BLINK    ) ? 1 : 0;

     return;
}



/* procedure CURSES_pnoutrefresh (pad             : in Surface_Handle;   */
/*                                pminrow, pmincol: in int;              */
/*                                sminrow: smincol: in int;              */
/*                                smaxrow: smaxcol: in int)              */
/*   with                                                                */
/*   Import        => True,                                              */
/*   Convention    => C,                                                 */
/*   External_Name => "__binding_curses_pnoutrefresh";                   */

void __binding_curses_pnoutrefresh
(
     WINDOW * pad,
     int pminrow, int pmincol,
     int sminrow, int smincol,
     int smaxrow, int smaxcol
)
{
     pnoutrefresh ( pad, pminrow, pmincol, sminrow, smincol, smaxrow, smaxcol );
     return;
}


/* procedure CURSES_touchline (win  : in Surface_Handle; */
/*                             start: in int;            */
/*                             count: in int)            */
/*   with                                                */
/*   Import        => True,                              */
/*   Convention    => C,                                 */
/*   External_Name => "__binding_curses_touchline";      */

void __binding_curses_touchline ( WINDOW * win, int start, int count )
{
     touchline ( win, start, count );
     return;
}


/* procedure CURSES_wclear (win: in Surface_Handle) */
/*   with                                           */
/*   Import        => True,                         */
/*   Convention    => C,                            */
/*   External_Name => "__binding_curses_wclear";    */

void __binding_curses_wclear ( WINDOW * win )
{
     wclear ( win );
     return;
}


/* procedure CURSES_wclrtoeol (win: in Surface_Handle) */
/*   with                                              */
/*   Import        => True,                            */
/*   Convention    => C,                               */
/*   External_Name => "__binding_curses_wcleartoeol";  */

void __binding_curses_wclrtoeol ( WINDOW * win )
{
     wclrtoeol ( win );
     return;
}


/* function CURSES_meta_wbkgd (win   : in Surface_Handle;          */
/*                             blank : in char;                    */
/*                             bold, standout, dim, uline, invert, */
/*                             blink : in unsigned;                */
/*                            return bool                          */
/*   with                                                          */
/*   Import        => True,                                        */
/*   Convention    => C,                                           */
/*   External_Name => "__binding_curses_meta_wbkgd";               */

bool __binding_curses_meta_wbkgd
(
     WINDOW * win, char blank, unsigned bold, unsigned standout, unsigned dim,
     unsigned uline, unsigned invert, unsigned blink
)
{
     int    retval;

     chtype set_char = (chtype)blank;

     internal_set_attrs
          ( &set_char, bold, standout, dim, uline, invert, blink );

     retval = wbkgd ( win, set_char );

     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function CURSES_meta_default_wborder (win: in Surface_Handle;             */
/*                                       bold, standout, dim, uline, invert, */
/*                                       blink: in unsigned)                 */
/*                                      return bool                          */
/*   with                                                                    */
/*   Import        => True,                                                  */
/*   Convention    => C,                                                     */
/*   External_Name => "__binding_curses_meta_default_wborder";               */

bool __binding_curses_meta_default_wborder
(
     WINDOW * win,

     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink
)
{
     chtype ls = ACS_VLINE;
     chtype rs = ACS_VLINE;
     chtype ts = ACS_HLINE;
     chtype bs = ACS_HLINE;
     chtype tl = ACS_ULCORNER;
     chtype tr = ACS_URCORNER;
     chtype bl = ACS_LLCORNER;
     chtype br = ACS_LRCORNER;

     return ( internal_do_wborder ( win,
                                    bold, standout, dim, uline, invert, blink,
                                    &ls, &rs, &ts, &bs, &tl, &tr, &bl, &br ) );
}


/* function CURSES_meta_default_wborder_color                        */
/*   (win  : in Surface_Handle;                                      */
/*    bold, standout, dim, uline, invert,                            */
/*    blink: in unsigned;                                            */
/*    pair : in short)                                               */
/*   return bool                                                     */
/* with                                                              */
/*   Import        => True,                                          */
/*   Convention    => C,                                             */
/*   External_Name => "__binding_curses_meta_default_wborder_color"; */

bool __binding_curses_meta_default_wborder_color
(
     WINDOW * win,

     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink,

     short pair
)
{
     chtype ls = ACS_VLINE;
     chtype rs = ACS_VLINE;
     chtype ts = ACS_HLINE;
     chtype bs = ACS_HLINE;
     chtype tl = ACS_ULCORNER;
     chtype tr = ACS_URCORNER;
     chtype bl = ACS_LLCORNER;
     chtype br = ACS_LRCORNER;

     return ( internal_do_wborder_color
              ( win,
                bold, standout, dim, uline, invert, blink,
                pair,
                &ls, &rs, &ts, &bs, &tl, &tr, &bl, &br ) );
}


/* function CURSES_meta_wborder (win: in Surface_Handle;                  */
/*                               bold, standout, dim, uline, invert,      */
/*                               blink: in unsigned;                      */
/*                               ls, rs, ts, bs, tl, tr, bl, br: in char) */
/*                              return bool                               */
/*   with                                                                 */
/*   Import        => True,                                               */
/*   Convention    => C,                                                  */
/*   External_Name => "__binding_curses_meta_wborder";                    */

bool __binding_curses_meta_wborder
(
     WINDOW * win,
     
     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink,

     char ls, char rs, char ts, char bs, char tl, char tr, char bl, char br
)
{
     chtype _ls = (chtype)ls;
     chtype _rs = (chtype)rs;
     chtype _ts = (chtype)ts;
     chtype _bs = (chtype)bs;
     chtype _tl = (chtype)tl;
     chtype _tr = (chtype)tr;
     chtype _bl = (chtype)bl;
     chtype _br = (chtype)br;

     return
     (
          internal_do_wborder
          ( win,
            bold, standout, dim, uline, invert, blink,
            &_ls, &_rs, &_ts, &_bs, &_tl, &_tr, &_bl, &_br )
     );
}


/* function CURSES_meta_wborder_color (win  : in Surface_Handle;           */
/*                                     bold, standout, dim, uline, invert, */
/*                                     blink: in unsigned;                 */
/*                                     pair : in short;                    */
/*                                     ls, rs, ts, bs, tl, tr, bl,         */
/*                                     br   : in char)                     */
/*                                    return bool                          */
/*   with                                                                  */
/*   Import        => True,                                                */
/*   Convention    => C,                                                   */
/*   External_Name => "__binding_curses_meta_wborder_color";               */

bool __binding_curses_meta_wborder_color
(
     WINDOW * win,
     
     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink,

     short pair,

     char ls, char rs, char ts, char bs, char tl, char tr, char bl, char br
)
{
     chtype _ls = (chtype)ls;
     chtype _rs = (chtype)rs;
     chtype _ts = (chtype)ts;
     chtype _bs = (chtype)bs;
     chtype _tl = (chtype)tl;
     chtype _tr = (chtype)tr;
     chtype _bl = (chtype)bl;
     chtype _br = (chtype)br;

     return
     (
          internal_do_wborder_color
          ( win,
            bold, standout, dim, uline, invert, blink,
            pair,
            &_ls, &_rs, &_ts, &_bs, &_tl, &_tr, &_bl, &_br )
     );
}
     


/* function CURSES_copywin (srcwin : in Surface_Handle; */
/*                          dstwin : in Surface_Handle; */
/*                          sminrow: in int;            */
/*                          smincol: in int;            */
/*                          dminrow: in int;            */
/*                          dmincol: in int;            */
/*                          dmaxrow: in int;            */
/*                          dmaxcol: in int)            */
/*                         return bool                  */
/*   with                                               */
/*   Import        => True,                             */
/*   Convention    => C,                                */
/*   External_Name => "__binding_curses_copywin";       */

bool __binding_curses_copywin
(
     WINDOW * srcwin, WINDOW * dstwin,
     int sminrow, int smincol, int dminrow, int dmincol,
     int dmaxrow, int dmaxcol
)
{
     int retval;

     retval = copywin ( srcwin, dstwin, sminrow, smincol, dminrow, dmincol,
                        dmaxrow, dmaxcol, 1 );

     if ( retval != ERR )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}


/* function CURSES_wresize (win    : in Surface_Handle; */
/*                          lines  : in int;            */
/*                          columns: in int)            */
/*                         return bool                  */
/*   with                                               */
/*   Import        => True,                             */
/*   Convention    => C,                                */
/*   External_Name => "__binding_curses_wresize";       */

bool __binding_curses_wresize ( WINDOW * win, int lines, int columns )
{
     int retval;

     retval = wresize ( win, lines, columns );

     if ( retval != ERR )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}


/* function CURSES_mvwin (win: in Surface_Handle; */
/*                        y  : in int;            */
/*                        x  : in int)            */
/*                       return bool              */
/*   with                                         */
/*   Import        => True,                       */
/*   Convention    => C,                          */
/*   External_Name => "__binding_curses_mvwin";   */

bool __binding_curses_mvwin ( WINDOW * win, int y, int x )
{
     int retval;

     retval = mvwin ( win, y, x );

     if ( retval != ERR )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
}
