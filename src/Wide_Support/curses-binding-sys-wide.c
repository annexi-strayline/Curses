/*****************************************************************************
**                                                                          **
**                        UNIX Terminal Control Package                     **
**                             (n)curses Binding                            **
**                                                                          **
** ************************************************************************ **
**                                                                          **
**  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   **
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

/* Wide_Character support binding facilities */

#define _XOPEN_SOURCE_EXTENDED

#ifdef __NADACURSES_HOST_OS_LINUX
#include <ncursesw/ncurses.h>

#elif defined (__NADACURSES_HOST_OS_FREE_BSD)
#include <ncurses.h>

#elif defined (__NADACURSES_HOST_OS_SOLARIS)
#include <ncurses/ncurses.h>

#else
#error "OS not supported."
#endif

#define BOOL_FALSE 0
#define BOOL_TRUE  1

/**************/
/** Internal **/
/**************/
static inline attr_t internal_set_wattrs
(unsigned   bold,
 unsigned   standout,
 unsigned   dim,
 unsigned   uline,
 unsigned   invert,
 unsigned   blink)
{
     attr_t attrs = 0;
     
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

     return attrs;
}





/* function CURSES_waddwstr (win: Surface_Handle; str: wchar_array) */
/*    return int                                                    */
/*                                                                  */
/*   with                                                           */
/*   Import        => True,                                         */
/*   Convention    => C,                                            */
/*   External_Name => "__binding_curses_waddwstr";                  */

int __binding_curses_waddwstr ( WINDOW * win, const wchar_t * str )
{
     int retval;

     retval = waddwstr ( win, str );
     
     return ( (retval == ERR) ? -1 : 0 );
}


/* function CURSES_winnwstr (win: in     Surface_Handle; */
/*                           str: in out wchar_array;    */
/*                           len: in     int)            */
/*                         return int                    */
/*   with                                                */
/*   Import        => True,                              */
/*   Convention    => C,                                 */
/*   External_Name => "__binding_curses_winnwstr";       */

int __binding_curses_winnwstr ( WINDOW * win, wchar_t * str, int len )
{
     return ( winnwstr ( win, str, len ) );
}


/* procedure CURSES_wmvin_wch (win                 : in     Surface_Handle; */
/*                             y, x                : in     int;            */
/*                             ch                  :    out wchar_t;        */
/*                                                                          */
/*                             bold, standout, dim,                         */
/*                             uline, invert, blink:    out unsigned;       */
/*                                                                          */
/*                             color_pair          :    out short)          */
/*   with                                                                   */
/*   Import        => True,                                                 */
/*   Convention    => C,                                                    */
/*   External_Name => "__binding_curses_wmvin_wch";                         */

void __binding_curses_mvwin_wch
(
     WINDOW * win,
     int y, int x,
     
     wchar_t * ch,
     
     unsigned * bold,  unsigned * standout, unsigned * dim,
     unsigned * uline, unsigned * invert,   unsigned * blink,

     short * color_pair
)
{
     cchar_t wc;
     attr_t attrs;
     wchar_t wch_str[2] = { L'\0', L'\0' };
     int wch_str_len;

     mvwin_wch ( win, y, x, &wc );

     // The number of wchar_t's stored at this location should be 1!
     wch_str_len = getcchar ( &wc, NULL, NULL, NULL, NULL );

     if (wch_str_len > 2)
     {
          *ch = L'\0';
          *bold = 0;
          *standout = 0;
          *dim = 0;
          *uline = 0;
          *invert = 0;
          *blink = 0;

          *color_pair = 0;
          return;
     }

     getcchar ( &wc, wch_str, &attrs, color_pair, NULL );

     *ch = wch_str[0];
     
     *bold     = (attrs & A_BOLD     ) ? 1 : 0;
     *standout = (attrs & A_STANDOUT ) ? 1 : 0;
     *dim      = (attrs & A_DIM      ) ? 1 : 0;
     *uline    = (attrs & A_UNDERLINE) ? 1 : 0;
     *invert   = (attrs & A_REVERSE  ) ? 1 : 0;
     *blink    = (attrs & A_BLINK    ) ? 1 : 0;

     return;
}



/* function CURSES_meta_wbkgrnd (win   : in Surface_Handle;          */
/*                               blank : in wchar_t;                 */
/*                               bold, standout, dim, uline, invert, */
/*                               blink : in unsigned;                */
/*                              return bool                          */
/*   with                                                            */
/*   Import        => True,                                          */
/*   Convention    => C,                                             */
/*   External_Name => "__binding_curses_meta_wbkgrnd";               */

bool __binding_curses_meta_wbkgrnd
(
     WINDOW * win, wchar_t blank, unsigned bold, unsigned standout,
     unsigned dim, unsigned uline, unsigned invert, unsigned blink
)
{
     int retval;
     const wchar_t blank_str[2] = { blank, L'\0' };

     cchar_t set_char;

     (void)setcchar (&set_char,           /* cchar_t       * wcval    */
                     blank_str,           /* const wchar_t * wch      */
                     internal_set_wattrs  /* const attr_t  attrs      */
                     ( bold, standout, dim, uline, invert, blink ),
                     0,                   /* short         color_pair */
                     NULL);               /* void          * opts     */

     retval = wbkgrnd ( win, &set_char );

     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function CURSES_meta_wbkgrnd_color (win  : in Surface_Handle;           */
/*                                     blank: in wchar_t;                  */
/*                                     bold, standout, dim, uline, invert, */
/*                                     blink: in unsigned;                 */
/*                                     pair : in short)                    */
/*                                    return bool                          */
/*   with                                                                  */
/*   Import        => True,                                                */
/*   Convention    => C,                                                   */
/*   External_Name => "__binding_curses_meta_wbkgrnd_color";               */
/* -- Returns Bool_False on failure                                        */

bool __binding_curses_meta_wbkgrnd_color
(
     WINDOW * win, wchar_t blank, unsigned bold, unsigned standout,
     unsigned dim, unsigned uline, unsigned invert, unsigned blink,
     short pair
)
{
     int retval;
     const wchar_t blank_str[2] = { blank, L'\0' };

     cchar_t set_char;

     (void)setcchar (&set_char,           /* cchar_t       * wcval    */
                     blank_str,           /* const wchar_t * wch      */
                     internal_set_wattrs  /* const attr_t  attrs      */
                     ( bold, standout, dim, uline, invert, blink ),
                     pair,                /* short         color_pair */
                     NULL);               /* void          * opts     */
                        
     
     retval = wbkgrnd ( win, &set_char );

     if (retval == ERR)
          return BOOL_FALSE;
     else
          return BOOL_TRUE;
     
}


/* function CURSES_meta_wborder_set                        */
/*   (win: in Surface_Handle;                              */
/*    bold, standout, dim, uline, invert,                  */
/*    blink: in unsigned;                                  */
/*    ls, rs, ts, bs, tl, tr, bl, br: in wchar_t)          */
/*   return bool                                           */
/*                                                         */   
/*   with                                                  */
/*   Import        => True,                                */
/*   Convention    => C,                                   */
/*   External_Name => "__binding_curses_meta_wborder_set"; */

bool __binding_curses_meta_wborder_set
(
     WINDOW * win,
     
     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink,

     wchar_t ls, wchar_t rs, wchar_t ts, wchar_t bs,
     wchar_t tl, wchar_t tr, wchar_t bl, wchar_t br
)
{
     int retval;
     
     cchar_t ls_cchar, rs_cchar, ts_cchar, bs_cchar;
     cchar_t tl_cchar, tr_cchar, bl_cchar, br_cchar;

     wchar_t wstr[2] = { ls, L'\0' };
     
     attr_t attrs = internal_set_wattrs
          ( bold, standout, dim, uline, invert, blink );

     
     setcchar ( &ls_cchar, wstr, attrs, 0, NULL );

     wstr[0] = rs;
     setcchar ( &rs_cchar, wstr, attrs, 0, NULL );

     wstr[0] = ts;
     setcchar ( &ts_cchar, wstr, attrs, 0, NULL );

     wstr[0] = bs;
     setcchar ( &bs_cchar, wstr, attrs, 0, NULL );
     

     wstr[0] = tl;
     setcchar ( &tl_cchar, wstr, attrs, 0, NULL );

     wstr[0] = tr;
     setcchar ( &tr_cchar, wstr, attrs, 0, NULL );

     wstr[0] = bl;
     setcchar ( &bl_cchar, wstr, attrs, 0, NULL );

     wstr[0] = br;
     setcchar ( &br_cchar, wstr, attrs, 0, NULL );

     
     retval = wborder_set
          ( win,
            &ls_cchar, &rs_cchar, &ts_cchar, &bs_cchar,
            &tl_cchar, &tr_cchar, &bl_cchar, &br_cchar );


     if ( retval == OK )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
                
}


/* function CURSES_meta_wborder_set_color                          */
/*   (win  : in Surface_Handle;                                    */
/*    bold, standout, dim, uline, invert,                          */
/*    blink: in unsigned;                                          */
/*    pair : in short;                                             */
/*    ls, rs, ts, bs, tl, tr, bl,                                  */
/*    br   : in wchar_t)                                           */
/*   return bool                                                   */
/*                                                                 */   
/*   with                                                          */
/*   Import        => True,                                        */
/*   Convention    => C,                                           */
/*   External_Name => "__binding_curses_meta_wborder_set_color";   */

bool __binding_curses_meta_wborder_set_color
(
     WINDOW * win,
     
     unsigned bold, unsigned standout, unsigned dim, unsigned uline,
     unsigned invert, unsigned blink,

     short pair,

     wchar_t ls, wchar_t rs, wchar_t ts, wchar_t bs,
     wchar_t tl, wchar_t tr, wchar_t bl, wchar_t br
)
{
     int retval;
     
     cchar_t ls_cchar, rs_cchar, ts_cchar, bs_cchar;
     cchar_t tl_cchar, tr_cchar, bl_cchar, br_cchar;

     wchar_t wstr[2] = { ls, L'\0' };
     
     attr_t attrs = internal_set_wattrs
          ( bold, standout, dim, uline, invert, blink );

     
     setcchar ( &ls_cchar, wstr, attrs, pair, NULL );

     wstr[0] = rs;
     setcchar ( &rs_cchar, wstr, attrs, pair, NULL );

     wstr[0] = ts;
     setcchar ( &ts_cchar, wstr, attrs, pair, NULL );

     wstr[0] = bs;
     setcchar ( &bs_cchar, wstr, attrs, pair, NULL );
     

     wstr[0] = tl;
     setcchar ( &tl_cchar, wstr, attrs, pair, NULL );

     wstr[0] = tr;
     setcchar ( &tr_cchar, wstr, attrs, pair, NULL );

     wstr[0] = bl;
     setcchar ( &bl_cchar, wstr, attrs, pair, NULL );

     wstr[0] = br;
     setcchar ( &br_cchar, wstr, attrs, pair, NULL );

     
     retval = wborder_set
          ( win,
            &ls_cchar, &rs_cchar, &ts_cchar, &bs_cchar,
            &tl_cchar, &tr_cchar, &bl_cchar, &br_cchar );


     if ( retval == OK )
          return BOOL_TRUE;
     else
          return BOOL_FALSE;
                
}
