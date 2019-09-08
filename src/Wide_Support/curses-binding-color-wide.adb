------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of ANNEXI-STRAYLINE nor the names of its         --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL ANNEXI-STRAYLINE   --
--  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR  --
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF    --
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR         --
--  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,   --
--  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  --
--  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                              --
--                                                                          --
------------------------------------------------------------------------------

-- No Wide support configuration

with Curses.Binding.Render.Wide;

package body Curses.Binding.Color.Wide is
   
   pragma Assert (Wide_Support_Configured);
   
   --
   -- C Imports
   --
   
   function CURSES_meta_wbkgrnd_color (win  : in Surface_Handle;
                                       blank: in wchar_t;
                                       bold, standout, dim, uline, invert,
                                       blink: in unsigned;
                                       pair : in short)
                                      return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wbkgrnd_color";
   -- Returns Bool_False on failure
   
   
   function CURSES_meta_wborder_set_color 
     (win  : in Surface_Handle;
      bold, standout, dim, uline, invert,
      blink: in unsigned;
      pair : in short;
      ls, rs, ts, bs, tl, tr, bl, 
      br   : in wchar_t)
     return bool
   
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wborder_set_color";
   
   
   procedure CURSES_mvwin_wch (win                 : in     Surface_Handle;
                               y, x                : in     int;
                               ch                  :    out wchar_t;
                               
                               bold, standout, dim, 
                               uline, invert, blink:    out unsigned;
                               
                               color_pair          :    out short)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_mvwin_wch";
   
   
   ---------------------------------
   -- Wide_Set_Colored_Background --
   ---------------------------------
   procedure Wide_Set_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Wide_Character;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair)
   is
      procedure Wide_Set_Colored_Background 
        is new Generic_Set_Colored_Background
          (Ada_Char_Type => Wide_Character,
           C_Char_Type   => wchar_t,
           To_C          => Interfaces.C.To_C,
           CURSES_generic_meta_set_background_color
             => CURSES_meta_wbkgrnd_color)
        with Inline;
   begin
      Wide_Set_Colored_Background (Handle           => Handle,
                                   Blank_Character  => Blank_Character,
                                   Reference_Cursor => Reference_Cursor,
                                   Color            => Color);
   end Wide_Set_Colored_Background;
   
   
   -----------------------------
   -- Wide_Set_Colored_Border --
   -----------------------------
   procedure Wide_Set_Colored_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair;
      LS, RS, TS, BS, TL, TR, BL, BR: in Wide_Character)
   is 
      procedure Wide_Set_Colored_Border_Actual is 
        new Generic_Set_Colored_Border 
          (Ada_Char_Type => Wide_Character,
           C_Char_Type   => wchar_t,
           To_C          => Interfaces.C.To_C,
           CURSES_generic_meta_wborder_color => CURSES_meta_wborder_set_color)
        with Inline;
   begin
      Wide_Set_Colored_Border_Actual (Handle           => Handle,
                                      Reference_Cursor => Reference_Cursor,
                                      Color            => Color,
                                      LS               => LS,
                                      RS               => RS,
                                      TS               => TS,
                                      BS               => BS,
                                      TL               => TL,
                                      TR               => TR,
                                      BL               => BL,
                                      BR               => BR);
   end Wide_Set_Colored_Border;
   
   --------------------------
   -- Wide_Query_Character --
   --------------------------
   procedure Wide_Query_Character
     (Handle  : in     Surface_Handle;
      Position: in     Cursor_Position;
      C       :    out Wide_Character;
      Style   :    out Cursor_Style;
      Color   :    out CURSES_Color_Pair)
   is
      procedure Wide_Query_Character_Actual is
        new Generic_Query_Character
          (Ada_Char_Type          => Wide_Character,
           C_Char_Type            => wchar_t,
           To_Ada                 => Interfaces.C.To_Ada,
           CURSES_generic_mvwinch => CURSES_mvwin_wch)
        with Inline;
   begin
      Wide_Query_Character_Actual
        (Handle   => Handle,
         Position => Position,
         C        => C,
         Style    => Style,
         Color    => Color);
   end Wide_Query_Character;
   
   
end Curses.Binding.Color.Wide;
