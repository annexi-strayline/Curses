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

package body Curses.Binding.Render.Wide is
   
   pragma Assert (Wide_Support_Configured);
   
   --
   -- C Bindings
   --
   
   function CURSES_waddwstr (win: Surface_Handle; str: wchar_array) return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_waddwstr";
   -- Returns non-zero on error
   
   
   function CURSES_winnwstr (win: in     Surface_Handle; 
                             str: in out wchar_array; 
                             len: in     int)
                           return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_winnwstr";
   -- Returns number of characters read-in
   
   
   function CURSES_meta_wbkgrnd (win   : in Surface_Handle;
                                 blank : in wchar_t;
                                 bold, standout, dim, uline, invert,
                                 blink : in unsigned)
                                return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wbkgrnd";
   -- Returns Bool_False on failure
   
   
   function CURSES_meta_wborder_set 
     (win: in Surface_Handle;
      bold, standout, dim, uline, invert,
      blink: in unsigned;
      ls, rs, ts, bs, tl, tr, bl, br: in wchar_t)
     return bool
   
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wborder_set";
   
   
   ---------------------
   -- Put_Wide_String --
   ---------------------
   procedure Put_Wide_String (Handle: in Surface_Handle;
                              Buffer: in Wide_String)
   is
      procedure Put_String_Actual is new Generic_Put_String
        (Ada_Character_Type     => Wide_Character,
         Ada_String_Type        => Wide_String,
         C_Character_Type       => Interfaces.C.wchar_t,
         C_String_Type          => Interfaces.C.wchar_array,
         To_C                   => Interfaces.C.To_C,
         CURSES_generic_waddstr => CURSES_waddwstr);
   begin
      Put_String_Actual (Handle => Handle, Buffer => Buffer);
   end Put_Wide_String;
   
   
   ---------------------
   -- Get_Wide_String --
   ---------------------
   procedure Get_Wide_String (Handle: in     Surface_Handle;
                              Buffer:    out Wide_String;
                              Last  :    out Natural)
   is
      procedure Get_Wide_String_Actual is new Generic_Get_String
        (Ada_Character_Type     => Wide_Character,
         Ada_String_Type        => Wide_String,
         C_Character_Type       => Interfaces.C.wchar_t,
         C_String_Type          => Interfaces.C.wchar_array,
         To_Ada                 => Interfaces.C.To_Ada,
         CURSES_generic_winnstr => CURSES_winnwstr)
        with Inline;
   begin
      Get_Wide_String_Actual (Handle => Handle,
                              Buffer => Buffer,
                              Last   => Last);
   end Get_Wide_String;
   
   
   ------------------------------------
   -- Wide_Set_Monochrome_Background --
   ------------------------------------
   procedure Wide_Set_Monochrome_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Wide_Character;
      Reference_Cursor: in Cursor'Class)
   is
      procedure Wide_Set_Monochrome_Background_Actual is 
        new Generic_Set_Monochrome_Background
          (Ada_Char_Type => Wide_Character,
           C_Char_Type   => wchar_t,
           To_C          => Interfaces.C.To_C,
           
           CURSES_generic_meta_set_background => CURSES_meta_wbkgrnd)
        with Inline;
   begin
      Wide_Set_Monochrome_Background_Actual
        (Handle           => Handle,
         Blank_Character  => Blank_Character,
         Reference_Cursor => Reference_Cursor);
   end Wide_Set_Monochrome_Background;
   
   
   --------------------------------
   -- Wide_Set_Monochrome_Border --
   --------------------------------
   procedure Wide_Set_Monochrome_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      LS, RS, TS, BS, TL, TR, BL, BR: in Wide_Character)
   is 
      procedure Wide_Set_Monochrome_Border_Actual is 
        new Generic_Set_Monochrome_Border 
          (Ada_Char_Type               => Wide_Character,
           C_Char_Type                 => wchar_t,
           To_C                        => Interfaces.C.To_C,
           CURSES_generic_meta_wborder => CURSES_meta_wborder_set)
        with Inline;
   begin
      Wide_Set_Monochrome_Border_Actual (Handle           => Handle,
                                         Reference_Cursor => Reference_Cursor,
                                         LS               => LS,
                                         RS               => RS,
                                         TS               => TS,
                                         BS               => BS,
                                         TL               => TL,
                                         TR               => TR,
                                         BL               => BL,
                                         BR               => BR);
   end Wide_Set_Monochrome_Border;
   
end Curses.Binding.Render.Wide;
