------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Exceptions;

package body Curses.Binding.Color is
   
   ----------------
   -- C Bindings --
   ----------------
   
   -- (n)curses --
   ---------------
   
   function CURSES_start_color return bool
     with 
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_start_color";
   -- curs_color(3X)
   -- int start_color(void)
   
   function CURSES_init_pair (pair, f, b: in short) return bool
     with 
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_init_pair";
   -- curs_color(3X)
   -- int init_pair(short pair, short f, short b)
   
   function CURSES_init_color (color, r, g, b: in short) return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_init_color";
   -- curs_color(3X)
   -- int init_color(short color, short r, short g, short b)
   
   function CURSES_has_colors return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_has_colors";
   -- curs_color(3X)
   -- bool has_colors(void)
   
   function CURSES_can_change_color return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_can_change_color";
   -- curs_color(3X)
   -- bool can_change_color(void)
   
   function CURSES_use_default_colors return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_use_default_colors";
   -- default_colors(3X)
   -- int use_default_colors(void)
   
   function CURSES_color_content (color: in short; r, g, b: out short)
                                 return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_color_content";
   -- curs_color(3X)
   -- int color_content(short color, short *r, short *g, short *b)
   
   function CURSES_pair_content (pair: in short; f, b: out short)
                                return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_pair_content";
   -- curs_color(3X)
   -- int pair_content(short pair, short *f, short *b)
   
   
   function CURSES_max_colors return short
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_max_colors";
   -- curs_color(3X)
   -- returns the value of the macro COLOR, cast to a short
   -- This includes color #0. So the max color number is COLOR - 1
   
   function CURSES_max_pairs return short
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_max_pairs";
   -- curs_color(3X)
   -- returns the value of the macro COLOR_PAIRS, cast to a short
   -- this includes pair 0, so the max color pair is COLOR_PAIRS - 1
   
   function CURSES_wcolor_set (win: Surface_Handle; pair: short)
                              return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wcolor_set";
   
   
   function CURSES_meta_wbkgd_color (win  : in Surface_Handle;
                                     blank: in char;
                                     bold, standout, dim, uline, invert,
                                     blink: in unsigned;
                                     pair : in short)
                                    return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wbkgd_color";
   -- Returns Bool_False on failure
   
   function CURSES_meta_default_wborder_color
     (win  : in Surface_Handle;
      bold, standout, dim, uline, invert,
      blink: in unsigned;
      pair : in short)
     return bool
   with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_default_wborder_color";
   -- calls wborder with the "default" arguments. 
   
   function CURSES_meta_wborder_color (win  : in Surface_Handle;
                                       bold, standout, dim, uline, invert,
                                       blink: in unsigned;
                                       pair : in short;
                                       ls, rs, ts, bs, tl, tr, bl, 
                                       br   : in char)
                                      return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wborder_color";
   
   
   procedure CURSES_mvwinch (win                 : in     Surface_Handle;
                             y, x                : in     int;
                             ch                  :    out char;
                             
                             bold, standout, dim, 
                             uline, invert, blink:    out unsigned;
                             
                             color_pair          :    out short)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_mvwinch";
   
   
   -----------------------
   -- Initialize_Colors --
   -----------------------
   procedure Initialize_Colors (TTY             : in     Terminal_Handle;
                                Color_Capable   :    out Boolean;
                                Color_Changeable:    out Boolean;
                                Default_Color   :    out Boolean)
   is
      Lock_OK: Boolean := False;
   begin
      -- We assume no capability, so that we can abort freely.
      -- The exception handler will still explicitly set this
      Color_Capable    := False;
      Color_Changeable := False;
      Default_Color    := False;
      
      if not Handle_Valid (TTY) then
         return;
      end if;
      
      -- For the rest, we pass onto the binding. 
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         -- Terminal can't be selected.
         -- unlock and bail
         Serial.Unlock;
         return;
      end if;
      
      -- Proceed with initialization attempt
      if CURSES_start_color /= Bool_True then
         -- Didn't work. Guess no color on this terminal!
         Serial.Unlock;
         return;
      end if;
      
      -- Verify color capability
      if CURSES_has_colors = Bool_True then
         -- Color capability confirmed!
         Color_Capable := True;

      else
         -- Though the color subsystem was apparently initialized ok,
         -- (n)curses is reporting that there are no color capabilities
         -- for this terminal
         Serial.Unlock;
         return;
         
      end if;
      
      -- Verify colors can be modified
      if CURSES_can_change_color = Bool_True then
         -- Can modify!
         Color_Changeable := True;
      end if;
      
      -- Lastly, attempt to activate Default_Color capabilities
      if CURSES_use_default_colors = Bool_True then
         Default_Color := True;
      end if;
      
      Serial.Unlock;
      
   exception
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         Color_Capable    := False;
         Color_Changeable := False;
         Default_Color    := False;
         return;
         
   end Initialize_Colors;
   
   
   --------------------
   -- Maximum_Colors --
   --------------------
   function Maximum_Colors (TTY: Terminal_Handle) return CURSES_Color
   is
      Lock_OK: Boolean := False;
   begin
      if not Handle_Valid (TTY) then
         raise Curses_Library with
           "Invalid terminal handle";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         raise Curses_Library with
           "Binding: cannot select terminal";
      end if;
      
      return Result: CURSES_Color do
        Result := CURSES_max_colors;
        Serial.Unlock;
      end return;
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Maximum_Colors;
   
   
   -------------------
   -- Maximum_Pairs --
   -------------------
   function Maximum_Pairs (TTY: Terminal_Handle) return CURSES_Color_Pair
   is
      Lock_OK: Boolean := False;
   begin
      if not Handle_Valid (TTY) then
         raise Curses_Library with
           "Invalid terminal handle";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         raise Curses_Library with
           "Binding: cannot select terminal";
      end if;
      
      return Result: CURSES_Color_Pair do
        Result := CURSES_max_pairs;
        
        Serial.Unlock;
      end return;
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Maximum_Pairs;
   
   
   ---------------------
   -- Decompose_Color --
   ---------------------
   procedure Decompose_Color (TTY             : in     Terminal_Handle;
                              Color           : in     CURSES_Color;
                              Red, Green, Blue:    out CURSES_RGB)
   is
      Lock_OK: Boolean := False;
      Result : bool;
   begin
      
      if not Handle_Valid (TTY) then
         raise Curses_Library with
           "Terminal handle invalid";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         raise Curses_Library with
           "Binding: cannot select terminal";
      end if;
         
         
      
      Result := CURSES_color_content (color => Color,
                                      r     => Red,
                                      g     => Green,
                                      b     => Blue);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to color_content () failed.";
      end if;
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Decompose_Color;
   
   
   ---------------
   -- Set_Color --
   ---------------
   procedure Set_Color (TTY             : in Terminal_Handle;
                        Color           : in CURSES_Color;
                        Red, Green, Blue: in CURSES_RGB)
   is
      Lock_OK: Boolean := False;
      Result : bool;
   begin
      
      if not Handle_Valid (TTY) then
         raise Curses_Library with
           "Terminal handle invalid";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         raise Curses_Library with
           "Binding: cannot select terminal";
      end if;
      
      Result := CURSES_init_color (color => Color,
                                   r     => Red,
                                   g     => Green,
                                   b     => Blue);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to init_color () failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Set_Color;
   
   
   --------------
   -- Set_Pair --
   --------------
   procedure Set_Pair (TTY                   : in Terminal_Handle;
                       Pair                  : in CURSES_Color_Pair;
                       Foreground, Background: in CURSES_Color)
   is
      Lock_OK: Boolean := False;
      Result : bool;
   begin
      
      if not Handle_Valid (TTY) then
         raise Curses_Library with
           "Terminal handle invalid";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         raise Curses_Library with
           "Binding: cannot select terminal";
      end if;
      
      Result := CURSES_init_pair (pair => Pair,
                                  f    => Foreground,
                                  b    => Background);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to init_color () failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Set_Pair;
   
   
   ----------------
   -- Apply_Pair --
   ----------------
   procedure Apply_Pair (Handle: in Surface_Handle;
                         Pair  : in CURSES_Color_Pair)
   is
      Lock_OK: Boolean := False;
      Result : bool;
   begin
      
      if not Handle_Valid (Handle) then
         raise Curses_Library with
           "Surface handle invalid";
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_wcolor_set (win => Handle, pair => Pair);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wcolor_set () failed.";
      end if;
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
   end Apply_Pair;
   
   
   ----------------------------
   -- Set_Colored_Background --
   ----------------------------
   procedure Generic_Set_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Ada_Char_Type;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair)
   is
      Set_Bold, Set_Standout, Set_Dim, 
      Set_Uline, Set_Invert, Set_Blink: unsigned := 0;
        
      Lock_OK     : Boolean := False;
      Result      : Bool;
      
   begin
      if not Handle_Valid (Handle) then
         raise Curses_Library with
           "Surface handle invalid";
      end if;
      
      if Reference_Cursor.Style.Bold then
         Set_Bold := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Standout then
         Set_Standout := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Dim then
         Set_Dim := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Underline then
         Set_Uline := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Inverted then
         Set_Invert := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Blink then
         Set_Blink := unsigned'Last;
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_generic_meta_set_background_color
        (win      => Handle,
         blank    => To_C (Blank_Character),
         bold     => Set_Bold, 
         standout => Set_Standout,
         dim      => Set_Dim,
         uline    => Set_Uline,
         invert   => Set_Invert,
         blink    => Set_Blink,
         pair     => Color);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkgd () (with color attributes) failed.";
      end if;
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
      
   end Generic_Set_Colored_Background;
   ----------------------------------------
   
   
   procedure Set_Colored_Background (Handle          : in Surface_Handle;
                                     Blank_Character : in Character;
                                     Reference_Cursor: in Cursor'Class;
                                     Color           : in CURSES_Color_Pair)
   is 
      procedure Set_Colored_Background is new Generic_Set_Colored_Background
        (Ada_Char_Type => Character,
         C_Char_Type   => char,
         To_C          => Interfaces.C.To_C,
         CURSES_generic_meta_set_background_color
           => CURSES_meta_wbkgd_color)
        with Inline;
   begin
      Set_Colored_Background (Handle           => Handle,
                              Blank_Character  => Blank_Character,
                              Reference_Cursor => Reference_Cursor,
                              Color            => Color);
   end Set_Colored_Background;
   
   
   --------------------------------
   -- Set_Default_Colored_Border --
   --------------------------------
   procedure Set_Default_Colored_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair)
   is
      Set_Bold, Set_Standout, Set_Dim, 
        Set_Uline, Set_Invert, Set_Blink: unsigned := 0;
      
      Lock_OK     : Boolean := False;
      Result      : Bool;
      
   begin
      if not Handle_Valid (Handle) then
         raise Curses_Library with
           "Surface handle invalid";
      end if;
      
      
      if Reference_Cursor.Style.Bold then
         Set_Bold := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Standout then
         Set_Standout := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Dim then
         Set_Dim := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Underline then
         Set_Uline := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Inverted then
         Set_Invert := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Blink then
         Set_Blink := unsigned'Last;
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_meta_default_wborder_color
        (win      => Handle,
         bold     => Set_Bold, 
         standout => Set_Standout,
         dim      => Set_Dim,
         uline    => Set_Uline,
         invert   => Set_Invert,
         blink    => Set_Blink,
         pair     => Color);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkg(rn)d () (with color attributes) failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
         
   end Set_Default_Colored_Border;
   
   
   ------------------------
   -- Set_Colored_Border --
   ------------------------
   procedure Generic_Set_Colored_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair;
      LS, RS, TS, BS, TL, TR, BL, BR: in Ada_Char_Type)
   is
      Set_Bold, Set_Standout, Set_Dim, 
        Set_Uline, Set_Invert, Set_Blink: unsigned := 0;
      
      Lock_OK     : Boolean := False;
      Result      : Bool;
      
   begin
      if not Handle_Valid (Handle) then
         raise Curses_Library with
           "Surface handle invalid";
      end if;
      
      
      if Reference_Cursor.Style.Bold then
         Set_Bold := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Standout then
         Set_Standout := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Dim then
         Set_Dim := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Underline then
         Set_Uline := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Inverted then
         Set_Invert := unsigned'Last;
      end if;
      
      if Reference_Cursor.Style.Blink then
         Set_Blink := unsigned'Last;
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_generic_meta_wborder_color
        (win      => Handle,
         bold     => Set_Bold, 
         standout => Set_Standout,
         dim      => Set_Dim,
         uline    => Set_Uline,
         invert   => Set_Invert,
         blink    => Set_Blink,
         
         ls => To_C (LS),
         rs => To_C (RS),
         ts => To_C (TS),
         bs => To_C (BS),
         tl => To_C (TL),
         tr => To_C (TR),
         bl => To_C (BL),
         br => To_C (BR),
         
         pair => Color);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkg(rn)d () (with color attributes) failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
         
   end Generic_Set_Colored_Border;
   ----------------------------------------
   
   procedure Set_Colored_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair;
      LS, RS, TS, BS, TL, TR, BL, BR: in Character)
   is
      procedure Set_Colored_Border_Actual is 
        new Generic_Set_Colored_Border 
          (Ada_Char_Type => Character,
           C_Char_Type   => char,
           To_C          => Interfaces.C.To_C,
           CURSES_generic_meta_wborder_color => CURSES_meta_wborder_color)
        with Inline;
      
   begin
      Set_Colored_Border_Actual (Handle           => Handle,
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
   end Set_Colored_Border;
   
   
   ---------------------
   -- Query_Character --
   ---------------------
   procedure Generic_Query_Character
     (Handle  : in     Surface_Handle;
      Position: in     Cursor_Position;
      C       :    out Ada_Char_Type;
      Style   :    out Cursor_Style;
      Color   :    out CURSES_Color_Pair)
   is
      Locked: Boolean := False;
      
      ch: C_Char_Type;
      bold, standout, dim, uline, invert, blink: unsigned;
      color_pair: short;
   begin
      if not Handle_Valid (Handle) then
         raise Surface_Unavailable;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_generic_mvwinch
        (win   => Handle,
         y     => int (Position.Row) - 1,
         x     => int (Position.Column) - 1,
         ch    => ch,
         
         bold  => bold,  standout => standout, dim   => dim,
         uline => uline, invert   => invert,   blink => blink,
         
         color_pair => color_pair);
      
      Serial.Unlock;
      Locked := False;
      
      C := To_Ada (ch);
      
      Style := (Bold      => (bold     > 0),
                Standout  => (standout > 0),
                Dim       => (dim      > 0),
                Underline => (uline    > 0),
                Inverted  => (invert   > 0),
                Blink     => (blink    > 0));
      
      Color := CURSES_Color_Pair (color_pair);
         
   exception
      when Surface_Unavailable =>
         raise;
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Ada.Exceptions.Exception_Information (e);
      
   end Generic_Query_Character;
   
   
   ----------------------------------------
   procedure Query_Character 
     (Handle  : in     Surface_Handle;
      Position: in     Cursor_Position;
      C       :    out Character;
      Style   :    out Cursor_Style;
      Color   :    out CURSES_Color_Pair)
   is
      procedure Query_Character_Actual is 
        new Generic_Query_Character
          (Ada_Char_Type          => Character,
           C_Char_Type            => char,
           To_Ada                 => Interfaces.C.To_Ada,
           CURSES_generic_mvwinch => CURSES_mvwinch)
        with Inline;
   begin
      Query_Character_Actual (Handle   => Handle,
                              Position => Position,
                              C        => C,
                              Style    => Style,
                              Color    => Color);
   end Query_Character;
   
end Curses.Binding.Color;
