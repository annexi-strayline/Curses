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

with Curses.Binding.Color;

with Ada.Exceptions; use Ada;

package body Curses.Binding.Render is 
   
   
   --
   -- C Bindings
   --
   
   function  CURSES_newwin (nlines, ncols, begin_y, begin_x: in int)
                           return Surface_Handle
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_newwin";
   
   
   function  CURSES_newpad (nlines, ncols: in int)
                           return Surface_Handle
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_newpad";
   
   
   procedure CURSES_meta_setup_win (win: in Surface_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_setup_win";
   -- A meta command to execute the following library commands (in order)
   -- on a surface
   -- 1. intrflush ( FALSE )
   -- 2. meta      ( TRUE  )
   -- 3. keypad    ( TRUE  )
   -- 4. nodelay   ( TRUE  )
   
   
   procedure CURSES_delwin (win: in Surface_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_delwin";
   
   
   procedure CURSES_getmaxyx (win       : in     Surface_Handle; 
                              maxy, maxx:    out int)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_getmaxyx";
   
   
   procedure CURSES_wmove (win : in Surface_Handle;
                           y, x: in int)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wmove";
   -- In case you're wondering - according to the wmove man page: "move may be
   -- a macro".
   
   procedure CURSES_meta_wattrset 
     (win                  : in Surface_Handle;
      bold, standout, dim, 
      uline, invert, blink : in unsigned)
   with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wattrset";
   -- Calls wattrset on the target window as per the opeartions specified in
   -- the various parameters. For "unsigned" parameters, zero is off, any other
   -- value is on.
   --
   -- Note that color is ignored in this operation, and for terminals that
   -- support color, this is set separately through the color binding package
   
   procedure CURSES_meta_wclrch (win: in Surface_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wclrch";
   -- Clears the character at the position 
   
   
   function CURSES_waddstr (win: Surface_Handle; str: char_array) return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_waddstr";
   -- Returns non-zero on error
   
   
   function CURSES_winnstr (win: in     Surface_Handle; 
                            str: in out char_array; 
                            len: in     int)
                           return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_winnstr";
   -- Returns number of characters read-in
   
   
   procedure CURSES_pnoutrefresh (pad             : in Surface_Handle;
                                  pminrow, pmincol: in int;
                                  sminrow, smincol: in int;
                                  smaxrow, smaxcol: in int)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_pnoutrefresh";
   
   procedure CURSES_touchline (win  : in Surface_Handle;
                               start: in int;
                               count: in int)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_touchline";
   
   procedure CURSES_wclear (win: in Surface_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wclear";
   
   procedure CURSES_wclrtoeol (win: in Surface_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wclrtoeol";
   
   function CURSES_meta_wbkgd (win   : in Surface_Handle;
                               blank : in char;
                               bold, standout, dim, uline, invert,
                               blink : in unsigned)
                              return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wbkgd";
   -- Returns Bool_False on failure
   
   function CURSES_meta_default_wborder (win: in Surface_Handle;
                                         bold, standout, dim, uline, invert,
                                         blink: in unsigned)
                                        return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_default_wborder";
   -- calls wborder with the "default" arguments. 
   
   function CURSES_meta_wborder (win: in Surface_Handle;
                                 bold, standout, dim, uline, invert,
                                 blink: in unsigned;
                                 ls, rs, ts, bs, tl, tr, bl, br: in char)
                                return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_wborder";
   
   function CURSES_copywin (srcwin : in Surface_Handle;
                            dstwin : in Surface_Handle;
                            sminrow: in int;
                            smincol: in int;
                            dminrow: in int;
                            dmincol: in int;
                            dmaxrow: in int;
                            dmaxcol: in int)
                           return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_copywin";
   -- Always calls with overlay set to false
   -- Returns Bool_False on failure
   
   
   function CURSES_wresize (win    : in Surface_Handle;
                            lines  : in int;
                            columns: in int)
                           return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wresize";
   
   
   function CURSES_mvwin (win: in Surface_Handle;
                          y  : in int;
                          x  : in int)
                         return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_mvwin";
   
   
   --
   -- Interface
   --
   
   
   --------------------
   -- Create_Surface --
   --------------------
   function  Create_Surface (TTY    : Terminal_Handle;
                             Extents: Cursor_Position)
                            return Surface_Handle
   is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (TTY) then
         raise Curses_Library with "Terminal handle is invalid";
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if not Select_Terminal (TTY) then
         -- This should be impossible, as we've already screened the validity
         -- of the Handle.
         raise Curses_Library with "Terminal select failed";
      end if;
      
      -- Get to it
      return New_Window: Surface_Handle do
        New_Window := CURSES_newpad (nlines => int (Extents.Row   ), 
                                     ncols  => int (Extents.Column));
        -- Note that we do not decrement Extents.Row/Column since this is
        -- actually asking for the _number_ of lines/cols
        
        if not Handle_Valid (New_Window) then
           Serial.Unlock;
           raise Curses_Library with "Unable to create window";
        end if;
        
        -- Set up the new window in a standard fashion
        CURSES_meta_setup_win (New_Window);
        Serial.Unlock;
        
      end return;
      
   exception
      when Curses_Library => 
         raise;
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
         
   end Create_Surface;
   
   
   ----------------
   -- Create_Pad --
   ----------------
   function  Create_Pad (TTY          : Terminal_Handle;
                         Rows, Columns: Cursor_Ordinal)
                        return Surface_Handle
   is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (TTY) then
         raise Curses_Library with "Terminal handle is invalid";
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if not Select_Terminal (TTY) then
         -- This should be impossible, as we've already screened the validity of
         -- the Handle.
         Serial.Unlock;
         raise Curses_Library with "Terminal select failed";
      end if;
      
      -- Get to it
      return New_Pad: Surface_Handle do
        New_Pad := CURSES_newpad (nlines => int (Rows    - 1), 
                                  ncols  => int (Columns - 1));
        
        if not Handle_Valid (New_Pad) then
           Serial.Unlock;
           raise Curses_Library with "Unable to create pad";
        end if;
      end return;
      
   exception
      when Curses_Library => 
         raise;
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
         
   end Create_Pad;
   
   
   ---------------------
   -- Destroy_Surface --
   ---------------------
   procedure Destroy_Surface (Handle: in out Surface_Handle)
   is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (Handle) then
        -- Nothing to do here
        return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_delwin (Handle);
      Serial.Unlock;
      Locked := False;
      
      Invalidate_Handle (Handle);
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
        
   end Destroy_Surface;
   
   
   
   ------------------
   -- Max_Position --
   ------------------
   function Max_Position (Handle  : in     Surface_Handle)
                         return Cursor_Position
   is
      Y, X: int;
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         raise Surface_Unavailable;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_getmaxyx (win => Handle, maxy => Y, maxx => X);
      
      Serial.Unlock;
      Locked := False;
      
      -- The returned "maxyx" is actually not the max y or x. It is the actual
      -- number of rows and columns. Note that we are called Max_Position, and
      -- we are returning a Cursor_Position, so this is exactly correct! We
      -- don't need to perform any adjustments
      
      return Cursor_Position'(Row    => Cursor_Ordinal (Y),
                              Column => Cursor_Ordinal (X));
      
   exception
      when Curses_Library | Surface_Unavailable => 
         raise;
         
      when Constraint_Error =>
         raise Curses_Library with "Reported Max Position not valid";
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with "Unexpected Exception: " &
           Exceptions.Exception_Information (e);
      
   end Max_Position;
   
   
   ------------
   -- Resize --
   ------------
   procedure Resize (Handle       : in Surface_Handle;
                     Rows, Columns: in Cursor_Ordinal)
   is
      Lock_OK: Boolean := False;
      Result : Bool;
      
   begin
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_wresize (win     => Handle, 
                                lines   => int (Rows    - 1), 
                                columns => int (Columns - 1));
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with "Library call to wresize () failed. " &
           "Memory structures failed allocation.";
         -- This is according to the man page (we ensured no zero values with a
         -- precondition).
      end if;
      
   exception
      when Curses_Library =>
         raise;
           
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with "Unexpected exception";
      
   end Resize;
   
   
   ----------
   -- Move --
   ----------
   procedure Move (Handle   : in Surface_Handle;
                   Top_Left : in Cursor_Position)
   is
      Lock_OK: Boolean := False;
      Result : Bool;
      
   begin
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_mvwin (win => Handle, 
                              x   => int (Top_Left.Column - 1), 
                              y   => int (Top_Left.Row    - 1));
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with "Library call to mvwin () failed. ";
         -- According to the manpage for mvwin, this really means the new
         -- coordinates were somehow out of range of the terminal..
      end if;
      
   exception
      when Curses_Library =>
         raise;
           
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with "Unexpected exception";
      
   end Move;
   
   
   ------------------
   -- Place_Cursor --
   ------------------
   procedure Place_Cursor (Handle  : in Surface_Handle;
                           Position: in Cursor_Position)
   is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (Handle) then
         -- Ignored operation - no surface on which to position cursor
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_wmove (win => Handle, 
                    y   => int (Position.Row    - 1), 
                    x   => int (Position.Column - 1));
      -- We don't check for any error here, as it is the responsiiblity of the
      -- Curses.Standard package to enforce Cursor bounds
      
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
   end Place_Cursor;
   
   
   --------------------
   -- Set_Attributes --
   --------------------
   procedure Set_Attributes (Handle   : in Surface_Handle;
                             Style    : in Cursor_Style)
   is
      Set_Bold, Set_Standout, Set_Dim, 
      Set_Uline, Set_Invert, Set_Blink: unsigned := 0;
      
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         -- Ignore this
         return;
      end if;

      if Style.Bold then
         Set_Bold := unsigned'Last;
      end if;
      
      if Style.Standout then
         Set_Standout := unsigned'Last;
      end if;
      
      if Style.Dim then
         Set_Dim := unsigned'Last;
      end if;
      
      if Style.Underline then
         Set_Uline := unsigned'Last;
      end if;
      
      if Style.Inverted then
         Set_Invert := unsigned'Last;
      end if;
      
      if Style.Blink then
         Set_Blink := unsigned'Last;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_meta_wattrset
        (win   => Handle,
         bold  => Set_Bold,  standout => Set_Standout, dim   => Set_Dim,
         uline => Set_Uline, invert   => Set_Invert,   blink => Set_Blink);
      
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
      
   end Set_Attributes;
   
   
   ---------------------
   -- Clear_Character --
   ---------------------
   procedure Clear_Character (Handle: in Surface_Handle)
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_meta_wclrch (Handle);
      
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
   end Clear_Character;
   
   
   ----------------
   -- Put_String --
   ----------------
   procedure Generic_Put_String (Handle: in Surface_Handle;
                                 Buffer: in Ada_String_Type)
   is
      Locked  : Boolean       := False;
      C_Buffer: C_String_Type := To_C (Item => Buffer, Append_Nul => True);
   begin
      if not Handle_Valid (Handle) then
         raise Surface_Unavailable;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if CURSES_generic_waddstr (win => Handle, str => C_Buffer) /= 0 then
         Serial.Unlock;
         raise Curses_Library with
           "Failed to write string to surface";
      end if;
      
      Serial.Unlock;
      
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Generic_Put_String;
   
   
   ----------------------------------------
   procedure Put_String (Handle: in Surface_Handle;
                         Buffer: in String)
   is
      procedure Put_String_Actual is new Generic_Put_String
        (Ada_Character_Type     => Character,
         Ada_String_Type        => String,
         C_Character_Type       => Interfaces.C.char,
         C_String_Type          => Interfaces.C.char_array,
         To_C                   => Interfaces.C.To_C,
         CURSES_generic_waddstr => CURSES_waddstr)
        with Inline;
   begin
      Put_String_Actual (Handle => Handle, Buffer => Buffer);
   end Put_String;

   
   
   ----------------
   -- Get_String --
   ----------------
   procedure Generic_Get_String (Handle: in     Surface_Handle;
                                 Buffer:    out Ada_String_Type;
                                 Last  :    out Natural)
   is
      Locked       : Boolean    := False;
      Buffer_Length: size_t;
      -- We set the Buffer size inside of the procedure in the off-chance that
      -- it raises an exception.
   begin
      if not Handle_Valid (Handle) then
         raise Surface_Unavailable;
      end if;
      
      -- Set up the buffer
      Buffer_Length := Buffer'Length;
      
      declare
         C_Buffer: C_String_Type (1 .. Buffer_Length + 1);
         -- Length + 1 since the man page for winnstr does not specify if n
         -- includes the NUL termination. So obviously it is safer to assume
         -- that it does not.
         
         Read_In: int;
      begin
         
         Lock_Or_Panic;
         Locked := True;
         
         Read_In := CURSES_generic_winnstr (win => Handle, 
                                            str => C_Buffer,
                                            len => int (Buffer_Length));
         -- Note that we do the size_t -> int conversion in the safety of Ada,
         -- which protects from any overflows. The actual curses winnstr
         -- function does weirdly take int for the length
         
         Serial.Unlock;
         Locked := False;
         
         -- Convert whatever we got
         Last := Buffer'First + Natural (Read_In) - 1;
         Buffer(Buffer'First .. Last) := To_Ada (Item     => C_Buffer, 
                                                 Trim_Nul => True);
      end;
      
   exception
      when Surface_Unavailable =>
         raise;
         
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
         
   end Generic_Get_String;
   
   ----------------------------------------
   procedure Get_String (Handle: in     Surface_Handle;
                         Buffer:    out String;
                         Last  :    out Natural)
   is
      procedure Get_String_Actual is new Generic_Get_String
        (Ada_Character_Type => Character,
         Ada_String_Type    => String,
         C_Character_Type   => Interfaces.C.char,
         C_String_Type      => Interfaces.C.char_array,
         To_Ada             => Interfaces.C.To_Ada,
         CURSES_generic_winnstr => CURSES_winnstr)
        with Inline;
   begin
      Get_String_Actual (Handle => Handle,
                         Buffer => Buffer,
                         Last   => Last);
   end Get_String;
   
   
   --------------------
   -- Render_Surface --
   --------------------
   procedure Render_Surface (Handle     : in Surface_Handle;
                             Surface_TL : in Cursor_Position;
                             Physical_TL: in Cursor_Position;
                             Physical_BR: in Cursor_Position;
                             Redraw     : in Boolean := False)
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if Redraw then
         CURSES_touchline 
           (win   => Handle,
            start => int (Surface_TL.Row - 1),
            count => int (Physical_BR.Row - Physical_TL.Row + 1));
      end if;
      
      CURSES_pnoutrefresh (pad     => Handle,
                           pminrow => int (Surface_TL.Row     - 1),
                           pmincol => int (Surface_TL.Column  - 1),
                           sminrow => int (Physical_TL.Row    - 1),
                           smincol => int (Physical_TL.Column - 1),
                           smaxrow => int (Physical_BR.Row    - 1),
                           smaxcol => int (Physical_BR.Column - 1));
      
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
         
   end Render_Surface;
   
   
   -------------------
   -- Clear_Surface --
   -------------------
   procedure Clear_Surface (Handle: in out Surface_Handle)
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_wclear (Handle);
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
         
   end Clear_Surface;
   
   
   --------------------------
   -- Clear_To_End_Of_Line --
   --------------------------
   procedure Clear_To_End_Of_Line (Handle: in out Surface_Handle)
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_wclrtoeol (Handle);
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
         
   end Clear_To_End_Of_Line;
   
   
   -------------------------------
   -- Set_Monochrome_Background --
   -------------------------------
   procedure Generic_Set_Monochrome_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Ada_Char_Type;
      Reference_Cursor: in Cursor'Class)
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
      
      Result := CURSES_generic_meta_set_background
        (win      => Handle,
         blank    => To_C (Blank_Character),
         bold     => Set_Bold, 
         standout => Set_Standout,
         dim      => Set_Dim,
         uline    => Set_Uline,
         invert   => Set_Invert,
         blink    => Set_Blink);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkg(rn)d () (without color attributes) failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         raise Curses_Library with "Unexpected exception";
         
   end Generic_Set_Monochrome_Background;
   
   
   ----------------------------------------
   procedure Set_Monochrome_Background (Handle          : in Surface_Handle;
                                        Blank_Character : in Character;
                                        Reference_Cursor: in Cursor'Class)
   is
      procedure Set_Monochrome_Background_Actual is 
        new Generic_Set_Monochrome_Background
          (Ada_Char_Type => Character,
           C_Char_Type   => char,
           To_C          => Interfaces.C.To_C,
           
           CURSES_generic_meta_set_background => CURSES_meta_wbkgd)
        with Inline;
   begin
      Set_Monochrome_Background_Actual
        (Handle           => Handle,
         Blank_Character  => Blank_Character,
         Reference_Cursor => Reference_Cursor);
   end Set_Monochrome_Background;
   
   
   -----------------------------------
   -- Set_Default_Monochrome_Border --
   -----------------------------------
   procedure Set_Default_Monochrome_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class)
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
      
      Result := CURSES_meta_default_wborder
        (win      => Handle,
         bold     => Set_Bold, 
         standout => Set_Standout,
         dim      => Set_Dim,
         uline    => Set_Uline,
         invert   => Set_Invert,
         blink    => Set_Blink);
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkg(rn)d () (without color attributes) failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         raise Curses_Library with "Unexpected exception";
         
   end Set_Default_Monochrome_Border;
   
   
   ---------------------------
   -- Set_Monochrome_Border --
   ---------------------------
   procedure Generic_Set_Monochrome_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
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
      
      Result := CURSES_generic_meta_wborder
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
         br => To_C (BR));
      
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with
           "Library call to wbkg(rn)d () (without color attributes) failed.";
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         raise Curses_Library with "Unexpected exception";
         
   end Generic_Set_Monochrome_Border;
   ----------------------------------------
   
   procedure Set_Monochrome_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      LS, RS, TS, BS, TL, TR, BL, BR: in Character)
   is
      procedure Set_Monochrome_Border_Actual is 
        new Generic_Set_Monochrome_Border 
          (Ada_Char_Type => Character,
           C_Char_Type   => char,
           To_C => Interfaces.C.To_C,
           CURSES_generic_meta_wborder => CURSES_meta_wborder)
        with Inline;
      
   begin
      Set_Monochrome_Border_Actual (Handle           => Handle,
                                    Reference_Cursor => Reference_Cursor,
                                    LS               => LS,
                                    RS               => RS,
                                    TS               => TS,
                                    BS               => BS,
                                    TL               => TL,
                                    TR               => TR,
                                    BL               => BL,
                                    BR               => BR);
   end Set_Monochrome_Border;
   
   
   ---------------
   -- Copy_Area --
   ---------------
   procedure Copy_Area (From_Handle : in Surface_Handle;
                        From_TL     : in Cursor_Position;
                        
                        To_Handle   : in Surface_Handle;
                        To_TL, To_BR: in Cursor_Position)
   is
      Lock_OK: Boolean := False;
      Result : Bool;
   begin
      if not Handle_Valid (From_Handle) then
         raise Curses_Library 
           with '"' & "From" & '"' &" surface handle invalid";
         
      elsif not Handle_Valid (To_Handle) then
         raise Curses_Library 
           with '"' & "To" & '"' & " surface handle invalid";
         
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      Result := CURSES_copywin (srcwin  => From_Handle,
                                dstwin  => To_Handle,
                                sminrow => int (From_TL.Row    - 1),
                                smincol => int (From_TL.Column - 1),
                                dminrow => int (To_TL.Row      - 1),
                                dmincol => int (To_TL.Column   - 1),
                                dmaxrow => int (To_BR.Row      - 1),
                                dmaxcol => int (To_BR.Column   - 1));
      Serial.Unlock;
      Lock_OK := False;
      
      if Result /= Bool_True then
         raise Curses_Library with "Library call to copywin () failed.";
      end if;
      
   exception
      when Curses_Library =>
         raise;
         
      when others =>
         if Lock_OK then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with "Unexpected exception"; 
      
   end Copy_Area;
   
   

        
   
   
end Curses.Binding.Render;
