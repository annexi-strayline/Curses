------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

with System;
with Interfaces.C; use Interfaces.C;

with Ada.Exceptions; use Ada;

with Ada.Strings;
with Ada.Strings.Fixed;

package body Curses.Binding.Terminals is
   
   ----------------
   -- C Bindings --
   ----------------
   
   -- (n)curses --
   ---------------
   -- We can't bind directly to the (n) curses library since the target
   -- procedures may be macros
   
   function CURSES_newterm
     (model: in char_array; outfd, infd: in File_Handle)
     return Terminal_Handle
     with 
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_newterm";
   -- curs_initscr(3X)
   -- SCREEN *newterm(char *type, FILE *outfd, FILE *infd)
   
   procedure CURSES_meta_initterm
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_meta_initterm";
   -- Meta command. Executes the following sys call and curses library calls in 
   -- order:
   -- 1. Set SIGWINCH signal handler to ignore
   -- 2. noecho
   -- 3. raw
   -- 4. nonl
   
   procedure CURSES_endwin
     with 
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_endwin";
   
   procedure CURSES_delscreen (sp: in Terminal_Handle)
     with 
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_delscreen";
   
   procedure CURSES_doupdate_meta (crow, ccol: in int)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_doupdate_meta";
   -- 1. Saves the virtual screen's cursor position, 
   -- 2. sets it to crow, ccol,
   -- 3. does a doupdate
   -- 4. restores the cursor
   
   procedure CURSES_clear
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_clear";
   
   procedure CURSES_query_extents 
     (line          : in     File_Handle;     -- FILE * 
      ws_row, ws_col:    out unsigned_short)
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_query_extents";
   -- Executes a TIOCGWINSZ IOCTL on fd, and returns the reported ws_row and
   -- ws_col values of the winsize structure returned. If the call failed,
   -- ws_col and ws_row will both be set to -1
   
   
   function CURSES_curs_set (visibility: int) return bool
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_curs_set";
   -- returns Bool_False if the operation failed.
   
   CURSES_curs_set_hide: constant := 0;
   CURSES_curs_set_show: constant := 1;
   
   
   function CURSES_wgetch (win: in Surface_Handle) return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_wgetch";
   -- Returns -1 on timeout or error, zero otherwise
   
   function CURSES_ungetch (ch: CURSES_Character) return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_ungetch";
   -- Returns -1 on error, zero otherwise
   
   --------------------------
   -- Incorporate_Terminal --
   --------------------------
   procedure Incorporate_Terminal (Line  : in     Line_Handle;
                                   Model : in     String;
                                   Handle:    out Terminal_Handle;
                                   Error :    out Library_Error_Message)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      
      Lock_OK     : Boolean          := False;
      
   begin
      
      -- Check that the Line is a valid handle
      if not Handle_Valid (Line) then
         Set_Library_Error_Message
           (Buffer  => Error,
            Message => "Line_Handle is invalid.");
         
         Invalidate_Handle (Handle);
         return;
         
      end if;
      
      -- Wait for exclusive access
      Lock_Or_Panic;
      Lock_OK := True;
      
      declare
         Term_Type: char_array 
           := To_C (Trim(Source => Model, Side => Right));
         -- Note
         -- There is additional logic in the c binding which passes a NULL
         -- pointer to the type parameter of newterm when the first character
         -- of Term_Type is a space or a null character.
      begin
         Handle := CURSES_newterm (model => Term_Type,
                                   outfd => Line.Output,
                                   infd  => Line.Input);
      end;
         
      if not Handle_Valid (Handle) 
        or else not Select_Terminal (Handle)
      then
         Serial.Unlock;
         Lock_OK := False;
         
         Invalidate_Handle (Handle); -- Just to be sure
         Set_Library_Error_Message 
           (Buffer  => Error,
            Message => "Unable to initialize terminal");
         return;
      end if;
      
      -- New terminal is selected, run the terminal initialization package
      CURSES_meta_initterm;
         
      -- Release lock and return
      Serial.Unlock;
      
   exception
      when others =>
         if Lock_OK then
            -- This means we got the lock, so we need to unlock it
            Serial.Unlock;
         end if;
         
         Set_Library_Error_Message (Buffer  => Error,
                                    Message => "Unexpected exception");
         Invalidate_Handle (Handle);
      
   end Incorporate_Terminal;
   
   
   ---------------------
   -- Disban_Terminal --
   ---------------------
   procedure Disban_Terminal (TTY: in out Terminal_Handle)
   is
      Lock_OK: Boolean := False;

   begin
      -- Is the handle valid?
      if not Handle_Valid (TTY) then
         -- We can't really disband an invalid handle..
         return;
      end if;
      
      Lock_Or_Panic;
      Lock_OK := True;
      
      -- First, we need to actually make sure the Terminal we are disbanding
      -- is the current terminal.
      if not Select_Terminal (TTY) then
         -- This is not supposed to happen, but looks like we can't
         -- switch to it anyways. We have a contract to not return any
         -- error or exception, so in this case we just invalidate the
         -- handle and call it quits
         
         Serial.Unlock;
         Invalidate_Handle (TTY);
         return;
         
      end if;
      
      -- According to the man-page, we have a two step process to execute.
      -- First we call endwin on the now selected terminal, and then
      -- call delscreen on the actual Terminal_Handle. We will ignore an
      -- error on endwin, since there is really nothing we could do about
      -- it anyways, and we are committed to closing this terminal.
      
      CURSES_endwin;
      CURSES_delscreen (TTY);
      
      -- Invalidate the "current terminal", and unlock
      Serial.Set_Terminal (Invalid_Handle);
      Serial.Unlock;
      
      Invalidate_Handle (TTY);
      
   exception
      when others =>
         -- Unlock the serializer if we got a lock
         if Lock_OK then
            Serial.Unlock;
         end if;
         
   end Disban_Terminal;
   
   
   ----------------------
   -- Refresh_Terminal --
   ----------------------
   procedure Refresh_Terminal (TTY   : in Terminal_Handle; 
                               Cursor: in Cursor_Position)
   is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (TTY) then
         -- This terminal is invalid. Not much we can do here
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         return;
      end if;
      
      CURSES_doupdate_meta (crow => int (Cursor.Row    - 1),
                            ccol => int (Cursor.Column - 1));
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
      
   end Refresh_Terminal;
   
   
   --------------------
   -- Clear_Terminal --
   --------------------
   procedure Clear_Terminal (TTY: in Terminal_Handle) is
      Locked: Boolean := False;
      
   begin
      if not Handle_Valid (TTY) then
         -- This terminal is invalid. Not much we can do here
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         return;
      end if;
      
      CURSES_clear;
      
      Serial.Unlock;
      
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
      
   end Clear_Terminal;
   
   
   ----------------------------
   -- Query_Physical_Extents --
   ----------------------------
   function  Query_Physical_Extents (Line: in Line_Handle)
                                    return Cursor_Position
   is
      Locked: Boolean := False;
      
      ws_row, ws_col: unsigned_short;
   begin
      if not Handle_Valid (Line) then
         raise Curses_Library with "Line handle invalid";
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      CURSES_query_extents (line   => Line.Output,
                            ws_row => ws_row,
                            ws_col => ws_col);
      Serial.Unlock;
      Locked := False;
      
      -- For physical terminals (they still exist in the wild!), this ioctl
      -- has a tendency to return zero. When that happens, we assume a standard
      -- 80x24 size. When an error is encountered, ws_row and ws_col are set to
      -- -1, in which case we make the same assumption, since this is a bit
      -- early to call it a day.
      if ws_row <= 0 then
         ws_row := 24;
      end if;

      if ws_col <= 0 then
         ws_col := 80;
      end if;
      
      return Extents: Cursor_Position do
        Extents.Row    := Cursor_Ordinal (ws_row);
        Extents.Column := Cursor_Ordinal (ws_col);
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
      
   end Query_Physical_Extents;
   
   
   -----------------
   -- Hide_Cursor --
   -----------------
   function  Hide_Cursor (TTY: in Terminal_Handle) return Boolean is
      Result: Boolean;
      Locked: Boolean := False;
   begin
      if not Handle_Valid (TTY) then
         return False;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      Result := (CURSES_curs_set (CURSES_curs_set_hide) = Bool_True);
      Serial.Unlock;
      Locked := False;
      
      return Result;
         
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
         return False;
   end Hide_Cursor;
         
   ----------------------------------------
   procedure Hide_Cursor (TTY: in Terminal_Handle) is
      Dont_Care: Boolean;
   begin
      Dont_Care := Hide_Cursor (TTY);
   end Hide_Cursor;
   
   
   -----------------
   -- Show_Cursor --
   -----------------
   procedure Show_Cursor (TTY: in Terminal_Handle) is
      Dont_Care: bool;
      Locked: Boolean := False;
   begin
      if not Handle_Valid (TTY) then
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      Dont_Care := CURSES_curs_set (CURSES_curs_set_show);
      Serial.Unlock;
         
   exception
      when others =>
         if Locked then
            Serial.Unlock;
         end if;
   end Show_Cursor;
   
   
   
   --
   -- Input Management
   --
   
   ----------------
   -- Pop_Buffer --
   ----------------
   function Pop_Buffer (Handle: Surface_Handle) return CURSES_Character
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (Handle) then
         return CURSES_KEY_NOKEY;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      return Char: CURSES_Character do
        Char := CURSES_wgetch (Handle);
        Serial.Unlock;
      end return;
      
   exception
      when others =>
         -- No conceivable reason for this happening, but if it does,
         -- unfortunately, and characters popped will be lost.
         if Locked then
            Serial.Unlock;
         end if;
         return CURSES_KEY_NOKEY;
         
   end Pop_Buffer;
   
   
   -----------------
   -- Push_Buffer --
   -----------------
   procedure Push_Buffer (TTY : in Terminal_Handle;
                          Char: in CURSES_Character)
   is
      Locked: Boolean := False;
   begin
      if not Handle_Valid (TTY) then
         -- This terminal is invalid. Not much we can do here
         return;
      end if;
      
      Lock_Or_Panic;
      Locked := True;
      
      if not Select_Terminal (TTY) then
         Serial.Unlock;
         return;
      end if;
      
      if CURSES_ungetch (Char) /= 0 then
         -- Some kind of error
         Serial.Unlock;
         Locked := False;
         
         raise Curses_Library with "Call to ungetch() failed.";
      end if;
      
      Serial.Unlock;
      
   exception
      when Curses_Library =>
         raise;
      
      when e: others =>
         if Locked then
            Serial.Unlock;
         end if;
         
         raise Curses_Library with "Unexpected exception: " &
           Exceptions.Exception_Information (e);
         
   end Push_Buffer;
   
   
end Curses.Binding.Terminals;
