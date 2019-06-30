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

-- This package provides bindings involved in the initialization and retirement
-- of Terminal_Handles

package Curses.Binding.Terminals is
   
   ------------------------
   -- Raw Character Type --
   ------------------------
   subtype CURSES_Character is int;
   -- The raw representation of a character from the binding (getch)
   
   ------------------------------
   -- Control Character Tokens --
   ------------------------------
   -- These are selectively returned by the getch operations as may be
   -- appropriate.
   --
   -- See the getch manpage
   --
   -- Note that, this binding is written in 2018. No one has > 24 F keys
   -- anymore.
   
   CURSES_KEY_NOKEY    : constant CURSES_Character;
   
   CURSES_KEY_ESC      : constant CURSES_Character;
   CURSES_KEY_DOWN     : constant CURSES_Character;
   CURSES_KEY_UP       : constant CURSES_Character;
   CURSES_KEY_LEFT     : constant CURSES_Character;
   CURSES_KEY_RIGHT    : constant CURSES_Character;
   CURSES_KEY_HOME     : constant CURSES_Character;
   CURSES_KEY_END      : constant CURSES_Character;
   CURSES_KEY_INSERT   : constant CURSES_Character;
   CURSES_KEY_DELETE   : constant CURSES_Character;
   CURSES_KEY_BACKSPACE: constant CURSES_Character;
   CURSES_KEY_ENTER    : constant CURSES_Character;
   CURSES_KEY_PAGEDN   : constant CURSES_Character;
   CURSES_KEY_PAGEUP   : constant CURSES_Character;
   
   CURSES_KEY_F1       : constant CURSES_Character;
   CURSES_KEY_F12      : constant CURSES_Character;
   -- The curses standard ensures that the F keys are in a linear sequence
   
   CURSES_KEY_RESIZE   : constant CURSES_Character;
   
   -- Conversions and Ranges --
   ----------------------------
   CURSES_KEY_ALT_OFFSET : constant CURSES_Character := 16#80#;
   -- Holding the Alt key sets the most significant bit of the character 
   -- "byte", and thus subtracting this offset produces the related key-press
   
   CURSES_KEY_CTRL_OFFSET: constant CURSES_Character := 96;
   -- Add to a Control+ character to get the corresponding graphic equivalent
   -- (lower-case)
   -- IE: Ctrl+A -> 1 + 96 => 97 => Character'Val(65) = 'a'
   
   subtype CURSES_KEYS_Graphic is CURSES_Character range 32 .. 126;
   -- Represents the range of regular graphic characters
   
   subtype CURSES_KEYS_CTRL_Graphic is CURSES_KEYS_Graphic
     range CURSES_KEY_CTRL_OFFSET .. CURSES_KEYS_Graphic'Last;
   -- Represents the limited selection of characters which can be computed from
   -- a Ctrl+ key combination
     
   
   ----------------
   -- Operations --
   ----------------
   
   -- Terminal Management --
   -------------------------
   procedure Incorporate_Terminal (Line  : in     Line_Handle;
                                   Model : in     String;
                                   Handle:    out Terminal_Handle;
                                   Error :    out Library_Error_Message);
   -- Performs (n)curses library initialization on the requested line, and
   -- returns a handle to the initialized terminal. if initialization fails,
   -- the handle returned is Invalid, and an appropriate error message is
   -- placed in the buffer.
   --
   -- If Model is an empty string, the (n)curses library will attempt to derive
   -- the terminal model type from the TERM environment variable of the
   -- partition.
   -- -- Suppresses All Exceptions --
   
   procedure Disban_Terminal (TTY: in out Terminal_Handle);
   -- Shuts-down Terminal, and also invalidates the handle
   -- -- Suppresses All Exceptions --
   
   procedure Refresh_Terminal (TTY   : in Terminal_Handle;
                               Cursor: Cursor_Position);
   -- Sends actual changes to the screen buffer to the terminal, and also
   -- ensures that the physical cursor is displayed at the specified position,
   -- without interrupting the "actual" position, through serialized library
   -- calls. This allows for positive of the visible cursor on the screen at
   -- all times.
   -- -- Suppresses All Exceptions --
   
   procedure Clear_Terminal (TTY: in Terminal_Handle);
   -- Clears the terminal contents. Causes a full redraw during next call to 
   -- Refresh_Terminal
   -- -- Suppresses All Exceptions --
   
   function  Query_Physical_Extents (Line: in Line_Handle)
                                    return Cursor_Position;
   -- Does a low-level system call (ioctl) to query the reported physical
   -- terminal dimensions, and returns a Curses_Position representing the
   -- maximum valid position reported by the physical terminal.
   --
   -- Note
   -------
   -- For some older literally "physical" terminals (i.e. terminals that can't 
   -- be resized), the ioctl call returns zeros. This binding returns the
   -- standard 24x80 for Row and Column respectively, if any or both are 
   -- reported as zero - since that is what happens in practice on an literally
   -- physical dumb terminal.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: The ioctl system call failed, or Line was invalid
   
   function  Hide_Cursor (TTY: in Terminal_Handle) return Boolean;
   procedure Hide_Cursor (TTY: in Terminal_Handle);
   -- hides the physical cursor. For the function version, returns False is the
   -- terminal is not capable of hiding the cursor
   -- -- Suppresses All Exceptions --
   
   procedure Show_Cursor (TTY: in Terminal_Handle);
   -- -- Suppresses All Exceptions --
   

   
   -- Input Management --
   ----------------------
   function Pop_Buffer (Handle: Surface_Handle) 
                       return CURSES_Character;
   -- Pops one character off the Terminal's input buffer. This procedure is
   -- non- blocking. If the Terminal's input buffer is empty, or of the
   -- Terminal handle is invalid, CURSES_KEY_NOKEY is returned.
   --
   -- -- Incidental Note --
   -- Handle is a Surface_Handle (unlike Push_Buffer) due to the way that 
   -- "keypad" processing is handled with the (n)curses library - the keypad 
   -- mode (and it's desired translations) are window-specific settings. Since 
   -- we turn keypad mode on for all windows, we have no choice but to use 
   -- wgetch to get the correct returns.
   -- -- Suppresses All Exceptions --
      
   procedure Push_Buffer (TTY : in Terminal_Handle;
                          Char: in CURSES_Character);
   -- Pushes Char back onto the input buffer for the terminal associated with
   -- the Terminal handle.
   --
   -- If Char is equal to CURSES_KEY_NOKEY, this procedure has no effect.
   --
   -- If Terminal is not a valid handle, this procedure has no effect, and does
   -- not raise an exception.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: The binding call indicated an error.
   
   
private
   
   --
   -- Elaboration routines
   --
   
   -- The actual values for the different "keypad" control code constants are
   -- obtained via C macros in (n)curses.h. These functions are implemented in 
   -- the C binding, which simply return the value of the macros. 
   
   function CURSES_KEY_NOKEY_INIT     return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_nokey_init";
   
   function CURSES_KEY_DOWN_INIT      return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_down_init";
     
   function CURSES_KEY_UP_INIT        return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_up_init";
   
   function CURSES_KEY_LEFT_INIT      return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_left_init";
   
   function CURSES_KEY_RIGHT_INIT     return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_right_init";
   
   function CURSES_KEY_HOME_INIT      return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_home_init";
   
   function CURSES_KEY_END_INIT       return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_end_init";
   
   function CURSES_KEY_INSERT_INIT    return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_insert_init";
   
   function CURSES_KEY_DELETE_INIT    return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_delete_init";
   
   function CURSES_KEY_BACKSPACE_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_backspace_init";
   
   function CURSES_KEY_ENTER_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_enter_init";
   
   function CURSES_KEY_PAGEDN_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_pagedn_init";
   
   function CURSES_KEY_PAGEUP_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_pageup_init";
   
   function CURSES_KEY_F0_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_f0_init";
   
   function CURSES_KEY_RESIZE_INIT return CURSES_Character
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_key_resize_init";
   
   
   -- Actual declarations
   CURSES_KEY_NOKEY    : constant CURSES_Character := CURSES_KEY_NOKEY_INIT;
   
   CURSES_KEY_ESC      : constant CURSES_Character := 27;  -- Literal esc
   
   CURSES_KEY_DOWN     : constant CURSES_Character := CURSES_KEY_DOWN_INIT;
   CURSES_KEY_UP       : constant CURSES_Character := CURSES_KEY_UP_INIT;
   CURSES_KEY_LEFT     : constant CURSES_Character := CURSES_KEY_LEFT_INIT;
   CURSES_KEY_RIGHT    : constant CURSES_Character := CURSES_KEY_RIGHT_INIT;
   CURSES_KEY_HOME     : constant CURSES_Character := CURSES_KEY_HOME_INIT;
   CURSES_KEY_END      : constant CURSES_Character := CURSES_KEY_END_INIT;
   CURSES_KEY_INSERT   : constant CURSES_Character := CURSES_KEY_INSERT_INIT;
   CURSES_KEY_DELETE   : constant CURSES_Character := CURSES_KEY_DELETE_INIT;
   CURSES_KEY_BACKSPACE: constant CURSES_Character := CURSES_KEY_BACKSPACE_INIT;
   CURSES_KEY_ENTER    : constant CURSES_Character := CURSES_KEY_ENTER_INIT;
   CURSES_KEY_PAGEDN   : constant CURSES_Character := CURSES_KEY_PAGEDN_INIT;
   CURSES_KEY_PAGEUP   : constant CURSES_Character := CURSES_KEY_PAGEUP_INIT;
   
   CURSES_KEY_F1       : constant CURSES_Character := CURSES_KEY_F0_INIT + 1;
   CURSES_KEY_F12      : constant CURSES_Character := CURSES_KEY_F0_INIT + 12;
   
   CURSES_KEY_RESIZE   : constant CURSES_Character := CURSES_KEY_RESIZE_INIT;
   
end Curses.Binding.Terminals;
