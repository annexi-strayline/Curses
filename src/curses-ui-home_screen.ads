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

with Curses.Terminals;
with Curses.Standard;

package Curses.UI.Home_Screen is
   
   
   type Key_Row_Unit is private;
   
   function Config_Key (Key_Label: String; Description: String)
                       return Key_Row_Unit;
   -- Initialize a Key_Row_Unit for use in a Key_Row with a label for the
   -- key, and a decription of what the key does
   
   type Home_Screen (TTY: not null access Terminals.Terminal) is
     tagged private;
   
   -- Panel Operations --
   -- ---------------- --
   -- Panels refer to the unused area in the center of a Home_Screen,
   -- the size of which depends on the configuration of a the menu bar,
   -- key rows, or the status bar.
   
   function Panel_Start   (S: in out Home_Screen'Class) return Cursor_Position;
   -- Returns the top-left corner of the panel region relative to the Screen
   
   function Panel_Extents (S: in out Home_Screen'Class) return Cursor_Position;
   -- Returns the bottom-right corner of the panel region relative to
   -- Panel_Start
   
   
   
private
   
   type Home_Screen (TTY: not null access Terminals.Terminal) is
     new Standard.Screen (TTY) with null record;
   -- We hide the fact that Home_Screen is actually a surface because we want
   -- to force the user to spawn windows from the Home_Screen, and not roll
   -- their own. This is necessary to ensure that the Home_Screen is always
   -- able to peek from the input buffer before allowing a given window to
   -- attempt Input_Key
   
end Curses.UI.Home_Screen;
