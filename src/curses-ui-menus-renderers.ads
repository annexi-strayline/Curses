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

with Curses.Standard;
with Curses.Frames;
with Curses.Terminals.Surfaces;

package Curses.UI.Menus.Renderers is 
   
   subtype Screen is Curses.Standard.Screen;
   subtype Window is Curses.Standard.Window;
   subtype Frame  is Curses.Frames.Frame;
   
   subtype Control_Class is Curses.Terminals.Surfaces.Control_Class;
   use all type Control_Class;
   
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
   
   
   generic
      -- Fill is called first, followed by Set_Border when creating a
      -- new Window
      with procedure Fill (The_Surface: in out Window'Class)
        is null;
      
      with procedure Set_Border (The_Surface: in out Window'Class;
                                 Main_Menu  : in     Boolean)
        is null;
      -- The "default" Border is an empty border - this can works well if
      -- a Fill is provided

      Vertical_Padding  : in Natural := 0;
      Horizontal_Padding: in Natural := 0;
      -- Padding refers to the inside of the border.
      
      Top_Clear_On_Main: in Boolean := False;
      -- If True, it is assumed that the first row of the main menu window
      -- should be the first useable row (including Vertical_Padding), i.e.
      -- the top row is set by Set_Border (except for the sides)
      
      Horizontal_Fly_Out_Offset: in Integer := 0;
      Vertical_Fly_Out_Offset  : in Integer := 0;
   -- Adjusts the relative position of fly-out submenus, with negative
   -- values being left, and up, respectively.
   
   function Generic_Style_And_Frame 
     (S                  : aliased in out Screen'Class;
      Main_Menu          :         in     Boolean;
      Conceptual_Top_Left:         in     Cursor_Position;
      Required_Extents   :         in     Cursor_Position;
      Frame_Top_Left     :            out Cursor_Position)
     return Window'Class;
   
   
   
end Curses.UI.Menus.Renderers;
