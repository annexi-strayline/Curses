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

-- "Cascade" menu-renderer for complex menus trees

with Curses.Terminals.Surfaces;

-- The Cascade renderer takes an arbitrary Menu_Branch, and starts to
-- render items in the branch into a new Window, continuting to recursively
-- continue as long as the user selects additional Submenu items.

-- The renderer returns as soon as a "Close" directive is received after
-- executing an item

generic
   with function Style_And_Frame
     (S                  : aliased in out Screen'Class;
      Main_Menu          :         in     Boolean;
      Conceptual_Top_Left:         in     Cursor_Position;
      Required_Extents   :         in     Cursor_Position;
      Frame_Top_Left     :            out Cursor_Position)
     return Window'Class;
   -- Given a Screen'Class, Style_And_Frame shall create a new Surface
   -- (typically a Window) sized to enable a frame to be installed on
   -- the returned surface at Frame_Top_Left, and with a Proposed_Extents equal
   -- to Required_Extents
   --
   -- Main_Menu is True when Style_And_Frame is called on initial menu passed
   -- in to Open_Submenu. This is to allow the Style_And_Frame subprogram to
   -- style this initial menu differently from others (e.g. for drop-down
   -- menus)
   --
   -- Conceptual_Top_Left is the proposed Top_Left of a conceptual Window that
   -- has no frame or padding, and has the first item of the menu in the first
   -- row.
   --
   -- If any particular base styling (such as frames/margin), or fill should be
   -- needed, Style_And_Frame shall apply that style to the new Surface before
   -- returning it
   
   Exit_Key: in Control_Character := (Class => Escape, others => <>);
   -- Receipt of this key causes the menu exploration to be aborted,
   -- (and all windows closed)
   
package Curses.UI.Menus.Renderers.Cascade is
   
   procedure Open_Menu (On_Screen: aliased in out Screen'Class;
                        Menu     :         in out Menu_Type'Class);
   -- Open_Menu ensures that all created windows have focus so that they may
   -- receive input to enable menu interaction.
   --
   -- Note that changes to Menu should occur only as the direct result of
   -- executing an item on the sub-tree defined by Menu. Asynchronous 
   -- modification of an open menu can result in unexpected behaviour (for the
   -- user), but should not cause program instability.
   
   
end Curses.UI.Menus.Renderers.Cascade;
