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

with Curses.Standard;              use Curses.Standard;
with Curses.Frames.Framed_Windows; use Curses.Frames.Framed_Windows;

generic
   with function  Frame_And_Style
     (On_Screen    : aliased in out Screen'Class;
      Root_Item    :         in     Menu_Cursor_Type'Class;
      Frame_Extents:         in     Cursor_Position;
      TL_Hint      :         in     Cursor_Position)
     return Framed_Window'Class;
   -- Called when opening a given Submenu to set up a Framed_Window that
   -- should (if applicable) accomodate a Frame of Frame_Extent extents.
   --
   -- TL_Hint gives the position relative to On_Screen that represents the
   -- last column of the parent item for the Submenu. For the first call to
   -- Cascade
   
   with procedure Configure_Renderer (Renderer: in out Menu_Renderer'Class)
     is null;
   -- Called when creating a new Menu_Renderer object for rendering the next
   -- submenu, to correctly configure that renderer before it is used
   
   
   -- Defaults for the relevant parameters. 
   Default_Min_Width: in Cursor_Ordinal := 1;
   Default_Max_Width: in Cursor_Ordinal := Cursor_Ordinal'Last;
   
   Default_Min_Height: in Cursor_Ordinal := 1;
   Default_Max_Height: in Cursor_Ordinal := Cursor_Ordinal'Last;

   Default_Scrollbar_Provision: in Natural := 0;
   
   -- Algorithm Configuration
   Left_Right_Navigation: in Boolean := True;
   -- When True, Right_Key will open a submenu if available. Left_Key will 
   -- close the currently open Submenu

   Hide_Parent: in Boolean := False;
   -- When True, the parent Submenu will be hidden when opening a child Submenu


procedure Curses.UI.Menus.Renderer.Cascade
  (On_Screen : aliased in out Screen'Class;
   Branch    : aliased in out Menu_Type'Class;
   
   Min_Width : in Cursor_Ordinal := Default_Min_Width;
   Max_Width : in Cursor_Ordinal := Default_Max_Width;
   
   Min_Height: in Cursor_Ordinal := Default_Min_Height;
   Max_Height: in Cursor_Ordinal := Default_Max_Height;
   
   Scrollbar_Provision: in Natural := Default_Scrollbar_Provision);
-- Cascade opens a cascading menu starting with Branch, on Screen On_Screen.
--
-- The Cascade procedure creates and interact Menu_Renderer objects and 
-- executes selected Items and responds to the After_Execute_Directive.
--
-- Cascade returns once a selected Item indicates either Close_Tree, or
-- Close_Menu if the Item is on the "main" (initial) branch
--
-- Min/Max Width/Height
--
-- These parameters defines the defaults for the calculated Frame_Extents
-- parameter passed to Frame_And_Style, which is based on the number of items,
-- and the longest label, of all Items in a Submenu.
--
-- Minimums will always be enforced even if not needed.
-- Maximums will force a maximum width to Frame_Extents
--
-- Scrollbar_Provision
-- Indicates the number of columns that should be added to the required
-- Frame_Extents to make room for a Scrollbar.
