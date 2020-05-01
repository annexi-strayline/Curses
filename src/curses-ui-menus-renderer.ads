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

with Curses.Terminals.Surfaces;

private with Ada.Finalization;
private with Curses.Terminals.Color;

package Curses.UI.Menus.Renderer is 
   
   subtype Rendered_Surface is Curses.Terminals.Surfaces.Rendered_Surface;
   
   subtype Control_Class is Curses.Terminals.Surfaces.Control_Class;
   use all type Control_Class;
   
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
   use type Control_Character;
   
   
   -------------------
   -- Menu_Renderer --
   -------------------
   type Menu_Renderer 
     (Canvas       : not null access Surface'Class;
      Input_Surface: not null access Rendered_Surface'Class;
      Branch       : not null access Menu_Type'Class)
   is tagged private;
   
   procedure Reset (Driver: in out Menu_Renderer);
   -- Resets the state of the Driver, clears the Canvas, and re-renders the
   -- menu. Selection resets to the first item on the list.
   --
   -- Reset is automatically invoked on Initialization of a Menu_Renderer
   -- object.
   --
   -- Reset suppresses all exceptions
   
   procedure Enable_Wrap_Around  (Driver: in out Menu_Renderer);
   procedure Disable_Wrap_Around (Driver: in out Menu_Renderer);
   -- Configures if the menu wraps-around upon reaching either end.
   -- Defaults to Disabled. Only affects Up and Down keys. Page-Up and
   -- Page-Down never wrap-around
   
   procedure Enable_Scrollbar
     (Driver          : in out Menu_Renderer;
      Marker_Style    : in     Cursor'Class;
      Marker_Character: in     Graphic_Character := ' ';
      Bar_Style       : in     Cursor'Class;
      Bar_Character   : in     Graphic_Character := ' ');
   
   procedure Wide_Enable_Scrollbar
     (Driver          : in out Menu_Renderer;
      Marker_Style    : in     Cursor'Class;
      Marker_Character: in     Wide_Graphic_Character := ' ';
      Bar_Style       : in     Cursor'Class;
      Bar_Character   : in     Wide_Graphic_Character := ' ';
      Wide_Fallback   : access function (Item: Wide_Graphic_Character) 
                                        return Graphic_Character := null);
   -- Enables and sets the style for a scroll bar.
   -- If not explictly called, a scrollbar is not rendered
   -- (Defaults to disabled). If the menu does not scroll, the scrollbar is
   -- not rendered.
   --
   -- Note that rendering the scrollbar assumes the right-most column of the
   -- Canvas
   
   procedure Disable_Scrollbar (Driver: in out Menu_Renderer);
   -- Scrollbar is not rendered if the list of items does not fit on the
   -- available rows on Canvas. 
   
   procedure Interaction_Loop 
     (Driver         : in out Menu_Renderer;
      Selected_Item  :    out Menu_Cursor_Type'Class;
      Canvas_Row     :    out Cursor_Ordinal;
      Update_Selected: in     Boolean := False;
      Last_Key       :    out Control_Character;
      Hotkey_Select  :    out Boolean);
   -- Manages rendering and selection of items in Branch. The following Keys
   -- are intercepted (never returned) during interaction:
   -- * Up, Down
   -- * Page_Up, Page_Down
   -- 
   -- Any hotkeys in Branch are used for selection, but are returned
   -- normally in Last_Key. If a Hotkey was used to change the selection,
   -- Hotkey_Select will be set to True, otherwise it will be set to False.
   --
   -- All other Keys cause the Interaction_Loop to exit, retruning the last
   -- Key obtained. 
   --
   -- If Update_Selected is True, the previously selected item is updated
   --
   -- Any changes to Branch, or Canvas in-between calls to Interaction_Loop
   -- should be followed by calls to Reset

   
private
   
   type Scrollbar_Component_Char (Wide: Boolean := False) is
      record
         case Wide is
            when True =>
               Wide_Char: Wide_Graphic_Character;
            when False =>
               Char: Graphic_Character;
         end case;
      end record;
   
   type Scrollbar_Component is
      record
         Style    : Curses.Terminals.Color.Colored_Cursor_Container;
         Character: Scrollbar_Component_Char;
      end record;
   
   type Menu_Renderer (Canvas       : not null access Surface'Class;
                     Input_Surface: not null access Rendered_Surface'Class;
                     Branch       : not null access Menu_Type'Class)
     is new Ada.Finalization.Controlled with
      record
         Initial      : Boolean          := True;
         
         Item_Count   : Natural          := 0;
         Longest_Label: Cursor_Ordinal   := 1;
         Have_Hotkeys : Boolean          := False;
         
         Canvas_Extents: Cursor_Position := (1,1);
         Selected_Row  : Cursor_Ordinal  := 1;
         Scroll_Offset : Natural         := 0;
         
         Wrap_Around   : Boolean         := False;
         
         Scrollbar_Enabled: Boolean      := False;
         Marker: Scrollbar_Component;
         Bar   : Scrollbar_Component;
      end record;
   
   overriding
   procedure Initialize (Driver: in out Menu_Renderer);
   
end Curses.UI.Menus.Renderer;
