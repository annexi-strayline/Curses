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

with Curses.Terminals;
with Curses.Terminals.Surfaces;
with Curses.Standard;

with Ada.Strings;

with Curses.UI.Menus;

generic
   
   type Wide_String_Container is private;
   type Length_Range is (<>);
   
   with function Length (Source: Wide_String_Container) return Length_Range;
   
   with function To_Wide_String (Source: Wide_String_Container)
                                return Wide_String;
   
   with function Slice (Source: Wide_String_Container;
                        Low   : Positive;
                        High  : Natural)
                       return Wide_String;
   
   with procedure Set_String (Target:    out Wide_String_Container;
                              Source: in     Wide_String);
   -- Exceptions raised by the above will be propegated
   
                             
package Curses.UI.Home_Screen is
   
   pragma Assertion_Policy (Pre => Check);
   
   package Menus renames Curses.UI.Menus;
   
   --------------------------
   -- Home_Screen_Instance --
   --------------------------
   -- A single screen instantiation includes the home screen rendering and
   -- menu control infrastructure. Windows can be created on the Screen, 
   -- and behave normally, except that their inputs are normally intercepted
   -- (unless this is specifically disabled)
   
   generic
      TTY: not null access Curses.Terminals.Terminal'Class;
      -- The Terminal on which the Home Screen will be associated
      
      Menu_Bar: in out Menus.Menu_Type'Class;
      with package Menu_Bar_Renderer is new Menus.Renderers.Cascade (<>);
      -- The Home_Screen instance handles calls to Render_Label for the top-
      -- level branch only. Submenus are always ignored.
      -- Hotkeys in the top-level shall be of the Graphic discimination. Alt
      -- is assumed True in all cases, and thus Alt+Key causes execution of
      -- that menu item.
      --
      -- Rendering of actual Menu_Items is handled via
      -- Curses.UI.Menus.Cascading.
      --
      -- If Menu_Bar is an empty Menu, no bar is rendered.
      
      Action_Bar: in out Menus.Menu_Type'Class;
      Action_Bar_Rows: in Natural := 1;
      -- Items in the Action_Bar menu are displayed in the indicated rows above
      -- the Status_Bar
      
   package Home_Screen_Instance is
      
      
      
   end Home_Screen_Instance;
   
   type Home_Screen (TTY: not null access Curses.Terminals.Terminal;
                     Max_Menus: Natural;
                     Max_Keys : Natural) is
     new Standard.Screen (TTY) with private;
   -- Creates a new Home_Screen attached to TTY which is capable of displaying
   -- Max_Menus Menus, and Max_Key Keys
   

   procedure Title_Bar_Background  (S: in out Home_Screen; C: in Cursor'Class);
   procedure Menu_Bar_Background   (S: in out Home_Screen; C: in Cursor'Class);
   procedure Key_Row_Background    (S: in out Home_Screen; C: in Cursor'Class);
   procedure Status_Bar_Background (S: in out Home_Screen; C: in Cursor'Class);
   -- Configures the rendering style for all blank areas of the Menu Bar,
   -- Key Rows, and Status Bar, respectively
   
   
   procedure Redraw (S: in out Home_Screen);
   -- Clears and redraws the screen.
   
   
   procedure Set_Margin (S: in out Home_Screen;
                         Left, Right, Top, Bottom: Cursor_Ordinal);
   -- Causes the Home_Screen to render in such a way that the specified margin
   -- will be cleared and remain untouched. This is helpful for faciliting
   -- side-bars or screen borders.
   --
   -- Calling this procedure will cause a Redraw
   --
   -- If the margin implies an impossible geometry, Cursor_Excusion is raised
   
   
   
   -- Title_Bar --
   ---------------
   procedure Remove_Title (S: in out Home_Screen);
   -- Removes the title-bar entirely. This invokes S.Redraw.
   
   procedure Set_Title
     (S             : in out Home_Screen;
      Justify       : in     Justify_Mode := Center;
      
      Title         : in     String;
      Title_Cursor  : in     Cursor'Class)
   with Pre => Check_Graphic (Title);
   
   procedure Wide_Set_Title
     (S             : in out Home_Screen;
      Justify       : in     Justify_Mode := Center;
      
      Title         : in     Wide_String;
      Title_Cursor  : in     Cursor'Class)
   with Pre => Wide_Check_Graphic (Title);
   
   
   
   procedure Set_Status_Bar
     (S             : in out Home_Screen;
      Justify       : in     Justify_Mode := Center;
      Title         : in     String;
      Title_Cursor  : in     Cursor'Class;
      
      Fill_Character: in     Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class);
   
   procedure Wide_Set_Title_Bar
     (S             : in out Home_Screen;
      Row           : in     Cursor_Ordinal;
      Justify       : in     Justify_Mode := Center;
      Title         : in     Wide_String;
      Title_Cursor  : in     Cursor'Class;
      
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class);
   
   
   
   
   -- Panel Operations --
   -- ---------------- --
   -- Panels refer to the unused area in the center of a Home_Screen,
   -- the size of which depends on the configuration of a the menu bar,
   -- key rows, or the status bar.
   
   function Panel_Top_Left (S: Home_Screen) return Cursor_Position;
   -- Returns the position on the S which correlates to the Top-left corner of
   -- a peroperly sized Panel.
   --
   -- -- All Possible Exceptions --
   -- *  Surface_Unavailable: S is not Available
   
   function Panel_Extents (S: Home_Screen) return Cursor_Position;
   -- Returns the correct extents of a Panel on S where the Top_Left corner
   -- is equal to Panel_Top_Left.
   --
   -- -- All Possible Exceptions --
   -- *  Surface_Unavailable: S is not Available
   
   function New_Panel (S: in out Home_Screen)
                      return Window'Class;
   -- Creates a new Window which is sized and positioned appropriately to
   -- serve as a panel.
   --
   -- This function is equivilent to calling 
   -- S.New_Window (Top_Left         => S.Panel_Top_Left,
   --               Proposed_Extents => S.Panel_Extents);
   -- -- Exceptions follow Standard.Surface.New_Window --
   
   procedure Fit_Panel (S: in out Home_Screen;
                        W: in out Window'Class);
   -- Attempts to fit an arbitrary Window to the size and position determined
   -- by S.Panel_Extents and S.Panel_Top_Left, respectively.
   --
   -- This procedure is useful for resizing panels if the Screen changes size,
   -- or if the Screen is reconfigured.
   
   function Panel_Geometry_Changed (S: Home_Screen) return Boolean;
   -- True if S the result of Panel_Top_Left or Panel_Extents have changed
   -- since the the last time Panel_Geometry_Changed returned False
   
   procedure Clear_Panel_Geometry_Changed (S: in out Home_Screen);
   -- Clears the Panel_Geometry_Changed state unconditionally
   
   procedure Wait_Panel_Geometry_Changed (S: in out Home_Screen);
   -- Blocks until Panel_Geometry_Changed evaluates True. The state is also
   -- immediately cleared prior to return.
   
   
private
   
   -----------------
   -- Home_Screen --
   -----------------
   type Home_Screen (TTY: not null access Curses.Terminals.Terminal) is
     new Standard.Screen (TTY) with
      record
         Has_Border  : Boolean := False;
         Has_Titlebar: Boolean := False;
      end record;
   
   
   -- Hidden Overrides --
   ----------------------
   overriding
   function Input_Key  (The_Surface: in out Home_Screen;
                        Peek       : in     Boolean  := False;
                        Wait       : in     Boolean  := True)
                       return Curses.Terminals.Control_Character;
   
   overriding
   procedure Window_Input_Intercept (The_Screen: in out Home_Screen);
   
   
   ------------------------
   -- Home_Screen_Window --
   ------------------------
   type Home_Screen_Window is new Standard.Window with null record;
   
   overriding
   function  Input_Key  (The_Surface: in out Home_Screen_Window;
                         Peek       : in     Boolean  := False;
                         Wait       : in     Boolean  := True)
                        return Curses.Terminals.Control_Character;
   
end Curses.UI.Home_Screen;
