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

-- This package provides a very basic and extendable Menu_Item_Type
-- implementation with a bounded Label length. This package provides a useful
-- simple bootstrap type for creating Menu_Item_Type implementations that can
-- be used throughout the Menus subsystem package.
--
-- Note that Submenu needs to be overriden in a further extension of the
-- Bounded_Menu_Item_Type in order to enable Submenus. This is done
-- automatically with the (Un)Bounded_Menu_Tree sibiling packages.

generic
   Maximum_Label_Size: Positive;
   
package Curses.UI.Menus.Bounded_Item is
   
   pragma Preelaborate (Bounded_Item);
   
   type Bounded_Menu_Item_Type is new Menu_Item_Type with private;
   pragma Preelaborable_Initialization (Bounded_Menu_Item_Type);
   
   
   overriding
   function  Label_Size (Item: Bounded_Menu_Item_Type) return Natural;
   
   overriding
   procedure Get_Label (Item : in     Bounded_Menu_Item_Type;
                        Value:    out String;
                        Key  :    out Natural);
   
   not overriding
   procedure Set_Label (Item : in out Bounded_Menu_Item_Type;
                        Value: in     String;
                        Key  : in     Natural := 0)
     with Pre => Value'Length <= Maximum_Label_Size;
   
   overriding
   function  Enabled (Item: Bounded_Menu_Item_Type) return Boolean;
   
   not overriding
   procedure Enabled (Item: in out Bounded_Menu_Item_Type;
                      Set : in     Boolean);
   
   overriding
   function  Toggled (Item: Bounded_Menu_Item_Type) return Boolean;
   
   not overriding
   procedure Toggled (Item: in out Bounded_Menu_Item_Type;
                      Set : in     Boolean);
   
   overriding
   function  Submenu (Item: Bounded_Menu_Item_Type) return Submenu_Type
     is (Null_Submenu);
   
private
   
   subtype Label_Buffer is String (1 .. Maximum_Label_Size);
   
   type Bounded_Menu_Item_Type is new Menu_Item_Type with
      record
         Label     : Label_Buffer;
         Label_Last: Natural := 0;
         
         Key_Index : Natural := 0;
         
         Is_Enabled: Boolean := False;
         Is_Toggled: Boolean := False;
      end record;
   
   
   overriding
   function  Label_Size (Item: Bounded_Menu_Item_Type) return Natural
     is (Item.Label_Last);
   
   overriding
   function  Enabled (Item: Bounded_Menu_Item_Type) return Boolean
     is (Item.Is_Enabled);
   
   overriding
   function  Toggled (Item: Bounded_Menu_Item_Type) return Boolean
     is (Item.Is_Toggled);
   
end Curses.UI.Menus.Bounded_Item;
  
