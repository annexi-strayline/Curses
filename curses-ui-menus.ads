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

-- This package is a bit essoteric. The goal is to provide a general interface
-- through which a user can provide their own specific Menu generator which
-- will be fully compatible with the Menu rendering processes in the various
-- child packages.
--
-- This interface is also used to implement some included stanard Menu
-- authoring generic packages that to provide Bounded and Unbounded menu trees

with Ada.Iterator_Interfaces;
with Ada.Finalization;        use Ada.Finalization;

package Curses.UI.Menus is
   
   pragma Preelaborate (Menus);
   
   --------------------
   -- Menu_Item_Type --
   --------------------
   type Menu_Item_Type is interface;
   -- Menu_Item_Types, or a reference thereof, provides the necessary
   -- information for the rending of the item in a menu.
   
   procedure Render_Label (Item    : in out Menu_Item_Type;
                           Canvas  : in out Surface'Class;
                           Selected: in     Boolean) is abstract;
   -- Renders the label of Item to Canvas.
   --
   -- Canvas shall be a Surface that is singally and wholy dedicated to the
   -- label of Item, and shall not include any borders rendered by the menu
   -- subsystem itself. This is typically acheived through a Frame.
   --
   -- The Canva's Current_Cursor shall be set by the caller to be at the
   -- correct position, and with an appropriate style. The style of 
   -- Canvas.Current_Cursor can be used as a reference, but can also be ignored
   -- if more elaborate styling is needed. It is provided for convenience and
   -- to support athetic conformity. When Selected is True,
   -- Canvas.Current_Cursor shall also reflect the subsystem's suggestion on
   -- how a selected item should be rendered, and could, for example, have the
   -- Inverted style option set to True.
   --
   -- Selected is True if the menu subsystem has decided that Item is currently
   -- selected, and the rendering should reflect as much.
   --
   -- For basic rendering, Render_Label can often ignore "Selected", as the
   -- menu subsystem will provide the appropriate Theme cursor, and can thus
   -- use exactly the same code to handle both states.
   
   function  Available (Item: Menu_Item_Type) return Boolean is abstract;
   -- Shall return True if Item is selectable (ie. False is "grayed-out").
   
   
   type After_Execute_Directive is 
   -- Returned from Execute, tells the menu subsystem what to do following
   -- Execution
     (Update,      -- Re-render Item, but keep the menu open
     Regenerate,   -- Re-render the entire Menu tree
     Close);       -- Close the Menu
     
   function  Execute (Item: in out Menu_Item_Type)
                     return After_Execute_Directive is abstract;
   -- Invoked by the menu subsystem when the user selects the particular Item.
   -- If Re_Render is set to True upon return, the menu is not closed, and Item
   -- is re-rendered in-place (though a subsequent call to Item.Render_Label),
   -- otherwise the entire menu tree is closed
   
   -- Submenu --
   -------------
   type Menu_Type is tagged;
   type Submenu_Type (Submenu: not null access Menu_Type'Class) is null record
     with Implicit_Dereference => Submenu;
     
   function  Submenu (Item: Menu_Item_Type) return Submenu_Type is abstract;
   -- If Item does not have an associated Submenu, it shall return the
   -- Null_Submenu predefined reference type constant declared in this package
   
   ---------------
   -- Menu_Type --
   ---------------
   
   -- Cursor Type --
   -----------------
   type Menu_Cursor_Type is new Controlled with null record;
   -- Making this a Controlled type allows for copies of Cursors to
   -- be tracked via some kind of "reference count", which can be used
   -- to prevent deletion of items which are being referenced by a
   -- cursor.
   --
   -- Obviously this does not save us from the user saving a reference
   -- type obtained via index, but that is really bad form anyways
   -- (totally defeats the purpose of user-defined indexing)
   
   function Has_Element (Position: Menu_Cursor_Type) return Boolean
     is (False);
     
   function Class_Has_Element (Position: Menu_Cursor_Type'Class)
                              return Boolean
     is (Position.Has_Element);
     
   Null_Menu_Cursor: constant Menu_Cursor_Type 
     := (Controlled with null record);
   -- May be returned from any Menu iterator which has no Menu_Items. As seen
   -- above, Has_Element will always return False on all objects of the root
   -- Menu_Cursor_Type type. Here is provided a single preelaborated constant
   -- object which may be returned at will.
   
   
   -- Iterators --
   ---------------
   package Menu_Iterators is new Ada.Iterator_Interfaces 
     (Cursor      => Menu_Cursor_Type'Class,
      Has_Element => Class_Has_Element);
   
   
   type Null_Iterator_Type is new Menu_Iterators.Forward_Iterator
     with null record;
   
   overriding function First (Object: Null_Iterator_Type) 
                             return Menu_Cursor_Type'Class
     is (Null_Menu_Cursor);
   
   overriding function Next  (Object  : Null_Iterator_Type; 
                              Position: Menu_Cursor_Type'Class)
                             return Menu_Cursor_Type'Class
     is (Null_Menu_Cursor);
     
   Null_Iterator: constant Null_Iterator_Type := (others => <>);
   
   
   -- Reference Types --
   ---------------------
   type Menu_Item_Reference_Type (Ref: not null access Menu_Item_Type'Class) is
     null record
   with Implicit_Dereference => Ref;
   
   ----------------------------------------
   type Menu_Type is tagged limited null record
   with Variable_Indexing => Index,
        Default_Iterator  => Iterate,
        Iterator_Element  => Menu_Item_Type'Class;
   -- Menu_Type container types represent a single linear vector of 
   -- Menu_Item_Type'Class objects.
     
   function  Index          (Menu  : Menu_Type; 
                             Cursor: Menu_Cursor_Type'Class) 
                            return Menu_Item_Reference_Type;
                       
   function  Iterate        (Menu: Menu_Type) 
                            return Menu_Iterators.Forward_Iterator'Class
     is (Null_Iterator);
     
     
   --
   -- Subjucate Declarations
   -- 
     
   -- Null_Submenu --
   ------------------
   Null_Submenu_Actual: aliased  Menu_Type := (others => <>);
   Null_Submenu       : constant Submenu_Type
     := (Submenu => Null_Submenu_Actual'Access); 
   -- Noting that Menu_Type is a null record, so access is always arbitrary, as
   -- nothing can be read or written.
   
   
   -- Null_Menu_Item_Type --
   -------------------------
   type Null_Menu_Item_Type is new Menu_Item_Type with null record;
   
   overriding
   procedure Render_Label (Item    : in out Null_Menu_Item_Type;
                           Canvas  : in out Surface'Class;
                           Selected: in     Boolean)
     is null;
                           
   overriding
   function Available (Item: Null_Menu_Item_Type) return Boolean 
     is (False);
   
   overriding
   function  Execute (Item: in out Null_Menu_Item_Type)
                     return After_Execute_Directive
     is (Close);
   
   overriding
   function  Submenu (Item: Null_Menu_Item_Type) return Submenu_Type
     is (Null_Submenu);
   
   Null_Menu_Item: aliased Null_Menu_Item_Type := (others => <>);
   -- Giving an access to this object is always safe since it contains no data,
   -- and has the same lifetime as the partition.
   
   Null_Menu_Reference: constant Menu_Item_Reference_Type
     := (Ref => Null_Menu_Item'Access);
   
   
   --
   -- Completions
   --
   
   --------------------
   -- Menu_Tree_Type --
   --------------------
   function  Index (Menu  : Menu_Type; 
                    Cursor: Menu_Cursor_Type'Class) 
                   return Menu_Item_Reference_Type
     is (Null_Menu_Reference);

      
end Curses.UI.Menus;
