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
-- through which a user can provide their own specific Menu generator which will
-- be fully compatible with the Menu rendering processes in the various child
-- packages.
--
-- This interface is also used to implement some included stanard Menu authoring
-- generic packages that to provide Bounded and Unbounded menu trees

with Ada.Iterator_Interfaces;
with Ada.Finalization;        use Ada.Finalization;

package Curses.UI.Menus is
   
   pragma Preelaborate (Menus);
   
   ----------------------
   -- Menu_Cursor_Type --
   ----------------------
   type Menu_Cursor_Type is new Controlled with null record;
   -- Menu_Cursor_Types are Controlled to allow for safe implementation of
   -- the underlying tree containers, by enabling "reference counts".
   -- The default generic menu tree containers provided as children to this
   -- package employ such methods to exclude erronious execution due to
   -- "invalid cursors" which may arrise through erronious copying of cursors.
   
   function Has_Element (Position: Menu_Cursor_Type) return Boolean
     is (False);
     
   function Class_Has_Element (Position: Menu_Cursor_Type'Class)
                              return Boolean
     is (Position.Has_Element);
     
     
   -- Null_Menu_Cursor --
   ----------------------
   Null_Menu_Cursor: constant Menu_Cursor_Type 
     := (Controlled with others => <>);
   -- May be returned from any Menu iterator which has no Menu_Items. As seen
   -- above, Has_Element will always return False on all objects of the root
   -- Menu_Cursor_Type type. Here is provided a single preelaborated constant
   -- object which may be returned at will.
   
   
   --------------------
   -- Menu_Item_Type --
   --------------------
   type Menu_Item_Type is interface;
   -- Menu_Item_Types, or a reference thereof, provides the necessary
   -- information for the rending of the item in a menu.
   
   function  Label_Size (Item: Menu_Item_Type) return Natural is abstract;
   -- Shall return the length of the Label text
   
   procedure Get_Label (Item : in     Menu_Item_Type;
                        Value:    out String;
                        Key  :    out Natural) is abstract;
   -- Called with an expected size determined by an earlier call to Label_Size.
   -- Label shall accept any Value size. If the size of Value is larger than the
   -- label, Label shall fill the remaining with Spaces. Otherwise, Label should
   -- be truncaated. Value may define a null string.
   --
   -- If a keyboard quick key is defined for the Item, Key shall be non-zero,
   -- and shall be an index into Value which selects the Key. Othwerise, Key
   -- shall be set to Zero.
   --
   -- A call to Label when Label_Size is zero shall be assumed to behave like a
   -- null procedure, with Value and Key left uninitialized.
   
   function  Enabled (Item: Menu_Item_Type) return Boolean is abstract;
   -- Shall return True if Item is selectable.
   
   function  Toggled (Item: Menu_Item_Type) return Boolean is abstract;
   -- Shall return True if the Item is stateful, and is currently toggled (On)
   
   -- Submenu --
   -------------
   type Menu_Type is tagged;
   type Submenu_Type (Submenu: not null access Menu_Type'Class) is null record
     with Implicit_Dereference => Submenu;
     
   function  Submenu (Item: Menu_Item_Type) return Submenu_Type is abstract;
   -- If Item does not have an associated Submenu, it shall return the
   -- Null_Submenu predefined reference type constant declared in this package
   
   -- Reference Types --
   ---------------------
   type Menu_Item_Reference_Type
     (Ref: not null access Menu_Item_Type'Class)
      is null record
      with Implicit_Dereference => Ref;
     
   type Menu_Item_Constant_Reference_Type 
     (Ref: not null access constant Menu_Item_Type'Class)
      is null record
      with Implicit_Dereference => Ref;
      
   -- Iterators --
   ---------------
   package Menu_Iterators is new Ada.Iterator_Interfaces 
     (Cursor      => Menu_Cursor_Type'Class,
      Has_Element => Class_Has_Element);
   
   -- Null_Iterator --
   -------------------
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
   
   
   ---------------
   -- Menu_Type --
   ---------------
   type Menu_Type is tagged limited null record
     with Constant_Indexing => Constant_Index,
          Variable_Indexing => Index,
          Default_Iterator  => Iterate,
          Iterator_Element  => Menu_Item_Type'Class;
   -- Menu_Type container types represent a single linear vector of 
   -- Menu_Item_Type'Class objects.
     
   function  Index          (Menu  : Menu_Type; 
                             Cursor: Menu_Cursor_Type'Class) 
                            return Menu_Item_Reference_Type;
                       
   function  Constant_Index (Menu  : Menu_Type;
                             Cursor: Menu_Cursor_Type'Class)
                            return Menu_Item_Constant_Reference_Type;
      
     
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
   function Label_Size (Item: Null_Menu_Item_Type) return Natural 
     is (0);
     
   overriding
   procedure Get_Label (Item : in     Null_Menu_Item_Type;
                        Value:    out String;
                        Key  :    out Natural) 
     is null;
     
   overriding
   function Enabled  (Item: Null_Menu_Item_Type) return Boolean 
     is (False);
   
   overriding 
   function Toggled  (Item: Null_Menu_Item_Type) return Boolean
     is (False);
     
   overriding
   function  Submenu (Item: Null_Menu_Item_Type) return Submenu_Type
     is (Null_Submenu);
   
   Null_Menu_Item: aliased Null_Menu_Item_Type := (others => <>);
   -- Giving an access to this object is always safe since it contains no data,
   -- and has the same lifetime as the partition.
   
   Null_Menu_Reference:
     constant Menu_Item_Reference_Type
     := (Ref => Null_Menu_Item'Access);
   
   Null_Menu_Constant_Reference:
     constant Menu_Item_Constant_Reference_Type
     := (Ref => Null_Menu_Item'Access);
   
   
   --
   -- Completions
   --
   
   --------------------
   -- Menu_Tree_Type --
   --------------------
   function  Index          (Menu  : Menu_Type; 
                             Cursor: Menu_Cursor_Type'Class) 
                            return Menu_Item_Reference_Type
      is (Null_Menu_Reference);
      
   function  Constant_Index (Menu  : Menu_Type;
                             Cursor: Menu_Cursor_Type'Class)
                            return Menu_Item_Constant_Reference_Type
      is (Null_Menu_Constant_Reference);
   

      
end Curses.UI.Menus;
