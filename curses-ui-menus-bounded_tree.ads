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

-- This package is task-safe insofar as multiple access will not corrupt the
-- Menu or SubMenu objects in any way. However, care must be exercised, as
-- changes to Menu or Submenus during rendering may cause artifacts such as
-- missing or repeated items, or incorrect size computations. The user should
-- take care to avoid updates to (Sub)menu items while a menu is open or being
-- rendered

with Ada.Containers;

generic
   type Menu_Item is new Menu_Item_Type with private;

package Curses.UI.Menus.Bounded_Tree is
   pragma Assertion_Policy (Check);
   
   -----------------
   -- Menu_Cursor --
   -----------------
   type Menu_Branch is tagged;
   type Menu_Cursor is new Menu_Cursor_Type with private;
   
   overriding
   procedure Adjust   (Object: in out Menu_Cursor);
   
   overriding
   procedure Finalize (Object: in out Menu_Cursor);
   
   overriding
   function Has_Element (Position: Menu_Cursor) return Boolean;
   
   not overriding
   function Of_Branch (Cursor: Menu_Cursor;
                       Branch: Menu_Branch'Class) 
                      return Boolean
     with Pre => Cursor.Has_Element;
   -- Returns True if and only if Cursor is currently on Branch.
     
   not overriding
   function Copies (Cursor: Menu_Cursor) return Positive;
   -- Returns the total number of copies of Cursor that exist.
   -- Unititalized copies are always single copies, copies of
   
   
   -----------------
   -- Menu_Branch --
   -----------------
   type Menu_Tree is tagged;
   type Menu_Branch (<>) is limited new Menu_Type with private;
   -- Menu_Branches are only ever exposed through the related Submenu_Type
   -- reference type for Menu_Type'Class. These references may be saved via
   -- calls to Menu_Item'Class.Submenu of Menu_Item'Class items returned by
   -- iterating through a parent Menu_Branch (Submenu_Type) reference
   
   overriding
   function  Index (Menu  : Menu_Branch; 
                    Cursor: Menu_Cursor_Type'Class) 
                   return Menu_Item_Reference_Type
     with Pre => Cursor in Menu_Cursor'Class;
                    
   overriding
   function  Constant_Index (Menu  : Menu_Branch;
                             Cursor: Menu_Cursor_Type'Class)
                            return Menu_Item_Constant_Reference_Type
     with Pre => Cursor in Menu_Cursor'Class;
      
   overriding
   function  Iterate (Menu: Menu_Branch) 
                     return Menu_Iterators.Forward_Iterator'Class;
   
   
   not overriding
   function Branch_Of_Tree (Branch: Menu_Branch; Tree: Menu_Tree'Class) 
                           return Boolean;
   -- Returns True if and only if Branch is a valid branch of Tree
   
   
   not overriding
   function  Root (Branch: Menu_Branch) return Menu_Cursor'Class;
   -- Returns a Menu_Cursor identifying the Root (top-most) Menu_Item of the
   -- Branch, or reutrns Null_Menu_Cursor if Branch is empty
                          
   not overriding
   procedure Append (Branch      : in out Menu_Branch;
                     Item        : in     Menu_Item;
                     Position    :    out Menu_Cursor'Class;
                     Init_Submenu: in     Boolean := False);
   -- Appends Item to Branch
   -- -- Explicit Raises --
   -- *  Capacity_Error: The Menu_Tree associated with Branch cannot accomodate
   --                    new elements.
   
   not overriding
   procedure Prepend (Branch      : in out Menu_Branch;
                      Item        : in     Menu_Item;
                      Position    :    out Menu_Cursor'Class;
                      Init_Submenu: in     Boolean := False);
   -- Prepends Item to Branch
   -- -- Explicit Raises --
   -- *  Capacity_Error: The Menu_Tree associated with Branch cannot accomodate
   --                    new elements.
   
   not overriding
   procedure Insert_Before (Branch      : in out Menu_Branch;
                            Before      : in     Menu_Cursor'Class;
                            Item        : in     Menu_Item;
                            Position    :    out Menu_Cursor'Class;
                            Init_Submenu: in     Boolean := False)
     with Pre => Before.Has_Element and then Before.On_Branch (Branch);
   -- Inserts Item ahead of the element at Position.
   -- -- Explicit Raises --
   -- *  Assertion_Erorr : Precondition violated
   -- *  Constraint_Error: Before is an empty cursor
   -- *  Capacity_Error  : The Menu_Tree associated with Branch cannot
   --                      accomodate new elements.
   
   not overriding
   procedure Insert_After (Branch      : in out Menu_Branch;
                           After       : in     Menu_Cursor'Class;
                           Item        : in     Menu_Item;
                           Position    :    out Menu_Cursor'Class;
                           Init_Submenu: in     Boolean := False)
     with Pre => Before.Has_Element and then Before.On_Branch (Branch);
   -- Inserts Item ahead of the element at Position.
   -- -- Explicit Raises --
   -- *  Assertion_Erorr : Precondition violated
   -- *  Constraint_Error: Before is an empty cursor
   -- *  Capacity_Error  : The Menu_Tree associated with Branch cannot
   --                      accomodate new elements.
   
   
   -- For all Item addition subprograms --
   -- If Init_Submenu is True, a new branch is created, which may be reference
   -- through a call to Branch(Position).Submenu (generalized indexing call to
   -- Index)

   
   not overriding
   procedure Delete (Branch  : in out Menu_Branch;
                     Position: in out Menu_Cursor'Class)
     with Pre => Position.Has_Element 
                 and then Position.On_Branch (Branch)
                 and then Position.Copies = 1;
   -- Deletes the element at Position.
   --
   -- If the Item donotes a Submenu, all menus of that Submenu are also deleted.
   --
   -- In this implementation, deleting an item will never result in erronious
   -- execution. Instead, the implemention will recognise any subsequent use
   -- of a stale cursor, and will raise Program_Error.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Erorr : Precondition violated
   -- *  Constraint_Error: Position is an empty cursor
   
   ---------------
   -- Menu_Tree --
   ---------------
   type Menu_Tree (Capacity: Positive) 
      is tagged limited private;
   -- The Menu_Tree object contains all Menu_Branches. All operations on the
   -- tree are focused on the individual Submenu "branches".
   
   function Main_Menu (Tree: Menu_Tree) return Submenu_Type;
   -- Returns the "Main_Menu" as a reference type for an iterable Menu_Type
   -- object which represents the root branch of the Menu_Tree.

   
private
   ----------------------
   -- Generation_Stamp --
   ----------------------
   type Generation_Stamp is mod 2**64;
   -- The Generation_Stamp is a value that is incremented for each
   -- Accounted_Item on a Tree, and is used to verify that a Cursor is valid.
   
   --------------------
   -- Accounted_Item --
   --------------------
   subtype Node_Index is Positive;
   
   type Accounted_Item is
      record
         Generation: Generation_Stamp;
         
         Next      : Node_Index;
         Child     : Node_Index;
      end record;
   -- Accounted_Item is the actual unit of storage in the Tree, and allows
   -- for the validation of Cursors, and the explicit protection from
   -- the deletion of elements which have multiple Cursors refering to the
   -- item.
   
   -----------------
   -- Menu_Branch --
   -----------------
   type Menu_Branch (Tree: not null access Menu_Tree) 
      is limited new Menu_Type with
      record
         Node: Node_Index;
         -- The node which represents the root of a branch
      end record;
   
   not overriding
   function Branch_Of_Tree (Branch: Menu_Branch; Tree: Menu_Tree'Class) 
                           return Boolean
     is (Branch.Tree = Tree'Unchecked_Access);
   
   
   -----------------
   -- Menu_Cursor --
   -----------------
   type Menu_Tree_Access is access all Menu_Tree
     with Storage_Size => 0;
   
   type Menu_Cursor is new Menu_Cursor_Type with
      record
         Tree  : Menu_Tree_Access := null;
         Item  : Node_Index       := 1;
      end record;
   
   overriding
   function Has_Element (Position: Menu_Cursor) return Boolean
     is (Item /= No_Element);
   
   
   not overriding
   function Of_Branch (Cursor: Menu_Cursor;
                       Branch: Menu_Branch'Class) 
                      return Boolean
     is (Cursor.Branch = Branch.Parent);
   
   
   ---------------
   -- Menu_Tree --
   ---------------
   protected type Tree_Core (Capacity: Ada.Containers.Count_Type) is
      
      procedure Append  (Parent    : in     Internal_Cursor; 
                         Item      : in     Accounted_Item; 
                         New_Cursor:    out Internal_Cursor);
      
      procedure Prepend (Parent    : in     Internal_Cursor; 
                         Item      : in     Accounted_Item; 
                         New_Cursor:    out Internal_Cursor);
      
      procedure Insert (Parent    : in     Internal_Cursor;
                        Before    : in     Internal_Cursor;
                        Item      : in     Accounted_Item;
                        New_Cursor:    out Internal_Cursor);
      
      procedure Delete (Target    : in     Internal_Cursor);
      
   private
      
   end Tree_Core;
   
end Curses.UI.Menus.Bounded_Tree;
