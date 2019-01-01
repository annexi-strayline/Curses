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

package Curses.UI.Menus.Standard_Trees with Preelaborate is
   
   ---------------------
   -- Standard_Cursor --
   ---------------------
   type Standard_Tree   is tagged;
   type Standard_Cursor is new Menu_Cursor_Type with null record;
   -- Standard_Cursor is a special extension of Menu_Cursor_Type with new
   -- On_Tree and On_Branch operatiosn required for the Standard_Tree interface
   
   not overriding
   function On_Tree (Position: in     Standard_Cursor;
                     Tree    : in out Standard_Tree'Class)
                    return Boolean is (False);
   -- Returns True if and only if Cursor denotes an item on Tree
   
   not overriding
   function On_Branch (Position: in     Standard_Cursor;
                       Branch  : in out Menu_Type'Class) 
                      return Boolean is (False);
   -- Returns True if and only if Cursor denotes an item on Branch
   
   
   
   -------------------
   -- Standard_Tree --
   -------------------
   type Standard_Tree is limited interface
     with Variable_Indexing => Index;
   -- Standard_Tree represents a collection of Items and Branches (submenus).
   -- The Standard_Tree provides an implicit "Main_Menu" branch, as well as
   -- allocation and deletion of Items and Branches throughout the Tree.
   
   function Index (Tree    : in out Standard_Tree;
                   Position: in     Standard_Cursor'Class)
                  return Menu_Item_Reference_Type
     is abstract
     with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Returns a reference to an actual Item in the tree.
   
   function Main_Menu (Tree: in out Standard_Tree) return Menu_Type'Class
     is abstract;
   -- Returns the "Main_Menu" as a reference type for an iterable Menu_Type
   -- object which represents the root branch of the Standard_Tree.

   
   -- Allocation --
   ----------------
   function New_Item (Tree: in out Standard_Tree)
     return Standard_Cursor'Class is abstract;
   -- Allocates a new Menu_Item from Tree, and returns a cursor referencing
   -- it. Newly allocated Items are not associated with a Branch. If the
   -- Item is not placed on a Branch, it will be Deleted when all referencing 
   -- Cursors have finalized.
   
   function New_Submenu (Tree: in out Standard_Tree;
                         Position: in Standard_Cursor'Class)
                        return Menu_Type'Class
     is abstract
     with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Creates a new submenu associated with Item at Position. If the Item
   -- already has a Submenu, a new reference to the existing submenu is
   -- returned, otherwise a new submenu is created.
   -- -- Explicit Raises --
   -- *  Assertion_Erorr : Precondition violated
   
   procedure Delete (Tree    : in out Standard_Tree;
                     Position: in out Standard_Cursor'Class)
     is abstract
     with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Deletes (deallocates) the Item at Position.
   --
   -- If the Position donotes a Submenu, the Submenu is iterated-over, with
   -- Delete invoked for each Item of the Submenu, and recursively for any
   -- Submenus of those Items
   --
   -- All Items are checked for active references. If any active references are
   -- found, except for the single reference denoting Position, Program_Error
   -- will be raised.
   --
   -- If Assertion_Error or Program_Error are explicitly raised, the operation
   -- is aborted without modifying Tree.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Erorr : Precondition violated
   -- *  Constraint_Error: Position denotes a sub-tree that contains active
   --                      references that would be left dangling.
   
   -- Manipulation --
   ------------------
   procedure Append (Tree    : in out Standard_Tree;
                     Branch  : in out Menu_Type'Class;
                     Position: in     Standard_Cursor'Class)
     is abstract
     with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Appends Item to Branch. If Position denotes an Item on a different
   -- branch, the item is moved to Branch.
   -- -- Explicit Raises --
   -- *  Assertion_Error: Precondition violated
   
   procedure Prepend (Tree    : in out Standard_Tree;
                      Branch  : in out Menu_Type'Class;
                      Position: in     Standard_Cursor'Class)
     is abstract
     with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Prepends Item to Branch. If Position denotes an Item on a different
   -- branch, the item is moved to Branch.
   -- -- Explicit Raises --
   -- *  Assertion_Error: Precondition violatde
   
   procedure Insert_Before (Tree    : in out Standard_Tree;
                            Before  : in     Standard_Cursor'Class;
                            Position: in     Standard_Cursor'Class)
     is abstract
     with Pre'Class => (Before.Has_Element and then Position.Has_Element)
                       and then (Before.On_Tree (Tree) 
                                 and then Position.On_Tree (Tree));
   -- Inserts Position ahead of Before. If Before is on a different Branch than
   -- Position, Position is moved to that branch.
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   
   procedure Insert_After (Tree    : in out Standard_Tree;
                           After   : in out Standard_Cursor'Class;
                           Position: in out Standard_Cursor'Class)
     is abstract
     with Pre'Class => (After.Has_Element and then Position.Has_Element)
                       and then (After.On_Tree (Tree)
                                 and then Position.On_Tree (Tree));
   -- Inserts Position ahead of the element at Position.
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   
   
   -- Standard_Tree_Access --
   --------------------------
   type Standard_Tree_Access is access all Standard_Tree'Class
     with Storage_Size => 0;
   
end Curses.UI.Menus.Standard_Trees;
