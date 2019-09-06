------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Containers;

package Curses.UI.Menus.Standard_Trees is
   
   pragma Assertion_Policy (Check);
   
   Capacity_Error: exception renames Ada.Containers.Capacity_Error;
   
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
   -- Returns True if and only if Cursor denotes an item on Tree. Note that
   -- Tree must be of mode in out to allow access values to be compared in an
   -- expression function.
   
   not overriding
   function On_Branch (Position: Standard_Cursor;
                       Branch  : Menu_Type'Class) 
                      return Boolean is (False);
   -- Returns True if and only if Cursor denotes an item on Branch
   
   
   not overriding
   function Progenitor_Of_Branch (Trial_Progenitor: Standard_Cursor;
                                  Trial_Descendent: Menu_Type'Class) 
                                 return Boolean is (True);
   -- Returns True if Trial_Progenitor is the root of a Sub that denotes a
   -- sub-tree which ultimately contains the branch Trail_Descendent. This is
   -- used to protect Standard_Tree insertion operations from inserting a
   -- branch root into it's own sub-tree, which would make a black-hole.
   
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
                  return Menu_Node_Reference
     is abstract
   with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Returns a reference to an actual Item in the tree.
   
   function Staging_Branch (Tree: aliased in out Standard_Tree)
                           return Menu_Type'Class
     is abstract;
   -- Returns a reference to the Tree's default root branch. All new items
   -- allocated on the Tree via New_Item are _prepended_ to the Staging_Branch.
   -- The Staging_Branch is permanent for the life of the associated Tree
   -- object.
   
   -- Allocation --
   ----------------
   function Create (Tree: aliased in out Standard_Tree)
                   return Standard_Cursor'Class is abstract;
   -- Allocates a new Menu_Item from Tree, and returns a cursor referencing
   -- it. New_Items are always appended to the Staging_Branch.
   
   procedure Delete (Tree    : in out Standard_Tree;
                     Position: in out Standard_Cursor'Class)
     is abstract
   with Pre'Class => Position.Has_Element and then Position.On_Tree (Tree);
   -- Deletes (deallocates) the Item at Position.
   --
   -- If the Position donotes a Submenu, the Submenu is iterated, with
   -- Delete invoked for each Item of the Submenu, and recursively for any
   -- Submenus of those Items
   --
   -- The user shall never attempt to Delete an Item which is currently
   -- referenced, or maintains a sub-tree of Items and Submenus which are also
   -- currently referenced. All Items are checked for active references. If any
   -- active references are found, except for the single reference denoting
   -- Position, Program_Error will be raised. 
   --
   -- If the operation fails and Program_Error is raised, all reasonable
   -- attempts are made to leave the Tree in a consistent state. If the Delete
   -- only fails due to active references, the Tree is not affected.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Erorr: Precondition violated.
   -- *  Program_Error  : The user has attempted to Delete an Item which is
   --                     still referenced, or denotes a sub-tree which
   --                     contains references
   
   
   -- Manipulation --
   ------------------
   procedure Append (Tree    : in out Standard_Tree;
                     Branch  : in out Menu_Type'Class;
                     Position: in out Standard_Cursor'Class)
     is abstract
   with Pre'Class => Position.Has_Element 
                     and then Position.On_Tree (Tree)
                     and then not Position.Progenitor_Of_Branch (Branch);
   -- Appends Item to Branch. If Position denotes an Item on a different
   -- branch, the item is moved to Branch.
   --
   -- Position (obviously) cannot be the root of a sub-tree which contains
   -- Branch.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   -- *  Constraint_Error: Branch does not belong to Tree.
   -- *  Program_Error   : A deadlock occured when trying to insert Position to
   --                      the end of branch. The attempt was aborted. This
   --                      indicates runaway modification to items on
   --                      Branch
   
   procedure Prepend (Tree    : in out Standard_Tree;
                      Branch  : in out Menu_Type'Class;
                      Position: in out Standard_Cursor'Class)
     is abstract
   with Pre'Class => Position.Has_Element 
                     and then     Position.On_Tree (Tree)
                     and then not Position.Progenitor_Of_Branch (Branch);
   -- Prepends Item to Branch. If Position denotes an Item on a different
   -- branch, the item is moved to Branch.
   -- Position (obviously) cannot be the root of a sub-tree which contains
   -- Branch.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   -- *  Constraint_Error: Branch does not belong to Tree.
   
   procedure Insert_Before (Tree    : in out Standard_Tree;
                            Branch  : in out Menu_Type'Class;
                            Before  : in     Standard_Cursor'Class;
                            Position: in out Standard_Cursor'Class)
     is abstract
     with Pre'Class => (Before.Has_Element and Position.Has_Element)
                     and then (Before.On_Tree (Tree) 
                                 and Position.On_Tree (Tree))
                     and then Before.On_Branch (Branch)
                     and then not Position.Progenitor_Of_Branch (Branch);
   -- Inserts Position ahead of Before. If Position is on a different branch
   -- than Before, Position is moved to that branch.
   -- Position (obviously) cannot be the root of a sub-tree which contains
   -- Branch.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   
   procedure Insert_After (Tree    : in out Standard_Tree;
                           Branch  : in out Menu_Type'Class;
                           After   : in out Standard_Cursor'Class;
                           Position: in out Standard_Cursor'Class)
     is abstract
     with Pre'Class => (After.Has_Element and Position.Has_Element)
                     and then (After.On_Tree (Tree) 
                                 and Position.On_Tree (Tree))
                     and then After.On_Branch (Branch)
                     and then not Position.Progenitor_Of_Branch (Branch);
   -- Inserts Position after After. If Position is on a different branch
   -- than After, Position is moved to that branch.
   --
   -- Position (obviously) cannot be the root of a sub-tree which contains
   -- Branch.
   --
   -- -- Explicit Raises --
   -- *  Assertion_Error : Precondition violated
   
   -- Standard_Tree_Access --
   --------------------------
   type Standard_Tree_Access is access all Standard_Tree'Class
   with Storage_Size => 0;
   
end Curses.UI.Menus.Standard_Trees;
