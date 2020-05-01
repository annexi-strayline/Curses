------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2020, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

-- This package provides the generic logic to implement any Standard_Tree,
-- both bounded and unbounded.

with System.Storage_Pools.Subpools;

with Curses.UI.Menus.Standard_Trees.Storage_Pools;

private package Curses.UI.Menus.Standard_Trees.Implementation is
   
   pragma Assertion_Policy (Check);
   
   package Tree_Pools renames Curses.UI.Menus.Standard_Trees.Storage_Pools;
   
   generic
      type Base_Item is limited new Menu_Item_Interface with private;
      type Subpool_Object is new Tree_Pools.Tree_Subpool with private;
   package Generic_Tree is
      
      All_Trees_Root_Pool: Tree_Pools.Standard_Trees_Root_Pool;
      
      
      type Menu_Tree is tagged;
      type Tree_Access is access all Menu_Tree'Class with
        Storage_Size => 0;
      
      ---------------
      -- Menu_Item --
      ---------------
      
      type Menu_Item;
      type Item_Access is access Menu_Item with
        Storage_Pool => All_Trees_Root_Pool;
      
      -- Item_Links --
      ----------------
      
      protected type Item_Links is
         procedure Reset;
         -- Reset the state to the default values (initial values in the
         -- private part)
         
         procedure Register_Reference;
         -- Increases the active reference count for the element
         --
         -- If the element is in the reset state, Program_Error is raised.
         -- The reference count itself is subject to the usual constraint
         -- check on increment.
         
         procedure Deregister_Reference;
         -- Decreases the active reference count for the Element.
         --
         -- If the reference count is zero on entry, Program_Error is raised.
         
         function  References return Natural;
         
         function  Next return Item_Access;
         function  Prev return Item_Access;
         
         procedure Next (Item: in Item_Access);
         procedure Prev (Item: in Item_Access);
         procedure Relink (Parent, Next, Prev: in Item_Access);
         -- Linking within the branch
         
         function  Sub_First return Item_Access;
         function  Sub_Last  return Item_Access;
         
         procedure Sub_First  (Item: in Item_Access);
         procedure Sub_Last   (Item: in Item_Access);
         procedure Sub_Init   (Item: in Item_Access); 
         -- Get or set the reference to the first or last element of a Branch
         -- rooted at this Element
         --
         -- Sub_Init sets Sub_First and Sub_Last both to Item
         
         function  Sub_Items return Natural;
         procedure Sub_Items_Increment;
         procedure Sub_Items_Decrement;
         procedure Sub_Items_Clear;
         -- Manages the count of Submenu items linked from this node
         
         
         function  Parent return Item_Access;
         procedure Parent (Item: in Item_Access);
         -- Get or set the reference to the Element which is the parent of the
         -- branch on which this Element is located.
         
      private
         Refs         : Natural     := 0;
         
         Next_Ptr     : Item_Access := null;
         Prev_Ptr     : Item_Access := null;
         
         Sub_First_Ptr: Item_Access := null;
         Sub_Last_Ptr : Item_Access := null;
         Sub_Count    : Natural     := 0;
         
         Parent_Ptr   : Item_Access := null;
      end Item_Links;
      
      ----------------------------------------

      type Menu_Item is limited new Base_Item and Menu_Node_Interface with
         record
            Links: Item_Links;
            Tree : Tree_Access := null;
            Self : Item_Access := null;
         end record;
      
      
      overriding
      function  Submenu (Item: in out Menu_Item) return Menu_Type'Class;
      
      
      -----------------
      -- Menu_Cursor --
      -----------------
      
      type Menu_Branch is tagged;
      type Menu_Cursor is new Standard_Cursor with private;
      
      overriding
      procedure Adjust (Cursor: in out Menu_Cursor);
      
      overriding
      procedure Finalize (Cursor: in out Menu_Cursor);
      
      overriding
      function Has_Element (Position: Menu_Cursor) return Boolean;
      
      overriding
      function On_Tree (Position: in     Menu_Cursor;
                        Tree    : in out Standard_Tree'Class)
                       return Boolean
      with Inline, Pre => Tree in Menu_Tree'Class;
      
      overriding
      function On_Branch (Position: Menu_Cursor;
                          Branch  : Menu_Type'Class) 
                         return Boolean
      with Inline, Pre => Branch in Menu_Branch'Class;
      
      overriding
      function Progenitor_Of_Branch (Trial_Progenitor: Menu_Cursor;
                                     Trial_Descendent: Menu_Type'Class) 
                                    return Boolean
      with Pre => Trial_Descendent in Menu_Branch'Class;
      
      
      -----------------
      -- Menu_Branch --
      -----------------
      
      type Menu_Branch is new Menu_Type with private;
      -- The Menu_Branch type represents an iterable handle of a particular
      -- submenu in a particular Tree.
      --
      -- Manipulating Items on a Branch requires access to the underling Tree.
      
      overriding
      procedure Adjust (Branch: in out Menu_Branch);
      
      overriding
      procedure Finalize (Branch: in out Menu_Branch);
      
      overriding
      function  Index (Menu  : Menu_Branch; 
                       Cursor: Menu_Cursor_Type'Class) 
                      return Menu_Node_Reference
      with Inline, Pre => Cursor in Menu_Cursor'Class;
      
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Reversible_Iterator'Class;
      
      overriding
      function  Item_Count (Menu: Menu_Branch) return Natural;
      
      
      
      ---------------
      -- Menu_Tree --
      ---------------
      
      type Menu_Tree is limited new Standard_Tree with private;
      
      overriding
      function Index (Tree    : in out Menu_Tree;
                      Position: in     Standard_Cursor'Class)
                     return Menu_Node_Reference
      with Inline, Pre => Position in Menu_Cursor'Class;
      
      overriding
      function New_Item (Tree: aliased in out Menu_Tree)
                        return Standard_Cursor'Class
      with Post => New_Item'Result in Menu_Cursor'Class;
      
      overriding
      procedure Delete (Tree    : in out Menu_Tree;
                        Position: in out Standard_Cursor'Class;
                        Deleted :    out Boolean)
      with Pre => Position in Menu_Cursor'Class;
      
      overriding
      procedure Append (Tree    : in out Menu_Tree;
                        Branch  : in out Menu_Type'Class;
                        Position: in out Standard_Cursor'Class)
      with Pre => Branch in Menu_Branch'Class
                  and then Position in Menu_Cursor'Class;
      
      overriding
      procedure Prepend (Tree    : in out Menu_Tree;
                         Branch  : in out Menu_Type'Class;
                         Position: in out Standard_Cursor'Class)
      with Pre => Branch in Menu_Branch'Class
                  and then Position in Menu_Cursor'Class;
      
      overriding
      procedure Insert_Before (Tree    : in out Menu_Tree;
                               Branch  : in out Menu_Type'Class;
                               Before  : in     Standard_Cursor'Class;
                               Position: in out Standard_Cursor'Class)
      with Pre => Branch in Menu_Branch'Class
                  and then Before   in Menu_Cursor'Class
                  and then Position in Menu_Cursor'Class;
      -- Note that the class-wide preconditions call on On_Tree for each
      -- Cursor, which implicitly will verify that Before and Position are
      -- Menu_Cursor'Class via On_Tree's precondition
      
      overriding
      procedure Insert_After (Tree    : in out Menu_Tree;
                              Branch  : in out Menu_Type'Class;
                              After   : in out Standard_Cursor'Class;
                              Position: in out Standard_Cursor'Class)
      with Pre => Branch in Menu_Branch'Class
                  and then After    in Menu_Cursor'Class
                  and then Position in Menu_Cursor'Class;
      
   private
      
      -----------------
      -- Menu_Cursor --
      -----------------
      
      type Menu_Cursor is new Standard_Cursor with
         record
            Tree : Tree_Access := null;
            Item : Item_Access := null;
         end record;
      
      -----------------
      -- Menu_Branch --
      -----------------
      
      type Menu_Branch is new Menu_Type with
         record
            Tree: Tree_Access := null;
            Root: Item_Access := null;
            -- Points to the parent element of the branch.
         end record;
      
      
      ---------------
      -- Menu_Tree --
      ---------------
      
      -- Tree_Controller --
      ---------------------
      -- The tree controller is used to ensure specific operations which
      -- modify the tree are executed atomically.
      protected type Tree_Controller (Our_Tree: not null access Menu_Tree) is
         
         -- Element Splicing --
         ----------------------
         -- Due to the task-safe requirements of the Tree (unfortunately),
         -- Progenitor_Of_Branch is checked again inside all splicing
         -- operations except for Extract. (That Before/After
         --
         -- This will most times result in this condition being checked twice,
         -- due to a precondition assertions. Unfortunately, the task-safe
         -- requirements of menu trees means we cannot assume a stable tree
         -- unless we are operating within a Tree_Controller protected
         -- operation, and thus the check needs to be made again under such a
         -- protected condition. Though the extra processing is not very
         -- efficient, menus really are not expected to be changed that
         -- frequently. Safety from memory leaks resulting from inaccessible
         -- island trees due to sub-tree roots being inserted inside of
         -- themselves is more important than thrashing a menu quickly.
         --
         -- These checks are implemented as Assert pragmas, and 
         -- Assertion_Error exceptions are propegated normally
         
         procedure Extract (Item: in not null Item_Access);
         -- Isolates Item from whichever branch it was on. Does not affect
         -- the submenu tree rooted at Item
         
         procedure Allocate_Item   (Item: out not null Item_Access);
         procedure Deallocate_Item (Item: in out Item_Access);
         -- Allocates/Deallocates a new item from the tree's subpool.
         --
         -- Interestingly enough, when using an unbounded pool, the underlying
         -- allocator is using the standard storage pool allocators via the
         -- RTS. These calls go via an RTS lock to ensure task safety.
         --
         -- In the case of the unbounded pool however, we have no lock and
         -- thus need the tree controller to force serialization of
         -- allocations.
         --
         -- Since this model implies tree-specific locking, it turns out that
         -- the unbounded tree should have better performance if there are
         -- many trees.
         --
         -- Why menu trees should be changing quickly enough to bennefit from
         -- such a fine-grained performance improvement is another story..
                  
         procedure Insert_Before (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access;
                                  Before_Item: in not null Item_Access);
         
         procedure Insert_After  (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access;
                                  After_Item : in not null Item_Access);
         -- Relocates From to Before/After, manipulating the links accordingly.
         -- Item is Extracted first.
         --
         -- Preconditions of associated Standard_Tree operations are assumed
         -- enforced
         
         procedure Branch_Prepend (Item       : in not null Item_Access;
                                   Branch_Root: in not null Item_Access);
         
         procedure Branch_Append (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access);
         -- Atomic operations used by Menu_Tree.Append and .Prepend
         --
         -- Calls Insert_Before with Before set to the first item Branch,
         -- ensuring that the first item in the Branch before Item gets
         -- inserted doesn't suddenly get moved to another branch before
         -- the insert happens.
         
      end Tree_Controller;
      
      ----------------------------------------
      type Menu_Tree is limited new Standard_Tree with
         record
            Controller    : Tree_Controller (Menu_Tree'Access);
            Subpool_Actual: aliased Subpool_Object;
            Subpool       : System.Storage_Pools.Subpools.Subpool_Handle;
         end record;
      
      overriding
      procedure Initialize (Tree: in out Menu_Tree);
      
      overriding
      procedure Finalize (Tree: in out Menu_Tree);
      -- Deallocates Subpool_Actual
      
      
      --
      -- Function Expressions
      --
      
      -- Menu_Tree --
      ---------------
      overriding
      function Index (Tree    : in out Menu_Tree;
                      Position: in     Standard_Cursor'Class)
                     return Menu_Node_Reference
        is ((Ref => Menu_Cursor(Position).Item));
      -- Note the type conversion is protected by the Precondition
      
      
      -- Menu_Cursor --
      -----------------
      overriding
      function Has_Element (Position: Menu_Cursor) return Boolean
        is (Position.Tree /= null and then Position.Item /= null);
      
      overriding
      function On_Tree (Position: in     Menu_Cursor;
                        Tree    : in out Standard_Tree'Class)
                       return Boolean
        is (Position.Tree = Menu_Tree(Tree)'Unchecked_Access);
      
      overriding
      function On_Branch (Position: Menu_Cursor;
                          Branch  : Menu_Type'Class) 
                         return Boolean
        is (Menu_Item (Position.Tree.all(Position).Ref.all).Links.Parent
              = Menu_Branch(Branch).Root);
      
      
      -- Menu_Branch --
      -----------------
      function  Index (Menu  : Menu_Branch; 
                       Cursor: Menu_Cursor_Type'Class) 
                      return Menu_Node_Reference
        is (Menu.Tree.Index (Menu_Cursor (Cursor)));
      
      
   end Generic_Tree;
   
end Curses.UI.Menus.Standard_Trees.Implementation;
