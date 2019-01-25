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

private package Curses.UI.Menus.Standard_Trees.Core with Preelaborate is
   
   --------------------------
   -- Generic_Tree_Element --
   --------------------------
   generic
      type Base_Item is limited new Menu_Item_Type with private;
      
      type Index_Type is private;
      Null_Index: in Index_Type;
      -- User-provided Tree indexing type, used for link management
      
   package Generic_Tree_Element is
      
      protected type Element_State is
         
         procedure Register_Reference (Success: out Boolean);
         procedure Deregister_Reference;
         -- Increases or decreases the active reference count for the Element
         -- If Register_Reference returns Success = False, this indicates that
         -- the Item may not take additional references at this time.
         --
         -- An unsuccessful Registration indicates that the Element is being
         -- removed from the Tree, or that the number of References has reached
         -- Natural'Last, and thus aborting the creation of the reference is
         -- the only realistic course of action. This situation generally
         -- indicates a problem with the user's program.
         --
         -- -- Explicit Raises --
         -- *  Program_Error: Dereigster_Reference called when the registered
         --                   reference count is zero. This indicates an
         --                   implementation error

         
         procedure Activate;
         procedure Deactivate (Ref_Remain: out Natural);
         -- Activate allows new references to be registered for the Element.
         --
         -- If not Activated, Register_Reference does not increase the
         -- reference count, and sets Success to False on any call to
         -- Register_Reference.
         --
         -- Deactivation requires that no References currently exist.
         -- If Ref_Remain is greator than zero, Deactivation failed, otherwise,
         -- Ref_Remain = 0 indicates the element has been Deactivated.
         --
         -- Elements default to Deactivated.
         --
         -- -- Explicit Raises --
         -- *  Program_Error: Activate invoked on an element that is already
         --                   Active (this should not happen, and would
         --                   indicate an implementation error)
         
         function  Next return Index_Type;
         function  Prev return Index_Type;
         
         procedure Next (Index: in Index_Type);
         procedure Prev (Index: in Index_Type);
         procedure Link (Next, Prev: in Index_Type);
         
         function  Sub  return Index_Type;
         procedure Sub  (Index: in Index_Type);
         -- Get or set the reference to the first element of a Branch rooted
         -- at this Element
         
         function  Parent return Index_Type;
         procedure Parent (Index: in Index_Type);
         -- Get or set the reference to the Element which is the parent of the
         -- branch on which this Element is located.
         
         
         procedure Identity (Tree : in not null Standard_Tree_Access;
                             Index: in Index_Type)
           with Pre => Index /= Null_Index;
         
         function  Tree  return Standard_Tree_Access;
         function  Index return Index_Type;
         -- All newly allocated elements (through Standard_Tree.New_Item) shall
         -- set Identity by providing the element with a reference to the Tree
         -- on which it has membership, as well as its own Index values.
         --
         -- This identity is required by Menu_Item_Type.Submenu
         --
         -- Tree and Index shall never be called on an element that has not
         -- been properly configured with a call to Identity.
         
      private
         Active    : Boolean              := False;
         Refs      : Natural              := 0;
         
         Next_Ptr  : Index_Type           := Null_Index;
         Prev_Ptr  : Index_Type           := Null_Index;
         
         Sub_Ptr   : Index_Type           := Null_Index;
         Parent_Ptr: Index_Type           := Null_Index;
         
         Tree_Ptr  : Standard_Tree_Access := null;
         Self_Ptr  : Index_Type           := Null_Index;
         
      end Element_State;
      
      ----------------------------------------
      type Tree_Element is limited new Base_Item with
         record
            State: Element_State;
         end record;

   end Generic_Tree_Element;
   
   
   -----------------------
   -- Generic_Menu_Tree --
   -----------------------
   generic
      with package GTE is new Generic_Tree_Element (<>);
      
      type Pool_Parameter is (<>);
      type Item_Pool (Param: Pool_Parameter) is limited private;
      
      with function  Allocate (Pool: in out Item_Pool)
                             return GTE.Index_Type is <>;
      
      with procedure Free (Pool : in out Item_Pool;
                           Index: in     GTE.Index_Type) is <>;
      -- The implementation will guaruntee no further access to the item
      -- referenced by Index after a call to Free, unless it is subsequently
      -- returned by a later call to Allocate
      --
      -- The implementer shall not modify any other Items of Index as a result
      -- of calling Free. Tree manipulation logic handles de-linking before
      -- Freeing
      --
      -- Freeing a Null_Index value shall have no effect.
      
      -- ** Allocate and Free do not need to be task-safe, All calls to
      -- Allocate or Free from a Tree object will be serialized.
      
      with function Lookup (Pool : in out Item_Pool;
                            Index: in     GTE.Index_Type)
                           return Menu_Item_Reference_Type;
      -- Return a Menu_Item_Reference_Type to a GTE.Tree_Element'Class object
      -- of Pool, referenced by Index. If Index is Null_Index, Reference shall
      -- return Curses.UI.Menus.Null_Menu_Reference
      
   package Generic_Menu_Tree is
   
      use all type GTE.Tree_Element;
   
      ---------------
      -- Menu_Item --
      ---------------
      type Menu_Item is new GTE.Tree_Element with null record;
      
      overriding
      function  Submenu (Item: in out Menu_Item) return Menu_Type'Class;
      
      
      -----------------
      -- Menu_Cursor --
      -----------------
      type Menu_Tree   is tagged;
      type Menu_Branch is tagged;
      type Menu_Cursor is new Standard_Cursor with private;
      
      overriding
      procedure Adjust (Cursor: in out Menu_Cursor);
      
      overriding
      procedure Finalize (Cursor: in out Menu_Cursor);
      
      overriding
      function Has_Element (Position: Menu_Cursor) return Boolean
        with Inline;
      
      overriding
      function On_Tree (Position: in     Menu_Cursor;
                        Tree    : in out Standard_Tree'Class)
                       return Boolean
        with Inline, Pre => Tree in Menu_Tree'Class;
      
      overriding
      function On_Branch (Position: in     Menu_Cursor;
                          Branch  : in out Menu_Type'Class) 
                         return Boolean
        with Inline, Pre => Branch in Menu_Branch'Class;
      
      
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
                      return Menu_Item_Reference_Type
        with Inline, Pre => Cursor in Menu_Cursor'Class;
      
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Forward_Iterator'Class;
      
      
      ---------------
      -- Menu_Tree --
      ---------------
      type Menu_Tree (Param: Pool_Parameter) is
        limited new Standard_Tree with private;
      
      overriding
      function Index (Tree    : in out Menu_Tree;
                      Position: in     Standard_Cursor'Class)
                     return Menu_Item_Reference_Type
        with Inline, Pre => Position in Menu_Cursor'Class;
      
      overriding
      function Staging_Branch (Tree: aliased in out Menu_Tree)
                         return Menu_Type'Class
        with Inline, Post => Staging_Branch'Result in Menu_Branch'Class;
      
      overriding
      function New_Item (Tree: aliased in out Menu_Tree)
                        return Standard_Cursor'Class;
      
      overriding
      procedure Delete (Tree    : in out Menu_Tree;
                        Position: in out Standard_Cursor'Class)
        with Pre => Position in Menu_Cursor'Class;
      
      overriding
      procedure Append (Tree    : in out Menu_Tree;
                        Branch  : in out Menu_Type'Class;
                        Position: in out Standard_Cursor'Class)
        with Pre => Branch in Menu_Branch'Class;
      
      overriding
      procedure Prepend (Tree    : in out Menu_Tree;
                         Branch  : in out Menu_Type'Class;
                         Position: in out Standard_Cursor'Class)
        with Pre => Branch in Menu_Branch'Class;
      
      overriding
      procedure Insert_Before (Tree    : in out Menu_Tree;
                               Before  : in     Standard_Cursor'Class;
                               Position: in out Standard_Cursor'Class);
      -- Note that the class-wide preconditions call on On_Tree for each
      -- Cursor, which implicitly will verify that Before and Position are
      -- Menu_Cursor'Class via On_Tree's precondition
      
      overriding
      procedure Insert_After (Tree    : in out Menu_Tree;
                              After   : in out Standard_Cursor'Class;
                              Position: in out Standard_Cursor'Class);
      
   private
      
      use type GTE.Index_Type;
      subtype Index_Type is GTE.Index_Type;
      
      Null_Index: Index_Type renames GTE.Null_Index;
      
      -----------------
      -- Menu_Cursor --
      -----------------
      type Tree_Access is access all Menu_Tree'Class
        with Storage_Size => 0;
      
      type Menu_Cursor is new Standard_Cursor with
         record
            Tree : Tree_Access := null;
            Index: Index_Type  := Null_Index;
         end record;
      
      -----------------
      -- Menu_Branch --
      -----------------
      type Menu_Branch is new Menu_Type with
         record
            Tree: Tree_Access := null;
            Root: Index_Type  := Null_Index;
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
         
         -- Element Allocation --
         ------------------------
         procedure Allocate_Index (Index: out Index_Type);
         procedure Free_Index     (Index: in  Index_Type);
         
         -- Element Splicing --
         ----------------------
         procedure Extract (Index: Index_Type);
         -- Cuts Index out from it's list, setting Prev.Next -> Next and
         -- Next.Prev -> Prev. If Index is the first item in a Submenu, that
         -- Parent item's Submenu is retargeted to Next.
         --
         -- Next, Prev, and Parent are then set to Null_Index.
         --
         -- The item's submenu is not modified.
         
         procedure Insert_Before (Item, Before: Index_Type);
         procedure Insert_After  (Item, After : Index_Type);
         -- Relocates From to Before/After, manipulating the links accordingly.
         -- Item is Extracted first.
         
         -- Splicing subprograms do not check for Null_Index on any parameters.
         -- These checks are handled by Preconditions for the public operations
         -- that make use of the operations.
         
         -- Submenu Operations
         
         procedure First_In_Submenu (Root, Item: in Index_Type);
         -- Atomic operation used by Menu_Tree.Append and .Prepend
         --
         -- Atomically installs/inserts Item as the first item for the
         -- Submenu Branch rooted at Root. 
         -- Item is Extracted first.
         
         procedure Next_In_Submenu (Root, Item, After: in     Index_Type;
                                    Success          :    out Boolean);
         -- Atomic operation used by Menu_Tree.Append
         --
         -- Atomically installs/inserts Item as the next item for After on
         -- the Submenu rooted on Root.
         -- Item is Extracted first.
         --
         -- Unlike Insert_After, Next_In_Submenu specifically checks that 
         -- After is still on the Submenu of Root on entry before proceeeding. 
         -- If it is not, or if any of Root, After, or Item equal, Null_Index
         -- Success is set to False and no action is taken.
         
      end Tree_Controller;
      
      ----------------------------------------
      type Menu_Tree (Param: Pool_Parameter) is
        limited new Standard_Tree with
         record
            Pool      : Item_Pool (Param);
            Controller: Tree_Controller (Menu_Tree'Access);
            Staging   : aliased Menu_Item;
         end record;
      
      Staging_Branch_Index: Index_Type renames Null_Index;
      -- Remember that the Staging_Branch of a given tree is a logical node. We
      -- use a Menu_Branch with a Null_Index root to refer to the
      -- Staging_Branch. As an extra matter of convenience, we don't need to
      -- register references for the Staging_Branch, since it will always exist
      -- as long as the Tree exists, and it cannot be deleted.
      --
      -- Though a Menu_Branch is functionally similar to a Menu_Cursor, unlike
      -- a cursor, it can never be directly deleted.
      
      
      --
      -- Function Expressions
      --
      
      -- Menu_Tree --
      ---------------
      overriding
      function Index (Tree    : in out Menu_Tree;
                      Position: in     Standard_Cursor'Class)
                     return Menu_Item_Reference_Type
        is (Lookup (Pool => Tree.Pool, Index => Menu_Cursor (Position).Index));
      -- Note the type conversion is protected by the Precondition
      
      
      -- Menu_Cursor --
      -----------------
      overriding
      function Has_Element (Position: Menu_Cursor) return Boolean
        is (Position.Tree /= null and then Position.Index /= Null_Index);
      
      overriding
      function On_Tree (Position: in     Menu_Cursor;
                        Tree    : in out Standard_Tree'Class)
                       return Boolean
        is (Position.Tree = Menu_Tree(Tree)'Unchecked_Access);
      
      overriding
      function On_Branch (Position: in     Menu_Cursor;
                          Branch  : in out Menu_Type'Class) 
                         return Boolean
        is (Menu_Item(Position.Tree.Index (Position).Ref.all).State.Parent
              = Menu_Branch(Branch).Root);
      
      
      -- Menu_Branch --
      -----------------
      overriding
      function  Index (Menu  : Menu_Branch; 
                       Cursor: Menu_Cursor_Type'Class) 
                      return Menu_Item_Reference_Type
        is (Menu.Tree.Index (Menu_Cursor (Cursor)));
      
      overriding
      function Staging_Branch (Tree: aliased in out Menu_Tree)
                              return Menu_Type'Class
        is (Menu_Branch'(Menu_Type with
                         Tree => Tree'Access, Root => Staging_Branch_Index));

      
   end Generic_Menu_Tree;
   
end Curses.UI.Menus.Standard_Trees.Core;
