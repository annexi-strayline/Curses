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

-- This package provides the generic logic to implement any Standard_Tree,
-- both bounded and unbounded.

private package Curses.UI.Menus.Standard_Trees.Logic is
   
   pragma Assertion_Policy (Check);
   
   --------------------------
   -- Generic_Tree_Element --
   --------------------------
   generic
      type Base_Item is limited new Menu_Item_Interface with private;
      
      type Index_Type is range <>; --private;
      Null_Index: in Index_Type;
      -- User-provided Tree indexing type, used for link management
      
   package Generic_Tree_Element is
      
      protected type Element_State is
         
         procedure Reset;
         -- Reset the state to the default values (initial values in the
         -- private part)

         
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
         -- This identity is required by Menu_Node_Interface.Submenu
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
      type Tree_Element is tagged;
      
      function Null_Processor (Item: in out Tree_Element'Class)
                              return Menu_Type'Class
        is (Null_Menu);
      -- This call-back is implemented in Generic_Menu_Tree to store the
      -- correct call-back function in the instantiation for a given
      -- tree, such that invoking Submenu on a Tree_Element will invoke
      -- the tree-specific logic in that package for rending the correct
      -- Menu_Type, namely one which tracks active references.
      --
      -- This callback is key in facilitating recursive iteration of the
      -- Tree without explicit cursor manipulation (for all Item of Menu ..)
      
      
      type Tree_Element is limited new Base_Item and Menu_Node_Interface with
         record
            State            : Element_State;
            Submenu_Processor: not null access function 
              (Item: in out Tree_Element'Class) return Menu_Type'Class
                := Null_Processor'Access;
         end record;
      
      -- Submenu --
      -------------
      overriding
      function  Submenu (Item: in out Tree_Element) return Menu_Type'Class
        is (Item.Submenu_Processor (Item));
      
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
      -- Shall return Null_Index is unable to allocated a new item.
      
      with procedure Free (Pool : in out Item_Pool;
                           Index: in     GTE.Index_Type) is <>;
      -- The implementation will guaruntee no further access to the item
      -- referenced by Index after a call to Free, unless it is subsequently
      -- returned by a later call to Allocate
      --
      -- Free shall not attempt to de-link the item at Index. That is handled
      -- by this package before calling Free.
      --
      -- Freeing a Null_Index value shall have no effect.
      
      -- ** Allocate and Free do not need to be task-safe, All calls to
      -- Allocate or Free from a Tree object will be serialized.
      
      with function Lookup (Pool : in out Item_Pool;
                            Index: in     GTE.Index_Type)
                           return Menu_Node_Reference is <>;
      -- Return a Menu_Node_Reference to a GTE.Tree_Element'Class object
      -- of Pool, referenced by Index. If Index is Null_Index, Reference shall
      -- return Curses.UI.Menus.Null_Menu_Reference
      
      with function Debug_Lookup (Index: in GTE.Index_Type) return String is <>;
      
   package Generic_Menu_Tree is
   
      use all type GTE.Tree_Element;
      
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
                        return Menu_Iterators.Forward_Iterator'Class;
      
      
      ---------------
      -- Menu_Tree --
      ---------------
      type Menu_Tree (Param: Pool_Parameter) is
        limited new Standard_Tree with private;
      
      overriding
      function Index (Tree    : in out Menu_Tree;
                      Position: in     Standard_Cursor'Class)
                     return Menu_Node_Reference
        with Inline, Pre => Position in Menu_Cursor'Class;
      
      overriding
      function Staging_Branch (Tree: aliased in out Menu_Tree)
                         return Menu_Type'Class
        with Inline, Post => Staging_Branch'Result in Menu_Branch'Class;
      
      overriding
      function New_Item (Tree: aliased in out Menu_Tree)
                        return Standard_Cursor'Class
        with Post => New_Item'Result in Menu_Cursor'Class;
      
      overriding
      procedure Delete (Tree    : in out Menu_Tree;
                        Position: in out Standard_Cursor'Class)
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
         
         procedure Extract (Item: in out GTE.Tree_Element);
         -- Cuts Index out from it's list, setting Prev.Next -> Next and
         -- Next.Prev -> Prev. If Index is the first item in a Submenu, that
         -- Parent item's Submenu is retargeted to Next.
         --
         -- Next, Prev, and Parent are then set to Null_Index.
         --
         -- The item's submenu is not modified.
         
         procedure Insert_Before (Item  : in out GTE.Tree_Element;
                                  Branch: in     Menu_Branch;
                                  Before: in     Menu_Cursor);
         
         procedure Insert_After  (Item  : in out GTE.Tree_Element;
                                  Branch: in     Menu_Branch;
                                  After : in     Menu_Cursor);
         -- Relocates From to Before/After, manipulating the links accordingly.
         -- Item is Extracted first.
         --
         -- Before/After must be on Branch - This is rechecked with an
         -- Assert pragma, since it may have changed since the public
         -- precondition
         
         
         procedure First_In_Branch (Item  : in out GTE.Tree_Element;
                                    Branch: in     Menu_Branch);
         -- Atomic operation used by Menu_Tree.Append and .Prepend
         --
         -- Calls Insert_Before with Before set to the first item Branch,
         -- ensuring that the first item in the Branch before Item gets
         -- inserted doesn't suddenly get moved to another branch before
         -- the insert happens.
         
      end Tree_Controller;
      
      ----------------------------------------
      type Menu_Tree (Param: Pool_Parameter) is
        limited new Standard_Tree with
         record
            Pool      : Item_Pool (Param);
            Controller: Tree_Controller (Menu_Tree'Access);
            Staging   : aliased GTE.Tree_Element;
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
                     return Menu_Node_Reference
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
      function On_Branch (Position: Menu_Cursor;
                          Branch  : Menu_Type'Class) 
                         return Boolean
        is (GTE.Tree_Element (Position.Tree.all(Position).Ref.all).State.Parent
              = Menu_Branch(Branch).Root);
      
      
      -- Menu_Branch --
      -----------------
      overriding
      function  Index (Menu  : Menu_Branch; 
                       Cursor: Menu_Cursor_Type'Class) 
                      return Menu_Node_Reference
        is (Menu.Tree.Index (Menu_Cursor (Cursor)));
      
      overriding
      function Staging_Branch (Tree: aliased in out Menu_Tree)
                              return Menu_Type'Class
        is (Menu_Branch'(Menu_Type with
                         Tree => Tree'Access, Root => Staging_Branch_Index));

      
   end Generic_Menu_Tree;
   
end Curses.UI.Menus.Standard_Trees.Logic;
