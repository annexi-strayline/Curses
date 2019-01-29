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

package body Curses.UI.Menus.Standard_Trees.Logic is
   
   --------------------------
   -- Generic_Tree_Element --
   --------------------------
   package body Generic_Tree_Element is
      
      -------------------
      -- Element_State --
      -------------------
      protected body Element_State is
         
         procedure Register_Reference (Success: out Boolean) is
         begin
            if Active and then Refs < Natural'Last then
               Refs    := Refs + 1;
               Success := True;
            else
               Success := False;
            end if;
         end Register_Reference;
         
         procedure Deregister_Reference is
         begin
            if Refs = 0 then
               raise Program_Error with
                 "Deregistration of element with no references (Refs = 0)";
            end if;
            
            Refs := Refs - 1;
         end Deregister_Reference;
         
         
         procedure Activate is
         begin
            Active := True;
         end Activate;
         
         procedure Deactivate (Ref_Remain: out Natural) is
         begin
            Active     := False;
            Ref_Remain := Refs;
         end Deactivate;
         
         
         function  Next return Index_Type is (Next_Ptr);
         function  Prev return Index_Type is (Prev_Ptr);
         
         procedure Next (Index: in Index_Type) is
         begin
            Next_Ptr := Index;
         end Next;
         
         procedure Prev (Index: in Index_Type) is
         begin
            Prev_Ptr := Index;
         end Prev;
         
         procedure Link (Next, Prev: in Index_Type) is
         begin
            Next_Ptr := Next;
            Prev_Ptr := Prev;
         end Link;
         
         
         function  Sub  return Index_Type is (Sub_Ptr);
         
         procedure Sub  (Index: in Index_Type) is
         begin
            Sub_Ptr := Index;
         end Sub;
         
         
         function  Parent return Index_Type is (Parent_Ptr);
         
         procedure Parent (Index: in Index_Type) is
         begin
            Parent_Ptr := Index;
         end Parent;
         
         
         procedure Identity (Tree : in not null Standard_Tree_Access;
                             Index: in Index_Type) is
         begin
            Tree_Ptr := Tree;
            Self_Ptr := Index;
         end Identity;
         
         function  Tree  return Standard_Tree_Access is (Tree_Ptr);
         function  Index return Index_Type           is (Self_Ptr);
      
      end Element_State;
   end Generic_Tree_Element;
   
   
   -----------------------
   -- Generic_Menu_Tree --
   -----------------------
   package body Generic_Menu_Tree is
      
      --
      -- Internal Infrastructure
      --
      
      
      -- Common_Add_Ref --
      --------------------
      -- Common subrogram for calling GTE.Tree_Element.State.Register_Reference
      -- when refering to a GTE.Tree_Element using an Index value.
      
      function  Common_Add_Ref (Tree: Tree_Access; Index: Index_Type)
                               return Boolean
        with Inline 
      is
         Ref_OK: Boolean := False;
         
         Item_Ref: Menu_Node_Reference := Lookup (Pool  => Tree.Pool,
                                                  Index => Index);
         Item: GTE.Tree_Element 
           renames GTE.Tree_Element (Item_Ref.Ref.all);
         
      begin
         Item.State.Register_Reference (Ref_OK);
         return Ref_OK;
      end Common_Add_Ref;
      
      
      -- Common_Remove_Ref --
      -----------------------
      -- Common subrogram for calling 
      -- GTE.Tree_Element.State.Deregister_Reference, when refering to a
      -- GTE.Tree_Element using an Index value.
      
      procedure Common_Remove_Ref (Tree : Tree_Access; Index: Index_Type)
        with Inline 
      is
         Item_Ref: Menu_Node_Reference 
           := Lookup (Pool  => Tree.Pool,
                      Index => Index);
         
         Item_Actual: GTE.Tree_Element 
           renames GTE.Tree_Element (Item_Ref.Ref.all);
         
         procedure Remove_Ref (Item: aliased in out GTE.Tree_Element'Class) is
         begin
            Item.State.Deregister_Reference;
         end Remove_Ref; 
         
      begin
         Item_Actual.State.Deregister_Reference;
      end Common_Remove_Ref;
      
      
      -- Common_Cursor_Adjust --
      --------------------------
      -- Shared code for Menu_Cursor and Menu_Branch - since both types are
      -- internally functionally equivilent - Menu_Branch is really a "cursor"
      -- pointing to a Menu_Branch root Item.
      --
      -- Adjust needs to increase the reference count for the newly copyied
      -- cursor's target.
      
      procedure Common_Cursor_Adjust (Tree : in out Tree_Access;
                                      Index: in out Index_Type)
      with Inline is
         Ref_OK: Boolean := False;

      begin
         -- This indicates a copy of an existing Cursor. If the Cursor is
         -- currently active, we need to increase the reference count. If
         -- anything goes wrong in the process, we invalidate the cursor
         
         if Tree /= null and then Index /= Null_Index then
            Ref_OK := Common_Add_Ref (Tree => Tree, Index => Index);
         else
            Tree  := null;
            Index := Null_Index;
         end if;
         
      exception
         when others =>
            if Ref_OK then
               declare begin
                  Common_Remove_Ref (Tree => Tree, Index => Index);
               exception
                  -- Extremely unlikely
                  when others => null;
               end;
            end if;
            
            Tree  := null;
            Index := Null_Index;
         
      end Common_Cursor_Adjust;
      
      
      -- Common_Cursor_Finalize --
      ----------------------------
      -- Shared code for Menu_Cursor and Menu_Branch - since both types are
      -- internally functionally equivilent - Menu_Branch is really a "cursor"
      -- pointing to a Menu_Branch root Item.
      --
      -- Finalize removes the old reference which has now either been replaced
      -- by an assignment, or has simply gone out of scope.
      
      procedure Common_Cursor_Finalize (Tree : in out Tree_Access;
                                        Index: in out Index_Type)
      is begin
         if Tree /= null and then Index /= Null_Index then
            Common_Remove_Ref (Tree => Tree, Index => Index);
         end if;
      end Common_Cursor_Finalize;
      
      
      --
      -- Menu_Cursor
      --
      
      ------------
      -- Adjust --
      ------------
      overriding
      procedure Adjust (Cursor: in out Menu_Cursor) is
      begin
         Common_Cursor_Adjust (Tree => Cursor.Tree, Index => Cursor.Index);
      end Adjust;
      
      
      --------------
      -- Finalize --
      --------------
      overriding
      procedure Finalize (Cursor: in out Menu_Cursor) is
      begin
         Common_Cursor_Finalize (Tree => Cursor.Tree, Index => Cursor.Index);
      end Finalize;
      
      
      --
      -- Menu_Branch
      --
      
      -----------------------
      -- Submenu_Processor --
      -----------------------
      -- Installed as a call-back for all new items allocated from a given
      -- Menu_Tree that is distinct to this process.
      function Submenu_Processor (Item: in out GTE.Tree_Element'Class) 
                                 return Menu_Type'Class
      is
         Tree      : Tree_Access := null;
         Ref_OK    : Boolean     := False;
         
      begin
         -- First check for any invalid conditions which require us to return
         -- a regular "null menu"
         if Item.State.Tree.all not in Menu_Tree'Class
         then
            return Null_Menu;
         end if;
         
         Tree := Tree_Access (Item.State.Tree);
         
         Item.State.Register_Reference (Ref_OK);
         
         if not Ref_OK then
            return Null_Menu;
         end if;
         
         return Menu_Branch'(Menu_Type with 
                             Tree => Tree, Root => Item.State.Index);
         
      exception
         when others =>
            declare begin
               if Ref_OK then
                  Item.State.Deregister_Reference;
               end if;
            exception
               -- Exceedingly unlikely!
               when others => null;
            end;
            
            return Null_Menu;
      end Submenu_Processor;
      
      
      ------------
      -- Adjust --
      ------------
      overriding
      procedure Adjust (Branch: in out Menu_Branch) is
      begin
         if Branch.Root = Staging_Branch_Index then
            -- The staging branch doesn't need to track references, since it
            -- can never be deleted.
            return;
         else
            Common_Cursor_Adjust (Tree => Branch.Tree, Index => Branch.Root);
         end if;
      end Adjust;
      
      
      --------------
      -- Finalize --
      --------------
      overriding
      procedure Finalize (Branch: in out Menu_Branch) is
      begin
         Common_Cursor_Finalize (Tree => Branch.Tree, Index => Branch.Root);
      end Finalize;
      
      
      -------------
      -- Iterate --
      -------------
      
      -- Standard "Null_Cursor"
      Null_Cursor: constant Menu_Cursor := (Standard_Cursor with 
                                            Tree  => null,
                                            Index => Null_Index);
      
      -- Forward_Iterator --
      ----------------------
      type Branch_Iterator is limited new Menu_Iterators.Forward_Iterator with
         record
            Branch: Menu_Branch;
            -- Using a standard Branch cursor ensures that the reference count 
            -- will be properly handled
         end record;
      -- Note that since this type is totally hidden here, the user can never
      -- create a "default" Branch_Iterator (which could theoretically trigger
      -- an null dereference in First and Next)
      
      overriding
      function First (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
        with Post => First'Result in Menu_Cursor'Class;
      
      overriding
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class
        with Post => Next'Result in Menu_Cursor'Class;
      
      -- First --
      overriding
      function First (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
      is
         Ref_OK     : Boolean    := False;
         First_Index: Index_Type;
         
      begin
         
         if Iterator.Branch.Root = Null_Index then
            -- This refers to the "Staging_Branch", which is stored outside of
            -- the pool
            First_Index := Iterator.Branch.Tree.Staging.State.Sub;
            
         else
            -- For the regular branches
            declare
               Branch_Root_Reference: Menu_Node_Reference 
                 := Lookup (Pool  => Iterator.Branch.Tree.Pool,
                            Index => Iterator.Branch.Root);
               
               Branch_Root: GTE.Tree_Element 
                 renames GTE.Tree_Element (Branch_Root_Reference.Ref.all);
            begin
               First_Index := Branch_Root.state.Sub;
            end;
         end if;
         
         
         if First_Index = Null_Index then
            return Null_Cursor;
         end if;
         
         -- We are hand-crafting a new Menu_Cursor, and so we need to ensure
         -- that we add the new reference first. Eventually, it will be removed
         -- by Finalize
         Ref_OK := Common_Add_Ref (Tree  => Iterator.Branch.Tree,
                                   Index => First_Index);
         
         if Ref_OK then
            return Menu_Cursor'(Standard_Cursor with
                                Tree  => Iterator.Branch.Tree,
                                Index => First_Index);
         else
            return Null_Cursor;
         end if;
         
      exception
         when others =>
            if Ref_OK then
               Common_Remove_Ref (Tree  => Iterator.Branch.Tree,
                                  Index => First_Index);
            end if;
            return Null_Cursor;
      end First;
      
      
      -- Next --
      overriding
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class
      is
         Tree: Menu_Tree renames Menu_Tree (Iterator.Branch.Tree.all);
         
         Ref_OK    : Boolean := False;
         Next_Index: Index_Type;

      begin
         -- The Cursor is less trust-worthy than Iterator, so we do our due-
         -- dilligence first
         
         if Position not in Menu_Cursor'Class then
            -- Protect from any exception in the following declare block's
            -- view conversion of Position
            return Null_Cursor;
         end if;
         
         declare
            Mark: Menu_Cursor renames Menu_Cursor (Position);
            
         begin
            if Iterator.Branch.Tree /= Mark.Tree
              or else Mark.Tree  = null 
              or else Mark.Index = Null_Index
            then
               return Null_Cursor;
            end if;
            
            -- Everything checks-out, we can now query the next position
            declare
               Mark_Actual: GTE.Tree_Element renames 
                 GTE.Tree_Element (Tree(Mark).Ref.all);
            begin
               Next_Index := Mark_Actual.State.Next;
            end;
            
            if Next_Index = Null_Index then
               return Null_Cursor;
            end if;
            
            Ref_OK :=  Common_Add_Ref (Tree  => Mark.Tree,
                                       Index => Next_Index);
            
            if Ref_OK then
               return Menu_Cursor'(Standard_Cursor with
                                   Tree  => Mark.Tree,
                                   Index => Next_Index);
            else
               return Null_Cursor;
            end if;
            
         exception
            when others =>
               if Ref_OK then
                  Common_Remove_Ref (Tree => Mark.Tree, Index => Next_Index);
               end if;
               return Null_Cursor;
         end;
      end Next;
      
      
      ----------------------------------------
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Forward_Iterator'Class
      is
      begin
         if Menu.Tree = null then
            -- Note that Menu.Root 
            return Null_Iterator;
         end if;
         
         return Iterator: Branch_Iterator do
            -- Note that Iterator has now been Initialized, and assigning to
            -- the Branch component will trigger Adjust, thereby setting-up
            -- the reference count properly
            Iterator.Branch := Menu;
         end return;
         
      exception
         when others =>
            return Null_Iterator;
      end Iterate;
      
      
      --
      -- Menu_Tree
      --
      
      ---------------------
      -- Tree_Controller --
      ---------------------
      protected body Tree_Controller is
         
         -- Allocate_Index --
         --------------------
         procedure Allocate_Index (Index: out Index_Type) is
         begin
            Index := Allocate (Pool => Our_Tree.Pool);
         exception
            when others =>
               Index := Null_Index;
         end Allocate_Index;
      
      
         -- Free_Index --
         ----------------
         procedure Free_Index (Index: in Index_Type) is
         begin
            Free (Pool => Our_Tree.Pool, Index => Index);
         exception
            when others => null;
         end Free_Index;
      
         -- Extract --
         -------------
         procedure Extract (Index: Index_Type) is
            Prev, Next, Parent: Index_Type;
            
            Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Index);
            
            Item: GTE.Tree_Element
              renames GTE.Tree_Element (Item_Ref.Ref.all);
            
         begin
            -- Extract the Item and separate all the links first
            Prev   := Item.State.Prev;
            Next   := Item.State.Next;
            Parent := Item.State.Parent;
            
            Item.State.Prev   (Null_Index);
            Item.State.Next   (Null_Index);
            Item.State.Parent (Null_Index);
            
            -- Check the Parent node, if it's Submenu points to us, then we
            -- need to retarget that node to our node
            if Parent = Staging_Branch_Index then
               -- Parent is the Staging_Branch
               if Our_Tree.Staging.State.Sub = Index then
                  Our_Tree.Staging.State.Sub (Next);
               end if;
               
            else
               declare
                  Parent_Ref: Menu_Node_Reference
                    := Lookup (Pool  => Our_Tree.Pool,
                               Index => Parent);
                  
                  Parent_Actual: GTE.Tree_Element renames
                    GTE.Tree_Element (Parent_Ref.Ref.all);
               begin
                  Parent_Actual.State.Sub (Next);
               end;
            end if;
            
            -- Link-through Prev, if applicable
            if Prev /= Null_Index then
               declare
                  Prev_Ref: Menu_Node_Reference
                    := Lookup (Pool  => Our_Tree.Pool,
                               Index => Prev);
                  Prev_Actual: GTE.Tree_Element
                    renames GTE.Tree_Element (Prev_Ref.Ref.all);
               begin
                  Prev_Actual.State.Next (Next);
               end;
            end if;
            
            -- Link-back Next, if applicable
            if Next /= Null_Index then
               declare
                  Next_Ref: Menu_Node_Reference
                    := Lookup (Pool => Our_Tree.Pool, Index => Next);
                  Next_Actual: GTE.Tree_Element 
                    renames GTE.Tree_Element (Next_Ref.Ref.all);
               begin
                  Next_Actual.State.Prev (Prev);
               end;
            end if;
         end Extract;
         
         
         -- Insert_Before --
         -------------------
         procedure Insert_Before (Item, Before: Index_Type) is
            
            Before_Prev, Before_Parent: Index_Type;
            
            Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Item);
            Item_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (Item_Ref.Ref.all);
            
            Before_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Before);
            Before_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (Before_Ref.Ref.all);
            
         begin
            Extract (Item);
            
            -- Relink Before
            Before_Prev   := Before_Actual.State.Prev;
            Before_Parent := Before_Actual.State.Parent;
            Before_Actual.State.Prev (Item);
            
            -- Relink Item
            Item_Actual.State.Next   (Before);
            Item_Actual.State.Prev   (Before_Prev);
            Item_Actual.State.Parent (Before_Parent);
            
            -- Retarget Before's Parent to Item, if needed
            if Before_Prev = Null_Index then
               -- Inserted item is at the front, we need to retarget the Parent
               -- Item to point to Item, which is now in front
               if Before_Parent = Staging_Branch_Index then
                  -- This item is on the Staging_Branch, so we can link it in
                  -- directly
                  Our_Tree.Staging.State.Sub (Item);
                  
               else
                  declare
                     Parent_Ref: Menu_Node_Reference
                       := Lookup (Pool  => Our_Tree.Pool,
                                  Index => Before_Parent);
                     Parent_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Parent_Ref.Ref.all);
                  begin
                     Parent_Actual.State.Sub (Item);
                  end;
               end if;
            end if;
         end Insert_Before;
         
         
         -- Insert_After --
         ------------------
         procedure Insert_After (Item, After : Index_Type) is
            After_Next: Index_Type;
            
            Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Item);
            Item_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (Item_Ref.Ref.all);
            
            After_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => After);
            After_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (After_Ref.Ref.all);
            
         begin
            Extract (Item);
            
            -- Relink After
            After_Next   := After_Actual.State.Next;
            After_Actual.State.Next (Item);
            
            -- Relink Item
            Item_Actual.State.Next   (After_Next);
            Item_Actual.State.Prev   (After);
            Item_Actual.State.Parent (After_Actual.State.Parent);
            
         end Insert_After;
         
         
         -- First_In_Submenu --
         ----------------------
         procedure First_In_Submenu (Root, Item: in Index_Type) is
            First: Index_Type;
            
            Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Item);
            Item_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (Item_Ref.Ref.all);
            
         begin
            Extract (Item);
            
            -- Retarget Root
            if Root = Staging_Branch_Index then
               First := Our_Tree.Staging.State.Sub;
               Our_Tree.Staging.State.Sub (Item);
               
            else
               declare
                  Root_Ref: Menu_Node_Reference
                    := Lookup (Pool => Our_Tree.Pool, Index => Root);
                  Root_Actual: GTE.Tree_Element
                    renames GTE.Tree_Element (Root_Ref.Ref.all);
               begin
                  First := Root_Actual.State.Sub;
                  Root_Actual.State.Sub (Item);
               end;
            end if;
            
            -- Relink Item
            Item_Actual.State.Next   (First);
            Item_Actual.State.Parent (Root);
            
         end First_In_Submenu;
         
         
         -- Next_In_Submenu --
         ---------------------
         procedure Next_In_Submenu (Root, Item, After: in     Index_Type;
                                    Success          :    out Boolean)
         is
            After_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => After);
            After_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (After_Ref.Ref.all);
            
         begin
            
            -- Verify that After's Parent is actually Root
            if After_Actual.State.Parent /= Root then
               Success := False;
            else
               Insert_After (Item => Item, After => After);
               Success := True;
            end if;
         end Next_In_Submenu;
            
      end Tree_Controller;
      
      
      --------------
      -- New_Item --
      --------------
      overriding
      function New_Item (Tree: aliased in out Menu_Tree)
                        return Standard_Cursor'Class
      is
         New_Index: Index_Type := Null_Index;
         
      begin
         -- Try to obtain a new item from the pool and then return a new cursor
         -- to it.
         Tree.Controller.Allocate_Index (New_Index);
         
         if New_Index = Null_Index then
            -- No more space!
            raise Capacity_Error;
         end if;
         
         -- Next step is to set the correct identity for the Item, and then
         -- Activate it so that it can register new references
         declare
            Ref_OK: Boolean;
            
            New_Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Tree.Pool, Index => New_Index);
            New_Item: GTE.Tree_Element
              renames GTE.Tree_Element (New_Item_Ref.Ref.all);
            
         begin
            New_Item.State.Identity (Tree  => Tree'Access,
                                     Index => New_Index);
            New_Item.State.Activate;
            New_Item.State.Register_Reference (Ref_OK);
            
            if not Ref_OK then
               -- This should literally not be possible
               Tree.Controller.Free_Index (New_Index);
            
               raise Program_Error with
                 "Unable to create first reference of new GTE.Tree_Element.";
            end if;
            
            -- Register the Submenu_Processor callback
            New_Item.Submenu_Processor := Submenu_Processor'Access;
         end;
            
         -- Finally, we prepend the new item to the Stanging_Branch
         Tree.Controller.First_In_Submenu (Root => Staging_Branch_Index,
                                           Item => New_Index);
         
         return Menu_Cursor'(Standard_Cursor with 
                             Tree => Tree'Access, Index => New_Index);
         
      end New_Item;
      
      
      ------------
      -- Delete --
      ------------
      overriding
      procedure Delete (Tree    : in out Menu_Tree;
                        Position: in out Standard_Cursor'Class)
      is
         Deletion_Failed: Boolean := False;
         
         -- Generic_Iterate_Branch --
         ----------------------------
         generic
            with procedure Process
              (Item: in out GTE.Tree_Element'Class);
         procedure Generic_Iterate_Branch (Start: in Index_Type);
         -- Recursion over the entire sub-tree rooted at Position needs to
         -- happen over two phases:
         -- 1. All Items need to be successfully deactivated
         -- 2. After total deactivation, all items need to be deleted
         --
         -- Both steps require recursion over branches. Outside of this package
         -- this would obviously be handled by the container iterator
         -- interface. However, we cannot use this here since that would create
         -- additional cursors, and thus references. So for Delete, we must
         -- do manual recursion. This generic procedure provides the abstracted
         -- machinery to do that for both phases.
         --
         -- Generic_Iterate_Branch does not handle further Submenus of iterms
         -- on a branch; this is handled by Process, which then recursively
         -- calls an instantation of Generic_Iterate_Branch
         
         ----------------------------------------
         procedure Generic_Iterate_Branch (Start: in Index_Type) is
            I: Index_Type := Start;
         begin
            while I /= Null_Index loop
               declare
                  Item_Ref: Menu_Node_Reference
                    := Lookup (Pool  => Tree.Pool,
                               Index => I);
                  Item: GTE.Tree_Element
                    renames GTE.Tree_Element (Item_Ref.Ref.all);
               begin
                  -- We advance first to ensure that if Process changes the
                  -- links (usually on Delete), that we can still advance
                  -- anyways.
                  I := Item.State.Next;
                  Process (Item);
               end;
            end loop;
         end Generic_Iterate_Branch;
         
         
         -- Deactivation --
         ------------------
         -- "Phase 1"
         -- All Deactivation tools
         
         procedure Deactivate_Item
           (Item: in out GTE.Tree_Element'Class)
           with Inline;
         
         procedure Deactivate_Branch is new Generic_Iterate_Branch
           (Process => Deactivate_Item)
           with Inline;
         
         ----------------------------------------
         procedure Deactivate_Item
           (Item: in out GTE.Tree_Element'Class)
         is
            Refs         : Natural;
            Deactivate_OK: Boolean := False;
         begin
            Item.State.Deactivate (Ref_Remain => Refs);
            
            if Refs /= 0 then
               raise Program_Error with
                 "Item in sub-tree still has active references";
            else
               Deactivate_OK := True;
            end if;
            
            -- Deactivation is ok. Check for any Submenu and iterate over
            -- that. If any of those fail, Program_Error is propagated,
            -- which we will catch and then reactivate our item
            
            Deactivate_Branch (Item.State.Sub);
            -- Note that if there is no Submenu (Sub = Null_Index),
            -- Deativate_Branch will simply return.
            --
            -- If any Items on the submenu branch cannot be deactivated,
            -- a Program_Error will eventually come back
            
         exception
            when others =>
               -- This is either a Program_Error, or some other extremely
               -- unlikely issue. We can count this is a failure to
               -- deactivate. If we have managed to Deactivate _our_ Item,
               -- we need to reactivate it
               
               if Deactivate_OK then
                  Item.State.Activate;
               end if;
               
               raise Program_Error with "Item deactivation fault";
               
         end Deactivate_Item;
         
         
         -- Deletion --
         --------------
         -- "Phase 2"
         -- All Deletion tools, follows successful Deactivation.
         
         procedure Delete_Item
           (Item: in out GTE.Tree_Element'Class)
           with Inline;
         
         procedure Delete_Branch is new Generic_Iterate_Branch
           (Process => Delete_Item)
           with Inline;
         
         ----------------------------------------
         procedure Delete_Item
           (Item: in out GTE.Tree_Element'Class)
         is
            Submenu: Index_Type := Item.State.Sub;
            -- After we Free, we can't touch Item anymore, so we will pop-off
            -- the Submenu part for our own reference now
         begin
            Tree.Controller.Extract (Item.State.Index);
            Item.Submenu_Processor := GTE.Null_Processor'Access;
            Tree.Controller.Free_Index (Item.State.Index);
            Delete_Branch (Submenu);
            
         exception
            when others =>
               -- This most likely indicates a user error in the implementation
               -- of Free. We will continue, but we cannot safely reactivate
               -- any of the deactivated sub-tree ever - this breaks the
               -- application probably, but it is the most stable outcome we
               -- can offer
               
               Deletion_Failed := True;
         end Delete_Item;
         
         Cursor: Menu_Cursor renames Menu_Cursor (Position);
         -- View conversion is protected by the precondition
         
         Root_Item_Ref: Menu_Node_Reference
           := Lookup (Pool  => Tree.Pool,
                      Index => Cursor.Index);
         Root_Item: GTE.Tree_Element
           renames GTE.Tree_Element (Root_Item_Ref.Ref.all);
         
      begin
         -- By virtue of the preconditions, we know that we have an active
         -- element that is on Tree.
         
         -- This process is two-step and recursive. The goal is to attempt
         -- Deactivation of the entire tree of Items and Submenus rooted at
         -- Position. If this fails, we unwind (reactivate) all successfully
         -- Deactivated Items along the tree and raise a Program_Error.
         --
         -- Remember that such a case *is* a Program_Error, and so any issues
         -- cause by temporary Deactivation is born by the programmer as a
         -- result of incorrect deletion practice. At least we can ensure no
         -- errnoeous execution.
         --
         -- If the operation is successful, the entire sub-tree will be safe
         -- from further references, which makes it safe to delete.
         --
         -- Deletion relies on a user-provided "Free" procedure, and thus
         -- encountering an exception during this phase is both possible and
         -- with unknown side-effects. Therefore if this happens, the entire
         -- sub-tree will be left Deactivated, and a Program_Error is raised
         
         -- All Items in the entire sub-tree are supposed to have no references
         -- before beginning. Of course the user has to have one remaining
         -- reference to the root item in order to invoke Delete. So we need to
         -- destroy that reference first
         -- Remove the ref first
         Common_Remove_Ref (Tree => Cursor.Tree, Index => Cursor.Index);
         
         -- If anything fails from here-on-out (we get an exception), then
         -- we need to re-register the reference.
         
         Deactivate_Item (Root_Item);  -- Phase 1
         Delete_Item     (Root_Item);  -- Phase 2
         
         -- This means either everything worked, or Phase 2 failed. If Phase 2
         -- fails, we cannot trust the tree anymore. Therefore we need to
         -- invalidate the cursor anyways
         
         Cursor.Tree  := null;
         Cursor.Index := Null_Index;
         
         if Deletion_Failed then
            raise Program_Error with 
              "Deletion failed. Sub-tree may have been isolated.";
         end if;
         
      exception
         when others =>
            -- Conceivably this should only be Program_Error, but we did not
            -- promise to raise exclusive exceptions
            
            if not Deletion_Failed then
               if not Common_Add_Ref (Tree => Cursor.Tree,
                                      Index => Cursor.Index)
               then
                  raise Program_Error with 
                    "Unable to restore failed deletion attempt on item";
               end if;
            end if;
            
            raise;
      end Delete;
      
      
      ------------
      -- Append --
      ------------
      overriding
      procedure Append (Tree    : in out Menu_Tree;
                        Branch  : in out Menu_Type'Class;
                        Position: in out Standard_Cursor'Class)
      is
         Item         : Menu_Cursor renames Menu_Cursor (Position);
         Branch_Actual: Menu_Branch renames Menu_Branch (Branch);
         -- Both are protected by the precondition
         
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         if Branch_Actual.Tree /= Tree'Unchecked_Access then
            raise Constraint_Error with "Branch does not belong to Tree";
         end if;
         
         -- Now everthing should be valid. We now need to seek to the end of
         -- the branch. We will simply use our own iteration scheme to do this.
         -- We are talking about actual Menus here, so they really ought not to
         -- be thousands of items long. Maintaining end of list pointers seems
         -- like a waste of effort.
         
         -- The good thing about iterating is that it creates Cursors. Each run
         -- through the loop has a Cursor which excludes any possible deletion
         -- of the Item until we are done with it.
         
         declare
            Iterator: Menu_Iterators.Forward_Iterator'Class := Branch.Iterate;
            -- Use Branch instead of Branch_Actual to ensure dispatch is always
            -- correct.
            
            Last_Selector: Menu_Cursor := Menu_Cursor (Iterator.First);
            -- Note t hat this is "guranteed" since our Iterator.First, as
            -- implemented in this package, always returns a Menu_Cursor, even
            -- if Has_Element is false. This is to ease this kind of manual
            -- iteration.
            
            Next_Selector: Menu_Cursor;
            
            Add_OK           : Boolean  := False;
            Add_Failure      : Natural  := 0;
            Give_Up_Threshold: constant := 100;
         begin
            Try_Append: loop
               -- First see if there is anything on this branch at all
               if not Last_Selector.Has_Element then
                  
                  -- Apparently the Branch is empty. We call First_In_Submenu,
                  -- since it convers the less likely case that an item has
                  -- been added since we called Iterator.First
                  Tree.Controller.First_In_Submenu
                    (Root => Branch_Actual.Root,
                     Item => Item.Index);
                  
                  return;
               end if;
               
               Seek_End: loop
                  Next_Selector := Menu_Cursor (Iterator.Next (Last_Selector));
                  exit Seek_End when not Next_Selector.Has_Element;
                  
                  Last_Selector := Next_Selector;
               end loop Seek_End;
               
               -- This_Selector should now point to the last item (currently)
               -- on the Branch. Note that since we have an active Cursor to
               -- that Item, it cannot be deleted, though it could be moved,
               -- which is then caught by Next_On_Submenu
               Tree.Controller.Next_In_Submenu (Root    => Branch_Actual.Root,
                                                Item    => Item.Index,
                                                After   => Last_Selector.Index,
                                                Success => Add_OK);
               
               exit Try_Append when Add_OK;
               -- If Add_OK is False, this means the the last item on Branch
               -- has been moved to another branch between Seek_End and the
               -- attempt to append Item. Therefore we need to do another
               -- seek attempt.
               
               -- It is possible, but extremely unlikely, to have a deadlock
               -- here. This keeps us working at least partially
               Add_Failure := Add_Failure + 1;
               if Add_Failure > Give_Up_Threshold then
                  raise Program_Error with 
                    "Append failed due to an unstable target branch";
               end if;
            end loop Try_Append;
         end;
      end Append;
      
      
      -------------
      -- Prepend --
      -------------
      overriding
      procedure Prepend (Tree    : in out Menu_Tree;
                         Branch  : in out Menu_Type'Class;
                         Position: in out Standard_Cursor'Class)
      is
         Item         : Menu_Cursor renames Menu_Cursor (Position);
         Branch_Actual: Menu_Branch renames Menu_Branch (Branch);
         -- Both are protected by the precondition
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         if Branch_Actual.Tree /= Tree'Unchecked_Access then
            raise Constraint_Error with "Branch does not belong to Tree";
         end if;
         
         -- This is significantly easier than for Append!
         Tree.Controller.First_In_Submenu (Root => Branch_Actual.Root,
                                           Item => Item.Index);
      end Prepend;
      
      
      -------------------
      -- Insert_Before --
      -------------------
      procedure Insert_Before (Tree    : in out Menu_Tree;
                               Before  : in     Standard_Cursor'Class;
                               Position: in out Standard_Cursor'Class)
      is
         Before_Cursor: Menu_Cursor renames Menu_Cursor (Before);
         Item_Cursor  : Menu_Cursor renames Menu_Cursor (Position);
      begin
         Tree.Controller.Insert_Before (Item   => Item_Cursor.Index,
                                        Before => Before_Cursor.Index);
      end Insert_Before;
      
      
      ------------------
      -- Insert_After --
      ------------------
      overriding
      procedure Insert_After (Tree    : in out Menu_Tree;
                              After   : in out Standard_Cursor'Class;
                              Position: in out Standard_Cursor'Class)
      is
         After_Cursor: Menu_Cursor renames Menu_Cursor (After);
         Item_Cursor : Menu_Cursor renames Menu_Cursor (Position);
      begin
         Tree.Controller.Insert_After (Item  => Item_Cursor.Index,
                                       After => After_Cursor.Index);
      end Insert_After;
      
   end Generic_Menu_Tree;
end Curses.UI.Menus.Standard_Trees.Logic;
