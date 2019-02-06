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

with Ada.Exceptions; use Ada;
with Debug; use Debug;

package body Curses.UI.Menus.Standard_Trees.Logic is
   
   --------------------------
   -- Generic_Tree_Element --
   --------------------------
   package body Generic_Tree_Element is
      
      -------------------
      -- Element_State --
      -------------------
      protected body Element_State is
         
         procedure Reset is
         begin
            Active     := False;
            Refs       := 0;
            
            Next_Ptr   := Null_Index;
            Prev_Ptr   := Null_Index;
            
            Sub_Ptr    := Null_Index;
            Parent_Ptr := Null_Index;
            
            Tree_Ptr   := null;
            Self_Ptr   := Null_Index;
         end Reset;
         
         entry Register_Reference (Success: out Boolean) 
           when Active or else Self_Ptr = Null_Index is
         begin
            if Self_Ptr = Null_Index then
               -- Item has been reset.
               Success := False;
            elsif Refs < Natural'Last then
               Refs    := Refs + 1;
               Success := True;
            else
               Success := False;
            end if;
         exception
            when others =>
               Success := False;
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
      
      Common_Entry_Timeout: constant Duration := 5.0;
      
      --
      -- Internal Infrastructure
      --
      
      -- Null_Cursor --
      -----------------
      -- Standard "Null_Cursor" that is still of type Menu_Cursor
      Null_Cursor: constant Menu_Cursor := (Standard_Cursor with 
                                            Tree  => null,
                                            Index => Null_Index);
      
      
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
         select
            Item.State.Register_Reference (Ref_OK);
            return Ref_OK;
         or
            delay Common_Entry_Timeout;
            return False;
         end select;
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
         when e: others =>
            Debug_Line ("Common Cursor Adjust: " &
                          Exceptions.Exception_Information (e) &
                          " [Ref_OK:" & Boolean'Image (Ref_OK) & "]");
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
      with Inline is begin
         if Tree /= null and then Index /= Null_Index then
            Common_Remove_Ref (Tree => Tree, Index => Index);
         end if;
      end Common_Cursor_Finalize;
      
      
      -- Generate_Cursor --
      ---------------------
      -- Create a new full cursor for Item (including adding a reference), or 
      -- return a Null_Cursor if it fails.
      function Generate_Cursor (Item: in out GTE.Tree_Element)
                               return Menu_Cursor
      with Inline is
         Ref_OK: Boolean := False;
         
      begin
         select
            Item.State.Register_Reference (Ref_OK);
         or
            delay Common_Entry_Timeout;
            Ref_OK := False;
         end select;
         
         if Ref_OK then
            return Menu_Cursor'(Standard_Cursor with
                                Tree  => Tree_Access (Item.State.Tree),
                                Index => Item.State.Index);
         else
            return Null_Cursor;
         end if;
      end Generate_Cursor;
      
      --
      -- Menu_Cursor
      --
      
      --------------------------
      -- Progenitor_Of_Branch --
      --------------------------

      function Progenitor_Of_Branch (Trial_Progenitor: GTE.Tree_Element;
                                     Trial_Descendent: Menu_Branch) 
                                    return Boolean with Inline;
      -- Internal override so that we don't need to roll our own cursors when
      -- we want to check this property for an actual GTE.Tree_Element
      
      
      overriding
      function Progenitor_Of_Branch (Trial_Progenitor: Menu_Cursor;
                                     Trial_Descendent: Menu_Type'Class) 
                                    return Boolean
      is begin
         if not Trial_Progenitor.Has_Element then
            return False;
         end if;
         
         declare
            Tree: Menu_Tree renames Menu_Tree (Trial_Progenitor.Tree.all);
            TP_Actual: GTE.Tree_Element renames GTE.Tree_Element
              (Tree(Trial_Progenitor).Ref.all);
         begin
            return Progenitor_Of_Branch 
              (Trial_Progenitor => TP_Actual,
               Trial_Descendent => Menu_Branch (Trial_Descendent));
         end;
      end Progenitor_Of_Branch;
                                     
      ----------------------------------------
      function Progenitor_Of_Branch (Trial_Progenitor: GTE.Tree_Element;
                                     Trial_Descendent: Menu_Branch) 
                                    return Boolean
      is
         Parent_Tracer:          Index_Type := Trial_Descendent.Root;
         Progen_Root  : constant Index_Type := Trial_Progenitor.State.Index;
         
         Tree: Menu_Tree renames Menu_Tree (Trial_Progenitor.State.Tree.all);
         
      begin
         while Parent_Tracer /= Progen_Root loop
            -- We enter the loop knowing that Check_Parent does not
            -- match the index of the Trial_Progenitor (TP).
            
            if Parent_Tracer = Staging_Branch_Index then
               -- We've made it up to the actual root branch of the tree
               -- without making a hit, this means Trial_Progenitor is not a
               -- progenitor of the Trial_Descendent.
               return False;
               
            else
               -- Otherwise advance to the next parent
               declare
                  Up_Root: GTE.Tree_Element renames GTE.Tree_Element 
                    (Lookup (Pool  => Tree.Pool,
                             Index => Parent_Tracer).Ref.all);
               begin
                  Parent_Tracer := Up_Root.State.Parent;
               end;
            end if;
         end loop;
         
         -- The loop ends when Parent_Tracer equals the index of the
         -- Trial_Progenitor, which means the Trial_Descendent is actually 
         -- descendent of Trial_Progenitor
         return True;
      end Progenitor_Of_Branch;
      
      
      ------------
      -- Adjust --
      ------------
      overriding
      procedure Adjust (Cursor: in out Menu_Cursor) is
      begin
         Common_Cursor_Adjust (Tree => Cursor.Tree, Index => Cursor.Index);
      exception
         when e: others =>
            Debug_Line ("Cursor adjust: " & Exceptions.Exception_Information (e));
            raise;
      end Adjust;
      
      
      --------------
      -- Finalize --
      --------------
      overriding
      procedure Finalize (Cursor: in out Menu_Cursor) is
      begin
         Common_Cursor_Finalize (Tree => Cursor.Tree, Index => Cursor.Index);
      exception
         when e: others =>
            Debug_Line ("Cursor finalize: " & Exceptions.Exception_Information (e));
            raise;
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
         Tree  : Tree_Access;
         Ref_OK: Boolean;
         
      begin
         -- First check for any invalid conditions which require us to return
         -- a regular "null menu"
         if Item.State.Tree.all not in Menu_Tree'Class then
            return Null_Menu;
            -- We'd prefer this check to be "harmless" (appear as an empty
            -- submenu) than to raise an Assertion_Error;
         end if;
         
         Tree := Tree_Access (Item.State.Tree);
         
         select
            Item.State.Register_Reference (Ref_OK);
         or
            delay Common_Entry_Timeout;
            Ref_OK := False;
         end select;
         
         if not Ref_OK then
            -- This item cannot be legally referenced, so we have to consider
            -- it DoA
            return Null_Menu;
            
         else
            return Menu_Branch'(Menu_Type with 
                                Tree => Tree, Root => Item.State.Index);
         end if;
         
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
         procedure Extract (Item: in out GTE.Tree_Element) is
            Index, Prev, Next, Parent: Index_Type;
            
         begin
            -- Extract the Item and separate all the links first
            Index  := Item.State.Index;
            Prev   := Item.State.Prev;
            Next   := Item.State.Next;
            Parent := Item.State.Parent;
            
            Item.State.Prev   (Null_Index);
            Item.State.Next   (Null_Index);
            Item.State.Parent (Null_Index);
            
            
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
               
            else
               -- This means we are necessarily the first on the submenu, so
               -- we need to ensure that we re-target the Parent to our Next
               if Parent = Staging_Branch_Index then
                  -- Parent is the Staging_Branch
                  if Our_Tree.Staging.State.Sub /= Index then
                     -- This indicates we have a double-extraction.
                     -- Without this check, we'd end-up setting the Parent's
                     -- sub to Null_Index, unteathering everything on that
                     -- submenu into space, never to be recovered.
                     return;
                     
                  else
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
                     if Parent_Actual.State.Sub /= Index then
                        -- double extract - See above
                        return;
                     else
                        Parent_Actual.State.Sub (Next);
                     end if;
                  end;
               end if;
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
         procedure Insert_Before (Item  : in out GTE.Tree_Element;
                                  Branch: in     Menu_Branch;
                                  Before: in     Menu_Cursor)
         is
            pragma Assertion_Policy (Check);
            
            Before_Prev: Index_Type;
            Item_Index : constant Index_Type := Item.State.Index;
            
            Before_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => Before.Index);
            Before_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (Before_Ref.Ref.all);
            
            
         begin
            -- Checks
            pragma Assert (Before.On_Branch (Branch));
            pragma Assert (not Progenitor_Of_Branch (Item, Branch));
            
            Extract (Item);
            
            -- Relink Before
            Before_Prev   := Before_Actual.State.Prev;
            Before_Actual.State.Prev (Item_Index);
            
            -- Relink Item
            Item.State.Next   (Before.Index);
            Item.State.Prev   (Before_Prev);
            Item.State.Parent (Branch.Root);
            
            
            -- Retarget Before's Parent to Item, if needed
            if Before_Prev = Null_Index then
               -- Inserted item is at the front, we need to retarget the Parent
               -- Item to point to Item, which is now going to the front
               if Branch.Root = Staging_Branch_Index then
                  -- This item is on the Staging_Branch, so we can link it in
                  -- directly
                  Our_Tree.Staging.State.Sub (Item_Index);
                  
               else
                  declare
                     Parent_Ref: Menu_Node_Reference
                       := Lookup (Pool  => Our_Tree.Pool,
                                  Index => Branch.Root);
                     Parent_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Parent_Ref.Ref.all);
                  begin
                     Parent_Actual.State.Sub (Item_Index);
                  end;
               end if;
            else
               -- We need to make sure that Before_Prev also points to Item!
               declare
                  Before_Prev_Ref: Menu_Node_Reference
                    := Lookup (Pool => Our_Tree.Pool,
                               Index => Before_Prev);
                  Before_Prev_Actual: GTE.Tree_Element
                    renames GTE.Tree_Element (Before_Prev_Ref.Ref.all);
               begin
                  Before_Prev_Actual.State.Next (Item_Index);
               end;
            end if;
         end Insert_Before;
         
         
         -- Insert_After --
         ------------------
         procedure Insert_After (Item  : in out GTE.Tree_Element;
                                 Branch: in     Menu_Branch;
                                 After : in     Menu_Cursor)
         is
            pragma Assertion_Policy (Check);
            
            After_Next: Index_Type;
            
            After_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => After.Index);
            After_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (After_Ref.Ref.all);
            
         begin
            -- Checks
            pragma Assert (After.On_Branch (Branch));
            pragma Assert (not Progenitor_Of_Branch (Item, Branch));
            
            Extract (Item);
            
            -- Relink After
            After_Next := After_Actual.State.Next;
            After_Actual.State.Next (Item.State.Index);
            
            -- Relink Item
            Item.State.Next   (After_Next);
            Item.State.Prev   (After.Index);
            Item.State.Parent (After_Actual.State.Parent);
         end Insert_After;
         
         
         -- First_In_Branch --
         ---------------------
         procedure First_In_Branch (Item  : in out GTE.Tree_Element;
                                    Branch: in     Menu_Branch)
         is
            pragma Assertion_Policy (Check);
            
            First_Index: Index_Type;
            Item_Index : Index_Type := Item.State.Index;
         begin
            -- Checks
            pragma Assert (not Progenitor_Of_Branch (Item, Branch));
            
            Extract (Item);
            
            -- Obtain the index for the first item of Branch (if any).
            if Branch.Root = Staging_Branch_Index then
               First_Index := Our_Tree.Staging.State.Sub;
               
            else
               declare
                  Root_Ref: Menu_Node_Reference
                    := Lookup (Pool => Our_Tree.Pool, Index => Branch.Root);
                  Root_Actual: GTE.Tree_Element
                    renames GTE.Tree_Element (Root_Ref.Ref.all);
               begin
                  First_Index := Root_Actual.State.Sub;
               end;
            end if;
            
            
            -- Now insert Item to the first of Branch
            if First_Index /= Null_Index then
               -- There is already something on this branch, so we need to
               -- retarget the parent, and then also set the item to point
               -- back at the new item
               
               -- Link the item with First_Index
               Item.State.Parent (Branch.Root);
               Item.State.Next   (First_Index);
               
               declare
                  First_Item_Ref: Menu_Node_Reference
                    := Lookup (Pool => Our_Tree.Pool, Index => First_Index);
                  First_Item: GTE.Tree_Element
                    renames GTE.Tree_Element (First_Item_Ref.Ref.all);
               begin
                  First_Item.State.Prev (Item_Index);
               end;
               
               -- Retarget Parent to the Item
               if Branch.Root = Staging_Branch_Index then
                  Our_Tree.Staging.State.Sub (Item_Index);
               else
                  declare
                     Root_Ref: Menu_Node_Reference
                       := Lookup (Pool => Our_Tree.Pool, Index => Branch.Root);
                     Root_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Root_Ref.Ref.all);
                  begin
                     Root_Actual.State.Sub (Item_Index);
                  end;
               end if;
               
            else
               -- No items on this branch, just drop Item right in
               
               Item.State.Parent (Branch.Root);
               
               if Branch.Root = Staging_Branch_Index then
                  Our_Tree.Staging.State.Sub (Item.State.Index);
               else
                  declare
                     Branch_Root_Ref: Menu_Node_Reference
                       := Lookup (Pool => Our_Tree.Pool, Index => Branch.Root);
                     Branch_Root: GTE.Tree_Element
                       renames GTE.Tree_Element (Branch_Root_Ref.Ref.all);
                  begin
                     Branch_Root.State.Sub (Item.State.Index);
                  end;
               end if;
            end if;
         end First_In_Branch;
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
            New_Item_Ref: Menu_Node_Reference
              := Lookup (Pool => Tree.Pool, Index => New_Index);
            New_Item: GTE.Tree_Element
              renames GTE.Tree_Element (New_Item_Ref.Ref.all);
            
            New_Cursor: Menu_Cursor;
         begin
            New_Item.State.Reset;
            New_Item.State.Identity (Tree  => Tree'Access,
                                     Index => New_Index);
            New_Item.State.Activate;
            
            return New_Cursor: Menu_Cursor := Generate_Cursor (New_Item) do
               if New_Cursor = Null_Cursor then
                  -- This should literally not be possible
                  Tree.Controller.Free_Index (New_Index);
                  
                  raise Program_Error with
                    "Unable to register newly allocated item.";
                  
               end if;
               
               -- Looks good.
               -- Register the Submenu_Processor callback
               New_Item.Submenu_Processor := Submenu_Processor'Access;
               
               -- Finally, we prepend the new item to the Stanging_Branch
               Tree.Controller.First_In_Branch 
                 (Item   => New_Item,
                  Branch => Menu_Branch'(Menu_Type with
                                         Tree => Tree'Access,
                                         Root => Staging_Branch_Index));
               -- Note that we don't need to worry about references for Branch,
               -- since it is always the Staging_Branch, which never tracks
               -- references.
               
            end return;
         end;
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
              (Item: in out GTE.Tree_Element);
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
           (Item: in out GTE.Tree_Element)
           with Inline;
         
         procedure Deactivate_Branch is new Generic_Iterate_Branch
           (Process => Deactivate_Item)
           with Inline;
         
         ----------------------------------------
         procedure Deactivate_Item
           (Item: in out GTE.Tree_Element)
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
            when e: others =>
               -- This is either a Program_Error, or some other extremely
               -- unlikely issue. We can count this is a failure to
               -- deactivate. If we have managed to Deactivate _our_ Item,
               -- we need to reactivate it
               
               if Deactivate_OK then
                  Item.State.Activate;
               end if;
               
               Debug_Line ("Deactivate_Item: deactivation fault: " &
                             Exceptions.Exception_Information (e));
               
               raise Program_Error with "Item deactivation fault";
               
         end Deactivate_Item;
         
         
         -- Deletion --
         --------------
         -- "Phase 2"
         -- All Deletion tools, follows successful Deactivation.
         
         procedure Delete_Item
           (Item: in out GTE.Tree_Element)
           with Inline;
         
         procedure Delete_Branch is new Generic_Iterate_Branch
           (Process => Delete_Item)
           with Inline;
         
         ----------------------------------------
         procedure Delete_Item
           (Item: in out GTE.Tree_Element)
         is
            Index  : Index_Type := Item.State.Index;
            Submenu: Index_Type := Item.State.Sub;
            -- After we Free, we can't touch Item anymore, so we will pop-off
            -- the Submenu part for our own reference now
         begin
--            Debug_Line (" Delete Item #" & Debug_Lookup (Item.State.Index) &
--                       " Parent:" & Debug_Lookup (Item.State.Parent) &
--                       " Sub:" & Debug_Lookup (Item.State.Sub) &
--                       " Next:" & Debug_Lookup (Item.State.Next) &
--                       " Prev:" & Debug_Lookup (Item.State.Prev));
            Tree.Controller.Extract (Item);
            
            Delete_Branch (Submenu);
            -- Deleting the submenu (if any) will often cause "retargeting" of
            -- the Oarent (I.E. Item) - so we really can't free Item until
            -- after the recursive deletion of Submenu completes
            
            Item.State.Reset;
            Tree.Controller.Free_Index (Index);            
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
         when e: others =>
            Debug_Line ("Delete exception: " & Exceptions.Exception_Information (e));
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

         Branch_Actual: Menu_Branch renames Menu_Branch (Branch);
         -- Both are protected by the precondition
         
         Item_Actual  : GTE.Tree_Element 
           renames GTE.Tree_Element (Tree(Position).Ref.all);
         
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         if Branch_Actual.Tree /= Menu_Cursor (Position).Tree then
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
            
            Add_Failure      : Natural  := 0;
            Give_Up_Threshold: constant := 10;
         begin
            Try_Append: loop
               -- First see if there is anything on this branch at all
               if not Last_Selector.Has_Element then
                  
                  -- Apparently the Branch is empty. We call First_In_Branch,
                  -- since it convers the less likely case that an item has
                  -- been added since we called Iterator.First.
                  
                  Tree.Controller.First_In_Branch (Item   => Item_Actual,
                                                   Branch => Branch_Actual);
                  -- Note that there will be another (protected) progenitor
                  -- check within First_In_Branch, and it is possible an
                  -- exception would be raised. In this case it should be
                  -- propregated back to the user normally
                  
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
               -- which is then caught by the Tree_Controller's Insert_After.
               declare begin
                  Tree.Controller.Insert_After (Item    => Item_Actual,
                                                Branch  => Branch_Actual,
                                                After   => Last_Selector);
                  
                  -- If all went well, we are done
                  return;
               exception
                  when others => null;
                  -- If Insert_After raises an exception, this likely means
                  -- that either:
                  -- 1. After has been moved to a different branch since
                  --    the last call to the iterator, or
                  -- 2. Branch's root has been moved "underneath" (become an
                  --    ancestor of) Item, making it illegal.
                  --
                  -- In the first case, we have a shot at retrying again to get
                  -- the "new" end of the Branch. If this menu is being
                  -- thrashed, we may need to give-up
                  --
                  -- For the second case (rare), we will also "try again", but
                  -- this will likely fail, and will also time-out eventually
                  --
                  -- In both cases, the user has really done a poor job at
                  -- managing the menu tree to get this to happen, so we are
                  -- being nice, but we arn't willing to allow an actual
                  -- deadlock
               end;
               
               
               Add_Failure := Add_Failure + 1;
               if Add_Failure > Give_Up_Threshold then
                  raise Program_Error with 
                    "Append failed due to an unstable target branch " & 
                    " or relocation of target branch root such that it " &
                    " became an ancestor of Item.";
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

         Branch_Actual: Menu_Branch renames Menu_Branch (Branch);
         Item_Actual  : GTE.Tree_Element renames 
           GTE.Tree_Element (Tree(Position).Ref.all);
         
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         if Branch_Actual.Tree /= Tree'Unchecked_Access then
            raise Constraint_Error with "Branch does not belong to Tree";
         end if;
         
         -- This is significantly easier than for Append!
         Tree.Controller.First_In_Branch (Item   => Item_Actual,
                                          Branch => Branch_Actual);
         end Prepend;
      
      
      -------------------
      -- Insert_Before --
      -------------------
      procedure Insert_Before (Tree    : in out Menu_Tree;
                               Branch  : in out Menu_Type'Class;
                               Before  : in     Standard_Cursor'Class;
                               Position: in out Standard_Cursor'Class)
      is
         Item_Actual: GTE.Tree_Element 
           renames GTE.Tree_Element (Tree(Position).Ref.all);
         
      begin
         -- Note that the preconditions checking that Before is both
         -- On_Tree (Tree) and On_Branch (Branch) implies that Branch
         -- is also on Tree
         
         Tree.Controller.Insert_Before (Item   => Item_Actual,
                                        Branch => Menu_Branch (Branch),
                                        Before => Menu_Cursor (Before));
      end Insert_Before;
      
      
      ------------------
      -- Insert_After --
      ------------------
      overriding
      procedure Insert_After (Tree    : in out Menu_Tree;
                              Branch  : in out Menu_Type'Class;
                              After   : in out Standard_Cursor'Class;
                              Position: in out Standard_Cursor'Class)
      is
         Item_Actual: GTE.Tree_Element 
           renames GTE.Tree_Element (Tree(Position).Ref.all);
         
      begin
         Tree.Controller.Insert_After (Item   => Item_Actual,
                                       Branch => Menu_Branch (Branch),
                                       After  => Menu_Cursor (After));
      end Insert_After;
      
   end Generic_Menu_Tree;
end Curses.UI.Menus.Standard_Trees.Logic;
