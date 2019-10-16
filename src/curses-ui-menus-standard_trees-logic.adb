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
   
   pragma Assertion_Policy (Check);
   
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
            Active        := False;
            Refs          := 0;
            
            Next_Ptr      := Null_Index;
            Prev_Ptr      := Null_Index;
            
            Sub_First_Ptr := Null_Index;
            Sub_Last_Ptr  := Null_Index;
            Sub_Count     := 0;
            
            Parent_Ptr    := Null_Index;
            
            Tree_Ptr      := null;
            Self_Ptr      := Null_Index;
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
         
         function  Sub_First return Index_Type is (Sub_First_Ptr);
         function  Sub_Last  return Index_Type is (Sub_Last_Ptr );
         
         procedure Sub_First (Index: in Index_Type) is
         begin
            Sub_First_Ptr := Index;
         end Sub_First;
         
         procedure Sub_Last (Index: in Index_Type) is
         begin
            Sub_Last_Ptr := Index;
         end Sub_Last;
         
         
         function  Sub_Items return Natural is (Sub_Count);
         
         procedure Sub_Items_Increment is
         begin
            Sub_Count := Sub_Count + 1;
         end Sub_Items_Increment;
            
         procedure Sub_Items_Decrement is
         begin
            Sub_Count := Sub_Count - 1;
         end Sub_Items_Decrement;
         
         procedure Sub_Items_Clear is
         begin
            Sub_Count := 0;
         end Sub_Items_Clear;
         
         
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
      -- Adjust needs to increase the reference count for the newly copied
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
         Tree  : Tree_Access;
         Ref_OK: Boolean;
         
      begin
         -- First check for any invalid conditions which require us to return
         -- a regular "null menu"
         if Item.State.Tree.all not in Menu_Tree'Class then
            return No_Branch;
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
            return No_Branch;
            
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
            
            return No_Branch;
      end Submenu_Processor;
      
      
      ------------
      -- Adjust --
      ------------
      overriding
      procedure Adjust (Branch: in out Menu_Branch) is
      begin
         if Branch.Root = Staging_Branch_Index then
            -- Either No_Branch or the Staging branch.
            --
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
      
      -- Reversible_Iterator --
      -------------------------
      type Branch_Iterator is limited 
        new Menu_Iterators.Reversible_Iterator with
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
      
      overriding
      function Last (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
        with Post => Last'Result in Menu_Cursor'Class;
      
      overriding
      function Previous (Iterator: Branch_Iterator;
                         Position: Menu_Cursor_Type'Class) 
                        return Menu_Cursor_Type'Class
        with Post => Previous'Result in Menu_Cursor'Class;
      
      
      Null_Iterator: constant Branch_Iterator 
        := (Menu_Iterators.Reversible_Iterator with 
            Branch => No_Branch);
      
      -- Generic Versions -----------------------------------------------------
      
      -- First/Last --
      generic
         with function Get_Sub_End_Index (Root: GTE.Tree_Element'Class) 
                                         return Index_Type;
         -- Get either the First or Last index of Branch's Sub
      function Generic_First_Last (Iterator: Branch_Iterator'Class)
                                  return Menu_Cursor_Type'Class;
      
      
      function Generic_First_Last (Iterator: Branch_Iterator'Class)
                                  return Menu_Cursor_Type'Class
      is
         Ref_OK       : Boolean    := False;
         Subject_Index: Index_Type;
         
      begin
         
         if Iterator.Branch.Root = Null_Index then
            -- This refers to the "Staging_Branch", which is stored outside of
            -- the pool
            Subject_Index := Get_Sub_End_Index (Iterator.Branch.Tree.Staging);
            
         else
            -- For the regular branches
            declare
               Branch_Root_Reference: Menu_Node_Reference 
                 := Lookup (Pool  => Iterator.Branch.Tree.Pool,
                            Index => Iterator.Branch.Root);
               
               Branch_Root: GTE.Tree_Element 
                 renames GTE.Tree_Element (Branch_Root_Reference.Ref.all);
            begin
               Subject_Index := Get_Sub_End_Index (Branch_Root);
            end;
         end if;
         
         
         if Subject_Index = Null_Index then
            return Null_Cursor;
         end if;
         
         -- We are hand-crafting a new Menu_Cursor, and so we need to ensure
         -- that we add the new reference first. Eventually, it will be removed
         -- by Finalize
         Ref_OK := Common_Add_Ref (Tree  => Iterator.Branch.Tree,
                                   Index => Subject_Index);
         
         if Ref_OK then
            return Menu_Cursor'(Standard_Cursor with
                                Tree  => Iterator.Branch.Tree,
                                Index => Subject_Index);
         else
            return Null_Cursor;
         end if;
         
      exception
         when others =>
            if Ref_OK then
               Common_Remove_Ref (Tree  => Iterator.Branch.Tree,
                                  Index => Subject_Index);
            end if;
            return Null_Cursor;
      end Generic_First_Last;
      
      
      -- Next/Prev --
      generic
         with function Get_Next_Prev_Index (Item: GTE.Tree_Element'Class) 
                                           return Index_Type;
      function Generic_Next_Prev (Iterator: Branch_Iterator'Class;
                                  Position: Menu_Cursor_Type'Class)
                                 return Menu_Cursor_Type'Class;
      
      function Generic_Next_Prev (Iterator: Branch_Iterator'Class;
                                  Position: Menu_Cursor_Type'Class)
                                 return Menu_Cursor_Type'Class
      is
         Tree: Menu_Tree renames Menu_Tree (Iterator.Branch.Tree.all);
         
         Ref_OK       : Boolean := False;
         Subject_Index: Index_Type;

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
               Subject_Index := Get_Next_Prev_Index (Mark_Actual);
            end;
            
            if Subject_Index = Null_Index then
               return Null_Cursor;
            end if;
            
            Ref_OK :=  Common_Add_Ref (Tree  => Mark.Tree,
                                       Index => Subject_Index);
            
            if Ref_OK then
               return Menu_Cursor'(Standard_Cursor with
                                   Tree  => Mark.Tree,
                                   Index => Subject_Index);
            else
               return Null_Cursor;
            end if;
            
         exception
            when others =>
               if Ref_OK then
                  Common_Remove_Ref (Tree  => Mark.Tree, 
                                     Index => Subject_Index);
               end if;
               return Null_Cursor;
         end;
      end Generic_Next_Prev;
      
      
      -- Iterator Implementations ---------------------------------------------
      
      -- First --
      overriding
      function First (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
      is 
         function Get_First_Sub (Branch: GTE.Tree_Element'Class)
                                return Index_Type
           is (Branch.State.Sub_First) with Inline;
         
         function First_Actual is new Generic_First_Last (Get_First_Sub)
           with Inline;
      begin
         return First_Actual (Iterator);
      end First;
      
      -- Last --
      overriding
      function Last (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
      is 
         function Get_Last_Sub (Branch: GTE.Tree_Element'Class)
                               return Index_Type
           is (Branch.State.Sub_Last) with Inline;
         
         function Last_Actual is new Generic_First_Last (Get_Last_Sub)
           with Inline;
      begin
         return Last_Actual (Iterator);
      end Last;
      
      
      -- Next --
      overriding
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class
      is
         function Get_Next_Index (Item: GTE.Tree_Element'Class)
                                 return Index_Type 
           is (Item.State.Next) with Inline;
         
         function Next_Actual is new Generic_Next_Prev (Get_Next_Index)
           with Inline;
      begin
         return Next_Actual (Iterator, Position);
      end Next;
      
      -- Previous --
      overriding
      function Previous (Iterator: Branch_Iterator;
                         Position: Menu_Cursor_Type'Class) 
                        return Menu_Cursor_Type'Class
      is
         function Get_Prev_Index (Item: GTE.Tree_Element'Class)
                                 return Index_Type 
           is (Item.State.Prev) with Inline;
         
         function Prev_Actual is new Generic_Next_Prev (Get_Prev_Index)
           with Inline;
      begin
         return Prev_Actual (Iterator, Position);
      end Previous;
      
      
      ----------------------------------------
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Reversible_Iterator'Class
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
      
      
      ----------------
      -- Item_Count --
      ----------------
      overriding
      function  Item_Count (Menu: Menu_Branch) return Natural is

      begin
         if Menu.Tree = null then
            return 0;
            
         elsif Menu.Root = Staging_Branch_Index then
            return Menu.Tree.Staging.State.Sub_Items;
            
         else
            declare
               Root_Ref: constant Menu_Node_Reference 
                 := Lookup (Pool  => Menu.Tree.Pool,
                            Index => Menu.Root);
               Root: GTE.Tree_Element renames 
                 GTE.Tree_Element (Root_Ref.Ref.all);
            begin
               return Root.State.Sub_Items;
            end;
         end if;
      end Item_Count;
      
      
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
            
            -- Link-through Prev -> Next
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
            
            
            -- Link-back Next <- Prev
            if Next /= Null_Index then
               declare
                  Next_Ref: Menu_Node_Reference
                    := Lookup (Pool  => Our_Tree.Pool,
                               Index => Next);
                  Next_Actual: GTE.Tree_Element
                    renames GTE.Tree_Element (Next_Ref.Ref.all);
               begin
                  Next_Actual.State.Prev (Prev);
               end;
            end if;
            
            
            -- Adjust the Parent's Submenu property
            declare
               procedure Adjust_Parent 
                 (Parent_Actual: in out GTE.Tree_Element'Class) is
               begin
                  Parent_Actual.State.Sub_Items_Decrement;
                  
                  if Parent_Actual.State.Sub_First = Index then
                     Parent_Actual.State.Sub_First (Next);
                  end if;
                  
                  if Parent_Actual.State.Sub_Last = Index then
                     Parent_Actual.State.Sub_Last (Prev);
                  end if;
               end Adjust_Parent;
               
            begin
               if Parent = Staging_Branch_Index then
                  -- Special check - if Parent is the staging branch, we
                  -- potentially have a parent that is "actually" a null
                  -- index. If that's the case, Item is already extracted.
                  -- So we don't want to adjust the parent!
                  if Parent = Null_Index
                    and then Next = Null_Index
                    and then Prev = Null_Index
                    and then Our_Tree.Staging.State.Sub_First /= Index
                    and then Our_Tree.Staging.State.Sub_Last  /= Index
                  then
                     -- Double extraction
                     return;
                  end if;
                  
                  Adjust_Parent (Our_Tree.Staging);
                  
               else
                  declare
                     Parent_Ref: Menu_Node_Reference
                       := Lookup (Pool  => Our_Tree.Pool,
                                  Index => Parent);
                     Parent_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Parent_Ref.Ref.all);
                  begin

                     
                     Adjust_Parent (Parent_Actual);
                  end;
               end if;
            end;
         end Extract;
         
         
         -- Insert_Before --
         -------------------
         procedure Insert_Before (Item  : in out GTE.Tree_Element;
                                  Branch: in     Menu_Branch;
                                  Before: in     Menu_Cursor)
         is
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
            
            -- Adjust new parent
            declare
               procedure Adjust_Parent (Parent: in out GTE.Tree_Element'Class)
               is begin
                  Parent.State.Sub_Items_Increment;
                  
                  if Parent.State.Sub_First = Before.Index then
                     Parent.State.Sub_First (Item_Index);
                  end if;
               end Adjust_Parent;
            begin
               if Branch.Root = Staging_Branch_Index then
                  Adjust_Parent (Our_Tree.Staging);
               else
                  declare
                     Parent_Ref: Menu_Node_Reference
                       := Lookup (Pool => Our_Tree.Pool, Index => Branch.Root);
                     Parent_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Parent_Ref.Ref.all);
                  begin
                     Adjust_Parent (Parent_Actual);
                  end;
               end if;
            end;
         end Insert_Before;
         
         
         -- Insert_After --
         ------------------
         procedure Insert_After (Item  : in out GTE.Tree_Element;
                                 Branch: in     Menu_Branch;
                                 After : in     Menu_Cursor)
         is
            After_Next: Index_Type;
            
            After_Ref: Menu_Node_Reference
              := Lookup (Pool => Our_Tree.Pool, Index => After.Index);
            After_Actual: GTE.Tree_Element
              renames GTE.Tree_Element (After_Ref.Ref.all);
            
            Item_Index: constant Index_Type := Item.State.Index;
            
         begin
            -- Checks
            pragma Assert (After.On_Branch (Branch));
            pragma Assert (not Progenitor_Of_Branch (Item, Branch));
            
            Extract (Item);
            
            -- Relink After
            After_Next := After_Actual.State.Next;
            After_Actual.State.Next (Item_Index);
            
            -- Relink Item
            Item.State.Next   (After_Next);
            Item.State.Prev   (After.Index);
            Item.State.Parent (After_Actual.State.Parent);
            
            -- Adjust new parent
            declare
               procedure Adjust_Parent (Parent: in out GTE.Tree_Element'Class)
               is begin
                  Parent.State.Sub_Items_Increment;
                  
                  if Parent.State.Sub_Last = After.Index then
                     Parent.State.Sub_Last (Item_Index);
                  end if;
               end Adjust_Parent;
            begin
               if Branch.Root = Staging_Branch_Index then
                  Adjust_Parent (Our_Tree.Staging);
               else
                  declare
                     Parent_Ref: Menu_Node_Reference
                       := Lookup (Pool => Our_Tree.Pool, Index => Branch.Root);
                     Parent_Actual: GTE.Tree_Element
                       renames GTE.Tree_Element (Parent_Ref.Ref.all);
                  begin
                     Adjust_Parent (Parent_Actual);
                  end;
               end if;
            end;
         end Insert_After;
         
         
         -- Branch_Prepend --
         --------------------
         procedure Branch_Prepend (Item  : in out GTE.Tree_Element;
                                   Branch: in     Menu_Branch)
         is
            Item_Index : Index_Type := Item.State.Index;
            
            Root_Ref: Menu_Node_Reference
              := (if Branch.Root = Staging_Branch_Index then
                    (Ref => Branch.Tree.Staging'Access)
                  else
                     Lookup (Pool  => Branch.Tree.Pool,
                             Index => Branch.Root));
            Root_Actual: GTE.Tree_Element 
              renames GTE.Tree_Element (Root_Ref.Ref.all);
            
            Root_Sub_First: constant Index_Type := Root_Actual.State.Sub_First;
         begin
            -- Does the branch have anything on it?
            if Root_Sub_First = Null_Index then
               -- Do it right here, right now
               pragma Assert (not Progenitor_Of_Branch (Item, Branch));
               pragma Assert (Root_Actual.State.Sub_Items = 0);
               
               Extract (Item);
               
               Item.State.Parent (Branch.Root);
               Root_Actual.State.Sub_First (Item_Index);
               Root_Actual.State.Sub_Last  (Item_Index);
               Root_Actual.State.Sub_Items_Increment;
            else
               -- This is just a regular Insert_Before
               declare
                  -- Engineer the Before_Cursor
                  Before_Cursor: Menu_Cursor 
                    := (Standard_Cursor with
                        Tree  => Our_Tree,
                        Index => Root_Sub_First);
                  
                  Ref_OK: Boolean;
               begin
                  Ref_OK := Common_Add_Ref 
                    (Tree  => Before_Cursor.Tree,
                     Index => Before_Cursor.Index);
                  
                  Insert_Before (Item   => Item,
                                 Branch => Branch,
                                 Before => Before_Cursor);
               exception
                  when others =>
                     if not Ref_OK then
                        -- Ensure finalization of Before_Cursor does not
                        -- dereference again
                        Before_Cursor.Tree  := null;
                        Before_Cursor.Index := Null_Index;
                     end if;
               end;
            end if;
         end Branch_Prepend;
         
         
         -- Branch_Append --
         -------------------
         procedure Branch_Append (Item  : in out GTE.Tree_Element;
                                  Branch: in     Menu_Branch)
         is
            Item_Index : Index_Type := Item.State.Index;
            
            Root_Ref: Menu_Node_Reference
              := (if Branch.Root = Staging_Branch_Index then
                    (Ref => Branch.Tree.Staging'Access)
                  else
                     Lookup (Pool  => Branch.Tree.Pool,
                             Index => Branch.Root));

            Root_Actual: GTE.Tree_Element 
              renames GTE.Tree_Element (Root_Ref.Ref.all);
            
            Root_Sub_Last: constant Index_Type := Root_Actual.State.Sub_Last;
         begin
            -- Does the branch have anything on it?
            if Root_Sub_Last = Null_Index then
               -- Do it right here, right now
               pragma Assert (not Progenitor_Of_Branch (Item, Branch));
               pragma Assert (Root_Actual.State.Sub_Items = 0);
               
               Extract (Item);
               
               Item.State.Parent (Branch.Root);
               Root_Actual.State.Sub_First (Item_Index);
               Root_Actual.State.Sub_Last  (Item_Index);
               Root_Actual.State.Sub_Items_Increment;
               
            else
               -- This is just a regular Insert_After
               declare
                  -- Engineer the After_Cursor
                  After_Cursor: Menu_Cursor 
                    := (Standard_Cursor with
                        Tree  => Our_Tree,
                        Index => Root_Sub_Last);
                  
                  Ref_OK: Boolean;
               begin
                  Ref_OK := Common_Add_Ref 
                    (Tree  => After_Cursor.Tree,
                     Index => After_Cursor.Index);
                  
                  Insert_After (Item   => Item,
                                Branch => Branch,
                                After  => After_Cursor);
               exception
                  when others =>
                     if not Ref_OK then
                        -- Ensure finalization of Before_Cursor does not
                        -- dereference again
                        After_Cursor.Tree  := null;
                        After_Cursor.Index := Null_Index;
                     end if;
               end;
            end if;
         end Branch_Append;
      end Tree_Controller;
      
      
      ------------
      -- Create --
      ------------
      overriding
      function Create (Tree: aliased in out Menu_Tree)
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
            New_Item.State.Identity (Tree  => Tree'Unchecked_Access,
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
               Tree.Controller.Branch_Append
                 (Item   => New_Item,
                  Branch => Menu_Branch'(Menu_Type with
                                         Tree => Tree'Access,
                                         Root => Staging_Branch_Index));
               -- Note that we don't need to worry about references for Branch,
               -- since it is always the Staging_Branch, which never tracks
               -- references.
            end return;
         end;
      end Create;
      
      
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
            
            Deactivate_Branch (Item.State.Sub_First);
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
            Submenu: Index_Type := Item.State.Sub_First;
            -- After we Free, we can't touch Item anymore, so we will pop-off
            -- the Submenu part for our own reference now
         begin

            Tree.Controller.Extract (Item);
            
            Delete_Branch (Submenu);
            -- Deleting the submenu (if any) will often cause "retargeting" of
            -- the Parent (I.E. Item) - so we really can't free Item until
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
         
         Branch_Actual: Menu_Branch renames Menu_Branch (Branch);
         Item_Actual  : GTE.Tree_Element renames 
           GTE.Tree_Element (Tree(Position).Ref.all);
         
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         if Branch_Actual.Tree /= Tree'Unchecked_Access then
            raise Constraint_Error with "Branch does not belong to Tree";
         end if;
         
         Tree.Controller.Branch_Append (Item   => Item_Actual,
                                        Branch => Branch_Actual);
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
         
         Tree.Controller.Branch_Prepend (Item   => Item_Actual,
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
