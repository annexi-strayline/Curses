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

package body Curses.UI.Menus.Standard_Trees.Core is
   
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
               Refs    := Regf + 1;
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
         function  Prev return Index_Type is (Prev_Prt);
         
         procedure Next (Index: in Index_Type) is
         begin
            Next_Ptr := Index;
         end Next;
         
         procedure Prev (Index: in Index_Type) is
         begin
            Prev_Prt := Index;
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
         
         
         procedure Tree (Set: not null Standard_Tree_Access) is
         begin
            Tree_Ptr := Set;
         end Tree;
         
         function  Tree return not null Standard_Tree_Access is (Tree_Ptr);
      
      end Element_State;
   end Generic_Tree_Element;
   
   
   -----------------------
   -- Generic_Menu_Tree --
   -----------------------
   package body Generic_Menu_Tree is
      
      --
      -- Menu_Item
      --
      
      -------------
      -- Submenu --
      -------------
      overriding
      function Submenu (Item: in out Menu_Item) return Menu_Type'Class
      is
         Tree      : Tree_Access := null;
         Ref_OK    : Boolean     := False;
         
      begin
         -- First check for any invalid conditions which require us to return
         -- a regular "null menu"
         if not Item.Has_Element
           or else Item.Tree.all not in Menu_Tree'Class
         then
            return Null_Menu;
         end if;
         
         Tree := Tree_Access (Item.Tree);
         
         Item.State.Register_Reference (Ref_OK);
         
         if not Ref_OK then
            return Null_Menu;
         end if;
         
         -- Note that once Root (Item) and Head (Item.Sub) references are
         -- successfully registered, we are promised by the implementation
         -- that that Item will not be deleted, and Sub will not be
         -- modified.
         
         -- We should be safe from further exceptions past this point
         return Menu_Branch'(Tree => Tree, Root => Root_Index);
         
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
      end Submenu;
      
      --
      -- Common Implementations
      --
      
      -- Common_Add_Ref --
      --------------------
      function  Common_Add_Ref (Tree : in out Tree_Access;
                                Index: in out Index_Type)
                               return Boolean
      with Inline is
         Ref_OK: Boolean := False;
      
         procedure New_Ref (Item: aliased in out GTE.Tree_Element'Class) is
         begin
            Item.Status.Register_Reference (Ref_OK);
         end New_Ref;
         
      begin
         Modify (Pool   => Tree.Pool,
                 Index  => Index,
                 Action => New_Ref'Access);
         
         return Ref_OK;
      end Common_Add_Reference;
      
      
      -- Common_Remove_Ref --
      -----------------------
      function Common_Remove_Ref (Tree: in out Tree_Access;
                                  Index: in out Index_Type)
                                 return Boolean
      with Inline is
         procedure Remove_Ref (Item: aliased in out GTE.Tree_Element'Class) is
         begin
            Item.Stats.Deregister_Reference;
         end Remove_Ref; 
         
      begin
         Modify (Pool   => Tree.Pool,
                 Index  => Index,
                 Action => Remove_Ref'Access);
      end Common_Remove_Ref;
      
      
      -- Common_Cursor_Adjust --
      --------------------------
      -- Shared code for Menu_Cursor and Menu_Branch
      procedure Common_Cursor_Adjust (Tree: in out Tree_Access;
                                      Index: in out Index_Type)
      with Inline is
         Ref_OK: Boolean := False;

      begin
         -- This indicates a copy of an existing Cursor. If the Cursor is
         -- currently active, we need to increase the reference count. If
         -- anything goes wrong in the process, we invalidate the cursor
         
         if Tree /= null and then Index /= Null_Index then
            Ref_OK := Common_Add_Reference (Tree => Tree, Index => Index);
         else
            Tree  := null;
            Index := Null_Index;
         end if;
         
      exception
         when others =>
            if Ref_OK 
              and then Tree /= null
              and then Index /= Null_Index
            then
               declare begin
                  Common_Remove_Reference (Tree => Tree, Index => Index);
               exception
                  when others => null;
               end;
            end if;
            
            Tree  := null;
            Index := Null_Index;
         
      end Common_Cursor_Adjust;
      
      
      -- Common_Cursor_Finalize --
      ----------------------------
      procedure Common_Cursor_Finalize (Tree : in out Tree_Access;
                                        Index: in out Index_Type)
      is
      begin
         
         if Tree /= null and then Index /= Null_Reference then
            Common_Remove_Ref (Tree => Tree, Index => Index);
         end if;
         
         -- We do not check for exceptions, as the only expected one would be
         -- a Program_Error due to an implementation error internal to this
         -- package.
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
         Common_Cursor_Finalize (Tree => Cursor.Tree, Index => Cursor.Root);
      end Finalize;
      
      
      --
      -- Menu_Branch
      --
      
      ------------
      -- Adjust --
      ------------
      overriding
      procedure Adjust (Branch: in out Menu_Branch) is
      begin
         if Branch.Index = Null_Index then
            -- This indicates a Main_Menu branch, which doesn't need to track
            -- references.
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
         if Branch.Index = Null_Index then
            return;
         else
            Common_Cursor_Finalize (Tree => Branch.Tree, Index => Branch.Root);
         end if;
      end Finalize;
      
      
      -------------
      -- Iterate --
      -------------
      
      -- Forward_Iterator --
      ----------------------
      type Branch_Iterator is limited new Menu_Iterators.Forward_Iterator with
         record
            Branch: Menu_Branch;
            -- Using a standard Branch cursor ensures that the reference will
            -- be properly Finalized
         end record;
      
      overriding
      function First (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
      is
         Ref_OK     : Boolean    := False;
         First_Index: Index_Type := Null_Index;
        begin
         
         if Iterator.Branch.Root = Null_Index then
            -- This refers to the "Main_Menu", which is stored outside of the
            -- pool
            First_Index := Iterator.Branch.Tree.Main_Menu.Sub;
         else
            -- For the regular branches
            First_Index := Iterator.Branch.Tree.Pool(Iterator.Branch.Root).Sub;
            -- Theoretically, it should not be possible to obtain a
            -- Branch_Iterator with a null Tree pointer. If that did somehow
            -- happen, we will get an access violation.
         end if;
         
         if First_Index = Null_Index then
            return Null_Menu_Cursor;
         end if;
         
         Ref_OK :=  Common_Add_Ref (Tree  => Iterator.Branch.Tree,
                                    Index => First_Index);
         
         if Ref_OK then
            return Menu_Cursor'(Tree  => Iterator.Branch.Tree,
                                Index => First_Index);
         else
            return Null_Menu_Cursor;
         end if;
         
      exception
         when others =>
            if Ref_OK then
               Common_Remove_Ref (Tree => Iterator.Tree, Index => First_Index);
            end if;
            return Null_Menu_Cursor;
      end First;
      
      
      overriding
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class
      is
         Ref_OK    : Boolean    := False;
         Next_Index: Index_Type := Null_Index;
      begin
         if Iterator.Tree /= Position.Tree or else Position.Tree = null then
            -- The Cursor is less trust-worth than Iterator, so we do an
            -- explicit check here
            return Null_Menu_Cursor;
         end if;
      
         Next_Index := Position.Tree.Pool(Position.Index).Next;
         
         if Next_Index = Null_Index then
            return Null_Menu_Cursor;
         end if;
         
         Ref_OK :=  Common_Add_Ref (Tree  => Iterator.Branch.Tree,
                                    Index => Next_Index);
         
         if Ref_OK then
            return Menu_Cursor'(Tree  => Position.Tree,
                                Index => Next_Index);
         else
            return Null_Menu_Cursor;
         end if;
         
      exception
         when others =>
            if Ref_OK then
               Common_Remove_Ref (Tree => Position.Tree, Index => Next_Index);
            end if;
            return Null_Menu_Cursor;
         
      end Next;
      
      
      ----------------------------------------
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Forward_Iterator'Class
      is
      begin
         if Menu.Tree = null then
            -- Note that Menu.Root 
            return Null_Iterator_Type;
         end if;
         
         return Iterator: Branch_Iterator do
            -- Note that Iterator has now been Initialized, and assigning to
            -- the Branch component will trigger Adjust, thereby setting-up
            -- the reference count properly
            
            Iterator.Branch := Menu;
         end return;
         
      exception
         when others =>
            return Null_Iterator_Type;
      end Iterate;
      
      
      --
      -- Menu_Tree
      --
      
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
         Tree.Controller.Allocate (New_Index);
         
         if New_Index = Null_Index then
            -- No more space!
            raise Capacity_Error;
         end if;
         
         
         -- Next step is to set the correct identity for the Item, and then
         -- Activate it so that it can register new references
         declare
            procedure Activate_Item
              (Item: aliased in out GRE.Tree_Element'Class) is
            begin
               Item.State.Identity (Tree => Tree'Access,
                                    Index => New_Index);
               
               Item.State.Activate;
            end Activate_Item;
         begin
            Modify (Pool   => Tree.Pool,
                    Index  => New_Index,
                    Action => Activate_Item'Access);
         
         end;
         
         -- Attempt to add the first reference
         if not Common_Add_Ref (Tree => Tree'Access, Index => New_Index) then
            -- This should literally not be possible
            Tree.Controller.Free (New_Index);
            
            raise Program_Error with
              "Unable to create first reference of new Menu_Item.";
         end if;
         
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
              (Item: aliased in out GTE.Tree_Element'Class);
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
            
            procedure Dispatch_And_Advance
              (Item: aliased in out GTE.Tree_Element'Class) is
            begin
               -- We advance first to ensure that if Process changes the links
               -- (usually on Delete), that we can still advance anyways.
               I := Item.State.Next (I);
               Process (Item);
               
            end Advance;
            
         begin
            while I /= Null_Index loop
               Modify (Pool   => Tree.Pool,
                       Index  => I,
                       Action => Dispatch_And_Advance'Access);
            end loop;
         end Generic_Iterate_Branch;
         
         
         -- Deactivation --
         ------------------
         -- All Deactivation tools
         
         procedure Deactivate_Item
           (Item: aliased in out GTE.Tree_Element'Class)
         with Inline;
         
         procedure Deactivate_Branch is new Generic_Iterate_Branch
           (Process => Deactivate_Item)
           with Inline;
         
         
         procedure Deactivate_Item
           (Item: aliased in out GTE.Tree_Element'Class)
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
               
               raise Program_Error;
               
         end Deactivate_Item;
         
         
         -- Deletion --
         --------------
         -- All Deletion tools, follows successful Deactivation.
         
         procedure Delete_Item
           (Item: aliased in out GTE.Tree_Element'Class)
         with Inline;
         
         procedure Delete_Branch is new Generic_Iterate_Branch
           (Process => Deactivate_Item)
           with Inline;
         
         
         procedure Delete_Item
           (Item: aliased in out GTE.Tree_Element'Class)
         is
            Submenu: Index_Type := Item.State.Sub;
            -- After we Free, we can't touch Item anymore, so we will pop-off
            -- the Submenu part for our own reference now
         begin
            Free (Pool => Tree.Pool, Item => Item.State.Index);
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
         
         Modify (Pool   => Tree.Pool,
                 Index  => Cursor.Index,
                 Action => Deactivate_Item'Access);
         -- Phase 1
         
         Modify (Pool   => Tree.Pool,
                 Index  => Cursor.Index,
                 Action => Delete_Item'Access);
         -- Phase 2
         
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
               Common_Add_Ref (Tree => Cursor.Tree, Index => Cursor.Index);
            end if;
            
            raise;
      end Delete;
      
   end Generic_Menu_Tree;
end Curses.UI.Menus.Standard_Trees.Core;
