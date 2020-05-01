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

with Ada.Assertions;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Deallocate_Subpool;

package body Curses.UI.Menus.Standard_Trees.Implementation is
   
   package body Generic_Tree is
      
      pragma Assertion_Policy (Check);
      
      procedure Assert (Check: in Boolean; Message: in String)
        renames Ada.Assertions.Assert;
      
      --
      -- Null Objects
      --
      
      -----------------
      -- Null_Cursor --
      -----------------
      
      -- Standard "Null_Cursor" that is still of type Menu_Cursor
      Null_Cursor: constant Menu_Cursor := (Standard_Cursor with 
                                            Tree => null, Item => null);
      
      ---------------
      -- No_Branch --
      ---------------
      
      -- Indicates there is no branch at a given node
      No_Branch: constant Menu_Branch := (Menu_Type with
                                          Tree => null, Root => null);
      
      
      --
      -- Menu_Item
      -- 
      
      ----------------
      -- Item_Links --
      ----------------
      
      protected body Item_Links is
         
         -----------
         -- Reset --
         -----------
         
         procedure Reset is
         begin
            Refs          := 0;
            
            Next_Ptr      := null;
            Prev_Ptr      := null;
            
            Sub_First_Ptr := null;
            Sub_Last_Ptr  := null;
            Sub_Count     := 0;
            
            Parent_Ptr    := null;
         end Reset;
         
         ------------------------
         -- Register_Reference --
         ------------------------
         
         procedure Register_Reference is
         begin
            Refs := Refs + 1;
         end Register_Reference;
         
         --------------------------
         -- Deregister_Reference --
         --------------------------
         
         procedure Deregister_Reference is
         begin
            Refs := Refs - 1;
         end Deregister_Reference;
         
         ----------------
         -- References --
         ----------------
         
         function References return Natural is (Refs);
         
         ---------------
         -- Next/Prev --
         ---------------
         
         function  Next return Item_Access is (Next_Ptr);
         function  Prev return Item_Access is (Prev_Ptr);
         
         procedure Next (Item: in Item_Access) is
         begin
            Next_Ptr := Item;
         end Next;
         
         procedure Prev (Item: in Item_Access) is
         begin
            Prev_Ptr := Item;
         end Prev;
         
         procedure Relink (Parent, Next, Prev: in Item_Access) is
         begin
            Parent_Ptr := Parent;
            Next_Ptr   := Next;
            Prev_Ptr   := Prev;
         end Relink;
         
         -------------------------
         -- Sub_First/Last/Init --
         -------------------------
         
         function  Sub_First return Item_Access is (Sub_First_Ptr);
         function  Sub_Last  return Item_Access is (Sub_Last_Ptr );
         
         procedure Sub_First (Item: in Item_Access) is
         begin
            Sub_First_Ptr := Item;
         end Sub_First;
         
         procedure Sub_Last (Item: in Item_Access) is
         begin
            Sub_Last_Ptr := Item;
         end Sub_Last;
         
         procedure Sub_Init (Item: in Item_Access) is
         begin
            Sub_First_Ptr := Item;
            Sub_Last_Ptr  := Item;
         end Sub_Init;
         
         ---------------
         -- Sub_Items --
         ---------------
         
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
         
         ------------
         -- Parent --
         ------------
         
         function  Parent return Item_Access is (Parent_Ptr);
         
         procedure Parent (Item: in Item_Access) is
         begin
            Parent_Ptr := Item;
         end Parent;
      
      end Item_Links;
      
      
      -------------
      -- Submenu --
      -------------
      
      function Submenu (Item: in out Menu_Item) return Menu_Type'Class
      is
      begin
         Item.Links.Register_Reference;
         
         return Menu_Branch'(Menu_Type with 
                             Tree => Item.Tree, Root => Item.Self);
      end Submenu;
      
      --
      -- Menu_Cursor
      --
      

      
      --------------------------
      -- Progenitor_Of_Branch --
      --------------------------

      function Progenitor_Of_Branch (Trial_Progenitor: Menu_Cursor;
                                     Trial_Descendent: Menu_Type'Class) 
                                    return Boolean
      is
         Parent_Tracer: Item_Access := Menu_Branch (Trial_Descendent).Root;
         Progen_Root  : Item_Access renames Trial_Progenitor.Item;
         
         Tree: Menu_Tree renames Menu_Tree (Trial_Progenitor.Tree.all);
         
      begin
         -- We start with the root of the descendent and work our way up until
         -- we get to an apex of some kind. If we encounter Trail_Progenitor
         -- along that path, it means Trial_Progenitor is infact a progenitor
         -- of Trial_Descendent
         
         while Parent_Tracer /= null loop
            if Parent_Tracer = Progen_Root then
               -- This means that the Trial_Progenitor
               return True;
               
            else
               Parent_Tracer := Parent_Tracer.Links.Parent;
            end if;
         end loop;
         
         return False;
      end Progenitor_Of_Branch;
      
      
      ------------
      -- Adjust --
      ------------
      
      procedure Adjust (Cursor: in out Menu_Cursor) is
      begin
         if Cursor.Item /= null then
            -- New reference, simply increment the count
            Cursor.Item.Links.Register_Reference;
         end if;
         
         Standard_Cursor(Cursor).Adjust;
      end Adjust;
      
      
      --------------
      -- Finalize --
      --------------
      
      procedure Finalize (Cursor: in out Menu_Cursor) is
      begin
         if Cursor.Item /= null then
            -- Release the reference
            Cursor.Item.Links.Deregister_Reference;
         end if;
         
         Standard_Cursor(Cursor).Finalize;
      end Finalize;
      
      
      --
      -- Menu_Branch
      --

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Branch: in out Menu_Branch) is
      begin
         if Branch.Root /= null then
            -- Menu_Branches are really just specialized cursors
            Branch.Root.Links.Register_Reference;
         end if;
         
         Menu_Type(Branch).Adjust;
      end Adjust;
      
      
      --------------
      -- Finalize --
      --------------
      
      overriding
      procedure Finalize (Branch: in out Menu_Branch) is
      begin
         if Branch.Root /= null then
            Branch.Root.Links.Deregister_Reference;
         end if;
         
         Menu_Type(Branch).Finalize;
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
      function First (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class;
      
      overriding
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class;
      
      overriding
      function Last (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class;
      
      overriding
      function Previous (Iterator: Branch_Iterator;
                         Position: Menu_Cursor_Type'Class) 
                        return Menu_Cursor_Type'Class;
      
      Null_Iterator: constant Branch_Iterator 
        := (Menu_Iterators.Reversible_Iterator with 
            Branch => No_Branch);
      
      
      -- First --
      function First (Iterator: Branch_Iterator) 
                     return Menu_Cursor_Type'Class 
      is begin
         if Iterator.Branch = No_Branch then
            return Null_Cursor;
         else
            -- Engineer a new cursor
            return C: Menu_Cursor do
               C.Tree := Iterator.Branch.Tree;
               C.Item := Iterator.Branch.Root.Links.Sub_First;
               
               if C.Item /= null then
                  C.Item.Links.Register_Reference;
               end if;
            end return;
         end if;
      end First;
      
      
      -- Last --
      function Last (Iterator: Branch_Iterator) return Menu_Cursor_Type'Class
      is begin
         if Iterator.Branch = No_Branch then
            return Null_Cursor;
         else      
            -- Engineer a new cursor
            return C: Menu_Cursor do
               C.Tree := Iterator.Branch.Tree;
               C.Item := Iterator.Branch.Root.Links.Sub_Last;
               
               if C.Item /= null then
                  C.Item.Links.Register_Reference;
               end if;
            end return;
         end if;
      end Last;
      
      
      -- Next --
      function Next (Iterator: Branch_Iterator;
                     Position: Menu_Cursor_Type'Class)
                    return Menu_Cursor_Type'Class
      is 
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         if Menu_Cursor (Position).Item = null 
           or else Iterator.Branch.Root = null
         then
            return Null_Cursor;
         else
            Assert (Check   => Iterator.Branch.Tree = Position_Actual.Tree,
                    Message => "Tree mismatch during iteration");
            
            return C: Menu_Cursor do
               C.Tree := Position_Actual.Tree;
               C.Item := Position_Actual.Item.Links.Next;
               
               if C.Item /= null then
                  C.Item.Links.Register_Reference;
               end if;
            end return;
         end if;
      end Next;
      
      -- Previous --
      overriding
      function Previous (Iterator: Branch_Iterator;
                         Position: Menu_Cursor_Type'Class) 
                        return Menu_Cursor_Type'Class
      is 
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         if Menu_Cursor (Position).Item = null 
           or else Iterator.Branch.Root = null
         then
            return Null_Cursor;
         else
            Assert (Check   => Iterator.Branch.Tree = Position_Actual.Tree,
                    Message => "Tree mismatch during iteration"); 
            
            return C: Menu_Cursor do
               C.Tree := Position_Actual.Tree;
               C.Item := Position_Actual.Item.Links.Prev;
               
               if C.Item /= null then
                  C.Item.Links.Register_Reference;
               end if;
            end return;
         end if;
      end Previous;
      
      
      ----------------------------------------
      overriding
      function  Iterate (Menu: Menu_Branch) 
                        return Menu_Iterators.Reversible_Iterator'Class
      is
      begin
         if Menu.Tree = null or else Menu.Root = null then
            return Null_Iterator;
         end if;
         
         return Iterator: Branch_Iterator do
            -- Note that Iterator has now been Initialized, and assigning to
            -- the Branch component will trigger Adjust, thereby setting-up
            -- the reference count properly
            Iterator.Branch := Menu;
         end return;
      end Iterate;
      
      
      ----------------
      -- Item_Count --
      ----------------
      
      overriding
      function  Item_Count (Menu: Menu_Branch) return Natural is
      begin
         if Menu.Root = null then
            return 0;
         else
            return Menu.Root.Links.Sub_Items;
         end if;
      end Item_Count;
      
      
      --
      -- Menu_Tree
      --
      
      ---------------------
      -- Tree_Controller --
      ---------------------
      
      -- All operations that change the links in any given tree happen within
      -- Tree_Controller. This ensures that changest to the Tree are
      -- serialized, and weird things don't happen.
      --
      -- Of course the operations must be executed in a way that is concious of
      -- iteration.
      
      protected body Tree_Controller is
         
         -------------
         -- Extract --
         -------------
         
         procedure Extract (Item: not null Item_Access) is
         begin
            
            -- First check if we are the head/tail of a submenu
            if Item.Links.Parent /= null then 
               if Item.Links.Parent.Links.Sub_First = Item then
                  Item.Links.Parent.Links.Sub_First (Item.Links.Next);
               end if;
               
               if Item.Links.Parent.Links.Sub_Last = Item then
                  Item.Links.Parent.Links.Sub_Last (Item.Links.Prev);
               end if;
               
               Item.Links.Parent.Links.Sub_Items_Decrement;
            end if;
            
            if Item.Links.Prev /= null then
               Item.Links.Prev.Links.Next (Item.Links.Next);
            end if;
            
            if Item.Links.Next /= null then
               Item.Links.Next.Links.Prev (Item.Links.Prev);
            end if;
            
            Item.Links.Relink (Parent => null, Next => null, Prev => null);
         end Extract;
         
         ------------------
         -- Allocate_New --
         ------------------
         
         procedure Allocate_Item (Item: out not null Item_Access) is
         begin
            Item := new (Our_Tree.Subpool) Menu_Item;
         end Allocate_Item;
         
         ---------------------
         -- Deallocate_Item --
         ---------------------
         
         procedure Deallocate_Item (Item: in out Item_Access) is
            procedure Free is new Ada.Unchecked_Deallocation 
              (Object => Menu_Item, Name => Item_Access);
         begin
            Extract (Item);
            
            pragma Assert (Item.Links.Sub_First = null);
            
            Free (Item);
         end Deallocate_Item;
         
         -------------------
         -- Insert_Before --
         -------------------
         
         procedure Insert_Before (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access;
                                  Before_Item: in not null Item_Access)
         is begin
            Extract (Item);
            Item.Links.Relink (Parent => Branch_Root,
                               Next   => Before_Item,
                               Prev   => Before_Item.Links.Prev);
            
            Before_Item.Links.Prev (Item);
            
            if Branch_Root.Links.Sub_First = Before_Item then
               Branch_Root.Links.Sub_First (Item);
            end if;
            
            Branch_Root.Links.Sub_Items_Increment;
         end Insert_Before;
         
         ------------------
         -- Insert_After --
         ------------------
         
         procedure Insert_After  (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access;
                                  After_Item : in not null Item_Access)
         is begin
            Extract (Item);
            Item.Links.Relink (Parent => Branch_Root, 
                               Next   => After_Item.Links.Next,
                               Prev   => After_Item);
            
            After_Item.Links.Next (Item);
            
            if Branch_Root.Links.Sub_Last = After_Item then
               Branch_Root.Links.Sub_Last (Item);
            end if;
            
            Branch_Root.Links.Sub_Items_Increment;
         end Insert_After;
         
         --------------------
         -- Branch_Prepend --
         --------------------
         
         procedure Branch_Prepend (Item       : in not null Item_Access;
                                   Branch_Root: in not null Item_Access)

         is begin
            if Branch_Root.Links.Sub_First /= null then
               -- Insert_Before does Extract (Item), and also increments
               -- the sub items
               Insert_Before (Item        => Item,
                              Branch_Root => Branch_Root,
                              Before_Item => Branch_Root.Links.Sub_First);
            else
               Extract (Item);
               Item.Links.Relink (Parent => Branch_Root,
                                  Next   => null,
                                  Prev   => null);
               Branch_Root.Links.Sub_Init (Item);
               Branch_Root.Links.Sub_Items_Increment;
            end if;
         end Branch_Prepend;
         
         -------------------
         -- Branch_Append --
         -------------------
         
         procedure Branch_Append (Item       : in not null Item_Access;
                                  Branch_Root: in not null Item_Access)
         is begin
            if Branch_Root.Links.Sub_Last /= null then
               -- Insert_After does Extract (Item), and also increments
               -- the sub items
               Insert_After (Item        => Item,
                             Branch_Root => Branch_Root,
                             After_Item  => Branch_Root.Links.Sub_Last);
            else
               Extract (Item);
               Item.Links.Relink (Parent => Branch_Root,
                                  Next   => null,
                                  Prev   => null);
               Branch_Root.Links.Sub_Init (Item);
               Branch_Root.Links.Sub_Items_Increment;
            end if;
         end Branch_Append;
      
      end Tree_Controller;
      
      
      --------------
      -- New_Item --
      --------------
      
      overriding
      function New_Item (Tree: aliased in out Menu_Tree)
                        return Standard_Cursor'Class
      is begin
         return NIC: Menu_Cursor do
            Tree.Controller.Allocate_Item (NIC.Item);
            NIC.Tree := Tree'Unchecked_Access;
            NIC.Item.Tree := NIC.Tree;
            NIC.Item.Self := NIC.Item;
            NIC.Item.Links.Register_Reference;
         end return;
      end New_Item;
      
      
      ------------
      -- Delete --
      ------------
      
      overriding
      procedure Delete (Tree    : in out Menu_Tree;
                        Position: in out Standard_Cursor'Class;
                        Deleted :    out Boolean)
      is
         Mark    : Menu_Cursor renames Menu_Cursor (Position);

         procedure Delete_Submenu is
            Sub_Next: Item_Access := Mark.Item.Links.Sub_First;
         begin
            -- We must iterate manually over the Submenu since we will
            -- need to remember "Next" when we destory the current item
            -- In order to actually call Delete recursively, we'll need
            -- to enginer a cursor each time.
            
            while Sub_Next /= null loop
               declare
                  pragma Assertion_Policy (Check);
                  Temp_Cursor: Menu_Cursor;
               begin
                  Temp_Cursor.Tree := Mark.Tree;
                  Temp_Cursor.Item := Sub_Next;
                  Temp_Cursor.Item.Links.Register_Reference;
                  Sub_Next := Temp_Cursor.Item.Links.Next;
                  Tree.Delete (Position => Temp_Cursor, Deleted => Deleted);
                  
                  if not Deleted then return; end if;
                  
                  pragma Assert (Temp_Cursor = Null_Cursor);
               exception
                  when others =>
                     Temp_Cursor := Null_Cursor;
                     raise;
               end;
            end loop;
            
            Deleted := True;
         end Delete_Submenu;
         
      begin
         if Mark.Item = null or else Mark.Item.Links.References > 1 then
            Deleted := False;
            return;
         end if;
         
         Delete_Submenu;
         if not Deleted then return; end if;
         
         -- Reference count is 1, so we know we have the only cursor, and the
         -- submenu (if any) has been deleted
         
         Tree.Controller.Deallocate_Item (Mark.Item);
         Mark := Null_Cursor;
         
      end Delete;
      
      
      ------------
      -- Append --
      ------------
      
      overriding
      procedure Append (Tree    : in out Menu_Tree;
                        Branch  : in out Menu_Type'Class;
                        Position: in out Standard_Cursor'Class)
      is 
         Branch_Actual  : Menu_Branch renames Menu_Branch (Branch);
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         Assert (Check   => Branch_Actual.Tree = Tree'Unchecked_Access,
                 Message => "Branch does not belong to Tree");
         
         Tree.Controller.Branch_Append (Item        => Position_Actual.Item,
                                        Branch_Root => Branch_Actual.Root);
      end Append;
      
      
      -------------
      -- Prepend --
      -------------
      
      overriding
      procedure Prepend (Tree    : in out Menu_Tree;
                         Branch  : in out Menu_Type'Class;
                         Position: in out Standard_Cursor'Class)
      is
         Branch_Actual  : Menu_Branch renames Menu_Branch (Branch);
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         -- The preconditions mean that Tree is valid, and that Position is a
         -- Cursor on it, but we also need to check that Branch belongs to Tree
         Assert (Check   => Branch_Actual.Tree = Tree'Unchecked_Access,
                 Message => "Branch does not belong to Tree");
         
         Tree.Controller.Branch_Prepend (Item        => Position_Actual.Item,
                                         Branch_Root => Branch_Actual.Root);
      end Prepend;
      
      
      -------------------
      -- Insert_Before --
      -------------------
      
      procedure Insert_Before (Tree    : in out Menu_Tree;
                               Branch  : in out Menu_Type'Class;
                               Before  : in     Standard_Cursor'Class;
                               Position: in out Standard_Cursor'Class)
      is
         Branch_Actual  : Menu_Branch renames Menu_Branch (Branch);
         Before_Actual  : Menu_Cursor renames Menu_Cursor (Before);
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         -- Note that the preconditions checking that Before is both
         -- On_Tree (Tree) and On_Branch (Branch) implies that Branch
         -- is also on Tree
         
         Tree.Controller.Insert_Before (Item        => Position_Actual.Item,
                                        Branch_Root => Branch_Actual.Root,
                                        Before_Item => Before_Actual.Item);
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
         Branch_Actual  : Menu_Branch renames Menu_Branch (Branch);
         After_Actual   : Menu_Cursor renames Menu_Cursor (After);
         Position_Actual: Menu_Cursor renames Menu_Cursor (Position);
      begin
         Tree.Controller.Insert_After (Item        => Position_Actual.Item,
                                       Branch_Root => Branch_Actual.Root,
                                       After_Item  => After_Actual.Item);
      end Insert_After;
      
      
      ----------------
      -- Initialize --
      ----------------
      
      overriding
      procedure Initialize (Tree: in out Menu_Tree) is
      begin
         Standard_Tree(Tree).Initialize;
         Tree.Subpool 
           := Tree.Subpool_Actual.Initialize_Subpool (All_Trees_Root_Pool);
      end Initialize;
      
      --------------
      -- Finalize --
      --------------
      
      overriding
      procedure Finalize (Tree: in out Menu_Tree) is
      begin
         Ada.Unchecked_Deallocate_Subpool (Tree.Subpool);
         Standard_Tree(Tree).Finalize;
      end Finalize;
      
   end Generic_Tree;
      
end Curses.UI.Menus.Standard_Trees.Implementation;
