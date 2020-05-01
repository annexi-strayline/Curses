------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

package body Curses.UI.Menus.Standard_Trees.Storage_Pools.Bounded is
   
   ----------------------
   -- Subpool_Allocate --
   ----------------------
   
   overriding
   procedure Subpool_Allocate
     (Subpool                 : in out Bounded_Tree_Subpool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count)
   is
      Tag         : Allocation_Tag_Access;
      Tag_Address : Address;
      Dope_Address: Address;
      
      Block_Index: Valid_Block_Index;
      
      Allocations  : Allocation_Pool renames Subpool.Allocations;
      Next_Fresh   : Positive        renames Subpool.Next_Fresh;
      Recycle_Stack: Allocation_Tag_Access
        renames Subpool.Recycle_Stack;
      
   begin
      -- Get a recycled block if possible, in preference to a fresh one.
      
      if Recycle_Stack /= null then
         -- For recycled blocks, the block will contain it's own allocation
         -- tag, which is what links it on the recycle stack. 
         
         -- Note the Recycle_Stack is implemented as a singly-linked list
         Tag           := Recycle_Stack;
         Recycle_Stack := Tag.Next;
         Block_Index   := Tag.Block_Index;
         
         -- We can't re-use the Tag since the allocation might not be the same
         -- size
      else
         if Next_Fresh > Allocations'Last then
            raise Storage_Error;
         end if;
         
         Block_Index := Next_Fresh;
         Next_Fresh := Next_Fresh + 1;
      end if;
      
      -- Set the Storage_Address
      Storage_Address := Allocations(Block_Index)'Address;
      Snap_Alignment (X => Storage_Address, Align => Alignment);
      
      -- Locate the dope's position next - where the deallocator will expect it
      -- according to the standard rules. The actual tag data will follow then
      -- follow the dope
      Dope_Address := Compute_Dope_Address
        (Storage_Address + Size_In_Storage_Elements);
      
      -- Position and initialize the actual tag
      Tag_Address := Dope_Address 
        + Allocation_Tag_Access'Max_Size_In_Storage_Elements;
      Snap_Alignment (X => Tag_Address, Align => Allocation_Tag'Alignment);
      
      declare
         This_Block: Allocation_Block renames Allocations(Block_Index);
      begin
         pragma Assert 
           (Tag_Address + Allocation_Tag'Max_Size_In_Storage_Elements - 1
              <= This_Block(This_Block'Last)'Address);
      end;
         
      declare
         Tag_Actual: aliased Allocation_Tag with
             Import, Convention => Ada, Address => Tag_Address;
      begin
         Tag := Tag_Actual'Unchecked_Access;
      end;
      
      Tag.all := Allocation_Tag'(Kind        => Static,
                                 Subpool     => Subpool'Unchecked_Access,
                                 Prev        => null,
                                 Next        => null,
                                 Block_Index => Block_Index);
      
      Write_Dope (Dope_Address => Dope_Address,
                  Tag          => Tag);
                                 
   end Subpool_Allocate;
   
   ------------------------
   -- Subpool_Deallocate --
   ------------------------
   
   overriding
   procedure Subpool_Deallocate (Subpool: in out Bounded_Tree_Subpool;
                                 Tag    : in out Allocation_Tag_Access)
   is begin
      -- Simply enough, we just push the tag onto the recycle stack and
      -- be done with it
      
      Tag.Prev := null;
      Tag.Next := Subpool.Recycle_Stack;
      Subpool.Recycle_Stack := Tag;
      -- As this is a stack, we don't need it to be doubly-linked
      
      Tag := null;
   end Subpool_Deallocate;
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools.Bounded;
   
