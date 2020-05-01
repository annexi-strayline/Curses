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

with Ada.Unchecked_Deallocation;

package body Curses.UI.Menus.Standard_Trees.Storage_Pools.Unbounded is
   
   --------------------------------
   -- Standard Pool Deallocation --
   --------------------------------
   
   procedure Free_Tag is new Ada.Unchecked_Deallocation
     (Object => Allocation_Tag,
      Name   => Allocation_Tag_Access);
   
   procedure Free_Block is new Ada.Unchecked_Deallocation
     (Object => Storage_Array,
      Name   => Secondary_Allocation);
   
   ------------------------
   -- Initialize_Subpool --
   ------------------------
   
   overriding
   function Initialize_Subpool
     (Subpool  : aliased in out Unbounded_Tree_Subpool;
      Root_Pool:         in out Standard_Trees_Root_Pool'Class)
     return not null Subpool_Handle
   is begin
      -- Initialize the Deallocation_Chain circular
      Subpool.Deallocation_Chain_Head.Next 
        := Subpool.Deallocation_Chain_Head'Access;
      Subpool.Deallocation_Chain_Head.Prev 
        := Subpool.Deallocation_Chain_Head.Next;
      
      return Tree_Subpool (Subpool).Initialize_Subpool (Root_Pool);
   end Initialize_Subpool;
   
   ----------------------
   -- Subpool_Allocate --
   ----------------------
   
   overriding
   procedure Subpool_Allocate
     (Subpool                 : in out Unbounded_Tree_Subpool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count)
   is 
      Block_Size: constant Storage_Count
        := Size_In_Storage_Elements 
          + Alignment 
          + Allocation_Tag_Access'Max_Size_In_Storage_Elements
          + Allocation_Tag_Access'Alignment;
      
      Tag: Allocation_Tag_Access 
        := new Allocation_Tag'
          (Kind      => Dynamic,
           Subpool   => Subpool'Unchecked_Access,
           Block_Ptr => new Storage_Array (1 .. Block_Size),
           Prev      => null,
           Next      => null);
      
      Block: Storage_Array renames Tag.Block_Ptr.all;
   
      Dope_Address: Address;
      
   begin
      -- Find the right alignment
      Storage_Address := Block(Block'First)'Address;
      Snap_Alignment (X => Storage_Address, Align => Alignment);
      
      -- Write out the dope
      Dope_Address := Compute_Dope_Address 
        (Storage_Address + Size_In_Storage_Elements);
      
      Write_Dope (Dope_Address => Dope_Address,
                  Tag          => Tag);
      
      
      -- Append to the deallocation chain
      declare
         Head: Allocation_Tag renames Subpool.Deallocation_Chain_Head;
      begin
         Tag.Prev      := Head.Prev;
         Tag.Next      := Tag.Prev.Next;
         Tag.Prev.Next := Tag;
         Tag.Next.Prev := Tag;
      end;
      
   exception
      when others =>
         Free_Block (Tag.Block_Ptr);
         Free_Tag   (Tag);
   end Subpool_Allocate;
   
   ------------------------
   -- Subpool_Deallocate --
   ------------------------
   
   overriding
   procedure Subpool_Deallocate (Subpool: in out Unbounded_Tree_Subpool;
                                 Tag    : in out Allocation_Tag_Access)
   is begin
      -- We need to remove the tag from the deallocation chain,
      -- and then deallocate the block and then the tag
      
      Tag.Next.Prev := Tag.Prev;
      Tag.Prev.Next := Tag.Next;
      
      Free_Block (Tag.Block_Ptr);
      Free_Tag   (Tag);   
   end Subpool_Deallocate;
   
   -------------------
   -- Subpool_Purge --
   -------------------
   
   -- Deallocate all Tags and their blocks from the Deallocation_Chain
   
   overriding 
   procedure Subpool_Purge (Subpool: in out Unbounded_Tree_Subpool) is
      Chain_Head: Allocation_Tag
        renames Subpool.Deallocation_Chain_Head;
      
      Next_Tag: Allocation_Tag_Access := Chain_Head.Next;
      This_Tag: Allocation_Tag_Access;
   begin
      
      pragma Assert (Next_Tag /= null);
      -- This should never happen if the subpool was initialized properly
      
      while Next_Tag /= Chain_Head'Unchecked_Access loop
         This_Tag := Next_Tag;
         Next_Tag := This_Tag.Next;
         Free_Block (This_Tag.Block_Ptr);
         Free_Tag (This_Tag);
      end loop;
      
   end Subpool_Purge;
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools.Unbounded;
