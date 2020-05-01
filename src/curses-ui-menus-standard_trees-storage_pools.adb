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

package body Curses.UI.Menus.Standard_Trees.Storage_Pools is
   
   pragma Assertion_Policy (Check);
   
   --------------------------
   -- Compute_Dope_Address --
   --------------------------
   
   -- The Dope is an access type pointing at a Tag, and is always stored at the
   -- first aligned address beyond the end of the allocation
   
   function Compute_Dope_Address (Start_Address: Address)
                                 return Address
   is begin
      return Adjusted_Address: Address := Start_Address do
         Snap_Alignment (X     => Adjusted_Address,
                         Align => Allocation_Tag_Access'Alignment);
      end return;
   end Compute_Dope_Address;
   
   ----------------
   -- Write_Dope --
   ----------------
   
   procedure Write_Dope (Dope_Address: in Address;
                         Tag         : in not null Allocation_Tag_Access)
   is 
      Tag_Dope: Allocation_Tag_Access with
          Import, Convention => Ada, Address => Dope_Address;
   begin
      Tag_Dope := Tag;
   end Write_Dope;
   
   ---------------
   -- Read_Dope --
   ---------------
   
   function Read_Dope (Dope_Address: in Address)
                      return not null Allocation_Tag_Access
   is 
      Tag_Dope: Allocation_Tag_Access with
          Import, Convention => Ada, Address => Dope_Address;
   begin
      return Tag_Dope;
   end Read_Dope;
   
   --------------------
   -- Snap_Alignment --
   --------------------
   
   procedure Snap_Alignment (X: in out Address; Align: in Storage_Count) 
   is begin
      while X mod Align > 0 loop
         X := X + 1;
      end loop;
   end Snap_Alignment;
   
   ---------------------------
   -- Allocate_From_Subpool --
   ---------------------------
   
   overriding
   procedure Allocate_From_Subpool
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count;
      Subpool                 : in     not null Subpool_Handle)
   is 
      Subpool_Actual: Tree_Subpool'Class 
        renames Tree_Subpool'Class (Subpool.all);
      
      -- It is imposssible for anything except Tree_Subpools to be
      -- associated with the Standard_Trees_Root_Pool.
   begin
      Subpool_Actual.Subpool_Allocate (Storage_Address,
                                       Size_In_Storage_Elements,
                                       Alignment);
   end Allocate_From_Subpool;
   
   
   ------------------------
   -- Deallocate_Subpool --
   ------------------------
   
   overriding
   procedure Deallocate_Subpool
     (Pool   : in out Standard_Trees_Root_Pool;
      Subpool: in out Subpool_Handle)
   is 
      Subpool_Actual: Tree_Subpool'Class 
        renames Tree_Subpool'Class (Subpool.all);

   begin
      -- Dispatch to the appropriate purge subprogram
      Subpool_Actual.Subpool_Purge;
      
      Subpool := null;
   end Deallocate_Subpool;
   
   --------------
   -- Allocate --
   --------------
   
   overriding
   procedure Allocate
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count)
   is begin
      raise Program_Error with "Allocate not supported";
   end Allocate;
   
   ----------------
   -- Deallocate --
   ----------------
   
   overriding
   procedure Deallocate
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         : in     Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count)
   is 
      Tag: Allocation_Tag_Access;
      Dope_Address: constant Address 
        := Compute_Dope_Address (Storage_Address + Size_In_Storage_Elements);
   begin
      -- We use the Tag to identify the owning subpool to which to give
      -- the tag
      
      Tag := Read_Dope (Dope_Address);
      
      declare
         Subpool_Actual: Tree_Subpool'Class
           renames Tree_Subpool'Class (Tag.Subpool.all);
      begin
         Subpool_Actual.Subpool_Deallocate (Tag);
         pragma Assert (Tag = null);
      end;
      
   end Deallocate;
   
   ------------------------
   -- Initialize_Subpool --
   ------------------------
   
   function Initialize_Subpool 
     (Subpool  : aliased in out Tree_Subpool;
      Root_Pool:         in out Standard_Trees_Root_Pool'Class)
     return not null Subpool_Handle
   is
   begin
      return New_Handle: not null Subpool_Handle := Subpool'Unchecked_Access do
         Set_Pool_Of_Subpool (Subpool => New_Handle,
                              To      => Root_Pool);
      end return;
   end Initialize_Subpool;
   
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools;
