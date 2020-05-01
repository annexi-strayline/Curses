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

generic
   type Unit is limited private;
   Subpool_Capacity: in Positive;
package Curses.UI.Menus.Standard_Trees.Storage_Pools.Bounded is
   
   type Bounded_Tree_Subpool is new Tree_Subpool with private;
   
   overriding
   procedure Subpool_Allocate
     (Subpool                 : in out Bounded_Tree_Subpool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count);
   
   overriding
   procedure Subpool_Deallocate (Subpool: in out Bounded_Tree_Subpool;
                                 Tag    : in out Allocation_Tag_Access);
   
   overriding 
   procedure Subpool_Purge (Subpool: in out Bounded_Tree_Subpool) is null;
   -- No need to purge the bounded subpool type since all storage is physically
   -- allocated within the subpool object itself
   
private
   
   subtype Allocation_Block is Storage_Array
     (1 .. 
        (Unit'Max_Size_In_Storage_Elements * 2)
        + (Allocation_Tag_Access'Max_Size_In_Storage_Elements * 2)
        + (Allocation_Tag'Max_Size_In_Storage_Elements * 2));

   
   subtype Valid_Block_Index is Positive range 1 .. Subpool_Capacity;
   
   type Allocation_Pool is array (Valid_Block_Index) of Allocation_Block;
   
   type Bounded_Tree_Subpool is new Tree_Subpool with
      record
         Allocations: Allocation_Pool;
         Next_Fresh : Valid_Block_Index := 1;
         
         Recycle_Stack: Allocation_Tag_Access;
      end record;
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools.Bounded;
