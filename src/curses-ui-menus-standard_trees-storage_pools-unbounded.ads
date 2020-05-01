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

package Curses.UI.Menus.Standard_Trees.Storage_Pools.Unbounded is
   
   type Unbounded_Tree_Subpool is new Tree_Subpool with private;
   
   overriding
   function Initialize_Subpool
     (Subpool  : aliased in out Unbounded_Tree_Subpool;
      Root_Pool:         in out Standard_Trees_Root_Pool'Class)
     return not null Subpool_Handle;

   overriding
   procedure Subpool_Allocate
     (Subpool                 : in out Unbounded_Tree_Subpool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count);
   
   overriding
   procedure Subpool_Deallocate (Subpool: in out Unbounded_Tree_Subpool;
                                 Tag    : in out Allocation_Tag_Access);
   
   overriding 
   procedure Subpool_Purge (Subpool: in out Unbounded_Tree_Subpool);
   
private
   
   type Unbounded_Tree_Subpool is new Tree_Subpool with
      record
         Deallocation_Chain_Head: aliased Allocation_Tag (Dynamic);
         -- Acts a the sentinal node in a circular doubly-linked list
         -- of all outstanding allocations associated with this
         -- subpool. Unchecked_Deallocate_Subpool causes all items in
         -- this list to be explicitly deallocated
      end record;
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools.Unbounded;
   
