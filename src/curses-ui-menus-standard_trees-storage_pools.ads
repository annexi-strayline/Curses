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

-- Note that allocation from the Subpools defined in this package is not
-- task-safe. It is expected that allocation will only happen from inside the
-- tree controller.

with System.Storage_Elements;
with System.Storage_Pools.Subpools;

private package Curses.UI.Menus.Standard_Trees.Storage_Pools is
   
   -- Rationale --
   ---------------
   -- The goal is to have a universal implementation for Standard_Trees that
   -- uses a single codebase for both unbounded bounded menu trees. It was
   -- decided that the best way to do this is through the use of the subpool
   -- mechanims and the regular storage pool allocation mechanisms of Ada.
   --
   -- For the bounded case, this can be somewhat challenging since we wish
   -- to have a per-tree subpool capacity. If the capacity was set in some
   -- kind of bounded "root pool", this would be impossible since we won't
   -- know the total number of trees that will exist at any point in time.
   -- To deal with this, we decided for a somewhat contrived "virtual"
   -- root pool model, where physical subpool objects could be directly
   -- created by each tree object, and would directly manage their own
   -- storage.
   --
   -- The "virtual" root pool cannot itself support any allocation or subpool
   -- creation, and does not offer a default subpool
   --
   -- To simplify the implementation as much as possible, there is one
   -- explicit object "All_Trees_Root_Pool" which serves as the storage pool
   -- for the menu item access type in the standard implementation. Since the
   -- "virtual" root pool does not have any state itself, it seems natural to
   -- provide a single object of the type for all access types to specify as
   -- their storage pool. The effect is that the logic of the virutal root pool
   -- is attributed to those access types, and serves as the logic for
   -- connecting (dispatching) subpool allocation logic to the appropriate
   -- subpool type (bounded or unbounded)
   --
   -- The concept is for each subpool to be fully independent, and to
   -- therefore act like it's own full storage pool. This subpool object
   -- can then be an actual object (typically a component of the tree object),
   -- which handles all allocation state of that subpool. This allows for
   -- bounded subpools, which the subpool having it's own reservation of
   -- space at the elaboration of the subpool object itself.
   --
   -- When allocating from a subpool, the virtual "root pool" simply dispatches
   -- to the subpool's extended operations for allocation and deallocation.
   --
   -- The implemenation ensures that all subpool allocations for a given tree
   -- object remain strictly internal to that object, and thus the subpool
   -- can safely be finalized and destroied independent of all other tree
   -- objects, alleviating the root pool of any responsibility in the matter.
   
   
   use type System.Address;
   subtype Address is System.Address;
   
   Null_Address: Address renames System.Null_Address;
   
   use System.Storage_Elements;
   use System.Storage_Pools;
   use System.Storage_Pools.Subpools;
   
   
   type Standard_Trees_Root_Pool is 
     new Root_Storage_Pool_With_Subpools with null record;
   -- Note that finalization of a Standard_Trees_Root_Pool has no effect.
   -- Each subpool must be individually deallocated, and this is intended
   -- to be done during finalization of any given tree object
   
   overriding
   function Create_Subpool (Pool: in out Standard_Trees_Root_Pool)
                           return not null Subpool_Handle is 
     (raise Program_Error);
   -- Create_Subpool is not to be directly called. Rather the actual
   -- Subpool object must be created by the user, and the appropriate
   -- handle derrived from an access value to that object
   
   overriding
   procedure Allocate_From_Subpool
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count;
      Subpool                 : in     not null Subpool_Handle);
   
   overriding
   procedure Deallocate_Subpool
     (Pool   : in out Standard_Trees_Root_Pool;
      Subpool: in out Subpool_Handle);
   
   overriding
   function Default_Subpool_For_Pool 
     (Pool: in out Standard_Trees_Root_Pool)
     return not null Subpool_Handle is (raise Program_Error);
   -- All allocations from this type must be from explicit subpools only
   
   overriding
   procedure Allocate
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         :    out Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count);
   
   -- Raises Program_Error if called. Standard_Trees_Root_Pool only accepts
   -- allocation via subpools. 
   
   overriding
   procedure Deallocate
     (Pool                    : in out Standard_Trees_Root_Pool;
      Storage_Address         : in     Address;
      Size_In_Storage_Elements: in     Storage_Count;
      Alignment               : in     Storage_Count);
   
   -- Raises Program_Error if called. Deallocation should be done through
   -- the provided generic Unchecked_Deallocate_From_Subpool procedure
   
   type Tree_Subpool is tagged;
   

   
   
   -------------------------
   -- All_Trees_Root_Pool --
   -------------------------
   

   -- Allocation from this pool must be via a Subpool as declared below,
   -- either bounded or unbounded. Allocation from the default subpool
   -- results in Program_Error.
   --
   -- Bounded and Unbounded subpools operate independently, and therefore
   -- can be freely mixed
   
   ------------------------
   -- Tree_Subpool Class --
   ------------------------
   
   type Allocation_Tag_Access is private;
   type Tree_Subpool          is abstract new Root_Subpool with null record;
   
   not overriding
   function Initialize_Subpool 
     (Subpool  : aliased in out Tree_Subpool;
      Root_Pool:         in out Standard_Trees_Root_Pool'Class)
     return not null Subpool_Handle;
   -- Generates the handle for an actual Subpool, and ensures it is properly
   -- registered.
   --
   -- This must be used to initialze a handle with at least the same
   -- lifespan of the actual Subpool. Not hard to enforce given that this
   -- package is private.
   --
   -- In reality, the point is for a menu tree object to physically contain
   -- it's own subpool, especially for the unbounded kind, ensuring that it
   -- is allocated on the stack   
   
   not overriding
   procedure Subpool_Allocate (Subpool                 : in out Tree_Subpool;
                               Storage_Address         :    out Address;
                               Size_In_Storage_Elements: in     Storage_Count;
                               Alignment               : in     Storage_Count)
     is abstract;
   -- Dispatched to via Allocate_From_Subpool
   
   not overriding
   procedure Subpool_Deallocate (Subpool: in out Tree_Subpool;
                                 Tag    : in out Allocation_Tag_Access)
     is abstract;
   -- Dispatched to automatically from the root pool Deallocate, which locates
   -- the Tag
   
   not overriding 
   procedure Subpool_Purge (Subpool: in out Tree_Subpool) is abstract;
   -- Deallocate any outstanding allocations if necessary. Invoked from
   -- Deallocate_Subpool
   

private
   
   type Allocation_Tag;
   type Allocation_Tag_Access is access all Allocation_Tag;
   
   type Secondary_Allocation is access Storage_Array;
   -- All unbounded allocations are actually done via this pointer, which
   -- allocates from the Standard Pool. It needs to be in this package since
   -- it is part of Allocation_Tag, which is shared between both bounded
   -- and unbounded subpool types
   
   type Allocation_Kind is (Static, Dynamic);
   
   type Allocation_Tag (Kind: Allocation_Kind := Static) is
      record
         Subpool: Subpool_Handle;
         
         Prev: Allocation_Tag_Access;
         Next: Allocation_Tag_Access;
         
         case Kind is
            when Static =>
               Block_Index: Positive;
               -- Index into an array of arbitrary sized Storage_Arrays
               -- (size of those arrays (block size) is determined
               -- by the generic instantiation of the bounded tree subpool
               -- child package
               
            when Dynamic =>
               Block_Ptr: Secondary_Allocation;
            -- An allocated Storage_Array
         end case;
      end record;
   
   -- Allocation tags are associated with every allocation from a Tree_Subpool
   -- and endable subpool-specific deallocation
   
   -------------------------
   -- Tag Dope Management --
   -------------------------
   
   function  Compute_Dope_Address (Start_Address: Address)
                                  return Address;
   -- Given a Start_Address (typically Storage_Address + Storage_Size), compute
   -- the actual (first aligned) address of the Dope.
   
   procedure Write_Dope (Dope_Address: in Address;
                         Tag         : in not null Allocation_Tag_Access);
   -- Copies the value of Tag (the access value) to the first aligned location
   -- in memory after Start_Address, but not after Limit_Address
   
   function Read_Dope (Dope_Address: in Address)
                      return not null Allocation_Tag_Access;
   -- Returns the tag stored in the Tag Dope of an allocation from the
   -- expected location as per Write_Dope. 
   
   -----------------------
   -- Alignment Helpers --
   -----------------------
   
   procedure Snap_Alignment (X: in out Address; Align: in Storage_Count);
   -- Snaps X to aligntment of Align
   
   
end Curses.UI.Menus.Standard_Trees.Storage_Pools;
