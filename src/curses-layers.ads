------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

with Ada.Iterator_Interfaces;

-- This package provides internal layered Surface management infrastructure.
--
-- Most actual use of Unchecked_Access throughout the Curses package will occur
-- within this package. However, the users of Layer_Rack objects shall act with
-- full understanding of that fact. However, such a design mostly ensures that
-- if Layer_Element items are only ever added (Prepend, Append) through
-- Initialization, and removed (Remove) through Finalization, the operation 
-- should be safe. This is a simple rule to follow.
--
-- The private nature of the package ensures that users are not able to create
-- Surface Racks outside of the Curses package. In practice, there are only two
-- uses of Surface Racks in this implementation - Screen_Rack's, one per
-- Terminal, and Window_Rack's, one per Screen object.

private package Curses.Layers is
   
   ----------
   -- Rack --
   ----------
   -- The Rack type refers to an actual Rack object, which is effectively a
   -- "stack" of Layers, which may be added, removed, or moved relative to
   -- one-another. The key feature of a Rack is that the Layer elements,
   -- which must be of Surface'Class, are not actually stored in the Rack,
   -- rather the Rack coordinates ordered iteration via a doubly-linked list
   -- whose pointers are part of the Surface type itself. Thus we avoid any
   -- use of regular "dynamic memory", while also being unbounded.
   --
   -- Furthermore, the user is free to allocated Layer_Objects with allocators
   -- in the normal way, as finalization of the object during free should cause
   -- it to be removed from the rack.
   
   
   -- Indexing and Iteration Support --
   ------------------------------------
   type Layer_Reference (Reference: not null access Surface'Class) is limited 
     null record
     with Implicit_Dereference => Reference;
   
   type Rack_Cursor (<>) is private;
   
   function Layer_Exists (Position: Rack_Cursor) return Boolean;
   
   package Rack_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor      => Rack_Cursor,
                                  Has_Element => Layer_Exists);
   
   
   type Rack is tagged limited private
     with Variable_Indexing => Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => Surface'Class;
   
   
   -- Indexing --
   --------------
   -- Cursor selection
   function Reference (The_Rack: in out Rack; 
                       Position: in     Rack_Cursor)
                      return Layer_Reference;
   
   type Direct_Layer_Select is (Top_Layer, Bottom_Layer);
   function Reference (The_Rack: in out Rack; 
                       Position: in     Direct_Layer_Select)
                      return Layer_Reference;
   -- Raises a Constraint_Error if the Rack is empty. It is recommended to
   -- first Freeze and then query via Rack.Empty.
   
   
   -- Iteration --
   ---------------
   function Iterate 
     (The_Rack: in out Rack) 
     return Rack_Iterator_Interfaces.Reversible_Iterator'Class;
   -- User-defined iteration interface
   
   procedure Iterate 
     (The_Rack: in out Rack;
      Action  : not null access procedure (The_Layer: in out Surface'Class));
   -- The Iterate procedure automatically Freezes the Rack before, and
   -- unfreezes it after the completion of the iteration. Exceptions are
   -- re-raised after unfreezing the Rack.
   
   
   -- Safety Mechanisms --
   -----------------------
   procedure Freeze     (The_Rack: in out Rack);
   procedure Thaw       (The_Rack: in out Rack);
   function  Lock_Count (The_Rack:        Rack)
                        return Natural;
   -- ** ALL RACK ITERATION MUST BE DONE WITH A FROZEN RACK **
   --    USE OF THE GENERAL ITERATION (Iterate) FUNCTION AUTOMATICALLY
   --    FREEZES AND THAWS THE RACK. 
   --
   --    USING "FOR X OF" RACK MUST BE WRAPPED WITH FREEZE AND THAW OPERATIONS.
   --
   -- Freezing the Rack disables any changes to the Rack, including
   -- registrations, removals, and appends. Such operations becomes blocking
   -- while the rack is frozen
   --
   -- Due to the nature of the dynamic-memory free model, accessing Layers
   -- on a Rack may be potentially unsafe, as a retrieved Layer may become
   -- finalized before an access if the Rack was not Frozen.
   --
   -- This is why this is a private package. Improper use could lead to
   -- program instability (erronious execution).
   --
   -- If the lock is locked Natural'Last times, calls to Freeze will block
   -- until the semaphore can take more locks
   --
   -- Calling Thaw when the lock count is zero has no effect.
   
   
   -- Registration --
   ------------------
   procedure Prepend (The_Rack   :         in out Rack;
                      The_Layer  : aliased in out Surface'Class; 
                      With_Freeze:         in     Boolean := False);
   
   procedure Append  (The_Rack   :         in out Rack;
                      The_Layer  : aliased in out Surface'Class;
                      With_Freeze:         in     Boolean := False);
   
   procedure Remove  (The_Rack   : in out Rack;
                      The_Layer  : in out Surface'Class;
                      With_Freeze: in     Boolean := False);
   
   -- Manipulation --
   ------------------
   procedure Promote   (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False);
   
   procedure Demote    (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False);
   -- Moves a layer element up or down in the overall rack.
   
   procedure Above     (The_Rack       : in out Rack;
                        Subject, Object: in out Surface'Class;
                        With_Freeze    : in     Boolean := False);
   
   procedure Below     (The_Rack       : in out Rack;
                        Subject, Object: in out Surface'Class;
                        With_Freeze    : in     Boolean := False);
   -- Places Subject above/below Object. Object is not moved.
   -- Subject and Object must both already be in the Rack.
   
   procedure Superior  (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False);
   -- Moves a layer element to the very top of the rack;
   
   procedure Inferior  (The_Rack   : in out Rack;
                        The_Layer  : in out Surface'Class;
                        With_Freeze: in     Boolean := False);
   -- Moves a layer element to the very bottom of the rack;
   
   -- If With_Freeze is True, the Rack is frozen atomically after completing
   -- the operation
   
   -- Status --
   ------------
   function Empty (The_Rack: in out Rack) return Boolean;
   -- Returns True if the Rack has no layers registered
   
   -- Hints --
   -----------
   procedure Hint_Changed (The_Rack: in out Rack; 
                           Changed :    out Boolean);
   -- Returns True if the Rack elements were changed or moved in any way
   -- since the last call to Hint_Changed. Clears the hint after such a call
   
   
private
   -------------------
   -- Rack_Iterator --
   -------------------
   type Rack_Iterator (The_Rack: not null access Rack) is 
     limited new Rack_Iterator_Interfaces.Reversible_Iterator
     with null record;
   
   -- Contractual Overrides --
   ---------------------------
   overriding function First    (Object  : Rack_Iterator) return Rack_Cursor;
   
   overriding function Next     (Object  : Rack_Iterator; 
                                 Position: Rack_Cursor) 
                                return Rack_Cursor;
   
   
   overriding function Last     (Object  : Rack_Iterator) return Rack_Cursor;
   
   overriding function Previous (Object  : Rack_Iterator; 
                                 Position: Rack_Cursor)
                                return Rack_Cursor;
   -- Constraint_Error is raised if Position is not actually associated with
   -- the Rack_Iterator
   
   ----------
   -- Rack --
   ----------
   -- The Surface_Rack Protected Type synchronizes access to a single linked-
   -- list which spans Surface objects. Typically, a Layer_Rack will be part
   -- of a single parent type to which a group of child Surfaces are
   -- associated, via access discriminants. All changes to the linked list 
   -- are managed through the Surface_Rack, which is accessible from the 
   -- children, via the access discriminant to their parent. This model 
   -- ensures efficient, thread-safe layer order management
   
   -- Sequence Type --
   -------------------
   type Sequence_Count is mod 2**32;
   
   type Sequence_Set is
      record
         Addition_Sequence: Sequence_Count;
         Removal_Sequence : Sequence_Count;
      end record;
   -- This is as a record rather than a single 64-bit modular type to ensure
   -- 1. It can be returned from a single protected function
   -- 2. Compatibility with "legacy" 32-bit systems is ensured
   
   
   -- Rack_Cursor --
   -----------------
   type Link_Pack is
      record
         Sequence: Sequence_Set;
         Link    : Surface_Access;
      end record;
   -- Allow this information to be returned in one shot from the Rack via a
   -- function call, allowing parallel reads.
   
   type Rack_Cursor (Of_Rack: not null access Rack) is
      record
         Actual: Link_Pack;
      end record;
   -- Note that the cursor needs the access discriminant to the rack to
   -- ensure that a call to "Has_Layer" confirms the Sequence against the rack
   -- to determine if the Layer does, in-fact, exist.
   
   
   -- Rack_Controller --
   ---------------------
   protected type Rack_Controller is
      
      -- Consistency Tools --
      -----------------------
      entry     Freeze;
      procedure Thaw;
      function  Lock_Count return Natural;
      -- Freezes any changes to the Rack. The Freeze lock is maintained as a
      -- semaphore, allowing for multiple parallel iterations of the Rack
      -- structure.
      --
      -- If the lock is locked Natural'Last times, calls to Freeze will block
      -- until the semaphore can take more locks
      --
      -- Calling Thaw when the lock count is zero has no effect.
      
      -- Registration --
      ------------------
      entry Prepend (The_Layer  : in out Surface'Class; 
                     With_Freeze: in     Boolean := False);
      entry Append  (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False);
      entry Remove  (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False);
      
      -- Manipulation --
      ------------------
      -- Note that all Manipulation operations do not modify the Rack
      -- Sequence. This is because the sequence is there to protect against
      -- access to Layer objects which might no longer exist. Manipulations
      -- are only a change in position on that list. One must also consider
      -- that Manipulation only works when the rack is not frozen. Iteration
      -- which would not handle changes in the table during iteration should
      -- Freeze the rack.
      
      entry Promote (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False);
      entry Demote  (The_Layer  : in out Surface'Class;
                     With_Freeze: in     Boolean := False);
      -- Moves a layer element up or down in the overall rack.
      
      entry Above   (Subject, Object: in out Surface'Class;
                     With_Freeze    : in     Boolean := False);
      
      entry Below   (Subject, Object: in out Surface'Class;
                     With_Freeze    : in     Boolean := False);
      -- Places Subject above/below Object. Object is not moved.
      
      entry Superior 
        (The_Layer  : in out Surface'Class;
         With_Freeze: in     Boolean := False);
      -- Moves a layer element to the very top of the rack;
      
      entry Inferior 
        (The_Layer  : in out Surface'Class;
         With_Freeze: in     Boolean := False);
      -- Moves a layer element to the very bottom of the rack
      
      -- If With_Freeze is True, the Rack is frozen atomically after
      -- completing the operation
      
      -- Traversal --
      ---------------
      function Head return Link_Pack;
      function Tail return Link_Pack;
      function Next (From: Link_Pack) return Link_Pack;
      function Prev (From: Link_Pack) return Link_Pack;
      -- Next and Prev both raise program errors if From's sequence does not
      -- match the current sequence
      
      -- Hints --
      -----------
      procedure Hint_Changed (Changed: out Boolean);
      -- Returns True if the Surface_Rack elements were changed or moved in
      -- any way since the last call to Hint_Changed.
      
      
      -- Sequence --
      --------------
      -- The sequence, essentially a 64-bit number that, if unchanged, 
      -- guarantees that any existing reference will be valid as long as the
      -- the sequence is not incremented. These values are used by the 
      -- Layer_Cursor type to maintain safety.
      --
      -- It should be noted that the sequence cannot ever change while the
      -- Rack is frozen
      
      function Sequence return Sequence_Set;
      -- Incremented with each Removal
      
   private
      Rack_Head   : Surface_Access  := null;
      Rack_Tail   : Surface_Access  := null;
      
      Lock        : Natural         := 0;
      The_Sequence: Sequence_Set    := (others => 0);
      
      Change_Hint : Boolean         := False;
   end Rack_Controller;
   
   
   ----------------------------------------
   type Rack is tagged limited
      record
         Actual: Rack_Controller;
      end record;
   
   
   -- Function expression overrides --
   -----------------------------------
   function Lock_Count (The_Rack: Rack) return Natural is 
     (The_Rack.Actual.Lock_Count);
   
   function Empty (The_Rack: in out Rack) return Boolean is
     (The_Rack.Actual.Head.Link = null);
   
end Curses.Layers;
