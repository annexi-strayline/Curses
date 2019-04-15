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

-- A generic task-safe indexed set of "units" which allows for one-time 
-- allocation. This package is used primarily by Terminal.Color to track
-- "Predefined" global Color_Swatch'es and Color_Style's. This Set facilitates
-- both Predefining specific Swatch/Style indexes, and allows for the
-- allocation of yet unused Swatch/Style indexes

generic
   type Unit        is private;
   type Unit_Index  is range <>;
   
   Default_Unit: Unit;
package Curses.Indexed_Set is
   
   
   -- Supporting Types --
   ----------------------
   type List_Select is (Reserved, Registered, None); 
   
   type Unit_Container;
   type Container_Access is access all Unit_Container;
   
   type Unit_Container is
      record
         Index     : Unit_Index       := Unit_Index'First;
         List      : List_Select      := None;
         Saved_Unit: Unit             := Default_Unit;
         
         Next, Prev: Container_Access := null;
      end record;
   -- It might seem strange to make all of these types public. This is ok since
   -- Set never expose any of objects of these types directly

   type Container_Vector is array (Unit_Index) of aliased Unit_Container;

   
   ---------
   -- Set --
   ---------
   protected Set is
      
      procedure Reserve (Index  : out Unit_Index;
                         Success: out Boolean);
      -- Allocates the first available free Unit_Index that is not on the
      -- Reserved_List or Registered_List. If successful, the unit is prepended
      -- to the Reserved_List and it's Index returned
      --
      -- If unsuccessful (no free Units remain), Index is set to
      -- Unit_Index'Last, and Success is set to False.
      
      
      procedure Claim (Index  : in     Unit_Index;
                       Success:    out Boolean);
      -- Attempts to allocate exactly the requested Index. The requested index
      -- must not be reserved, claimed, or registered.
      --
      -- If successful, the unit is preprended to the Reserved_List.
      
      
      procedure Register (Index   : in     Unit_Index;
                          New_Unit: in     Unit;
                          Success :    out Boolean);
      -- Index must be on the Reserved_List.
      -- Stores New_Unit at Index in the set, and moves the Index to
      -- Registered_List
      
      function  Query (Index: Unit_Index) return Unit;
      -- Returns the Unit at Index of Set.
      
      function  Have_Registered return Boolean;
      -- Returns True if Registered_List is populated
      
      function  First_Registered return Unit_Index;
      -- Returns the Unit_Index for the first Unit on the Registered_List, if
      -- the list is populated. Otherwise, returns Unit_Index'First.
      
      function  Next_Registered (Index: Unit_Index) return Unit_Index;
      -- Returns the Unit_Index for the next Unit in the list. If Index selects
      -- the last item in the list, or an item that is not on the 
      -- Registered_List, List_Next returns Index Unit_Index'First (which 
      -- incidentally can never logically come "next"). 
      
   private
            
      The_Set         : Container_Vector;
       
      Reserved_List   : Container_Access := null;
      Registered_List : Container_Access := null;
      
      Next_Free       : Unit_Index       := Unit_Index'First;
      Filled          : Boolean          := False;
      
      -- Protected Operations --
      --------------------------
      procedure Prepend (Container: not null Container_Access);
      -- Enters the container into the selected list
      
      procedure Cut (Container: not null Container_Access);
      -- Removes the Container from it's current list.
      
      procedure Allocate (Container: out Container_Access);
      -- Attempts to allocated a free container from The_Set. Sets Container to
      -- null if none are available
      
   end Set;
   
end Curses.Indexed_Set;
