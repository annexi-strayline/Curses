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

package body Curses.Indexed_Set is
   
   protected body Set is
      
      -------------
      -- Reserve --
      -------------
      procedure Reserve (Index  : out Unit_Index;
                         Success: out Boolean)
      is
         New_Container: Container_Access;
         
      begin
         Allocate (New_Container);
         
         if New_Container = null then
            Index   := Unit_Index'Last;
            Success := False;
            
         else
            New_Container.List := Reserved;
            Prepend (New_Container);
            
            Index   := New_Container.Index;
            Success := True;
         end if;
         
      end Reserve;
      
      
      -----------
      -- Claim --
      -----------
      procedure Claim (Index  : in     Unit_Index;
                       Success:    out Boolean)
      is
         Container: Unit_Container renames The_Set(Index);
      begin
         Success := False;
         if Container.List /= None then
            -- Illegal
            return;
         end if;
         
         -- Make sure not to interfere with future Reserve operations, which
         -- always scan forward.
         if Index = Next_Free then
            if Filled then
               -- Request for last item which was already Reserved
               Success := False;
               return;
            end if;
            
            -- We now the next free is the requested index, so we can dispatch
            -- to allocate
            declare
               Temporary: Container_Access;
            begin
               Allocate (Temporary);
               
               if Temporary = null then
                  -- This shouldn't happen
                  Success := False;
                  return;
                  
               end if;
            end;
         end if;
         -- Otherwise, we can simply take it right out of the pool, and
         -- Allocate will notice it as it scans forward            
         
         Container.List := Reserved;
         Prepend (Container'Access);
         
         Success := True;
         
      end Claim;
      
      
      --------------
      -- Register --
      --------------
      procedure Register (Index   : in     Unit_Index;
                          New_Unit: in     Unit;
                          Success :    out Boolean)
      is
         Container: Unit_Container renames The_Set(Index);
      begin
         Success := False;
         
         if Container.List /= Reserved then
            -- Illegal
            return;
         end if;
         
         -- Cut from the list
         Cut (Container'Access);
         
         -- Set-up
         Container.List       := Registered;
         Container.Saved_Unit := New_Unit;
         
         Prepend (Container'Access);
         Success := True;
         
      end Register;
      
      
      -----------
      -- Query --
      -----------
      function Query (Index: Unit_Index) return Unit is
         (The_Set(Index).Saved_Unit);
         
         
      ---------------------
      -- Have_Registered --
      ---------------------
      function Have_Registered return Boolean is
         (Registered_List /= null);
            
         
      ----------------------
      -- First_Registered --
      ----------------------
      function First_Registered return Unit_Index
      is
      begin
         -- Is there a list?
         if Registered_List = null then
            return Unit_Index'First;
         end if;
         
         return Registered_List.Index;
      end First_Registered;
      
      
      ---------------------
      -- Next_Registered --
      ---------------------
      function Next_Registered (Index: Unit_Index) return Unit_Index
      is
         Container: Unit_Container renames The_Set(Index);
      begin
         if Container.Next = null
           or else Container.List /= Registered
           or else Container.Next.List /= Registered
         then
            return Unit_Index'First;
            
         else
            return Container.Next.Index;
            
         end if;
      end Next_Registered;
         
      --
      -- Protected operations
      --
         
      -------------
      -- Prepend --
      -------------
      procedure Prepend (Container: not null Container_Access)
      is
         Dummy_List: Container_Access;
      begin
         
         case Container.List is
            when Reserved =>
               Dummy_List := Reserved_List;
               
            when Registered =>
               Dummy_List := Registered_List;
               
            when None =>
               return;
            
         end case;
         
         Container.Prev := null;
         Container.Next := null;
         
         -- Prepend to list
         if Dummy_List = null then
            Dummy_List := Container;
            
         else
            Container.Next       := Dummy_List;
            Dummy_List.Prev      := Container;
            Dummy_List           := Container;
            
         end if;
         
         -- Update the actual list
         case Container.List is
            when Reserved =>
               Reserved_List := Dummy_List;
               
            when Registered =>
               Registered_List := Dummy_List;
               
            when None =>
               -- Literally impossible
               return;
               
         end case;
      end Prepend;
      
      
      ---------
      -- Cut --
      ---------
      procedure Cut (Container: not null Container_Access)
      is
         Dummy_List: Container_Access;
      begin
         case Container.List is
            when Reserved =>
               Dummy_List := Reserved_List;
               
            when Registered =>
               Dummy_List := Registered_List;
               
            when None =>
               return;
            
         end case;
         
         
         if Container.Prev = null then
            -- Better be the list head
            if Dummy_List /= Container then
               -- This is invalid, somehow
               return;
            end if;
            
            Dummy_List := null;
               
         elsif Container.Next = null then
            -- Must be the end
            Container.Prev.Next := null;
            
         else
            -- Must be in the middle
            Container.Prev.Next := Container.Next;
            Container.Next.Prev := Container.Prev;
            
         end if;
         
         Container.List := None;
         Container.Prev := null;
         Container.Next := null;
         
      end Cut;
      
      
      --------------
      -- Allocate --
      --------------
      procedure Allocate (Container: out Container_Access) is
      begin
         if Filled then
            -- None left
            Container := null;
            return;
         end if;
         
         
         -- Select and initialize the new container
         Container     := The_Set(Next_Free)'Access;
         Container.all := (Index => Next_Free, others => <>);
         
         
         -- Increment Next_Free, if we're not at the end
         if Next_Free = Unit_Index'Last then
            Filled := True;
            
         end if;
         
         
         -- Find the next free container
         for I in Unit_Index range (Next_Free + 1) .. Unit_Index'Last loop
            if The_Set(I).List = None then
               -- Hit
               Next_Free := I;
               return;
               
            end if;
         end loop;
         
         -- Looks like there are no more left
         Next_Free := Unit_Index'Last;  -- Just for good form
         Filled    := True;
            
      end Allocate;
         
   end Set;
end Curses.Indexed_Set;
