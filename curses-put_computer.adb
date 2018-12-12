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

with Ada.Exceptions; use Ada;

package body Curses.Put_Computer is
   
   Start_Column: constant Cursor_Ordinal := Write_Head.Column;
   
   First_Computation: Boolean := True;
   
   ------------------
   -- Compute_Line --
   ------------------
   function Compute_Line return Boolean is
      Remain: String renames Content (Select_Last + 1 .. Content'Last);
      -- Everything beyond the currently selected portion
      -- Notice that if Select_Last is Content'Last, this gives us a null
      -- range, which means Remain'Length = 0, which is brillient.
      
      Write :          Cursor_Position renames Write_Head;
      Extent: constant Cursor_Position := The_Surface.Extents;
      
      Space : Positive;
      -- We will always have room for at least 1 character - the 
      -- Write_Heads's position itself. 
      
   begin
      -- Before we waste too much time, see if there is any Content left.
      if Remain'Length = 0 then
         return False;
      end if;
      
      -- For computations beyond the first, we also need to (possibly) advance 
      -- Write_Head to the next line before proceeding, as well as homing the 
      -- column. For the first run, Write_Head is already set-up for us.
      
      if First_Computation then
         First_Computation := False;
         
      else
         
         case Overflow is
            when Error =>
               -- Not allowed to wrap, and infact, this is an error
               raise Cursor_Excursion with
                 "Content does not fit on the target line.";
               
            when Truncate =>
               -- Now allowed to wrap, but this is not an error, we're just
               -- not supposed to do any more
               return False;
               
            when Wrap_Truncate =>
               if Write.Row >= Extent.Row then
                  return False;
               end if;
               
            when Wrap_Error =>
               if Write.Row >= Extent.Row then
                  -- illegal
                  raise Cursor_Excursion with
                    "Content does not fit on the target Surface.";
               end if;
         end case;
         
         -- This means we are supposed to wrap, and that we can wrap.
         Write.Row := Write.Row + 1;
         
         -- Column home depends on justification
         case Justify is
            when Left =>
               Write.Column := Cursor_Ordinal'First;
               
            when Center | Right =>
               Write.Column := Start_Column;
         end case;
         
      end if;
      
      -- The Justification tells us the bounds of the line we have to work in
      -- Left  : Write -> Extent
      -- Center: 1 -> Write = Write -> Extent (whichever is smaller)
      -- Right : 1 -> Write
      
      case Justify is
         when Left =>
            Space := Positive (Extent.Column - Write.Column + 1);
            
            
         when Center =>
            declare
               Space_Left : Natural := Natural (Write.Column - 1);
               -- 1 -> Write, not including Write
               
               Space_Right: Natural := Natural (Extent.Column - Write.Column);
               -- Everything to the right of Write, not including Write
               
            begin
               -- Which one is smaller? That becomes both sides, regardless
               -- of the total space avilable, since we are writing from the
               -- center, one side being larger than the other doesn't mean 
               -- anything, except for the Right side, which can be one 
               -- character larger than the left in the case of even Content
               -- selections
               if Space_Left < Space_Right then
                  Space_Right := Space_Left;
                  
               elsif Space_Right < Space_Left then
                  Space_Left := Space_Right;
               
               end if;
               
               -- The above calculations didn't include the Write position,
               -- so now we add that back in
               
               Space := Space_Right + Space_Left + 1;
               
            end;
            
         when Right =>
            Space := Positive (Write.Column); -- 1 -> Write
            
      end case;
      
      -- We need to set Select_First and _Last to be however much of Remain
      -- that we can fit into the space available. Since Remain is a rename of
      -- a slice of Content, we can use Remain'First for the new Select_First,
      -- and then determine Select_Last, up to a maximum of Remain'Last,
      -- according to the space available, which must be at least 1.
      
      Select_First := Remain'First;
      
      if Remain'Length <= Space then
         Select_Last := Remain'Last;
         
      else
         Select_Last := Select_First + Space - 1;
         
      end if;
      
      -- Lastly, we need to compute the proper location of Write in order
      -- to actually execute the put properly, and After_Write to indicate
      -- where the Cursor should land after the write, depending on the
      -- justification and the actual selected range
      declare
         Selection_Size: constant Positive := Select_Last - Select_First + 1;
         
      begin
         After_Write.Row := Write.Row;
         -- This is true for all cases
         
         case Justify is
            when Left =>
               -- The starting position is correct in this case, only 
               -- After_Write needs to be computed
               After_Write.Column := Write.Column +
                 Cursor_Ordinal (Selection_Size);
               
            when Center =>
               -- We will bias any uneven selections to the right
               
               declare
                  Left_Size : Natural := Selection_Size / 2;
                  Right_Size: Positive 
                    := (Selection_Size / 2) + (Selection_Size rem 2);
               begin
                  After_Write.Column 
                    := Write.Column + Cursor_Ordinal (Right_Size);
                  -- Note this correctly adds an extra such that the
                  -- After_Write position will be one past the last character
                  
                  Write.Column := Cursor_Ordinal 
                    (Natural (Write.Column) - Left_Size);
                  -- Left_Size may be zero! (1/2)
               end;
               
            when Right =>
               -- The cursor should notionally move right-to-left
               if Write.Column = Cursor_Ordinal'First then
                  -- Limited, the cursor should land on the "last" character
                  After_Write.Column := Write.Column;
               else
                  After_Write.Column := Write.Column - 1;
               end if;
            
               Write.Column 
                 := Write.Column - Cursor_Ordinal (Selection_Size) + 1;
            
         end case;
         
         -- Limit if needed
         if After_Write.Column >= Extent.Column then
            After_Write.Column := Extent.Column;
         end if;
      end;
      
      -- If we got here, all is well
      return True;
      
   exception
      when Cursor_Excursion => 
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Compute_Line;
   
   
end Curses.Put_Computer;
