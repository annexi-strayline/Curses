------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--                         Standard Surfaces Package                        --
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

with Curses.Binding.Terminals;
with Curses.Binding.Render;

package body Curses.Terminals.Surfaces.Standard is
   
   --
   -- Official_Position Protected Type
   --
   
   protected body Official_Position is
      
      entry Propose (New_Position: in Cursor_Position)
        when not Proposal_Active
      is
      begin
         Proposal_Active := True;
         Proposed        := New_Position;
      end Propose;
      
      procedure Commit is
      begin
         Current         := Proposed;
         Proposal_Active := False;
         Was_Updated     := True;
      end Commit;
      
      procedure Abandon is
      begin
         Proposal_Active := False;
      end Abandon;
      
      function Get return Cursor_Position is (Current);
      
      procedure Updated (State: out Boolean) is
      begin
         State       := Was_Updated;
         Was_Updated := False;
      end Updated;
         
   end Official_Position;
   
   --
   -- General Utilities
   -- 
   
   ------------------
   -- De_Focus_All --
   ------------------
   -- Ensure that there are no Focused Surfaces anywhere in the Screen context.
   -- It is assumed that the Window_Rack is already Frozen.
   procedure De_Focus_All (The_Screen: in out Screen) is
      procedure Defocus_Surface (The_Surface: in out Surface'Class) is
      begin
         Rendered_Surface (The_Surface).Focus.Has_Focus (False);
      end Defocus_Surface;
      
   begin
      if The_Screen.Focus.Has_Focus then
         The_Screen.Focus.Has_Focus (False);
      end if;
      
      The_Screen.Window_Rack.Iterate (Defocus_Surface'Access);
   end De_Focus_All;
   
   
   --------------------------
   -- Decide_Queue_Refresh --
   --------------------------
   -- Invoked when a Window has been modified through this package (resize, or
   -- move in either two dimensions), and may need a terminal refresh to be
   -- queued
   procedure Decide_Queue_Refresh (The_Window: in out Window) with Inline is
   begin
      if The_Window.Parent_Screen.Visible
        and then (The_Window.Visible or else The_Window.Armed)
      then
         The_Window.TTY.Refresh.Queue_Refresh;
      end if;
   end Decide_Queue_Refresh;
   
   
   -----------------------
   -- Configure_Screens --
   -----------------------
   -- Invoked when changing a Screen's general position on the Terminal's
   -- Screen_Rack. The Screen_Rack may already be frozen going into this
   -- operation, but one does not need to make that assumption, as multiple
   -- freeze operations are also acceptable
   --
   -- The job of this subprogram is to ensure that if the top-most Screen is
   -- Armed, it is Visible, and to ensure that all other Screens on the Rack
   -- are not Visible, and are set to Armed (along with all of their Windows)
   procedure Configure_Screens (Screen_Rack: in out Rack) is
      Screen_Rack_Locked: Boolean := False;
      
      Screen_Iterator: 
        Layers.Rack_Iterator_Interfaces.Reversible_Iterator'Class
        := Screen_Rack.Iterate;
      
      This_Screen_Cursor: Rack_Cursor := Screen_Iterator.First;
   begin
      Screen_Rack.Freeze;
      Screen_Rack_Locked := True;
      
      if Screen_Rack.Empty then
         -- No Screens here
         Screen_Rack.Thaw;
         return;
      end if;
      
      -- Set-up to the top-most Screen
      This_Screen_Cursor := Screen_Iterator.First;
      -- Do this a second time, just in case the first initialization is stale
      -- after locking the Screen_Rack
      
      declare
         Top_Screen: Screen renames 
           Screen (Screen_Rack(This_Screen_Cursor).Reference.all);
      begin
         if Top_Screen.Armed then
            Top_Screen.Properties.Armed_To_Visible;
            
            -- The top-most screen has apparently just got there and is going
            -- from armed to visible, so we need to queue a refresh on its
            -- behalf
            Top_Screen.TTY.Refresh.Queue_Refresh;
         end if;
      end;
      
      -- Iterate over each remaining Screen and Arm each visible Screen and
      -- Window
      

      This_Screen_Cursor:= Screen_Iterator.Next (This_Screen_Cursor);
         
      while Layer_Exists (This_Screen_Cursor) loop
         declare
            This_Screen: Screen renames 
              Screen (Screen_Rack(This_Screen_Cursor).Reference.all);
            
            Window_Rack_Locked: Boolean := False;
         begin
            if This_Screen.Visible then
               -- This Screen should not be visible, so if it is, we move it to
               -- Armed, and also set each of it's Visible Windows (if any) to 
               -- Armed.
               This_Screen.Properties.Visible_To_Armed;
               
               -- Now we need to make sure all Visible Windows are also moved 
               -- to armed
               This_Screen.Window_Rack.Freeze;
               Window_Rack_Locked := True;
               
               for S of This_Screen.Window_Rack loop
                  if S.Visible then
                     S.Properties.Visible_To_Armed;
                  end if;
               end loop;
               
               This_Screen.Window_Rack.Thaw;
            end if;
         exception
            when others =>
               if Window_Rack_Locked then
                  This_Screen.Window_Rack.Thaw;
               end if;
               raise;
         end;
         
         This_Screen_Cursor := Screen_Iterator.Next (This_Screen_Cursor);
      end loop;
      
      Screen_Rack.Thaw;
      
   exception
      when others =>
         if Screen_Rack_Locked then
            Screen_Rack.Thaw;
         end if;
         raise;
         
   end Configure_Screens;
   
   
   --
   -- Screen Surface Implementation
   -- 
   
   ------------------
   -- Screen_Modes --
   ------------------
   protected body Screen_Modes is
      
      ------------------
      -- Focus_Config --
      ------------------
      procedure Focus_Config (Mode: in Focus_Mode) is
      begin
         Focus_Config_Value := Mode;
      end Focus_Config;
      
      function  Focus_Config return Focus_Mode is (Focus_Config_Value);
      
      
      -------------------
      -- Update_Active --
      -------------------
      procedure Update_Active (Set: in Boolean) is
      begin
         Update_Active_Value := Set;
      end Update_Active;
      
      function  Update_Active return Boolean is (Update_Active_Value);
      
   end Screen_Modes;
   
   
   ----------------
   -- Auto_Focus --
   ----------------
   procedure Auto_Focus (The_Screen: in out Screen) is
   begin
      if The_Screen.Modes.Focus_Config = Auto then
         -- Already auto!
         return;
      end if;
      
      The_Screen.Window_Rack.Freeze;
      De_Focus_All (The_Screen);
      The_Screen.Modes.Focus_Config (Auto);
      The_Screen.Window_Rack.Thaw;
      
      The_Screen.TTY.Refresh.Queue_Refresh;
   end Auto_Focus;
   
   
   ------------
   -- Resize --
   ------------
   procedure Resize (The_Screen: in out Screen) is
      New_Extents:          Cursor_Position;
      Old_Extents: constant Cursor_Position := The_Screen.Extents;
      
      package Terminals renames Curses.Binding.Terminals;
      
      procedure Do_Resize 
        (Handle   : in Surface_Handle := The_Screen.Handle;
         R_Rows   : in Cursor_Ordinal := New_Extents.Row;
         R_Columns: in Cursor_Ordinal := New_Extents.Column)
        renames Binding.Render.Resize;
      
   begin
      New_Extents := Terminals.Query_Physical_Extents (The_Screen.TTY.Line);
      
      Do_Resize;
      The_Screen.Properties.Extents (New_Extents);
      
   exception
      when Surface_Unavailable | Curses_Library =>
         The_Screen.Properties.Extents (Old_Extents);
         raise;
         
      when e: others =>
         The_Screen.Properties.Extents (Old_Extents);
         raise Curses_Library with
            "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Resize;
   
   
   ------------
   -- Update --
   ------------
   -- Update has a generally complicated job, which is broken down into
   -- components (not in any particular order)
   --
   -- 1. Query any change in the physical terminal size, and apply that change
   --    to all Visible or Armed Surfaces associated with the active Screen by
   --    way of a resize operation.
   --
   -- 2. Determine and update the Visibility of any/all Windows associataed
   --    with the Screen
   --    * Visible Windows that become hidden are toggled to Armed
   --    * Armed Windows that become visible are toggled to Visible
   --
   -- 3. Execute an optimal update, which attempts to do the minimum possible 
   --    work by handling common cases, such as the updating of a single, 
   --    fully-visible window, while ensuring that all visible surfaces are 
   --    rendered completely
   --
   -- 4. Set the Focus of the top-most Visible surface, unless focus is locked.

   function Update (The_Screen: in out Screen) return Cursor
   is
      Screen_Extents    : Cursor_Position := The_Screen.Extents;
      -- This value is often queried. We keep a local copy to avoid excessive 
      -- calls into The_Screen's properties. Screen_Extents may also be 
      -- limited when the Screen itself is Clipped by a terminal resize
      
      Window_Rack_Locked: Boolean := False; -- Window_Rack removal lock
                                            -- tracking for exception handling
      
      Redraw_All        : Boolean := False; -- All windows "from now on" need
                                            -- to be redrawn.
      
      Need_Refresh      : Boolean := False; -- We definitely need to do some 
                                            -- updates
      Rack_Modified     : Boolean;          -- the Window_Rack has changed

      Screen_Clipping   : Boolean;          -- True if the Screen's Clipped
                                            -- property changed since last 
                                            -- Update
      Need_Evaluate     : Boolean := False; -- We need to re-evaluate 
                                            -- visibilities
      
      Snapshot_Cursor: Cursor := Cursor'(Visible => False, others => <>);
      -- The cursor position passed to the Refresh_Controller. The default here
      -- is the fall-back. It would be a "homed" (1,1) hidden cursor. In
      -- theory, this shouldn't get returned, and if it does, it's likely
      -- transitory. Either way, this will be harmless
      
      
      -- Set_Snapshot_Cursor --
      -------------------------
      -- This was often repeated code in different sections of the body.
      -- Basically this is to set Snapshot_Cursor to be the Current_Cursor of
      -- the "focused" Surface (Window or Screen), we also need to compare it
      -- against the computed Cursor_Visiblity property, as well as adding 
      -- Window.Top_Left for Windows, to get the "real" location
      
      procedure Set_Snapshot_Cursor
        (The_Surface: in out Rendered_Surface'Class)
      with Inline is
      begin
         Snapshot_Cursor := Cursor (The_Surface.Current_Cursor);
            
         if not The_Surface.Cursor_Visibility then
            -- Force the cursor hidden
            Snapshot_Cursor.Visible := False;
         end if;
         
         -- If this is a Window, we need to add the Top_Left to the
         -- Snapshot_Cursor's position as well
         if The_Surface in Window'Class then
            Snapshot_Cursor.Position  
              := Snapshot_Cursor.Position                + 
                 Window (The_Surface).Top_Left           -
                 Cursor_Position'(1, 1);
         end if;
         
         -- Lastly, if the Cursor is beyond the Screen_Extents (due to
         -- clipping) it should be forced invisible.
         if Snapshot_Cursor.Position > Screen_Extents then
            Snapshot_Cursor.Visible := False;
         end if;
      end Set_Snapshot_Cursor;
        
        
      -- Evaluate_Visibility --
      -------------------------
      -- The job of this procedure is to calculate the Total_Visibility
      -- property of each Window on the Rack. This is then used to inform the
      -- actual Visibility state of the Windows (from the user's perspective)
      -- as well as the update/redraw strategy employed
      --
      -- Due to the physical terminal resizing logic, we may have some Windows
      -- which are Visible or Armed that have "Top_Left" positions beyond the
      -- Screen_Extents. Those Windows need to not be rendered
      -- (Visible->Armed). We do this by always forcing such windows to have a
      -- Total_Visibility of Hidden
      --
      -- Finally, we also determine if the Current_Cursor on each Visible 
      -- Surface is visible or hidden
      
      procedure Evaluate_Visibility with Inline is
         
         subtype Row_Coordinate is Cursor_Ordinal range
           Cursor_Ordinal'First .. Screen_Extents.Row;
         
         subtype Column_Coordinate is Cursor_Ordinal range
           Cursor_Ordinal'First .. Screen_Extents.Column;
         
         type Coverage_Map is 
           array (Row_Coordinate range <>, Column_Coordinate range <>) 
           of Boolean;
         
         Map: Coverage_Map(Row_Coordinate'Range, Column_Coordinate'Range)
           := (others => (others => False));
         -- False means nothing is occupying the position
         
         Win_TL, Win_BR: Cursor_Position;
         
      begin
         -- Note that the Window_Rack is always locked as soon as the Update 
         -- process begins in the main body.
         
         -- Starting from the top-down, we look at each Window on the screen,
         -- and accumulate a coverage map.
         for W of The_Screen.Window_Rack loop
            declare
               This_Window: Window renames Window (W);
            begin
               Win_TL := This_Window.Top_Left;
               
               if (This_Window.Visible or else This_Window.Armed) 
                 and then Win_TL <= Screen_Extents
               then
                  Win_BR := This_Window.Bottom_Right;
                  -- Deferred for efficiency
                  
                  -- Check for clipping
                  if Win_BR > Screen_Extents then
                     -- Window goes off screen, the effective extents for the
                     -- visibility computation therefore is limited to the
                     -- Screen Extents.
                     
                     -- This also means that the Window is Clipped, so we
                     -- register that fact now.
                     
                     -- The Total_Visibility is in relation to the Window's
                     -- visibility part on the screen with respect to other
                     -- Windows, therefore a Clipped Window may still have
                     -- "Full Visibility" if it's physically visible part is
                     -- not covered by other Windows.
                     
                     -- Careful not to assume that both dimensions are outside
                     -- of the Screen_Extents. Only one needs to be to register
                     -- as "greater than"
                     
                     if Win_BR.Row > Screen_Extents.Row then
                        Win_BR.Row := Screen_Extents.Row;
                     end if;
                     
                     if Win_BR.Column > Screen_Extents.Column then
                        Win_BR.Column := Screen_Extents.Column;
                     end if;

                     This_Window.Properties.Clipped (True);
                  else
                     This_Window.Properties.Clipped (False);
                  end if;
                  
                  declare
                     -- Set up subtypes for the accepted Window size, and use
                     -- it to establish a Check_Map which is the same size of
                     -- this Window
                     
                     subtype Win_Rows is 
                       Cursor_Ordinal range Win_TL.Row .. Win_BR.Row;
                     subtype Win_Columns is
                       Cursor_Ordinal range Win_TL.Column .. Win_BR.Column;
                     
                     Check_Map: 
                       Coverage_Map(Win_Rows'Range, Win_Columns'Range);
                  begin
                     
                     -- Unfortunately, multi-dimensional array slices are not
                     -- yet a thing.. Build our visibility map (Check_Map)
                     -- while also imprinting our shadow on the main Map.
                     -- For Ada 202X, this would be a great application of
                     -- parallel loops
                     for I in Win_Rows loop
                        for J in Win_Columns loop
                           Check_Map (I, J) :=  Map (I, J);
                           Map (I, J) := True;
                        end loop;
                     end loop;
                     
                     
                     if (for all K of Check_Map => K = True) then
                        -- This means the Window is totally covered by the
                        -- above Windows
                        This_Window.Total_Visibility := Hidden;
                        
                     elsif (for some K of Check_Map => K = True) then
                        -- This means the Window is partially covered
                        This_Window.Total_Visibility := Partial;
                        
                        -- We can also check if the Current_Cursor is hidden or
                        -- not
                        declare
                           Cursor_Pos: Cursor_Position 
                             := This_Window.Current_Cursor.Position;
                        begin
                           if Cursor_Pos > Win_BR   -- same as > Screen_Extents
                             
                             -- Note that this protects the next expression
                             -- from a possible Constraint_Error, since the
                             -- Cursor_Pos that is beyond the Screen_Extents
                             -- (and hence Win_BR) would result in an index out
                             -- of bounds.
                             
                             or else Map (Cursor_Pos.Row, Cursor_Pos.Column) 
                           then
                              -- Cursor is hidden
                              This_Window.Cursor_Visibility := False;
                           else
                              This_Window.Cursor_Visibility := True;
                           end if;
                        end;
                        
                     else
                        -- Totally clear, for now
                        This_Window.Total_Visibility  := Full;
                        This_Window.Cursor_Visibility := True;
                     end if;
                  end;
                  
               else
                  -- This Window is specifically "hidden" (not Visible and not 
                  -- Armed), or is totally off the screen. We also set Clipping
                  -- for any Windows which are set to Visible or Armed
                 
                  This_Window.Total_Visibility := Hidden;
                  
                  if This_Window.Visible or else This_Window.Armed then
                     -- By deduction, this means Win_TL is beyond the Screen
                     -- Extents, so this Window is hidden due to clipping
                     This_Window.Properties.Clipped (True);
                  else
                     This_Window.Properties.Clipped (False);
                  end if;
               end if;
            end;
         end loop;
         
         -- Finally check the Screen Visibility itself
         if not The_Screen.Window_Rack.Empty
           and then (for some K of Map => K = True) then
            -- Some coverage
            if (for all K of Map => K = True) then
               -- Total coverage!
               The_Screen.Total_Visibility  := Hidden;
               The_Screen.Cursor_Visibility := False;
            else
               The_Screen.Total_Visibility := Partial;
               
               -- Check the cursor
               declare
                  Cursor_Pos: Cursor_Position 
                    := The_Screen.Current_Cursor.Position;
               begin
                  if         Cursor_Pos <= Screen_Extents
                    and then Map (Cursor_Pos.Row, Cursor_Pos.Column)
                  then
                     -- Cursor is hidden
                     The_Screen.Cursor_Visibility := False;
                  else
                     The_Screen.Cursor_Visibility := True;
                  end if;
               end;
            end if;
            
         else
            The_Screen.Total_Visibility  := Full;
            
            if The_Screen.Current_Cursor.Position > Screen_Extents then
               The_Screen.Cursor_Visibility := False;
            else
               The_Screen.Cursor_Visibility := True;
            end if;
         end if;
         
      end Evaluate_Visibility;
      
   ----------------------------------------   
   begin
      if The_Screen.Modes.Update_Active
        or else not The_Screen.Visible
      then
         return Snapshot_Cursor;
      end if;
      -- If there is already an Update underway for The_Screen, or else
      -- The_Screen is not actually Visible, then there is nothing to update
      
      The_Screen.Modes.Update_Active (True);
      The_Screen.Window_Rack.Freeze;
      Window_Rack_Locked := True;
      
      -- The query of the physical terminal may result in an exception. If we 
      -- get one, the call to Extents will be canceled, and the last-known
      -- extents will not be changed, and thus we can fall-back to those last
      -- known from here-on-out. After this block, we can assume that the
      -- Screen extents are limited by the physical terminal extents for the
      -- time-being
      declare
         Terminal_Extents: Cursor_Position;
         
      begin
         Terminal_Extents 
           := Binding.Terminals.Query_Physical_Extents (The_Screen.TTY.Line);
         
         if Screen_Extents > Terminal_Extents then
            -- Limit the effective Screen_Extents to the physical terminal
            -- extents, (in either dimension) and note that the Screen is 
            -- Clipped
            if Screen_Extents.Row > Terminal_Extents.Row then
               Screen_Extents.Row := Terminal_Extents.Row;
            end if;
            
            if Screen_Extents.Column > Terminal_Extents.Column then
               Screen_Extents.Column := Terminal_Extents.Column;
            end if;
            
            The_Screen.Properties.Clipped (True);
            
            if Terminal_Extents > The_Screen.Prev_Physical then
               -- This also means that the Screen changed size in such a way 
               -- that it exposed parts of the Screen that were previously 
               -- hidden. In cases where the Screen is no-longer Clipped, a
               -- full redraw is also triggered below.
               
               Need_Refresh  := True;
               Need_Evaluate := True;
               Redraw_All := True;
               
            end if;
            
         else
            -- Screen is not Clipped
            The_Screen.Properties.Clipped (False);
         end if;
         
         The_Screen.Prev_Physical := Terminal_Extents;
            
      exception
         when others => null;
      end;
      
      -- Collect some relevant Hints from the Screen which will tell us right-
      -- away if we need to do a full, bottom-up refresh and evaluation.
      
      The_Screen.Properties.Hint_Clipping (Screen_Clipping);
      -- True if the Screen has either become un-clipped, or clipped since last
      -- Update. Either way, a full refresh and evaluate are needed.
      
      The_Screen.Window_Rack.Hint_Changed (Rack_Modified);
      -- True if Windows were added, removed, or re-ordered. This calls for a
      -- full refresh and evaluate
      
      if Screen_Clipping then
         Need_Refresh  := True;
         Need_Evaluate := True;
         Redraw_All    := True;
         
      elsif Rack_Modified then
         Need_Refresh  := True;
         Need_Evaluate := True;
         
      end if;
         
      -- Next we need to walk the entire Window_Rack, and compute the
      -- individual Update_Directives for each one. Using this information,
      -- we can also determine if we need a Refresh or Evaluate, if that has
      -- not already scheduled
      
      declare
         Geometry, Moved, Modified, Presented, Withdrawn, Armed, Cursor_Pos
           : Boolean;
         
         Redraw_Below: Boolean := False;
         
      begin
         for W of The_Screen.Window_Rack loop
            declare
               This_Window: Window renames Window (W);
            begin
               -- Collect all relevant hints
               This_Window.Properties.Hint_Extents   (Geometry);
               This_Window.Properties.Hint_Modified  (Modified);
               This_Window.Properties.Hint_Withdrawn (Withdrawn);
               This_Window.Properties.Hint_Armed     (Armed);
               This_Window.Position.Updated          (Moved);
            
               This_Window.Cursor_State.Position_Changed (Cursor_Pos);
               
               -- First decisions: 
               -- 1. do we need to refresh the screen?
               -- 2. If yes, do we need to redraw below?
               if        Modified 
                 or else Moved
                 or else Geometry 
                 or else Withdrawn 
               then
                  Need_Refresh := True;
                  
                  if Moved or else Withdrawn then
                     -- This means we need to do a full refresh from here down,
                     -- since things may have been uncovered
                     Redraw_Below := True;
                  end if;
               end if;
               
               -- Second decision, set the update directive for this window
               if Moved or else Geometry or else Armed then
                  -- When geometry changes, or the Window is freshly shown, we
                  -- need to do a full redraw and we also need to re-evaluate
                  -- Visibilities
                  This_Window.Update_Directive := Hard;
                  Need_Evaluate := True;
                  
               elsif Modified then
                  -- Only the content is modified, this means the Window is
                  -- eligible for a Soft-update, provided it had 
                  -- Full_Visibility, and the Rack didn't change (i.e. the
                  -- Windows have not changed in their "vertical" positions)
                  if (not Rack_Modified or else not Redraw_Below)
                    and then This_Window.Total_Visibility = Full
                  then
                     This_Window.Update_Directive := Soft;
                  else
                     This_Window.Update_Directive := Hard;
                  end if;
                  
               else
                  -- Either no change or withdrawn.
                  This_Window.Update_Directive := None;
               end if;
            
               -- Finally, test for Has_Focus, Limited_Visibility, with a moved
               -- Current_Cursor, which also requires a Re-evaluation to
               -- determine if the Cursor is still Visible
               if This_Window.Focus.Has_Focus
                 and then Cursor_Pos
                 and then This_Window.Total_Visibility = Partial
               then
                  Need_Evaluate := True;
               end if;
            end;
         end loop;
         
         -- Lastly, check the Screen itself. If the Screen is updated, then
         -- this means we need to go into full redraw
         The_Screen.Properties.Hint_Modified  (Modified);
         The_Screen.Properties.Hint_Presented (Presented);
         
         The_Screen.Cursor_State.Position_Changed (Cursor_Pos);
         
         -- Note that the Withdrawn hint is not relevant to Screens, as they
         -- cannot actually be hidden.
         
         -- Presented, however, indicates that a Screen has newly become the 
         -- primary Screen for the terminal, and therefore we need to trigger a
         -- full redraw
         
         if        Presented 
           or else Redraw_Below
           or else Redraw_All
         then 
            -- Modified might be true also, but it doesn't really matter, since
            -- either of these require us to go all-out anyways
            
            Redraw_All   := True;
            Need_Refresh := True;
            
            Binding.Terminals.Clear_Terminal (The_Screen.TTY.Handle);
            
            -- Do the actual redraw of the Screen right here and now
            Binding.Render.Render_Surface 
              (Handle      => The_Screen.Handle,
               Surface_TL  => (1,1),
               Physical_TL => (1,1),
               Physical_BR => Screen_Extents,
               Redraw      => True);
            
         elsif Modified then
            Need_Refresh := True;
            
            -- We can do an update only if Redraw_All is false and there are no
            -- Windows, otherwise we need to redraw.
            Binding.Render.Render_Surface 
              (Handle      => The_Screen.Handle,
               Surface_TL  => (1,1),
               Physical_TL => (1,1),
               Physical_BR => Screen_Extents,
               Redraw      => (not The_Screen.Window_Rack.Empty));
         end if;
         
         
         -- Lastly, if the Screen has focus and the cursor has moved, as well
         -- as the Screen having partial visibility, we need to evaluate for
         -- potential new visibility of the Screen's Cursor
         if The_Screen.Focus.Has_Focus
           and then Cursor_Pos
           and then The_Screen.Total_Visibility = Partial
         then
            Need_Evaluate := True;
         end if;
         
      end;
      
      
      if not Need_Refresh and not Need_Evaluate then
         -- No windows or focused-window cursors changed. We therefore just
         -- re-return the current Focused Cursor.
         
         if The_Screen.Focus.Has_Focus then
            Set_Snapshot_Cursor (The_Screen);
            
         else
            declare
               Found_Window: Boolean := False;
            begin
               -- Try to find a focused Window. If we can't find any, we force
               -- The_Screen to have Focus. This shouldn't happen.

               for W of The_Screen.Window_Rack loop
                  declare
                     This_Window: Window renames Window (W);
                  begin
                     if This_Window.Focus.Has_Focus then
                        Set_Snapshot_Cursor (This_Window);
                        Found_Window := True;
                     end if;
                  end;
               end loop;
            
               if not Found_Window then
                  The_Screen.Focus.Has_Focus (True);
                  Set_Snapshot_Cursor (The_Screen);
               end if;
            end;
         end if;
         
         The_Screen.Window_Rack.Thaw;
         Window_Rack_Locked := False;
         The_Screen.Modes.Update_Active (False);
         return Snapshot_Cursor;
      end if;
      
      -- Only if we need to
      if Need_Evaluate then
         Evaluate_Visibility;
      end if;
      
      
      -- Finally, we can do the big one. We will start at the bottom of the
      -- Rack evaluating how to refresh each surface as we go
      
      -- Getting here means that something has changed somewhere, and
      -- therefore, if Auto_Focus is enabled, this is where we would evaluate
      -- any change in focus. We will take advantage of this full bottom-up
      -- loop to clear all focuses, and then finally set it to the top-most
      -- Visible surface.

      declare
         Focus: Focus_Mode renames The_Screen.Modes.Focus_Config;
         -- Note that any user-driven change in the Focus mode will Freeze the
         -- Window_Rack, and thus will wait for any active Update to complete, 
         -- and will also block any further Updates until it Thaws the 
         -- Window_Rack. We can take advantage of this to assume the Focus mode
         -- will not change.

      begin
         
         for W of reverse The_Screen.Window_Rack loop
            declare
               This_Window: Window renames Window (W);
            begin
               if This_Window.Armed 
                 and then This_Window.Total_Visibility /= Hidden
               then
                  This_Window.Properties.Armed_To_Visible;
                  -- We need to force a Hard update of a window which has
                  -- become newly visible
                  This_Window.Update_Directive := Hard;
               end if;
               
               if This_Window.Visible then
                  if This_Window.Total_Visibility = Hidden then
                     This_Window.Properties.Visible_To_Armed;
                  else
                     if This_Window.Update_Directive = Hard then
                        Redraw_All := True;
                     end if;
                     
                     declare
                        Limit_BR: Cursor_Position := This_Window.Bottom_Right;
                     begin
                        if Limit_BR > Screen_Extents then
                           if Limit_BR.Row > Screen_Extents.Row then
                             Limit_BR.Row := Screen_Extents.Row;
                           end if;
                           
                           if Limit_BR.Column > Screen_Extents.Column then
                              Limit_BR.Column := Screen_Extents.Column;
                           end if;
                        end if;
                        
                        Binding.Render.Render_Surface
                          (Handle      => This_Window.Handle,
                           Surface_TL  => (1,1),
                           Physical_TL => This_Window.Top_Left,
                           Physical_BR => Limit_BR,
                           Redraw      => Redraw_All);
                        
                     end;
                     
                     -- So we just did an update on our first Window. The 
                     -- possibly unfortunate part here is if that Window is not
                     -- Fully visible, it will force all windows above to be 
                     -- redrawn. However, you might be surprised how often such
                     -- Windows in-fact have Full Total_Visibility. Tiled 
                     -- Windows, for example, will fully take advantage of
                     -- soft updates for the whole screen!
                     if This_Window.Total_Visibility /= Full then
                        Redraw_All := True;
                     end if;

                  end if;
               end if;
               
               if This_Window.Focus.Has_Focus then
                  if Focus = Auto then
                     -- For Auto Focus, we clear all focuses.
                     This_Window.Focus.Has_Focus (False);
                  else
                     -- Otherwise, we assume this is the focused screen, and
                     -- so we can set-up the Snapshot_Cursor, with visibility
                     Set_Snapshot_Cursor (This_Window);
                     
                  end if;
               end if;
               
            end;
         end loop;
         
         -- Finally select the top Window to set the focus for, or the Screen
         -- if Auto Focus is enabled.
         
         if Focus = Auto then
            -- Search for a candidate window
            declare
               Window_Focused: Boolean := False;
            begin
               -- Search from the top-down
               for W of The_Screen.Window_Rack loop
                  declare
                     This_Window: Window renames Window (W);
                  begin
                     if This_Window.Visible then
                        This_Window.Focus.Has_Focus (True);
                        Window_Focused := True;
                        
                        -- Also get a snapshot for the physical cursor
                        Set_Snapshot_Cursor (This_Window);
                        
                        exit;
                     end if;
                  end;
               end loop;
               
               if not Window_Focused then
                  -- The Screen itself has focus
                  The_Screen.Focus.Has_Focus (True);
                  Set_Snapshot_Cursor (The_Screen);
                  
               else
                  -- Ensure that the Screen (no-longer) has Focus
                  The_Screen.Focus.Has_Focus (False);
                  
               end if;
            end;
         end if;
         
         -- End of block renaming Focus
      end;
      
      The_Screen.Window_Rack.Thaw;
      Window_Rack_Locked := False;
      The_Screen.Modes.Update_Active (False);
      return Snapshot_Cursor;
      
   exception
      when others =>
         The_Screen.Modes.Update_Active (False);
         
         if Window_Rack_Locked then
            The_Screen.Window_Rack.Thaw;
         end if;
         
         return Snapshot_Cursor;
      
   end Update;
   
   --------------
   -- Superior --
   --------------
   overriding procedure Superior (The_Surface: in out Screen) is
      Rack_Locked: Boolean := False;
   begin
      The_Surface.TTY.Screen_Rack.Superior (The_Layer   => The_Surface,
                                            With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (The_Surface.TTY.Screen_Rack);
      The_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            The_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Superior;
   
   --------------
   -- Inferior --
   --------------
   overriding procedure Inferior (The_Surface: in out Screen) is
      Rack_Locked: Boolean := False;
   begin
      The_Surface.TTY.Screen_Rack.Inferior (The_Layer   => The_Surface,
                                            With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (The_Surface.TTY.Screen_Rack);
      The_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            The_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Inferior;
   
   
   -------------
   -- Promote --
   -------------
   overriding procedure Promote (The_Surface: in out Screen) is
      Rack_Locked: Boolean := False;
   begin
      The_Surface.TTY.Screen_Rack.Promote (The_Layer   => The_Surface,
                                           With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (The_Surface.TTY.Screen_Rack);
      The_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            The_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Promote;
   
   ------------
   -- Demote --
   ------------
   overriding procedure Demote (The_Surface: in out Screen) is
      Rack_Locked: Boolean := False;

   begin
      The_Surface.TTY.Screen_Rack.Demote (The_Layer   => The_Surface,
                                          With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (The_Surface.TTY.Screen_Rack);
      The_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            The_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Demote;
   
   
   -----------
   -- Above --
   -----------
   overriding procedure Above (Subject_Surface: in out Screen; 
                               Object_Surface : in out Screen)
   is
      Rack_Locked: Boolean := False;
   begin
      if Subject_Surface.TTY /= Object_Surface.TTY then
         -- These Screens are on different Terminals. If we proceeded, the
         -- Screen_Rack on both Terminals would be fatally corrupted.
         return;
      end if;
      
      Subject_Surface.TTY.Screen_Rack.Above
        (Subject     => Subject_Surface, 
         Object      => Object_Surface,
         With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (Subject_Surface.TTY.Screen_Rack);
      Subject_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            Subject_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Above;
   
   
   -----------
   -- Below --
   -----------
   overriding procedure Below (Subject_Surface: in out Screen;
                               Object_Surface : in out Screen)
   is
      Rack_Locked: Boolean := False;
   begin
      if Subject_Surface.TTY /= Object_Surface.TTY then
         -- These Screens are on different Terminals. If we proceeded, the
         -- Screen_Rack on both Terminals would be fatally corrupted.
         return;
      end if;
      
      Subject_Surface.TTY.Screen_Rack.Below
        (Subject     => Subject_Surface, 
         Object      => Object_Surface,
         With_Freeze => True);
      Rack_Locked := True;
      Configure_Screens (Subject_Surface.TTY.Screen_Rack);
      Subject_Surface.TTY.Screen_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            Subject_Surface.TTY.Screen_Rack.Thaw;
         end if;
   end Below;
   
   
   ----------------
   -- Lock_Focus --
   ----------------
   overriding procedure Lock_Focus (The_Surface: in out Screen) is
      The_Screen : Screen renames The_Surface;
      Rack_Locked: Boolean := False;
   begin
      The_Screen.Window_Rack.Freeze;
      Rack_Locked := True;
      The_Screen.Modes.Focus_Config (Lock);
      De_Focus_All (The_Screen);
      The_Screen.Focus.Has_Focus (True);
      The_Screen.Window_Rack.Thaw;
      
   exception
      when others =>
         if Rack_Locked then
            The_Screen.Window_Rack.Thaw;
         end if;
   end Lock_Focus;
      
   
   -------------------
   -- Release_Focus --
   -------------------
   overriding procedure Release_Focus (The_Surface: in out Screen) is
      The_Screen: Screen renames The_Surface;
   begin
      if not The_Screen.Focus.Has_Focus then
         return;
      end if;
      
      The_Screen.Auto_Focus;
   exception
      when others => null;
   end Release_Focus;
   
   
   ----------------
   -- Initialize --
   ----------------
   overriding procedure Initialize (The_Screen: in out Screen) is
      use Curses.Binding.Render;
      
      package Terminals renames Curses.Binding.Terminals;
      
      Rack_Locked: Boolean := False;
   begin
      Rendered_Surface (The_Screen).Initialize;
      
      -- As is usually the case in the beginning, the first thing we need to do
      -- is get out handle, before anyone else can see this object.
      
      -- Make sure the Terminal is actually available
      if not The_Screen.TTY.Available then
         -- new Terminal_Surface'Class objects always start with an invalidated
         -- handle
         return;
      end if;
      
      declare
         Terminal_Extents: Cursor_Position 
           := Terminals.Query_Physical_Extents (The_Screen.TTY.Line);
      begin
         -- Create a new screen-sized Surface, which occupies the entire 
         -- terminal
         The_Screen.Handle := Create_Surface
           (TTY     => The_Screen.TTY.Handle, 
            Extents => Terminal_Extents);

         -- Set the extents.
         -- This will basically guarantee that Hint_Extents will be true on the
         -- next The_Screen.Update, which is what we want since it forces a
         -- full redraw
         The_Screen.Properties.Extents (Terminal_Extents);
         
      end;
      
      -- Flag as armed so that if it finds itself at the top of the 
      -- Screen_Rack, it will becomes visible automatically
      The_Screen.Properties.Arm (True);
      
      
      -- Finally, with the screen available and set to modified, we can insert
      -- it into the bottom of the Terminal's screen rack.
      The_Screen.TTY.Screen_Rack.Append (The_Layer   => The_Screen,
                                         With_Freeze => True);
      Rack_Locked := True;
      
      -- In case this was the first Screen in the list, invoke
      -- Configure_Screens on the Terminal's Screen_Rack
      Configure_Screens (The_Screen.TTY.Screen_Rack);
      
      The_Screen.TTY.Screen_Rack.Thaw;
      
   exception
      when e: others =>
         -- If we have a valid handle, destroy the window and invalidate
         if Handle_Valid (The_Screen.Handle) then
            Destroy_Surface (The_Screen.Handle);
         end if;
         
         if Rack_Locked then
            The_Screen.TTY.Screen_Rack.Thaw;
         end if;
         
         The_Screen.Handle := Invalid_Handle;
      
   end Initialize;
   
   
   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (The_Screen: in out Screen)
   is
      use Curses.Binding.Terminals;
      use Curses.Binding.Render;
      
      Rack_Locked: Boolean := False;
   begin
      -- Note that Window objects are limited with unknown discriminants to the
      -- user. They also have access discriminants to their parent Screen. Both
      -- of these conditions make it impossible for any Screen to Finalize
      -- before all of its child Windows. Therefore we can make that assumption
      -- that the Window_Rack is empty.
      
      -- First, remove the Screen with a immediate Freeze. We need to do this
      -- to ensure that we correctly determine if we need to do an actual
      -- blanking of the terminal before we go, if there are no other eligable
      -- Screens to replace us.
      The_Screen.TTY.Screen_Rack.Remove (The_Layer   => The_Screen,
                                         With_Freeze => True);
      Rack_Locked := True;
      
      if The_Screen.Properties.Visible then
         -- This means our Surface is the currently active one.
         
         -- We check to see if the new Head is Armed, in which case we can just
         -- call for a Configure_Screens, otherwise we need to clear the
         -- terminal and let the others look after themselves. This way, the 
         -- terminal won't look frozen, but will rather be totally blank.
         -- Honestly, this should only happen when we are the only Screen, or
         -- in some error condition, since the user cannot directly control the
         -- Arming of Screens on the Terminal's Screen_Rack
         
         if The_Screen.TTY.Screen_Rack.Empty
           or else not The_Screen.TTY.Screen_Rack(Top_Layer).Armed
         then
            -- Do our own blanking
            Set_Monochrome_Background
              (Handle           => The_Screen.Handle,
               Blank_Character  => ' ',
               Reference_Cursor => Cursor'(others => <>));
            
            Clear_Surface    (The_Screen.Handle);
            
            Render_Surface   (Handle      => The_Screen.Handle,
                              Surface_TL  => (1,1),
                              Physical_TL => (1,1),
                              Physical_BR => The_Screen.Extents);
            Refresh_Terminal (TTY      => The_Screen.TTY.Handle,
                              Cursor   => Cursor_Position'
                                (others => Cursor_Ordinal'First)); 
         else
            -- Regular Configure_Screens and let it go
            Configure_Screens (The_Screen.TTY.Screen_Rack);
         end if;
      end if;
      
      -- Done with this part
      The_Screen.TTY.Screen_Rack.Thaw;
      Rack_Locked := False;
      
      Rendered_Surface (The_Screen).Finalize;
      
   exception
      when others =>
         if Rack_Locked then
            The_Screen.TTY.Screen_Rack.Thaw;
         end if;
   end Finalize;
   
   
   --
   -- Window Surface Implementation
   -- 
   
   ----------------
   -- New_Window --
   ----------------
   function  New_Window (On_Screen       : not null access Screen'Class;
                         Top_Left        : in Cursor_Position;
                         Proposed_Extents: in Cursor_Position)
                        return Window
   is
      use Curses.Binding;
      use Curses.Binding.Render;
      
   begin
      -- Go right to a build-in-place
      return The_Window: Window (Parent_Screen => On_Screen,
                                 TTY           => On_Screen.TTY)
      do
        -- Note that all Surfaces are initialized as neither Visible or Armed.
        -- The user is always responsible for 
        
        -- We *always* add ourselves to the Parent_Screen's Window_Rack, no 
        -- matter what the situation. This is reversed during finalization.
        On_Screen.Window_Rack.Prepend (The_Window);
        
        if not The_Window.Parent_Screen.Available then
           return;
        end if;
        
        declare begin
           The_Window.Handle := Create_Surface
             (TTY => The_Window.TTY.Handle,
              Extents  => Proposed_Extents);
        exception
           when others =>
              -- Create_Surface failed.
              The_Window.Handle := Invalid_Handle;
              return;
        end;
        
        -- Set the current Extents for the Surface
        The_Window.Properties.Extents (Proposed_Extents);
        
        -- Looks good, set-up the Window position and commit
        The_Window.Position.Propose (Top_Left);
        The_Window.Position.Commit;
        
      exception
         when others =>
            if Handle_Valid (The_Window.Handle) then
               Destroy_Surface (The_Window.Handle);
            end if;
            
      end return;
      
   end New_Window;
   
   ----------------------------------------
   function  New_Window (On_Screen       : not null access Screen'Class;
                         Proposed_Extents: in Cursor_Position)
                        return Window
   is
      Screen_Extents: constant Cursor_Position := On_Screen.Extents;
      Top_Left      :          Cursor_Position;
   begin
      
      -- Our basic job is to calculate the center of the Screen, and then to
      -- pass it up to the full New_Window as Top_Left
      
      -- Note that this could lead quite easily lead to to an exception, since
      -- there are a number of Proposed_Extent values that would result in
      -- a Constraint_Error in the below computations. This is generally
      -- handled by the exception handler.
      
      Top_Left.Row    := (Screen_Extents.Row / 2) - (Proposed_Extents.Row / 2);
      Top_Left.Column := (Screen_Extents.Column / 2) 
        - (Proposed_Extents.Column / 2);
      
      return New_Window (On_Screen        => On_Screen,
                         Proposed_Extents => Proposed_Extents,
                         Top_Left         => Top_Left);
      
   exception
      when others =>
         return Null_Window: Window (Parent_Screen => On_Screen,
                                     TTY           => On_Screen.TTY)
         do
           Invalidate_Handle (Null_Window.Handle);
           On_Screen.Window_Rack.Prepend (Null_Window);
         end return;
   end New_Window;
   
   
   
   --------------
   -- Top_Left --
   --------------
   function Top_Left (The_Window: in out Window) return Cursor_Position is
   begin
      if not The_Window.Available then
         raise Surface_Unavailable;
      end if;
      
      return The_Window.Position.Get;
   end Top_Left;
   
   
   ------------
   -- Resize --
   ------------
   procedure Resize (The_Window   : in out Window;
                     Rows, Columns: in     Cursor_Ordinal)
   is
      New_Extents     : Cursor_Position := (Row => Rows, Column => Columns);
      Old_Extents     : Cursor_Position := The_Window.Extents;
      New_Bottom_Right: Cursor_Position 
        := The_Window.Top_Left + New_Extents - Cursor_Position'(1,1);
      -- Top_Left counts for one row and one column
      
      procedure Set_Extents
        (The_New_Extents: in Cursor_Position := New_Extents)
        renames The_Window.Properties.Extents;
      
      procedure Limit_Cursor with Inline is
      begin
         if The_Window.Cursor_State.Get.Position > New_Extents then
            The_Window.Cursor_State.Set_Position (New_Extents);
         end if;
      end Limit_Cursor;
      
      procedure Do_Resize 
        (Handle   : in Surface_Handle := The_Window.Handle;
         R_Rows   : in Cursor_Ordinal := Rows;
         R_Columns: in Cursor_Ordinal := Columns)
        renames Binding.Render.Resize;
      
   begin
      if not The_Window.Available then
         raise Surface_Unavailable;
      end if;
      
      if New_Extents > The_Window.Extents then
         Do_Resize;
         Set_Extents;
         
      else
         Set_Extents;
         Limit_Cursor;  -- Move the cursor to extents if it is currently beyond
         Do_Resize;
         
      end if;
      
      Decide_Queue_Refresh (The_Window);
      
   exception
      when Surface_Unavailable | Cursor_Excursion | Curses_Library =>
         The_Window.Properties.Extents (Old_Extents);
         raise;
         
      when e: others =>
         The_Window.Properties.Extents (Old_Extents);
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Resize;
   
   
   ----------
   -- Move --
   ----------
   procedure Move (The_Window: in out Window;
                   Top_Left  : in     Cursor_Position)
   is
   begin
      -- "Moves" are completely Virtual. It is really just an adjustment of the
      -- Window - specific "Position" property. The Screen.Update subprogram 
      -- will handle the actual rendering details.
      
      The_Window.Position.Propose (Top_Left);
      
      if not The_Window.Available then
         raise Surface_Unavailable;
      end if;

      The_Window.Position.Commit;
      
      Decide_Queue_Refresh (The_Window);
      
   exception
      when Surface_Unavailable | Cursor_Excursion | Curses_Library =>
         The_Window.Position.Abandon;
         raise;
         
      when e: others =>
         The_Window.Position.Abandon;
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);  
   end Move;
   
   --------------
   -- Superior --
   --------------
   overriding procedure Superior (The_Surface: in out Window) is
   begin
      The_Surface.Parent_Screen.Window_Rack.Superior (The_Surface);
      Decide_Queue_Refresh (The_Surface);
   end Superior;
   
   
   --------------
   -- Inferior --
   --------------
   overriding procedure Inferior (The_Surface: in out Window) is
   begin
      The_Surface.Parent_Screen.Window_Rack.Inferior (The_Surface);
      Decide_Queue_Refresh (The_Surface);
   end Inferior;
   
   
   -------------
   -- Promote --
   -------------
   overriding procedure Promote (The_Surface: in out Window) is
   begin
      The_Surface.Parent_Screen.Window_Rack.Promote (The_Surface);
      Decide_Queue_Refresh (The_Surface);
   end Promote;
   
   
   ------------
   -- Demote --
   ------------
   overriding procedure Demote (The_Surface: in out Window) is
   begin
      The_Surface.Parent_Screen.Window_Rack.Demote (The_Surface);
      Decide_Queue_Refresh (The_Surface);
   end Demote;
   
   
   -----------
   -- Above --
   -----------
   overriding procedure Above (Subject_Surface: in out Window; 
                               Object_Surface : in out Window)
   is
   begin
      if Subject_Surface.Parent_Screen /= Object_Surface.Parent_Screen then
         -- These Windows are on different Screens. If we proceeded, the
         -- Window_Rack on both Screens would be fatally corrupted.
         return;
      end if;
      
      Subject_Surface.Parent_Screen.Window_Rack.Above
        (Subject => Subject_Surface, Object => Object_Surface);
      
      Decide_Queue_Refresh (Subject_Surface);
      
   end Above;
   
   
   -----------
   -- Below --
   -----------
   overriding procedure Below (Subject_Surface: in out Window;
                               Object_Surface : in out Window)
   is
   begin
     if Subject_Surface.Parent_Screen /= Object_Surface.Parent_Screen then
         -- These Windows are on different Screens. If we proceeded, the
         -- Window_Rack on both Screens would be fatally corrupted.
         return;
      end if;
      
      Subject_Surface.Parent_Screen.Window_Rack.Below
        (Subject => Subject_Surface, Object => Object_Surface); 
      
      Decide_Queue_Refresh (Subject_Surface);
      
   end Below;
   
   
   ----------------
   -- Lock_Focus --
   ----------------
   overriding procedure Lock_Focus (The_Surface: in out Window) is
      The_Window : Window renames The_Surface;
      Rack_Locked: Boolean := False;
   begin
      The_Window.Parent_Screen.Window_Rack.Freeze;
      Rack_Locked := True;
      The_Window.Parent_Screen.Modes.Focus_Config (Lock);
      
      if not The_Window.Focus.Has_Focus then
         De_Focus_All (Screen (The_Window.Parent_Screen.all));
         The_Window.Focus.Has_Focus (True);
      end if;
      
      The_Window.Parent_Screen.Window_Rack.Thaw;
      
      Decide_Queue_Refresh (The_Surface);
      
   exception
      when others =>
         if Rack_Locked then
            The_Window.Parent_Screen.Window_Rack.Thaw;
         end if;
   end Lock_Focus;
      
   
   -------------------
   -- Release_Focus --
   -------------------
   overriding procedure Release_Focus (The_Surface: in out Window) is
      The_Window: Window renames The_Surface;
   begin
      if not The_Window.Focus.Has_Focus then
         return;
      end if;
      
      The_Window.Parent_Screen.Auto_Focus;
      
      Decide_Queue_Refresh (The_Surface);
   exception
      when others => null;
   end Release_Focus;
   

   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (The_Window: in out Window) is
      Was_Visible: Boolean;
      Rack_Locked: Boolean := False;
   begin
      -- Note that the purpose of the Window_Rack is to allow the Terminal's
      -- Refresh task to asses the individual Visibility of each Window 
      -- associated with a Screen. Therefore, removing a Window from a Screen
      -- requires that we invoke an Update on the Screen with the Window
      -- Withdrawn, if it is Visible.
      
      -- Wait for any active Update to finish, and then prevent further Updates
      The_Window.Parent_Screen.Window_Rack.Freeze;
      Rack_Locked := True;
      
      Was_Visible := The_Window.Visible;
      -- Remember that only the Screen's Update subprogram actually sets a
      -- Window to be Visible. 
      
      if Was_Visible then
         The_Window.Properties.Withdraw;
      
         -- If this Window has locked focus, we need to set the Screen's lock
         -- mode to Auto
         if The_Window.Focus.Has_Focus
           and then The_Window.Parent_Screen.Modes.Focus_Config = Lock
         then
            The_Window.Parent_Screen.Modes.Focus_Config (Auto);
         end if;
      end if;
      
      The_Window.Parent_Screen.Window_Rack.Thaw;
      Rack_Locked := False;
      
      if Was_Visible then
         -- Now execute an Update to ensure that the newly removed Window is
         -- properly "drawn out"
         declare
            Discard: Cursor;
         begin
            Discard := The_Window.Parent_Screen.Update;
         end;
         
         -- Also register for an actual Refresh
         The_Window.TTY.Refresh.Queue_Refresh;
         
      end if;
      
      -- Now we are clear to remove it entirely
      The_Window.Parent_Screen.Window_Rack.Remove (The_Window);
      
      -- That's all for our bit.
      Rendered_Surface (The_Window).Finalize;
   exception
      when others =>
         if Rack_Locked then
            The_Window.Parent_Screen.Window_Rack.Thaw;
         end if;
   end Finalize;
   
   
end Curses.Terminals.Surfaces.Standard;
