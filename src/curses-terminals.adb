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
with Curses.Layers;
with Curses.Terminals.Color;
with Curses.Terminals.Surfaces.Standard;

package body Curses.Terminals is
   

   --
   -- Exclusive Access Units
   --
   
   --------------------------
   -- TTY_Exclusive_Status --
   --------------------------
   protected body TTY_Exclusive_Status is
      
      --
      -- Status Flags
      --
      
      --------------
      -- Activate --
      --------------
      procedure Activate is
      begin
         Is_Available := True;
      end Activate;
      
      ----------------
      -- Deactivate --
      ----------------
      procedure Deactivate is
      begin
         Is_Available := False;
      end Deactivate;
      
      ---------------
      -- Available --
      ---------------
      function Available return Boolean is (Is_Available);
      
      
      --------------------------
      -- Note_Palette_Limited --
      --------------------------
      procedure Note_Palette_Limited is
      begin
         Palette_Limit := True;
      end Note_Palette_Limited;
      
      
      ---------------------
      -- Palette_Limited --
      ---------------------
      function Palette_Limited return Boolean is (Palette_Limit);
         
         
      ---------------------------
      -- Note_Palette_Degraded --
      ---------------------------
      procedure Note_Palette_Degraded is
      begin
         Palette_Degrade := True;
      end Note_Palette_Degraded;
      
      
      ----------------------
      -- Palette_Degraded --
      ----------------------
      function Palette_Degraded return Boolean is (Palette_Degrade);
         
      --
      -- Physical Cursor
      --
         
      ---------------------
      -- Can_Hide_Cursor --
      ---------------------
      procedure Can_Hide_Cursor (Set: in Boolean) is
      begin
         Cursor_Can_Hide := Set;
      end Can_Hide_Cursor;
      
      ----------------------------------------
      function Can_Hide_Cursor return Boolean is (Cursor_Can_Hide);
      
         
      -----------------
      -- Cursor_Park --
      -----------------
      procedure Cursor_Park (Set: in Cursor_Park_Select) is
      begin
         Cursor_Park_Set := Set;
      end Cursor_Park;
      
      ----------------------------------------
      function Cursor_Park return Cursor_Park_Select is (Cursor_Park_Set);
      
      
      -----------------
      -- Cursor_Mode --
      -----------------
      procedure Cursor_Mode (Set: in Cursor_Mode_Select) is
      begin
         Cursor_Mode_Set := Set;
      end Cursor_Mode;
      
      ----------------------------------------
      function  Cursor_Mode return Cursor_Mode_Select is (Cursor_Mode_Set);
      
      
      --
      -- Error Message 
      -- 
         
      ------------------
      -- Report_Error --
      ------------------
      procedure Report_Error (Message: in Library_Error_Message) is
      begin
         Error_Message := Message;
         Is_Available  := False;
      end Report_Error;
      
      
      -----------------
      -- Clear_Error --
      -----------------
      procedure Clear_Error is
      begin
         Error_Message := (others => ' ');
      end Clear_Error;
      
      
      -----------
      -- Error --
      -----------
      function Error return Library_Error_Message is (Error_Message);
      
   end TTY_Exclusive_Status;
   
   
   
   ---------------------------
   -- TTY_Executive_Liaison --
   ---------------------------
   task body TTY_Executive_Liaison is
   begin
      loop
         select
            accept Assign (Order: in out Tasking_Order'Class) do
               Order.Execute;
               
            exception
               -- Catch any raised exception here. We quash it, but it will
               -- still be send back to the caller
               when others => null;
                  
            end Assign;
         or
            terminate;
            
         end select;
      end loop;
   end TTY_Executive_Liaison;
   
   
   ----------------------------
   -- TTY_Refresh_Controller --
   ----------------------------
   task body TTY_Refresh_Controller is
      Queued    : Boolean := False;
      Exec_Go   : Boolean := False;
   begin
      loop
         -- Collect requests from everyone without keeping them
         -- waiting. We will only go the Executive when it reports
         -- to us that it isn't busy.
         loop
            select 
               accept Queue_Refresh;
               Queued := True;
               
            or
               accept Executive_Ready;
               Exec_Go := True;
               
            or
               terminate;
            end select;
            
            exit when Exec_Go;
         end loop;
         
         -- Drain the queues completely
         loop
            select
               accept Queue_Refresh;
               Queued := True;
               
            else
               exit;
            end select;
         end loop;
         
         
         if Queued then
            TTY.Refresh_Exec.Execute;
            Queued  := False;
            Exec_Go := False;
         end if;

      end loop;
   end TTY_Refresh_Controller;
   
   
   ---------------------------
   -- TTY_Refresh_Executive --
   ---------------------------
   task body TTY_Refresh_Executive is
      use Curses.Terminals.Surfaces.Standard;
      use Binding.Terminals;
      
      Rack_Locked: Boolean := False;
      Physical_Cursor: Cursor;
      
      Screen_Rack: Rack renames TTY.Screen_Rack;
      
      procedure Park_Cursor with Inline is
      begin
         Physical_Cursor := Cursor (Screen_Rack(Top_Layer).Current_Cursor);
         
         if TTY.Status.Cursor_Park = Screen_Extent then
            Physical_Cursor.Position := Screen_Rack(Top_Layer).Extents;
         end if;
         
         Physical_Cursor.Visible := True;
      end Park_Cursor;
      
   begin
      TTY.Refresh.Executive_Ready;
      
      loop
         select
            accept Execute;
         or
            terminate;
         end select;
         
         if TTY.Status.Available then
            begin
               -- Ensure the Screen_Rack doesn't change while we work
               Screen_Rack.Freeze;
               Rack_Locked := True;
               
               -- In case an exception happens, we can make sure it gets
               -- unlocked
               
               -- We keep our job as simple as possible - it is simply to 
               -- take the top-most Screen in the Screen_Rack, and invoke
               -- Update on it, if it exists and is Visible. Everything else
               -- is handled by the package which defines Screen - our child
               -- package .Surfaces.Standard
               
               if not Screen_Rack.Empty
                 and then Screen_Rack(Top_Layer).Visible
               then
                  Screen'Class(Screen_Rack(Top_Layer).Reference.all).Update
                    (Physical_Cursor);
                  -- Update is to return us a suggested Physical Cursor which
                  -- assumed the terminal is in Follow_Focus mode, and is
                  -- able to hide the cursor. In any other context, we will
                  -- need to modify the Physical_Cursor before we set it
                  
                  case TTY.Status.Cursor_Mode is
                     when Follow_Focus =>
                        -- This is correct.
                        null;
                        
                     when Always_Parked =>
                        Park_Cursor;
                        
                     when Always_Hidden =>
                        Physical_Cursor.Visible := False;
                        
                  end case;
                  
                  -- Deal with the visibility
                  if TTY.Status.Can_Hide_Cursor then
                     if Physical_Cursor.Visible then
                        Show_Cursor (TTY.Handle);
                     else
                        Hide_Cursor (TTY.Handle);
                     end if;
                     
                  elsif not Physical_Cursor.Visible then
                     -- When it should be hidden but we can't, we Park it
                     Park_Cursor;
                  end if;
                  
                  Refresh_Terminal (TTY    => TTY.Handle,
                                    Cursor => Physical_Cursor.Position);

               end if;
               
               Screen_Rack.Thaw;
               Rack_Locked := False;
               
            exception
               when others =>
                  if Rack_Locked then
                     Screen_Rack.Thaw;
                  end if;
                  
            end;
         end if;
         
         TTY.Refresh.Executive_Ready;
         
      end loop;
   end TTY_Refresh_Executive;
   
   
   --
   -- Terminal Implementation
   --
   
   -------------------------
   -- Clear_Error_Message --
   -------------------------
   procedure Clear_Error_Message (TTY: in out Terminal)
   is
   begin
      TTY.Status.Clear_Error;
   end Clear_Error_Message;
   
   
   -----------------
   -- Cursor_Park --
   -----------------
   procedure Cursor_Park (TTY: in out Terminal; Park: in Cursor_Park_Select) is
   begin
      TTY.Status.Cursor_Park (Park);
   end Cursor_Park;
   
   
   -----------------
   -- Cursor_Mode --
   -----------------
   procedure Cursor_Mode (TTY: in out Terminal; Mode: in Cursor_Mode_Select) is
   begin
      TTY.Status.Cursor_Mode (Mode);
   end Cursor_Mode;
   
   
   ------------
   -- Attach --
   ------------
   procedure Attach (TTY  : in out Terminal;
                     Model: in     String := "")
   is
      use Binding.Terminals;
      use Binding.Color;
      
      Error: Library_Error_Message := (others => ' ');
      Line_Go, Terminal_Go: Boolean := False;
   begin
      if TTY.Available then
         raise Line_Depend with "Terminal already Attached.";
      end if;
         
      -- Initialization of the terminal happens in four steps:
      -- 1. Request a Line from the Line_Device, and then request
      -- 2. Incorporate the terminal on the line (put (n)curses in control)
      -- 3. Activate the Refresh_Manager task
      -- 4. Install the global user palette (if any) onto the terminal,
      --    and also evaluate the palette condition

      
      TTY.Device.Request_Line (Handle => TTY.Line,
                               Error  => Error);
      
      if not Handle_Valid (TTY.Line) then
         -- Can't get a line. Report the error, and give up
         TTY.Status.Report_Error (Error);
         raise Line_Error with "No lines available from the Line_Device. " &
           "See error message in the Terminal error message buffer.";
      end if;
      
      Line_Go := True;
      
      -- Attempt to initialize the terminal!
      -- We wrap this in a block in the very unlikely case we somehow get an 
      -- exception while trying to bring the terminal up. That way we can do our
      -- duty to free the Line_Handle.
      
      Incorporate_Terminal (Line   => TTY.Line,
                            Handle => TTY.Handle,
                            Model  => Model,
                            Error  => Error);
      -- Note that this is save vis-a-vis the Refresh_Executor (which has been
      -- active this whole time), the executor only executes a Refresh when the
      -- Terminal's Status is Available, which it isn't, or else we wouldn't be
      -- here!
      
      if not Handle_Valid (TTY.Handle) then
         -- Can't initialize the terminal. Also game-over, but we need to 
         -- release the Line first
         TTY.Device.Release_Line (TTY.Line);
         Line_Go := False;
         TTY.Status.Report_Error (Error);
         raise Line_Error with "Could not initialize the physical " &
           "Terminal. See error message in the Terminal message buffer.";
         
      else
         Terminal_Go := True;
         
      end if;
      
      -- Try to hide the Cursor, and set Can_Hide_Cursor appropriately
      if Hide_Cursor (TTY.Handle) then
         TTY.Status.Can_Hide_Cursor (True);
      else
         TTY.Status.Can_Hide_Cursor (False);
      end if;
         
      -- Try to initialize colors. If it worked it worked, it worked, the rest 
      -- is handled by the color-specific subprograms which we invoke, but 
      -- otherwise are not part of
      
      Initialize_Colors (TTY              => TTY.Handle,
                         Color_Capable    => TTY.Color_Capable,
                         Color_Changeable => TTY.Color_Changeable,
                         Default_Color    => TTY.Default_Color);
      
      -- Note that the color capabilities are not part of the Status protected 
      -- type of the Terminal object. This is because this is a property of
      -- the Terminal object (like the handle), and cannot change. Like the
      -- handle, it cannot be considered valid if the Terminal is not in an
      -- "Available" state. (Status.Available = True).
      
      -- if the capability is there, let's query the maximum colors and
      -- styles
      if TTY.Color_Capable then
         TTY.Max_Swatches := Maximum_Colors (TTY.Handle);
         TTY.Max_Styles   := Maximum_Pairs  (TTY.Handle);
      end if;
      
      -- Clear the terminal to get off to a fresh start
      Clear_Terminal (TTY.Handle);
      
      -- And we are all systems go.
      TTY.Status.Activate;
      
      -- Attempt to install the Predefined palette items, if possible
      Color.Install_Palette (TTY);
      
   exception
      when Line_Error | Line_Depend | Curses_Library =>
         raise;
      
      when e: others =>
         TTY.Status.Deactivate;
         
         if Terminal_Go then
            Disban_Terminal (TTY.Handle);
         end if;
         
         if Line_Go then
            TTY.Device.Release_Line (TTY.Line);
         end if;
         
         raise Curses_Library with
           "Unexpected exception in Terminal Attachment: " &
           Exceptions.Exception_Information (e);
   end Attach;
   
   
   ------------
   -- Detach --
   ------------
   procedure Detach (TTY: in out Terminal) is
      use Binding.Terminals;
      Rack_Locked: Boolean := False;
   begin
      if not TTY.Available then
         -- Nothing to do here
         return;
      end if;
      
      -- First, we need to ensure that Surface_Rack is locked and then empty.
      -- Otherwise, we need to abort
      TTY.Screen_Rack.Freeze;
      Rack_Locked := True;
      
      if not TTY.Screen_Rack.Empty then
         -- Abort!
         TTY.Screen_Rack.Thaw;
         raise Line_Depend with "Detach attempted with Screens associated";
      end if;
      
      -- While the Screen_Rack is locked, we know we arn't going to get any
      -- new Screens, and we know there are none currently (which equates to
      -- having no Windows, or ability to add more, either). We are safe to
      -- take down the Terminal. It also means that no other calls of attach
      -- can be executing in parallel, since they'd be waiting to lock the
      -- Rack right now. This is important for invalidating the Terminal and 
      -- Line Handles, since those are not protected
      
      TTY.Status.Deactivate;
      
      Disban_Terminal (TTY.Handle);
      TTY.Device.Release_Line (TTY.Line);
      TTY.Screen_Rack.Thaw;
      
   exception
      when Line_Depend =>
         raise;
         
      when others =>
         -- This is *extremely* unlikely, but it would likely mean the curses
         -- library (or the system) has had a total meltdown. That justifies
         -- abandoning the handles entirely
         TTY.Status.Deactivate;
         TTY.Handle := Invalid_Handle;
         TTY.Line   := Invalid_Handle;
         
         if Rack_Locked then
            TTY.Screen_Rack.Thaw;
         end if;
   end Detach;
   
   
   
   --------------
   -- Finalize --
   --------------
   overriding procedure Finalize (TTY: in out Terminal) is
   begin
      TTY.Detach;
      
   exception
      when others => null;
   end Finalize;

   
end Curses.Terminals;
