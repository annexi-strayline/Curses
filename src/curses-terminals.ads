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

with Ada.Finalization;
with System;

with Curses.Device; use Curses.Device;

private with Curses.Layers;
private with Curses.Binding.Color;
private with Curses.Binding.Terminals;

package Curses.Terminals is
   
   type Terminal (Device: not null access Line_Device'Class) is 
     tagged limited private;
   -- The Terminal represents the actual terminal device attached to Line
   -- assigned by Device
      
   -- Line Management --
   ---------------------   
   procedure Attach (TTY  : in out   Terminal;
                     Model: in       String := "");
   -- Attempts to attach the Terminal object to a physical terminal of type
   -- Model, via a Line offered by the Line_Driver Device.
   --
   -- If Model is an empty string, (as per the default), this generally
   -- indicates the (n)curses library should refer to the TERM environment
   -- variable of the partition. The default configuration should work most of 
   -- the time for typical single-user cases. For multiple terminal
   -- applications, a Model should be explicitly provided.
   --
   -- Examples of common values for the Model parameter include:
   -- *  "xterm"
   -- *  "xterm-256color" (needed to use all 256 colors)
   -- *  "vt100"
   --
   -- Attach must be called to make a Terminal object Available. Attaching a
   -- Screen object to a Terminal which is not Available (has not been
   -- Attached), always fails, and is irrevocable for the life of that Screen
   -- object. The Screen will thus be not "Available" for its life.
   --
   -- If the Terminal is already Attached, Line_Depend is raised. 
   --
   -- Architectural Rationale:
   -- This is not a function returning the type since Terminal object
   -- themselves contain tasks, and those tasks must be activated before
   -- Screens can be associated with a Terminal. Making this a procedure
   -- prevents what could otherwise be a common user error of possibly
   -- associating Screen objects with a Terminal declared in the same
   -- declarative region, which would never succeed, since the required tasks
   -- of the Terminal type would not yet be running.
   --
   -- -- All Possible Exceptions --
   -- *  Line_Error    : The Terminal was unable to obtain a Line from the
   --                    Line_Driver
   -- *  Line_Depend   : The Terminal is already Attached.
   -- *  Curses_Library: The Terminal was unable to initialize the Terminal
   
   procedure Detach (TTY: in out Terminal);
   -- ** Note that Terminal object Finalization invokes Detach automatically.
   --    it is not normally necessary to Detach a Terminal object unless it
   --    will be reused for another session (for multi-terminal applications)
   --
   -- Detaches the Terminal from the Line and returns the Line to Device,
   -- freeing the Terminal for later Attachment to another Line. This is
   -- intended to allow easier session management, by allowing Terminal objects
   -- to be "recyclable" for all supported Lines of a single Line_Device, and
   -- conceptually associated with its own task in a client-server model.
   --
   -- Detaching a Terminal must be done after all associated Screens and
   -- Windows have been Finalized..
   --
   -- If this is not the case, Detach raises a Line_Depend exception.
   --
   -- If the Terminal is already detached, Detach has no effect.
   --
   -- -- All Possible Exceptions --
   -- *  Line_Depend: The Terminal still has dependent Screen objects.
   
   
   -- Status and Capabilities --
   -----------------------------
   function  Available (TTY: Terminal) return Boolean;
   -- Indicates if the Terminal was successfully initialized, and is available
   -- to host Screen objects. Also used to indicate failure, such as through a
   -- lost connection.
   -- -- Suppresses All Exceptions --
   
   function Error_Message (TTY: Terminal) return Library_Error_Message;
   -- Returns any available error message that resulted in an unexpected
   -- unavailable state. The error message is a maximum of 80 characters.
   -- If Clear is True, the error message is cleared.
   --
   -- This message buffer is especially useful for storing asynchronous errors,
   -- such as lost connections.
   -- -- Suppresses All Exceptions --
   
   procedure Clear_Error_Message (TTY: in out Terminal);
   
   function Supports_Color         (TTY: in Terminal) return Boolean;
   -- False indicates the Terminal is strictly monochromatic.
   
   function Supports_Default_Color (TTY: in Terminal) return Boolean;
   -- Indicates that the Terminal supports the use of Default_Color,
   -- specifically in definition or overriding of Color_Styles
   
   function Supports_Custom_Colors (TTY: in Terminal) return Boolean;
   -- Indicates that the Terminal supports the overriding or predefinition of
   -- Color_Swatch'es
   
   function Degraded_Palette       (TTY: in Terminal) return Boolean;
   -- Indicates True if there was an unexpected error when attempting to
   -- install the Predefined Swatches/Styles, or to Override a Swatch/Style.
   
   function Limited_Palette        (TTY: in Terminal) return Boolean;
   -- Indicates True when the Terminal cannot support the full set of 
   -- Predefined Swatches/Styles
   
   function Max_Color_Swatches     (TTY: in Terminal) return Natural;
   function Max_Color_Styles       (TTY: in Terminal) return Natural;
   -- All of the above default to False or zero, in the event of no color
   -- support or internal error. When color is supported, the value of the 
   -- maximum Color_Swatches or Color_Styles is limited to the maximum
   -- supported by the Curses package itself, though this is rarely exceeded
   -- by existing terminals.
   
   
   -- Physical Cursor Configuration --
   -----------------------------------
   -- To prevent distracting dances of the physical cursor across the screen as
   -- various surfaces are updated, the Terminal object directly manages the
   -- position of the physical cursor.
   --
   -- The following two procedures allow for configuration of the strategy used
   -- to position the physical surface.
   
   -- Cursor_Park --
   type Cursor_Park_Select is
     -- Selects the strategy to use when the Physical cursor needs to be 
     -- "parked". See Physical_Cursor_Park below
     
     (Screen_Cursor,
      -- The position of the Physical Cursor Park is determined by the
      -- position of the active Screen's Current_Cursor always.
      
      Screen_Extent);
      -- The position of the Physical Cursor Park is determined by the
      -- position of the active Screen's Extent_Cursor always.
      -- ** This is the default **
   
   procedure Cursor_Park (TTY : in out Terminal; Park: in Cursor_Park_Select);
   -- Selects where the Physical Cursor is positioned whenever it is "parked".
   -- See Cursor_Mode_Select for more details on when the Cursor is parked.
   
   
   -- Cursor_Mode --
   type Cursor_Mode_Select is
     -- Selects the strategy to use when setting the position or state of the
     -- terminal's physical cursor.
     
     (Follow_Focus,
      -- The Physical_Cursor follows the Current_Cursor of the currently
      -- Focused Surface.
      -- *  If the Surface is not Visible, the Physical_Cursor is Hidden, or 
      --    Parked if it cannot be Hidden.
      -- *  If the Current_Cursor itself is hidden by higher Surfaces, or is 
      --    explicitly Hidden the Physical_Cursor is Hidden or Parked if it
      --    cannot be Hidden..
      --
      -- ** This is the default **
      
      Always_Parked,
      -- The Physical_Cursor is always Parked.
      
      Always_Hidden);
      -- The Physical_Cursor is always Hidden. If it cannot be Hidden, it is
      -- parked.
   
   procedure Cursor_Mode (TTY : in out Terminal;
                          Mode: in     Cursor_Mode_Select);
   -- Sets the Physical Cursor positioning mode of the Terminal object. Please
   -- see Cursor_Mode_Select description above for details.
   
   
private
   use Ada.Finalization;
   use Layers;
   use Binding.Color;
   
   -----------------------------
   -- Tasking_Order Interface --
   -----------------------------
   -- A Tasking_Order interface provides a generic interface for an object 
   -- which may be passed to the Executive_Liaison task for serialized execution
   -- of some task-unsafe complex operation.
   
   type Tasking_Order is limited interface;
   
   procedure Execute (Order: in out Tasking_Order) is abstract;
   -- To be invoked by the Executive_Liaison task. Any exceptions raised are
   -- propagated back to the submitter of the Tasking_Order
   
   ----------------------------
   -- Exclusive Access Units --
   ----------------------------
   -- We use a fine-grained synchronization model to ensure maximum efficiency.
   
   -- Status --
   ------------
   protected type TTY_Exclusive_Status is
      
      -- Status Flags --
      ------------------
      procedure Activate;
      procedure Deactivate;
      -- Sets availability state
      
      function  Available    return Boolean;
      -- Available indicates the Terminal has valid terminal and line handles
      -- Deactivation must always coincide with the disbandment of the terminal
      -- handle, as well as release of the line handle.
      
      procedure Note_Palette_Limited;
      function  Palette_Limited return Boolean;
      -- The palette is considered limited whenever the entire user swatch
      -- and user style sets cannot be installed. 
      
      procedure Note_Palette_Degraded;
      function  Palette_Degraded return Boolean;
      -- The palette is considered degraded when a palette configuration
      -- operation, which was otherwise expected to succeed, fails.
      
      -- Physical Cursor --
      ---------------------
      procedure Can_Hide_Cursor (Set: in Boolean);
      function  Can_Hide_Cursor return Boolean;
      -- Initialization routines test hiding the cursor, and set this value
      -- accordingly
      
      procedure Cursor_Park (Set: in Cursor_Park_Select);
      function  Cursor_Park return Cursor_Park_Select;
      
      procedure Cursor_Mode (Set: in Cursor_Mode_Select);
      function  Cursor_Mode return Cursor_Mode_Select;
      -- Gets and sets the Cursor_Park and Cursor_Mode strategies
      
      -- Error Message --
      -------------------
      procedure Report_Error (Message: in Library_Error_Message);
      -- Report error also sets Available to False automatically.
      
      procedure Clear_Error;
      
      function  Error return Library_Error_Message;
      
   private
      Is_Available   : Boolean               := False;
      Error_Message  : Library_Error_Message := (others => ' ');
      
      Palette_Degrade: Boolean               := False;
      Palette_Limit  : Boolean               := True;
      
      Cursor_Can_Hide: Boolean               := False;
      Cursor_Park_Set: Cursor_Park_Select    := Screen_Extent;
      Cursor_Mode_Set: Cursor_Mode_Select    := Follow_Focus;
   end TTY_Exclusive_Status;
   
   
   -- Executive_Liaison Task --
   ----------------------------
   -- The TTY_Executive_Liaison task type is associated with each Terminal
   -- object, and provides a serialization facility for all Surface operations.
   -- Essentially, the Surface update operations use TTY_Executive_Liaison to
   -- execute large, iterative changes (especially when setting the cursor
   -- before writing), atomically. This happens by passing a Tasking_Order
   -- object to the Executive_Liaison, which then makes a dispatching call on
   -- the object, one at a time.
   --
   -- This is necessary because the underlying curses library shares a single
   -- cursor for the entire terminal screen, but yet the binding associates a
   -- cursor local to every surface. In order to maintain consistency on all
   -- output operations, many of which involve multiple steps, all steps are
   -- executed through the TTY_Executive_Liaison Task which ensures that all 
   -- actions are properly serialized.
   --
   -- This model, when compared with more traditional mutex-based locking,
   -- reduces the risk of human error, and the potential for deadlocks.
   
   task type TTY_Executive_Liaison is
      entry Assign (Order: in out Tasking_Order'Class);
      -- Simply executes Operation. All exceptions are propagated to the
      -- caller
   end TTY_Executive_Liaison;
   
   
   -- Refresh_Controller Task --
   -----------------------------
   -- The TTY_Refresh_Controller task is provided as a server to the Standard 
   -- Rendered_Surface family of the Surfaces.Standard child package, to allow 
   -- facilitate efficient background terminal refresh operations. This allows
   -- actual updates to various Surfaces by other tasks to be non-blocking in
   -- relation to the Terminal update operations, which can be arduous.
   --
   -- The TTY_Refresh_Controller tracks requests for Refreshing, and passes
   -- these on to the TTY_Refresh_Executive task.
   --
   -- Actual refresh rates are bounded by the speed at which an Update can be
   -- executed with a single Task. However, this will not bound the processing
   -- or updating of specific Surfaces on the same Terminal.
   
   task type TTY_Refresh_Controller (TTY: not null access Terminal) is
      entry Queue_Refresh;
      -- Queues for the execution of an Update operation on the Screen
      -- occupying the top-most position on the rack.
      
      ----------------------------------------
      -- All queued refreshes are cleared after the respective refresh (full
      -- or limited). This keeps refresh rates limited to the ability of a
      -- single Task to update the active Screen, while allowing actual updates
      -- to the constituent Surfaces to be light-weight, reducing the logic
      -- required (every modifying operation always calls Queue_Referesh).
      
      entry Executive_Ready;
      -- Invoked by the Refresh_Executive once it has finished executing an
      -- ordered refresh.

   end TTY_Refresh_Controller;
   
   
   -- Refresh_Executive Task --
   ----------------------------
   -- This task is actually responsible for executing the Update operation, and
   -- is managed by the Refresh_Liaison task
   task type TTY_Refresh_Executive (TTY: not null access Terminal) is
      entry Execute;
   end TTY_Refresh_Executive;


   -- Terminal --
   --------------
   type Terminal (Device: not null access Line_Device'Class) is 
     new Limited_Controlled with
      record
         Handle: Terminal_Handle     := Invalid_Handle;
         Line  : Line_Handle         := Invalid_Handle;
         
         -- These handles do not need to be protected by since they are only
         -- ever modified at two times: Initialization (Acquire) and
         -- Finalization, during which no other tasks will have access to the
         -- object.
         --
         -- Line is saved so that it may be returned to Device during
         -- finalization, and otherwise is not operationally significant during
         -- the life of a Terminal object.
         
         Status            : TTY_Exclusive_Status;
         
         Liaison           : TTY_Executive_Liaison;
         Refresh           : TTY_Refresh_Controller (Terminal'Access);
         Refresh_Exec      : TTY_Refresh_Executive  (Terminal'Access);
         
         Color_Capable     : Boolean           := False;
         Color_Changeable  : Boolean           := False;
         Default_Color     : Boolean           := False;

         Max_Swatches      : CURSES_Color      := 1; -- Default color/style
         Max_Styles        : CURSES_Color_Pair := 1;
         
         Screen_Rack       : aliased Rack;
         
      end record;
   -- The Terminal object defaults to not Available (unconnected terminal)
   
   -- Finalization Overrides --
   ----------------------------
   overriding
   procedure Finalize (TTY: in out Terminal);
   
   
   -- Expression Implementations --
   --------------------------------
   function Available (TTY: Terminal) return Boolean is
     (TTY.Status.Available);
   
   function Error_Message (TTY: Terminal) return Library_Error_Message is
     (TTY.Status.Error);
      
   function Supports_Color         (TTY: in Terminal) return Boolean is
     (TTY.Color_Capable);
      
   function Supports_Default_Color (TTY: in Terminal) return Boolean is
     (TTY.Default_Color);
      
   function Supports_Custom_Colors (TTY: in Terminal) return Boolean is
     (TTY.Color_Changeable);
      
   function Degraded_Palette       (TTY: in Terminal) return Boolean is
     (TTY.Status.Palette_Degraded);
      
   function Limited_Palette        (TTY: in Terminal) return Boolean is
     (TTY.Status.Palette_Limited);
      
   function Max_Color_Swatches     (TTY: in Terminal) return Natural is
     (Natural(TTY.Max_Swatches));
   
   function Max_Color_Styles       (TTY: in Terminal) return Natural is
     (Natural(TTY.Max_Styles));

      
end Curses.Terminals;
