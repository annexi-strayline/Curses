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

-- This package contains the two most common basic components of a typical
-- Curses-based interface - the Screen and the Window.
--
-- Screen objects represent full-screen terminal contexts that include all
-- associated Windows.
--
-- Windows are as colloquially expected.

private with Curses.Layers;

package Curses.Terminals.Surfaces.Standard is
   
   ------------
   -- Screen --
   ------------
   type Window is tagged;
   type Screen (TTY: not null access Terminal) is
      limited new Rendered_Surface with private;
   -- The Screen type represents both a full-screen Surface, and a notional
   -- context of a children Windows, somewhat similar to the concept of a 
   -- "Desktop" in most graphical window management paradigms. A Screen Surface
   -- is always size of the terminal at its point of initialization. The Screen
   -- will maintain its size, even if the Terminal changes size. This can be
   -- adjusted by invoking Resize.
   --
   -- Only one Screen (and it's children Windows) may be Visible at any moment.
   --
   -- Screens are automatically created and allocated through initialization.
   -- However, new screens are not visible by default, unless
   
   procedure Auto_Focus (The_Screen: in out Screen);
   -- Forces Auto Focus, which is the default state. Auto Focus always causes
   -- the top-most (Superior) Visible Window of The_Screen to have Focus, or
   -- the Screen itself, if there are no Visible Windows.
   --
   -- Auto_Focus overrides any existing Focus Locks active on the Screen
   -- context and forces the focus mode into Auto_Focus.
   
   procedure Resize (The_Screen: in out Screen);
   -- Resizes The_Screen according to the reported current size of the
   -- associated terminal.
   -- -- All Possible Exceptions --
   -- *  Surface_Unavailable: Screen is not Available
   -- *  Curses_Library     : - Any unexpected library error
   --                         - Any unexpected exception
   
   
   -- Internal Procedures --
   -------------------------
   function  Update (The_Screen: in out Screen) return Cursor;
   -- Invoked automatically by the Terminal refresh task periodically if
   -- The_Screen is currently Visible. Only one Screen is Visible at a time,
   -- per Terminal.
   --
   -- If The_Screen is not Visible, or if Update is already executing for the
   -- Screen, Update does not execute, and will always return False.
   --
   -- Update redraws the lowest-most modified Window, or the Screen itself,
   -- followed by a subsequent redraw of all Windows above it which cover the
   -- redrawn Window, or are also modified.
   --
   -- Any Armed Windows are made Visible, and are always thereafter redrawn.
   --
   -- Redrawing is done only in on Terminal's screen buffer, The actual 
   -- terminal screen is only updated by the Terminal's periodic Refresh Task.
   -- Therefore user invocation of this subprogram has no obvious effect, and
   -- no benefit.
   --
   -- Update returns a computation of the suggested location of the physical
   -- cursor on the terminal screen (including visibility), based on the
   -- currently Focused surface.
   -- -- Suppresses All Exceptions --
   
   
   -- Derivation Contracts --
   --------------------------
   overriding procedure Superior      (The_Surface: in out Screen);
   overriding procedure Inferior      (The_Surface: in out Screen);
   overriding procedure Promote       (The_Surface: in out Screen);
   overriding procedure Demote        (The_Surface: in out Screen);
   
   overriding procedure Above         (Subject_Surface: in out Screen; 
                                       Object_Surface : in out Screen);
   overriding procedure Below         (Subject_Surface: in out Screen;
                                       Object_Surface : in out Screen);
   
   overriding procedure Lock_Focus    (The_Surface: in out Screen);
   overriding procedure Release_Focus (The_Surface: in out Screen);
   
   overriding procedure Initialize    (The_Screen: in out Screen); 
   overriding procedure Finalize      (The_Screen: in out Screen);
   
   
   ------------
   -- Window --
   ------------
   type Window (<>) is limited new Rendered_Surface with private;
   -- The Window type represents a specifically-sized window that is associated
   -- with a Screen. Windows maintain an adjustable order in relation to other
   -- Windows of the Screen.
   --
   -- New Windows are always placed at the Top of the Screen's Window
   -- hierarchy, but are initialized as not Visible, and not Armed.
   
   
   function  New_Window (On_Screen       : not null access Screen'Class;
                         Proposed_Extents: in Cursor_Position)
                        return Window;
   -- Attempts to open a new window of the size specified by Extents.
   -- The new Window will, if possible, be centered on the Screen.
   
   function  New_Window (On_Screen       : not null access Screen'Class;
                         Top_Left        : in Cursor_Position;
                         Proposed_Extents: in Cursor_Position)
                        return Window;
   
   -- Attempts to open a new window of the size and/or location specified. If
   -- no position is specified, the Window is centered on the parent Screen.
   --
   -- If provided, Top_Left is a coordinate on the parent Screen, from which
   -- the Window's top-left corner will be located.
   --
   -- The Window size and position are not dependent on the Screen or Terminal
   -- size. Any Windows which are not within the Screen Extents will not be 
   -- will not be Visible, while partial coverage will result in the Window
   -- being Clipped (if Visible or Armed)
   --
   -- New Windows are always placed at the top of the Window hierarchy for the
   -- parent Screen, and are always initialized as not Visible, and not Armed.
   --
   -- Windows can only be "closed" by finalization, through the Window object
   -- going out of scope. Otherwise, Show and Hide should be used to 
   -- temporarily change the visibility.
   --
   -- If the Window could not be opened for any reason, it will be initialized
   -- as an "Unavilable" Surface. This includes the case where the parent
   -- Screen is also not Available.
   -- -- Suppresses All Exceptions --

   
   function Top_Left     (The_Window: in out Window) return Cursor_Position;
   function Bottom_Right (The_Window: in out Window) return Cursor_Position 
     is (The_Window.Top_Left     + 
         The_Window.Extents      -
         Cursor_Position'(1,1));
   -- Returns the respective positions relative to the parent Screen
   -- the Curses_Window on the attached Screen
   --
   -- -- Explicitly Raised --
   -- *  Surface_Unavailable: Window is not Available
   -- *  Curses_Library     : - Any unexpected library error
   --                         - Any unexpected exception
   
   procedure Resize (The_Window   : in out Window;
                     Rows, Columns: in     Cursor_Ordinal);
   -- Resizes the Curses_Window
   -- If the new size is smaller, existing data is lost for the regions
   -- that no longer fit in the window.
   -- -- All Possible Exceptions
   -- *  Surface_Unavailable: Window is not Available
   -- *  Curses_Library     : - Any unexpected library error
   --                         - Any unexpected exception
   
   procedure Move (The_Window: in out Window;
                   Top_Left  : in     Cursor_Position);
   
   -- Moves the Window to the indicated position 
   --
   -- -- All Possible Exceptions
   -- * Surface_Unavailable: Window is not Available
   -- * Curses_Library     : - Any unexpected library error
   --                        - Any unexpected exception
   
   
   function  Same_Screen (A, B: in Window) return Boolean;
   -- Returns True if both Windows share the same parent Screen
   
   
   -- Derrivation Contracts --
   ---------------------------
   overriding procedure Superior      (The_Surface: in out Window);
   overriding procedure Inferior      (The_Surface: in out Window);
   overriding procedure Promote       (The_Surface: in out Window);
   overriding procedure Demote        (The_Surface: in out Window);
   
   overriding procedure Above         (Subject_Surface: in out Window; 
                                       Object_Surface : in out Window);
   overriding procedure Below         (Subject_Surface: in out Window;
                                       Object_Surface : in out Window);
   
   overriding procedure Lock_Focus    (The_Surface: in out Window);
   overriding procedure Release_Focus (The_Surface: in out Window);
   
   -- Finalization --
   ------------------
   overriding procedure Finalize (The_Window: in out Window);
   
   
private
   use Curses.Layers;
   
   ------------
   -- Screen --
   ------------
   type Focus_Mode is (Lock, Auto);
   
   protected type Screen_Modes is
      procedure Focus_Config (Mode: in Focus_Mode);
      function  Focus_Config return Focus_Mode;
      
      procedure Update_Active (Set: in Boolean);
      function  Update_Active return Boolean;
      
   private
      Focus_Config_Value : Focus_Mode := Auto;
      Update_Active_Value: Boolean    := False;
        
   end Screen_Modes;
   
   type Screen (TTY: not null access Terminal) is 
      limited new Rendered_Surface (TTY) with
      record
        Modes        : Screen_Modes;
        Window_Rack  : Rack;
        
        Prev_Physical: Cursor_Position;
        -- Used by Update to track changes to the physical terminal size
        -- between Updates for the Screen.
        --
        -- This value is only modified and referenced from
        -- within Update, which itself cannot execute in parallel on
        -- any given Terminal. Therefore protection is not needed.
      end record;
   
   
   ------------
   -- Window --
   ------------
   
   -- Official_Position --
   -----------------------
   -- Serialized setting of a Cursor_Position value
   protected type Official_Position is
      entry Propose (New_Position: in Cursor_Position);
      -- Blocks if any proposals are active.
      
      procedure Commit;
      -- Commits the proposed coordinates, and clears the proposal
      
      procedure Abandon;
      -- Discards proposed coordinates, and clears the proposal
      
      function Get return Cursor_Position;
      -- Returns the committed current position
      
      procedure Updated (State: out Boolean);
      -- State is True if a new position was Committed since the last call to
      -- Updated
      
   private
      Proposal_Active: Boolean := False;
      Was_Updated    : Boolean := False;
      
      Proposed       : Cursor_Position;
      Current        : Cursor_Position;
   end Official_Position;
   
   ----------------------------------------
   type Window (Parent_Screen: not null access Screen'Class;
                TTY          : not null access Terminal) is 
      limited new Rendered_Surface (TTY) with
      record
        Position: Official_Position;
         -- The configured top-left corner position on the actual 
         -- terminal screen
        
        Sub_Window_Rack: Rack;
      end record;
   
   function Same_Screen (A, B: in Window) return Boolean is
      (A.Parent_Screen = B.Parent_Screen);
      
end Curses.Terminals.Surfaces.Standard;
