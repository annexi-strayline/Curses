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

-- Frames virtualize the coordinate space of any Surface'Class object, simply
-- passing adjusted coordinates through to the Target Surface. The Extents of
-- the Frame are modified (automatically limited) if the Target Surface changes
-- size such that the computed actual Extent of the Frame exceeds  that of the
-- Target Surface.
--
-- Frames are intended for controlling text placement and other UI elements.
-- Some operations, such as Clear and Set_Background will often be implemented
-- less much less efficiently than the underlying Surface, since the Frame
-- never makes direct use of low-level bindings, but rather only manipulates
-- the Target Surface through the standard Surface'Class interface.
--
-- ** Note that Set_Background invokes Clear.


private with Curses.Terminals.Color;

package Curses.Frames is
   
   pragma Assertion_Policy (Check);
   
   -----------
   -- Frame --
   -----------
   type Frame (<>) is limited new Surface with private;
   
   function  New_Frame (Target          : not null access Surface'Class;
                        Top_Left        : in Cursor_Position;
                        Proposed_Extents: in Cursor_Position)
                       return Frame;
   -- Sets-up a new Frame on the Target Surface at the position specified by
   -- Top_Left, and with an Extents specified by Proposed_Extents. 
   
   function  New_Frame (Target          : not null access Surface'Class;
                        Margin          : in Cursor_Ordinal)
                       return Frame;
   -- Sets-up a new Frame on the Target Surface with a position and size
   -- computed by the requested Margin size.
                       
   -- If the specified parameters cannot be accommodated by the Target Surface,
   -- or the Target Surface is not Available, the new Frame will be initialized
   -- as not Available.
   -- -- Suppresses All Exceptions --
   
   
   procedure Reframe (The_Frame       : in out Frame;
                      Top_Left        : in     Cursor_Position;
                      Proposed_Extents: in     Cursor_Position);
   
   procedure Reframe (The_Frame       : in out Frame;
                      Margin          : in     Cursor_Ordinal);
   -- Re-configures a Frame to a different location and position on the Target
   -- Surface.
   --
   -- ** REFRAME IGNORES ANY EXISTING CONTENT **
   -- Existing content of a Frame actually lives on the Target Surface.
   -- Therefore, Reframe operations do not move or change the existing content.
   -- The Reframe operation simply adjusts the position of the Frame. Any 
   -- Set_Background configuration is lost.
   --
   -- -- All Possible Exceptions --
   -- *  Surface_Unavailable: Target is not Available
   -- *  Cursor_Excursion   : The specified Frame cannot be accommodated by
   --                         Target
   -- *  Curses_Library     : Any other unexpected error.
   --
   -- In case of any Exception, Frame is made not Available, but no other
   -- changes are made.
   
   
   -- Cursor Handling --
   ---------------------
   procedure Assert_Cursor (The_Frame: in out Frame);
   -- Forces the Target Surface's Current_Cursor to match the actual position
   -- and style of the Frame's Current_Cursor. Fails silently.
   -- -- Suppresses All Exceptions --
   
   procedure Auto_Assert_Cursor (The_Frame: in out Frame;
                                 Set      : in     Boolean := True);
   -- If set to True, Assert_Cursor is invoked any time the Current_Cursor is
   -- modified.
   --
   -- The default setting is False
   -- -- Suppresses All Exceptions --
   
   procedure Derive_Cursor (The_Frame: in out Frame);
   -- Sets the Frame's Current_Cursor to match the position and style of the
   -- Target Surface's Current_Cursor. If the Target's Current_Cursor is not
   -- within the bounds of the Frame, the Frame's Current_Cursor is placed as
   -- close as possible (same Row or Column, if possible) but otherwise limited
   -- to the Top_Left or Extents of the Frame.
   -- -- Suppresses All Exceptions --
   
   
   -- Surface'Class Overrides --
   -----------------------------
   overriding 
   function  Available (The_Surface: Frame) return Boolean;
   
   overriding
   function  Current_Cursor (The_Surface: Frame) return Cursor'Class;
   
   overriding
   procedure Current_Cursor (The_Surface: in out Frame;
                             New_Cursor : in     Cursor'Class);
   
   overriding
   procedure Position_Cursor (The_Surface: in out Frame;
                              Position   : in     Cursor_Position);
   
   overriding
   procedure Clear (The_Surface  : in out Frame);
   
   overriding
   procedure Clear_Row (The_Surface  : in out Frame;
                        Row          : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Rows (The_Surface  : in out Frame;
                         First_Row    : in     Cursor_Ordinal;
                         Last_Row     : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Column (The_Surface: in out Frame;
                           Column     : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Columns (The_Surface : in out Frame;
                            First_Column: in     Cursor_Ordinal;
                            Last_Column : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_To_End (The_Surface: in out Frame);
   
   overriding
   procedure Clear_To_End (The_Surface: in out Frame;
                           From       : in     Cursor'Class);
   -- All Clear overrides are implemented through Put operations of the Space 
   -- character, or Strings of that character composed.
   --
   -- Note that if Set_Background has not been invoked, the Cursor used to
   -- clear the Frame is Current_Cursor, including applicable styling.
   
   
   overriding 
   procedure Put (The_Surface   : in out Frame;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False);
   
   overriding
   procedure Fill (The_Surface: in out Frame;
                   Pattern    : in     String);
   
   overriding
   procedure Fill (The_Surface: in out Frame;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class);
   
   
   overriding
   procedure Set_Background (The_Surface   : in out Frame;
                             Fill_Character: in     Character := ' ');
   
   overriding
   procedure Set_Background (The_Surface   : in out Frame;
                             Fill_Character: in     Character := ' ';
                             Fill_Cursor   : in     Cursor'Class);
   
   -- Set_Background is emulated with Clear
   
   
   overriding
   procedure Transcribe (Source : in out Frame;
                         Target : in out Surface'Class;
                         From   : in     Cursor'Class;
                         To     : in     Cursor'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False);
   
   overriding
   procedure Transcribe (Source : in out Frame;
                         Target : in out Surface'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False);
   
   
private
   
   -----------
   -- Frame --
   -----------
   
   subtype Colored_Cursor_Container is 
     Terminals.Color.Colored_Cursor_Container;

   -- Frame_State --
   -----------------
   protected type Frame_State is
      function  Available return Boolean;
      procedure Available (Set: in Boolean);
      
      function  Current_Cursor return Cursor'Class;
      procedure Current_Cursor (Set: in Cursor'Class);
      
      
      function  Background_Cursor return Cursor'Class;
      procedure Background_Cursor (Set: in Cursor'Class);
      
      function  Background_Character return Character;
      procedure Background_Character (Set: in Character);
      -- Background is configured through Set_Background, and is used
      -- for all Clear operations. Cursor defaults to the parent Surface's 
      -- Current_Cursor during initialization. Character defaults to Space.
      
      function  Target_TL return Cursor_Position;
      procedure Target_TL (Set: in Cursor_Position);
      -- The current actual position on the Target Surface
      
      function  Auto_Assert return Boolean;
      procedure Auto_Assert (Set: in Boolean);
      
   private
      Active       : Boolean := False;
      Assert_Config: Boolean := False;
                                      
      Our_Cursor   : Colored_Cursor_Container;
      
      BG_Cursor    : Colored_Cursor_Container;
      BG_Char      : Character := ' ';
      
      TL_Pos       : Cursor_Position;
      
   end Frame_State;
   
   ----------------------------------------
   type Frame (Target: not null access Surface'Class) is limited
     new Surface with
      record
         State: Frame_State;
      end record;
   
   --
   -- function expression implementations
   --
   
   overriding function Available (The_Surface: Frame) return Boolean
     is (The_Surface.State.Available);
     
   overriding function Current_Cursor (The_Surface: Frame) return Cursor'Class
     is (The_Surface.State.Current_Cursor);
     
   
end Curses.Frames;