------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with System;
with Ada.Finalization;        use Ada;
with Curses.Terminals.Color;

private with Curses.Layers;

package Curses.Terminals.Surfaces is
   
   pragma Assertion_Policy (Check);
     
   ----------------------
   -- Terminal_Surface --
   ----------------------
   type Terminal_Surface (<>) is abstract limited new Surface with private;
   -- Represents a generic Surface object which is directly associated with
   -- a Terminal object
   
   -- Surface'Class Required Implementations --
   --------------------------------------------
   overriding
   function  Wide_Support (The_Surface: Terminal_Surface) return Boolean;
   
   overriding
   function  Available (The_Surface: Terminal_Surface)    return Boolean;
   
   overriding
   function  Current_Cursor (The_Surface: Terminal_Surface)
                            return Cursor'Class;
   -- ** Additional to This implementation **
   -- Returns either a pure Cursor or a pure Colored_Cursor, corresponding to 
   -- the Current_Cursor's type, which is view-constrained to one of those two
   -- types.
   
   overriding
   procedure Current_Cursor (The_Surface: in out Terminal_Surface; 
                             New_Cursor : in     Cursor'Class);
   -- -- Additional to This Implementation --
   -- The Current_Cursor is stored as either a pure Cursor type, or a pure
   -- Colored_Cursor type. if New_Cursor is a derivative of either of these
   -- types, a view conversion will be made, forcing the Current Cursor to be
   -- a pure Cursor or a pure Colored_Cursor. Additional properties would be
   -- lost (calling the Current_Cursor function subsequently would return
   -- either a pure Cursor or Colored_Cursor object.
   
   
   overriding
   procedure Position_Cursor (The_Surface: in out Terminal_Surface;
                              Position   : in     Cursor_Position);
   
   
   overriding
   procedure Clear         (The_Surface: in out Terminal_Surface);
   
   overriding
   procedure Clear_Row     (The_Surface: in out Terminal_Surface;
                            Row        : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Rows    (The_Surface: in out Terminal_Surface;
                            First_Row  : in     Cursor_Ordinal;
                            Last_Row   : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Column  (The_Surface: in out Terminal_Surface;
                            Column     : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_Columns (The_Surface : in out Terminal_Surface;
                            First_Column: in     Cursor_Ordinal;
                            Last_Column : in     Cursor_Ordinal);
   
   overriding
   procedure Clear_To_End  (The_Surface: in out Terminal_Surface;
                            From       : in     Cursor'Class);
   
   overriding
   procedure Put (The_Surface   : in out Terminal_Surface;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode  := Left;
                  Overflow      : in     Overflow_Mode := Truncate;
                  Advance_Cursor: in     Boolean       := False);
   
   
   overriding
   procedure Wide_Put
     (The_Surface   : in out Terminal_Surface;
      Set_Cursor    : in out Cursor'Class;
      Content       : in     Wide_String;
      Justify       : in     Justify_Mode          := Left;
      Overflow      : in     Overflow_Mode         := Truncate;
      Advance_Cursor: in     Boolean               := False;
      Wide_Fallback : access 
        function (Item: Wide_String) return String := null);
   
   
   overriding
   procedure Fill (The_Surface: in out Terminal_Surface;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class);
   
   overriding
   procedure Wide_Fill (The_Surface  : in out Terminal_Surface;
                        Pattern      : in     Wide_String;
                        Fill_Cursor  : in     Cursor'Class;
                        Wide_Fallback: access 
                          function (Item: Wide_String) return String := null);
   

   overriding
   procedure Set_Background (The_Surface   : in out Terminal_Surface;
                             Fill_Character: in     Graphic_Character := ' ';
                             Fill_Cursor   : in     Cursor'Class);
   
   
   overriding
   procedure Wide_Set_Background
     (The_Surface   : in out Terminal_Surface;
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class;
      Wide_Fallback : access function (Item: Wide_Character) 
                                      return Character := null);
   
   overriding
   procedure Set_Border (The_Surface: in out Terminal_Surface;
                         Use_Cursor : in     Cursor'Class);
   
   overriding
   procedure Set_Border (The_Surface: in out Terminal_Surface;
                         Use_Cursor : in     Cursor'Class;
                         
                         Left_Side,
                         Right_Side,
                         Top_Side,
                         Bottom_Side,
                           
                         Top_Left_Corner,
                         Top_Right_Corner,
                         Bottom_Left_Corner,
                         Bottom_Right_Corner: in Graphic_Character);
   
   overriding
   procedure Wide_Set_Border (The_Surface: in out Terminal_Surface;
                              Use_Cursor : in     Cursor'Class;
                              
                              Left_Side,
                              Right_Side,
                              Top_Side,
                              Bottom_Side,
                                
                              Top_Left_Corner,
                              Top_Right_Corner,
                              Bottom_Left_Corner,
                              Bottom_Right_Corner: in Wide_Graphic_Character;
                              
                              Wide_Fallback: access 
                                function (Item: Wide_Character) 
                                         return Character := null);
   
   overriding
   procedure Transcribe (Source : in out Terminal_Surface;
                         Target : in out Surface'Class;
                         From   : in     Cursor'Class;
                         To     : in     Cursor'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False);
   
   overriding
   procedure Transcribe (Source : in out Terminal_Surface;
                         Target : in out Surface'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False);
   

   
   
   -- Finalization --
   ------------------
   overriding
   procedure Finalize (The_Surface: in out Terminal_Surface);
   
   ----------------------
   -- Rendered_Surface --
   ----------------------
   
   -- Control_Character --
   -----------------------
   type Control_Class is
     (No_Key,   Invalid,  Graphic,    Unknown,    Lost_Focus,
      Ctrl,     F_Key,    Escape,     Enter,      Backspace,
      Up_Key,   Down_Key, Left_Key,   Right_Key,
      Home_Key, End_Key,  Insert_Key, Delete_Key,
      Page_Up,  Page_Down);
   -- Indicates the type of character or character combination represented by
   -- a Control_Character
   --
   -- No_Key indicates a null Control_Character. This is used to indicate that
   -- no control key was retrieved for the operation. This happens when a call
   -- to Input was configured to be non-blocking, and there was no input
   -- available from the terminal.
   --
   -- Invalid Control_Characters result from non-alphanumeric inputs that could
   -- not be properly interpreted by the library. 
   --
   -- Graphic is used to retrieve single characters from the input queue.
   -- Since a single queue contains both control characters, and regular
   -- graphic characters (including punctuation and alpha-numerics) are also 
   -- passed on as Control_Characters. This also indicates regular Alt+__ and
   -- Ctl+__ combinations.
   --
   -- Lost_Focus is returned if the Focused property of the Rendered_Surface
   -- was False at or during a call to Input.
   
   subtype F_Key_Range is Positive range 1 .. 24;
   -- Range of 'F' keys, from F1 .. F24
   -- Curses theoretically supports more than this, but it's just not something
   -- one would realistically come across anymore. This range is sure to work.
   
   type Control_Character (Class: Control_Class := Invalid) is
      record
         case Class is
            when Ctrl | Graphic =>
               Alt: Boolean;    -- Alt was also held
               Key: Character;
               
            when F_Key =>
               F_Number : F_Key_Range;
               
            when Unknown =>
               Unknown_Character: Wide_Character;
               -- Handles any character within a 16-bit space. This Character
               -- represents exactly what was received from the underlying
               -- Curses library
               
            when others =>
               null;
               
         end case;
      end record;
   
   
   ----------------------------------------
   type Rendered_Surface (<>) is abstract limited 
     new Terminal_Surface with private;
   -- A Rendered_Surface represents a Terminal_Surface that is rendered on the
   -- screen when Visible, and can accept input from the terminal.
   
   procedure Show (The_Surface: in out Rendered_Surface);
   procedure Hide (The_Surface: in out Rendered_Surface);
   -- The Show/Hide procedures set the intention of the Rendered_Surface's 
   -- visibility within its context (the Terminal for Screen objects, and the
   -- parent Screen for Window objects). The actual Visibility, as determined
   -- by the Visible property of Surface'Class, indicates the partial or
   -- complete Visibility of the Surface within it's context. Ergo, if
   -- Visibility of a Rendered_Surface is True, it is at least partially
   -- visible on the terminal.
   --
   -- These procedures directly set the Surface's Arm (Show/Hide) and Visible 
   -- (Hide) properties
   
   procedure Superior  (The_Surface: in out Rendered_Surface) is abstract;
   procedure Inferior  (The_Surface: in out Rendered_Surface) is abstract;
   procedure Promote   (The_Surface: in out Rendered_Surface) is abstract;
   procedure Demote    (The_Surface: in out Rendered_Surface) is abstract;
   -- Changes the relative position of The_Surface with other Rendered_Surfaces
   -- sharing a context.
   
   procedure Above (Subject_Surface: in out Rendered_Surface; 
                    Object_Surface : in out Rendered_Surface) is abstract;
   procedure Below (Subject_Surface: in out Rendered_Surface;
                    Object_Surface : in out Rendered_Surface) is abstract;
   -- The Subject_Surface is placed either Above or Below the Object_Surface,
   -- if and only if both Surfaces share the same context. For examples,
   -- Windows sharing a Screen. If the Surfaces do not share a context, these
   -- operations have no effect.
   --
   -- -- Suppresses All Exceptions --
   
   
   function Focused (The_Surface: Rendered_Surface) return Boolean;
   -- Returns True if the Rendered_Surface has focus of the terminal for the
   -- purposes of Input. Focus is maintained by the Screen object, defined in
   -- child package Standard
   --
   -- -- Suppresses All Exceptions --
   
   procedure Wait_Focused (The_Surface: in out Rendered_Surface);
   -- Blocks until Focused is True. This is implemented through a
   -- Protected Entry.
   
   
   procedure Lock_Focus (The_Surface: in out Rendered_Surface) is abstract;
   -- Causes Focus to be locked onto The_Surface for the Surface context 
   -- (eg Screen) which The_Surface shares. A Locked Focus may be revoked at
   -- any time, according to the rules of the context.
   -- -- Suppresses All Exceptions --
   
   procedure Release_Focus (The_Surface: in out Rendered_Surface) is abstract;
   -- Returns Focus control to the context-specific rules for automatic focus
   -- selection (if any). This subprogram does not guarantee loss of Focus.
   -- If The_Surface does not have Focus, this subprogram has no effect.
   -- -- Suppresses All Exceptions --
   
   function  Input_Key  (The_Surface  : in out Rendered_Surface;
                         Peek         : in     Boolean  := False;
                         Wait         : in     Boolean  := True)
                        return Control_Character;
   -- Returns a single key from the Surface's Terminal input buffer. If Peek is
   -- True the key is left in the buffer.
   --
   -- If Wait is true, the operation blocks until a key is available.
   -- Otherwise, No_Key Class Control_Key is returned. 
   --
   -- If no key is available and The_Surface currently has focus, Input_Key
   -- will block until a key is available
   --
   -- The_Surface must be both Visible and Focused to receive input. If it is
   -- not currently Focused and Visible, Input_Key will either block until
   -- Focused and Visible return True, or else will return a Lost_Focus 
   -- discriminated Control_Character.
   --
   -- If The_Surface is not Available, an Invalid Control_Key class
   -- is returned
   --
   -- Upon any unexpected error, an Invalid Control_Key class is returned.
   -- -- Suppresses All Exceptions --
   
   
private
   use Curses.Layers;
   
   ----------------------
   -- Terminal_Surface --
   ----------------------
   
   -- Protected_Cursor --
   ----------------------
   -- Ensures synchronized access and modification of Current_Cursor
   
   subtype Colored_Cursor_Container is Color.Colored_Cursor_Container;
   
   protected type Protected_Cursor is
      procedure Set       (New_Cursor : in Cursor'Class);
      -- Accepts Colored_Cursor'Class Cursors, which are view-converted into
      -- a pure Colored_Cursor, or a pure Cursor
      
      procedure Set_Position (New_Position: in Cursor_Position);
      -- Simply sets the position of the existing cursor.
      
      function  Get return Cursor'Class;
      -- Returns either a pure Cursor or pure Colored_Cursor object
      
      procedure Position_Changed (Changed: out Boolean);
      -- Returns True if the Cursor position changed since the last call to
      -- Hint_Position_Changed. This is used by Screen.Update to determine if
      -- a Visibility calculation is needed
      
   private
      Container            : Colored_Cursor_Container;
      Position_Changed_Hint: Boolean := False;
   end Protected_Cursor;


   ----------------------------------------
   type Terminal_Surface (TTY: not null access Terminal'Class) is 
     abstract limited new Surface with
      record
         Handle      : Surface_Handle    := Invalid_Handle;
         Cursor_State: Protected_Cursor;
      end record;
   -- Note: Handle is only ever set once during Initialization, and before 
   -- adding it to any Surface_Rack, when we are guaranteed that no other Task
   -- is going to be able to look at it until it is set.
   
   overriding function Available (The_Surface: Terminal_Surface) return Boolean
     is (Handle_Valid (The_Surface.Handle));
   
   overriding function Current_Cursor (The_Surface: Terminal_Surface)   
                                      return Cursor'Class 
     is (The_Surface.Cursor_State.Get);
   
   
   ----------------------
   -- Rendered_Surface --
   ----------------------
   
   -- Focus_State --
   -----------------
   protected type Focus_State is
      function  Has_Focus return Boolean;
      procedure Has_Focus (Set: in Boolean);
      entry     Wait_Focus;
   private
      Focused: Boolean := False;
   end Focus_State;
   
   
   -- Visibility_Class --
   ----------------------
   -- Used by the Refresh subprogram to store information about the relative
   -- visibility on the Screen
   type Visibility_Class   is 
     (Full,     -- Surface has no Surfaces covering it above
      Partial,  -- Surface is partially covered by other Surface above
      Hidden);  -- Surface is completely covered by Windows above, or
                -- is not Armed to be Visible
   
   -- Update_Class --
   ------------------
   -- Used by the Refresh subprogram to store information about the need for
   -- updating of the Surface's content
   type Update_Class is 
     (None,  -- Surface does not need to be updated
      Hard,  -- Surface must be redrawn
      Soft); -- Surface can be updated (changes only)
   
   ----------------------------------------
   type Rendered_Surface (TTY: not null access Terminal'Class) is
      abstract limited new Terminal_Surface (TTY) with
      record
         Focus: Focus_State;
         
         -- Screen Update Tracking Variables --
         --------------------------------------
         -- These variables are set specifically by the Screen.Update
         -- operation. The Update operation itself implements a lock through
         -- the subject Screen for the entire Update operation. Since these
         -- variables are only ever accessed from the Screen.Update operation,
         -- these elements don't need to be protected themselves.
         
         Total_Visibility : Visibility_Class := Hidden;
         Cursor_Visibility: Boolean          := True;
         Update_Directive : Update_Class     := None;
      end record;
   
   
   -- Expression Functions --
   --------------------------
   function Focused (The_Surface: Rendered_Surface) return Boolean
     is (The_Surface.Focus.Has_Focus);
   
end Curses.Terminals.Surfaces;
