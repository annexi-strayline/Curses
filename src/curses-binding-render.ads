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

-- The Render binding child package contains all of the most common output and
-- surface management operations

package Curses.Binding.Render is
   
   
   --
   -- Surface creation and destruction
   --
   
   -- Surface Services --
   ---------------------
   function  Create_Surface (TTY    : Terminal_Handle;
                             Extents: Cursor_Position)   
                            return Surface_Handle;
   -- Creates a new anonymous "WINDOW" object (as a (n)curses pad), of the
   -- indicated size (Extents)
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Creation of the window failed
   
   
   procedure Destroy_Surface (Handle: in out Surface_Handle);
   -- Destroys the Handle, and also invalidates the handle. If Handle is
   -- already invalid, no action is taken.
   -- -- Suppresses All Exceptions --
   
   
   --
   -- Geometry
   -- 
   
   function Max_Position (Handle: Surface_Handle)
                         return Cursor_Position;
   -- Returns the library-reported maximum valid position of the Surface
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library     : The reported value could not be used to compute a
   --                         valid Cursor_Position
   -- *  Surface_Unavailable: Handle was invalid
   
   procedure Resize (Handle       : in Surface_Handle;
                     Rows, Columns: in Cursor_Ordinal);
   -- Attempts to Resize the Surface at handle. The Surface should be a
   -- member of the standard Surface type classes defined in Curses.Standard.
   --
   -- Note that Rows and Columns are counts, not coordinates
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - Memory reallocation likely failed. 
   --                    This surface should be made unavailable.
   
   procedure Move (Handle   : in Surface_Handle;
                   Top_Left : in Cursor_Position);
   -- Repositions a Surface on the terminal. Typically this applies to objects
   -- of Curses.Standard.Window'Class.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - generally indicates the position is
   --                    off of the terminal screen.
     
     
   --
   -- Input/Output
   --
   
   procedure Place_Cursor (Handle  : in Surface_Handle;
                           Position: in Cursor_Position);
   -- Positions the cursor on the selected window.
   -- -- Suppresses All Exceptions --
   
   procedure Set_Attributes (Handle: in Surface_Handle;
                             Style : in Cursor_Style);
   -- Sets the relevent style attributes for the cursor on the specified 
   -- surface. 
   --
   -- -- Suppresses All Exceptions --
   
   procedure Clear_Character (Handle: in Surface_Handle);
   -- Clears the character at the location of the Cursor. Style attributes at
   -- the location are preserved.
   -- -- Suppresses All Exceptions --
      
   procedure Put_String (Handle: in Surface_Handle;
                         Buffer: in String);
   -- Outputs the content of Buffer to the surface.
   -- -- All Possible Exceptions --
   -- *  Curses_Library     : General failure of the operation
   -- *  Surface_Unavailable: Handle was invalid
   
   procedure Get_String (Handle: in     Surface_Handle;
                         Buffer:    out String;
                         Last  :    out Natural);
   -- Retreives a String from the current cursor position on the screen
   -- -- All Possible Exceptions --
   -- *  Curses_Library     : General failure of the operation
   -- *  Surface_Unavailable: Handle was invalid
   
   procedure Render_Surface (Handle     : in Surface_Handle;
                             Surface_TL : in Cursor_Position;
                             Physical_TL: in Cursor_Position;
                             Physical_BR: in Cursor_Position;
                             Redraw     : in Boolean := False);
   -- Stages changes of a surface for output to the terminal. All commited
   -- changes are consolodated in memory. An actual refresh of the terminal is
   -- achevied through a call to Refresh_Terminal.
   --
   -- If Redraw is True, the entire region is redrawn, otherwise, only modified
   -- parts are updated
   -- -- Suppresses All Exceptions --
   
   procedure Clear_Surface (Handle: in out Surface_Handle);
   -- Clears all content of the surface.
   -- If the handle is invalid, no action is taken
   -- -- Suppresses All Exceptions --
      
   procedure Clear_To_End_Of_Line (Handle: in out Surface_Handle);
   -- Clears all content from after the cursor location to the end of the line
   -- If the handle is invalid, no action is taken
   -- -- Suppresses All Exceptions --
   
   procedure Set_Monochrome_Background (Handle          : in Surface_Handle;
                                        Blank_Character : in Character;
                                        Reference_Cursor: in Cursor'Class);
   -- Applies a Style to a new Blank_Chracter and sets it as the background 
   -- for Handle based on the Style component of Reference_Cursor. This
   -- procedure ignores Colored_Cursor'Class Reference_Cursor's.
   -- 
   -- On TTYs that support color, Set_Monochrome_Background implies
   -- Default_Color_Style, by nature of Curses attributes.
   --
   -- The Surface package is responsible for calling this procedure, or the
   -- related Set_Colored_Background of our sibling Color package, depending
   -- on the actual Reference_Cursor type and TTY capabilities.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to apply color style
   
   
   procedure Set_Default_Monochrome_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class);
   
   procedure Set_Monochrome_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      LS, RS, TS, BS, TL, TR, BL, BR: in Character);
   
   -- Sets the border around a given Surface. Set_Default_Border defers to the
   -- (n) curses library itself to determine which box drawing characters are
   -- appropriate. Set_Border allows the specification of a specific set of
   -- characters to be used for box drawing.
   --
   -- Reference_Cursor is used to set the appropriate attributes for the
   -- characters used to draw the border
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to set border
   
   
   
   procedure Copy_Area (From_Handle : in Surface_Handle;
                        From_TL     : in Cursor_Position;
                        
                        To_Handle   : in Surface_Handle;
                        To_TL, To_BR: in Cursor_Position);
                        
   -- Attempts to copy all content of a Surface to another. The destination
   -- surface specifies a full rectangle, for which content is sourced from
   -- the From Surface
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to copy area.
                        
private
   
   --
   -- Generic implementations where Wide_Support is needed
   --
   
   ------------------------
   -- Generic_Put_String --
   ------------------------
   generic
      type Ada_Character_Type is (<>);
      type Ada_String_Type is array (Positive range <>) of Ada_Character_Type;
      
      type C_Character_Type is (<>);
      type C_String_Type is array (size_t range <>) of C_Character_Type;
      
      with function To_C (Item      : in Ada_String_Type;
                          Append_Nul: in Boolean)
                         return C_String_Type;
   
      with function CURSES_generic_waddstr (win: Surface_Handle;
                                            str: C_String_Type)
                                           return int;
   
   procedure Generic_Put_String (Handle: in Surface_Handle;
                                 Buffer: in Ada_String_Type);
   
   
   ------------------------
   -- Generic_Get_String --
   ------------------------
   generic
      type Ada_Character_Type is (<>);
      type Ada_String_Type is array (Positive range <>) of Ada_Character_Type;
      
      type C_Character_Type is (<>);
      type C_String_Type is array (size_t range <>) of C_Character_Type;
      
      with function To_Ada (Item    : in C_String_Type;
                            Trim_Nul: in Boolean)
                           return Ada_String_Type;
   
      with function CURSES_generic_winnstr (win: in     Surface_Handle; 
                                            str: in out C_String_Type;
                                            len: in     int)
                                           return int;
   
   procedure Generic_Get_String (Handle: in     Surface_Handle;
                                 Buffer:    out Ada_String_Type;
                                 Last  :    out Natural);
   
   ---------------------------------------
   -- Generic_Set_Monochrome_Background --
   ---------------------------------------
   generic
      type Ada_Char_Type is (<>);
      type C_Char_Type   is (<>);
      
      with function To_C (Item: in Ada_Char_Type) return C_Char_Type;
      
      with function CURSES_generic_meta_set_background 
        (win  : in Surface_Handle;
         blank: in C_Char_Type;
         bold, standout, dim, uline, invert,
         blink: in unsigned)
        return bool;
   
   procedure Generic_Set_Monochrome_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Ada_Char_Type;
      Reference_Cursor: in Cursor'Class);
   
   
   -----------------------------------
   -- Generic_Set_Monochrome_Border --
   -----------------------------------
   generic
      type Ada_Char_Type is (<>);
      type C_Char_Type   is (<>);
      
      with function To_C (Item: in Ada_Char_Type) return C_Char_Type;
      
      with function CURSES_generic_meta_wborder
        (win  : in Surface_Handle;
         bold, standout, dim, uline, invert,
         blink: in unsigned;
         ls, rs, ts, bs, tl, tr, bl,
         br   : in C_Char_Type)
        return bool;
   
   procedure Generic_Set_Monochrome_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      LS, RS, TS, BS, TL, TR, BL, BR: in Ada_Char_Type);
   
end Curses.Binding.Render;
