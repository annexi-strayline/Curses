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

-- This package contains color capability initialization, and configuration.
-- Color styles are, however, applied through the Binding.Standard package

package Curses.Binding.Color is
   
   -- Color/Pair data types
   subtype CURSES_Color      is short;
   subtype CURSES_Color_Pair is short;
   subtype CURSES_RGB        is short;
   
   procedure Initialize_Colors (TTY             : in     Terminal_Handle;
                                Color_Capable   :    out Boolean;
                                Color_Changeable:    out Boolean;
                                Default_Color   :    out Boolean);
   -- Attempts to initialize color capabilities for the terminal, if possible.
   -- If the terminal supports color, Color_Capable will be set to True.
   -- If the terminal supports color, and can re-define colors and pairs,
   -- Color_Changeable will also be set to True. If the Terminal/Curses
   -- supports default colors (color index = -1), Default_Color will be set to
   -- True
   --
   -- If the Terminal is invalid, Color_Capable and Color_Changeable will
   -- simply return False.
   --
   -- -- Suppresses All Exceptions --
   
   
   function Maximum_Colors (TTY: Terminal_Handle) return CURSES_Color;
   function Maximum_Pairs  (TTY: Terminal_Handle) return CURSES_Color_Pair;
   -- Retrieve the maximum number of colors available or color pairs available.
   -- On error, both return 1, which includes the default.
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Terminal invalid or other unexpected internal error
   
   
   procedure Decompose_Color (TTY             : in     Terminal_Handle;
                              Color           : in     CURSES_Color;
                              Red, Green, Blue:    out CURSES_RGB);
   -- Queries the terminal directly for the indicated color index
   --
   -- Note that Decompose_Color is only used to query a terminal's own
   -- interpretation of any of the Default colors, otherwise, the Curses
   -- binding package maintains it's own database. This is why there is no 
   -- "Decompose_Pair" binding, as the Default_Style is well defined, constant,
   -- and is the only default.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed or Red, Green, Blue variables were
   --                    invalid, or other unexpected internal error.
   
   
   procedure Set_Color (TTY             : in Terminal_Handle;
                        Color           : in CURSES_Color;
                        Red, Green, Blue: in CURSES_RGB);
   -- Attempts to set the color index for the provided terminal.
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed or Red, Green, Blue values were invalid,
   --                    or other unexpected internal error.
   
   
   procedure Set_Pair (TTY                   : in Terminal_Handle;
                       Pair                  : in CURSES_Color_Pair;
                       Foreground, Background: in CURSES_Color);
   -- Attempts to set the pair index for the provided terminal.
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed or Pair, Foreground, or Background values
   --                    were invalid, or other unexpected internal error.
   
   
   procedure Apply_Pair (Handle: in Surface_Handle;
                         Pair  : in CURSES_Color_Pair);
   -- Applies a color pair to the active style of the selected Surface.
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to apply color style
   
   
   procedure Set_Colored_Background (Handle          : in Surface_Handle;
                                     Blank_Character : in Character;
                                     Reference_Cursor: in Cursor'Class;
                                     Color           : in CURSES_Color_Pair);
   -- Applies a Style, including Color to a new Blank_Character and sets it as 
   -- the background for Handle based on the Style component of
   -- Reference_Cursor
   --
   -- This procedure is called from the Terminals.Color package via the
   -- Apply_Colored_Background procedure, which provides the correct value
   -- for Color, based on the content of the Color property of the
   -- Colored_Cursor type also defined in the Terminals.Color package.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to apply color style
   
   
   procedure Set_Default_Colored_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair);
   
   procedure Set_Colored_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair;
      LS, RS, TS, BS, TL, TR, BL, BR: in Character);
   -- Sets the border around a given Surface. Set_Default_Border defers to the
   -- (n) curses library itself to determine which box drawing characters are
   -- appropriate. Set_Border allows the specification of a specific set of
   -- characters to be used for box drawing.
   --
   -- Reference_Cursor is used to set the appropriate attributes for the
   -- characters used to draw the border
   --
   -- The provided Color pair is applies to border along with the attributes
   -- of Reference_Cursor
   --
   -- This procedure is called from the Terminals.Color package via the
   -- Apply_Colored_Border procedure, which provides the correct value for
   -- Color, based on the content of the Color property of the Colored_Cursor
   -- type also defined in the Terminals.Color package.
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Call failed - unable to set border
   
   
private
   
   ------------------------------------
   -- Generic_Set_Colored_Background --
   ------------------------------------
   generic
      type Ada_Char_Type is (<>);
      type C_Char_Type   is (<>);
   
      with function To_C (Item: in Ada_Char_Type) return C_Char_Type;
   
      with function CURSES_generic_meta_set_background_color
        (win  : in Surface_Handle;
         blank: in C_Char_Type;
         bold, standout, dim, uline, invert,
         blink: in unsigned;
         pair : in CURSES_Color_Pair)
        return bool;
        
   procedure Generic_Set_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Ada_Char_Type;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair);
   
   
   --------------------------------
   -- Generic_Set_Colored_Border --
   --------------------------------
   generic
      type Ada_Char_Type is (<>);
      type C_Char_Type   is (<>);
      
      with function To_C (Item: in Ada_Char_Type) return C_Char_Type;
      
      with function CURSES_generic_meta_wborder_color
        (win  : in Surface_Handle;
         bold, standout, dim, uline, invert,
         blink: in unsigned;
         pair : in short;
         ls, rs, ts, bs, tl, tr, bl,
         br   : in C_Char_Type)
        return bool;
   
   procedure Generic_Set_Colored_Border
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      Color           : in CURSES_Color_Pair;
      LS, RS, TS, BS, TL, TR, BL, BR: in Ada_Char_Type);
   
end Curses.Binding.Color;
