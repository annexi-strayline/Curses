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

-- -- This package (including all children) are fully task-safe. -- --

with System;
with Ada.Finalization; use Ada.Finalization;
with Ada.Characters.Conversions;
with Ada.Wide_Characters.Handling;


with Interfaces.C;

package Curses is
   
   pragma Preelaborate (Curses);
   pragma Assertion_Policy (Check);

   --
   -- General Exceptions
   --
   
   Cursor_Excursion: exception;
   -- Raised when a Surface's Cursor was attempted to be placed beyond the
   -- Extents of a Surface.
   
   Surface_Unavailable: exception;
   -- Raised when an operation is unable to complete due to Surface.Available
   -- being False.
   
   Resource_Exhausted: exception;
   -- Raised when sufficient resources required for the operation could not be
   -- allocated, or are not available.
   
   Line_Error: exception;
   -- A general exception raised when a Terminal or Line_Device fails due to
   -- physical driver/line errors.
   
   Line_Depend: exception;
   -- Raised if Detach or Destroy is attempted on a Terminal which still has
   -- associated Screens which have not been Finalized
   
   Curses_Library: exception;
   -- Unexpected failure in external curses library
   
   
   --
   -- Contractual Subtypes
   --
   
   -----------------------
   -- Graphic_Character --
   -----------------------
   subtype Graphic_Character is Character
   with Static_Predicate => Graphic_Character in
                            Character'Val (32)  .. Character'Val (126) |
                            Character'Val (160) .. Character'Val (255);
   -- For Character parameters which must be "graphic" only. This predicate is
   -- based on the ARM defition of Characters.Handling.Is_Graphic (A.3.2)
   
   -- Temporary GNAT bug workaround
   -- For whatever reason, GNAT can't handle the below expression in the actual
   -- Precondition
   function Check_Graphic (S: String) return Boolean is
     (for all C of S => C in Graphic_Character)
     with Inline;
   
   
   ----------------------------
   -- Wide_Graphic_Character --
   ----------------------------
   subtype Wide_Graphic_Character is Wide_Character
     with Dynamic_Predicate => 
       Ada.Wide_Characters.Handling.Is_Graphic (Wide_Graphic_Character);
   -- For optionally supported "Wide_Characters". 
   
   -- Temporary GNAT bug workaround
   function Wide_Check_Graphic (S: Wide_String) return Boolean is
     (for all C of S => C in Wide_Graphic_Character)
     with Inline;
   
   --
   -- Fundamental Types
   --
   
   ---------------------------
   -- Library_Error_Message --
   ---------------------------
   subtype Library_Error_Message is String(1 .. 80);
   -- The Library_Error_Message represents a standard string component which is
   -- used to transmit specific error conditions which caused various object
   -- types to become unavailable or inactive for reasons that were caused by
   -- library interactions involved in initialization of objects where
   -- exceptions are suppressed.
   
   procedure Set_Library_Error_Message 
      (Buffer : in out Library_Error_Message;
       Message: in     String);
   -- Properly clears and sets a Library_Error_Message to an arbitrary String.
   -- Truncates if unable to fit.
   -- -- Suppresses All Exceptions --
   
   
   ------------------
   -- Cursor_Style --
   ------------------
   type Cursor_Style is
      record
         Bold     : Boolean := False;
         Standout : Boolean := False;
         Dim      : Boolean := False;
         Underline: Boolean := False;
         Inverted : Boolean := False;
         Blink    : Boolean := False;
      end record;
   -- Cursor_Style maintains the active stylistic treatment for a given cursor,
   -- and also tracks color information where color is enabled for the 
   -- terminal.
   --
   -- Each member of Curses_Surface'Class maintains an "Active Cursor", which
   -- also maintains the default Cursor_Style. 
   --
   -- The basic Cursor_Style includes the common attributes of the traditional
   -- Curses library.
   
   function "+" (Left,Right: Cursor_Style) return Cursor_Style is
      (Cursor_Style'(Bold      => Left.Bold      or Right.Bold,
                     Standout  => Left.Standout  or Right.Standout,
                     Dim       => Left.Dim       or Right.Dim,
                     Underline => Left.Underline or Right.Underline,
                     Inverted  => Left.Inverted  or Right.Inverted,
                     Blink     => Left.Blink     or Right.Blink));
      
   function "-" (Left,Right: Cursor_Style) return Cursor_Style is
     (Cursor_Style'
       (Bold      => Left.Bold      and (Left.Bold      xor Right.Bold     ),
        Standout  => Left.Standout  and (Left.Standout  xor Right.Standout ),
        Dim       => Left.Dim       and (Left.Dim       xor Right.Dim      ),
        Underline => Left.Underline and (Left.Underline xor Right.Underline),
        Inverted  => Left.Inverted  and (Left.Inverted  xor Right.Inverted ),
        Blink     => Left.Blink     and (Left.Blink     xor Right.Blink    )));
   -- Performs a logical [Left and (Left xor Right)] of each Boolean style
   -- component. This effectively turns off only those styles that are on
   -- in Right, and also in Left, while leaving others that are on in Left
   -- untouched. In essence, Style_A - Style_B ensures that Style_B is
   -- excluded from Style_A
      
     
   -- Common Styles
   Normal_Style   : constant Cursor_Style :=                    (others => <>);
   Bold_Style     : constant Cursor_Style := (Bold      => True, others => <>);
   Inverted_Style : constant Cursor_Style := (Inverted  => True, others => <>);
   Underline_Style: constant Cursor_Style := (Underline => True, others => <>);
   
   
   --------------------
   -- Cursor_Ordinal --
   --------------------
   type Cursor_Ordinal is new Positive;
   -- Row or Column position value
   
   ---------------------
   -- Cursor_Position --
   ---------------------
   type Cursor_Position is
      record
         Row   : Cursor_Ordinal := 1;
         Column: Cursor_Ordinal := 1;
      end record;
   
   
   function "<" (Left, Right: Cursor_Position) return Boolean is
      ((Left.Row < Right.Row) and then (Left.Column < Right.Column));
   
   function "<=" (Left, Right: Cursor_Position) return Boolean is
     (((Left < Right) or else (Left = Right))
      or else ((Left.Row = Right.Row) and then (Left.Column < Right.Column))
      or else ((Left.Row < Right.Row) and then (Left.Column = Right.Column)));
   
   function ">" (Left, Right: Cursor_Position) return Boolean is
      ((Left.Row > Right.Row) or else (Left.Column > Right.Column));
      
   function ">=" (Left, Right: Cursor_Position) return Boolean is
      ((Left > Right) or else (Left = Right));
      
   -- All Cursor_Position comparison functions are framed conceptually against
   -- the ever-present Extents of a Surface. For any Surface, the Extents
   -- represent the "bottom-right corner" position, or the maximum valid
   -- position on the Surface, with respect to _either_ Row or Column. The 
   -- comparison functions are modeled upon Left as a test position, and Right 
   -- as the extents. Therefore a Left position is greater than the Right 
   -- if it is _either_ beyond the column or the row of the Right position.
   
      
   function "+" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    + Right.Row,
                        Column => Left.Column + Right.Column));
      
   function "-" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    - Right.Row,
                        Column => Left.Column - Right.Column));
      
   function "*" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    * Right.Row,
                        Column => Left.Column * Right.Column));
   
   function "/" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    / Right.Row,
                        Column => Left.Column / Right.Column));
      
   function "mod" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    mod Right.Row,
                        Column => Left.Column mod Right.Column));
      
   function "rem" (Left, Right: Cursor_Position) return Cursor_Position is
      (Cursor_Position'(Row    => Left.Row    rem Right.Row,
                        Column => Left.Column rem Right.Column));
      
   ------------
   -- Cursor --
   ------------
   type Cursor is tagged
      record
         Position: Cursor_Position := (others => <>);
         Style   : Cursor_Style    := Normal_Style;
         Visible : Boolean         := True;
      end record;
   -- If Visible is False, if the terminal is so capable, and is in the proper
   -- mode such that it would normally represent the Physical Cursor, the
   -- cursor itself will be hidden on the Terminal.

   -- See the Terminal.Color package for an extended Colored Cursor type, which
   -- provides an additional Color attribute, which is applicable only to
   -- Terminal objects that support color
   
   
   -------------------
   -- Overflow_Mode --
   -------------------
   -- Overflow_Mode configures the result of output operations which cannot be
   -- accommodated at the location specified by the cursor.
   type Overflow_Mode is 
     (Error,          -- Content is truncated to fit on the target row but if
                      -- the content cannot fit, Cursor_Excursion is raised.
                      -- No output will result.
      
      Truncate,       -- Content is truncated to fit on the target row
      
      Wrap_Error,     -- Content is wrapped to lower rows as needed. 
                      -- If rows are insufficient to accommodate all content,
                      -- Cursor_Excursion is raised, but the physical result
                      -- will be identical to Wrap_Truncate
      
      Wrap_Truncate); -- Content is wrapped for as long as possible, any
                      -- overflow is truncated at the last available row. 
   
   
   ------------------
   -- Justify_Mode --
   ------------------
   -- Justify_Mode configures the direction, relative to the Cursor, that
   -- output strings are placed on the Surface. Note that, unless truncated,
   -- the Cursor is always re-positioned to be one space to the right of the
   -- end of the content string, regardless of the justification mode.
   type Justify_Mode is
     (Left,           -- Text is placed at the cursor and to the right
      
      Center,         -- Text is placed with the starting cursor at the
                      -- center of the text
      
      Right);         -- Text is placed with the cursor starting at the end of
   
   
   --
   -- Surface Root Type
   --
   
   -------------
   -- Surface --
   -------------
   type Surface is abstract limited new Limited_Controlled with private;
   -- The Surface'Class represents 2-dimensional rendered planes which can be
   -- individually operated on, and can be stacked in layered hierarchies.

   
   -- Surface Properties --
   ------------------------
   function  Wide_Support (The_Surface: Surface) return Boolean is abstract;
   -- For any given Surface, Wide_Support returns true if and only if the
   -- Surface fully supports all Wide_String and Wide_Character specific
   -- operations.
   
   function  Available (The_Surface: Surface) return Boolean is abstract;
   -- Indicates if the Surface has been activated, and is eligible for normal
   -- IO operations. Surfaces are made available at initialization of the type.
   -- A Surface  which is not Available can never become Available, and as
   -- such, Available returning False indicates a general error.
   
   function  Visible (The_Surface: in out Surface) return Boolean;
   -- True if the Surface is truly Visible in some capacity. This is not 
   -- intention but rather the actual current visibility state. A layer which
   -- is not Visible cannot be seen at all. It is either explicitly hidden
   -- (not Armed), covered, part of an inactive screen, or otherwise invalid.
   
   procedure Wait_Visible (The_Surface: in out Surface);
   -- Blocks until The_Surface.Visible is True. Multiple threads may queue on
   -- this procedure, as it will only block while The_Surface is not Visible
   
   function  Armed (The_Surface: in out Surface) return Boolean;
   -- True if the Surface is waiting to be Visible, but is not yet Visible
   
   procedure Wait_Armed (The_Surface: in out Surface);
   -- Blocks until The_Surface.Armed is True
   
   function  Clipped (The_Surface: in out Surface) return Boolean;
   -- True if the Surface is Visible, but does not completely fit on the
   -- terminal, or is Armed and cannot be made Visible due to it being
   -- completely out of bounds of the terminal. This indicates that the
   -- terminal may have experienced a resize event which effects the Surface.
   -- This property is provided to facilitate dynamic resizing of Surfaces.
   
   procedure Wait_Clipped (The_Surface: in out Surface);
   -- Blocks until The_Surface.Clipped is True. Multiple tasks may queue on
   -- this procedure, as it only blocks while The_Surface remains Clipped.
   
   function  Modified (The_Surface: in out Surface) return Boolean;
   -- True if the _visual content_ of the layer has changed. This does not
   -- include changes in geometry, position, or any other state.
   -- This procedure is provided to facilitate more advanced "window
   -- management" techniques for complex, multi-threaded terminal-based UIs.
   
   procedure Wait_Modified (The_Surface: in out Surface; 
                            Clear      : in     Boolean := True);
   -- Blocks until The_Surface.Modified is True. If Clear is True, the
   -- Modified status is immediately cleared upon unblocking. This is
   -- implemented through a protected entry. 
   --
   -- ** Only a single Task is released on a Modification event. **
   
   
   function  Extents (The_Surface: in out Surface'Class) 
                     return Cursor_Position;
   -- Indicates the maximum valid position of the Surface. The Extents do not
   -- change automatically, and are only updated with appropriate Resize
   -- operations (if any) of the Surface.
   --
   -- The below Extents_Changed property can be used to affect dynamic re-
   -- render dispatch on resize.
   -- -- Suppresses All Exceptions --
   
   
   function  Extents_Changed (The_Surface: in out Surface) return Boolean;
   -- True if the Extents of the Surface have changed.

   procedure Clear_Extents_Changed (The_Surface: in out Surface);
   -- Unconditionally clears the Extents_Changed flag.
   
   procedure Wait_Extents_Changed (The_Surface: in out Surface;
                                   Clear      : in     Boolean  := True);
   -- Blocks until The_Surface.Extents_Changed is True. If Clear is True, the
   -- Geometry_Changed status is immediately cleared upon unlocking.
   -- This is implemented through a protected entry.
   -- Only a single Task is released on a Extents_Changed event.
   
   
   function  Current_Cursor (The_Surface: Surface) return Cursor'Class
      is abstract;
   -- The class of Cursor returned is dependent on the specific implementation
   -- of decedent types of Surface'Class. The user should expect that any
   -- user-defined extensions of Cursor will not be stored as full views of the
   -- type.
   
   procedure Current_Cursor (The_Surface: in out Surface; 
                             New_Cursor : in     Cursor'Class)
      is abstract;
   -- Sets the Current Cursor for the Surface.
   --
   -- The classes of Cursor objects supported is dependent on the specific 
   -- implementation of descendant types of Surface'Class
   --
   -- -- All Possible Exceptions --
   -- *  Cursor_Excursion   : New_Cursor is beyond Extent_Cursor
   
   
   -- Cursor modification short-cuts --
   ------------------------------------
   -- Allows for in-situ modification of the Current Cursor of a Surface
   
   procedure Position_Cursor (The_Surface: in out Surface;
                              Position   : in     Cursor_Position)
      is abstract;
   -- Sets Current Cursor's position on the Surface
   -- -- All Possible Exceptions --
   -- *  Cursor_Excursion: Attempt to position cursor beyond the Extent_Cursor
   
   
   -- I/O Operations --
   --------------------
   procedure Clear (The_Surface  : in out Surface)
      is abstract;
   -- Clears the entire surface
   
   procedure Clear_Row (The_Surface  : in out Surface;
                        Row          : in     Cursor_Ordinal)
      is abstract;
   
   procedure Clear_Rows (The_Surface  : in out Surface;
                         First_Row    : in     Cursor_Ordinal;
                         Last_Row     : in     Cursor_Ordinal)
      is abstract;
   -- Clears the selected Row(s)
   
   procedure Clear_Row (The_Surface: in out Surface'Class);
   -- Clears the row on which the The_Surface.Current_Cursor is positioned
   
   
   procedure Clear_Column (The_Surface: in out Surface;
                           Column     : in     Cursor_Ordinal)
      is abstract;
   
   procedure Clear_Columns (The_Surface : in out Surface;
                            First_Column: in     Cursor_Ordinal;
                            Last_Column : in     Cursor_Ordinal)
      is abstract;
   -- Clears the selected Column(s)
   
   procedure Clear_Column (The_Surface: in out Surface'Class);
   -- Clears the column on which The_Surface.Current_Cursor is positioned
   

   procedure Clear_To_End (The_Surface: in out Surface;
                           From       : in     Cursor'Class) is abstract;
   
   procedure Clear_To_End (The_Surface: in out Surface'Class);
   -- Clears to the end of the line from the Current Cursor, or a user-supplied
   -- Cursor, inclusive of the position under the Cursor.
   --
   -- If the Cursor is beyond the extents of the Surface, no action is taken.
   
   -- Clear and Clear_To_End has no effect if the Surface is not allocated.
   -- -- Suppresses All Exceptions --
   
   
   procedure Put (The_Surface   : in out Surface;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
     is abstract
     with Pre'Class => (Check_Graphic (Content));
   
   procedure Put (The_Surface   : in out Surface'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
     with Pre => (Check_Graphic (Content));
   
   -- Wide_String
   procedure Wide_Put
     (The_Surface   : in out Surface;
      Set_Cursor    : in out Cursor'Class;
      Content       : in     Wide_String;
      Justify       : in     Justify_Mode          := Left;
      Overflow      : in     Overflow_Mode         := Truncate;
      Advance_Cursor: in     Boolean               := False;
      Wide_Fallback : access 
        function (Item: Wide_String) return String := null)
     is abstract
   with Pre'Class => (Wide_Check_Graphic (Content));
   
   procedure Wide_Put
     (The_Surface   : in out Surface'Class;
      Content       : in     Wide_String;
      Justify       : in     Justify_Mode          := Left;
      Overflow      : in     Overflow_Mode         := Truncate;
      Advance_Cursor: in     Boolean               := False;
      Wide_Fallback : access 
        function (Item: Wide_String) return String := null)
   with Pre => (Wide_Check_Graphic (Content));
   
   -- Puts the Content at the location of the selected cursor. If no Cursor is 
   -- provided, the selected cursor is the active cursor for the Surface.
   --
   -- If Advance_Cursor is True, Set_Cursor is re-positioned to one position 
   -- past the end of Content, as written on the screen, if there is room, or 
   -- else at the last character to be output. Any exception results in
   -- no change to Set_Cursor under any circumstance.
   --
   -- If the Wide_String override is invoked on a Surface type that does not
   -- support Wide_Strings, the Wide_Fallback is invoked to convert the input
   -- string before redispatching to the String Put override.
   --
   -- If Wide_Fallback is null (default), and Wide_Strings are not supported by
   -- the Surface type, the Curses_Library exception is propegated.
   --
   -- -- All Possible Exceptions --
   -- * Assertion_Error    : Content contained non-graphic characters
   -- * Surface_Unavailable: Raised if Surface is not Available
   -- * Cursor_Excursion   : Raised if Overflow is set to Error or Wrap_Error, 
   --                        and there was insufficient space on the surface to
   --                        accomodate the Content
   -- * Curses_Library     : - Wide_String Put not supported, and no
   --                          substitution fallback specified, or;
   --                        - Any unexpected internal error or exception
   
   
   procedure Fill (The_Surface: in out Surface;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class)
     is abstract
     with Pre'Class => (for all C of Pattern => C in Graphic_Character);
   
   procedure Fill (The_Surface: in out Surface'Class;
                   Pattern    : in     String)
     with Pre => (for all C of Pattern => C in Graphic_Character);
   
   
   -- Wide_String support
   procedure Wide_Fill (The_Surface  : in out Surface;
                        Pattern      : in     Wide_String;
                        Fill_Cursor  : in     Cursor'Class;
                        Wide_Fallback: access 
                          function (Item: Wide_String) return String := null)
     is abstract
     with Pre'Class => (for all C of Pattern => C in Wide_Graphic_Character);
   
   procedure Wide_Fill (The_Surface  : in out Surface'Class;
                        Pattern      : in     Wide_String;
                        Wide_Fallback: access 
                          function (Item: Wide_String) return String := null)
     with Pre => (for all C of Pattern => C in Wide_Graphic_Character);
   -- Fills the entire surface with the Pattern provided, starting at
   -- Row 1, Column 1, and proceeding left-right, top-bottom. Styling is
   -- applied by the Fill_Cursor. If no Fill_Cursor, the Current_Cursor of the
   -- Surface is used.
   --
   -- -- All Possible Exceptions --
   -- * Assertion_Error    : Raised if Pattern contains non-graphic characters
   --                        (Via dispatching calls to Put)
   -- * Surface_Unavailable: Raised if The_Surface is not Available
   -- * Curses_Library     : - Wide_String Fill not supported, and no
   --                          substitution fallback specified, or;
   --                        - Any unexpected internal error or exception

   
   procedure Set_Background (The_Surface   : in out Surface;
                             Fill_Character: in     Graphic_Character := ' ';
                             Fill_Cursor   : in     Cursor'Class)
      is abstract;
   
   procedure Set_Background (The_Surface   : in out Surface'Class;
                             Fill_Character: in     Graphic_Character := ' ');
   
   procedure Wide_Set_Background
     (The_Surface   : in out Surface;
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class;
      Wide_Fallback : access function (Item: Wide_Character) 
                                      return Character := null)
     is abstract;
   
   procedure Wide_Set_Background 
     (The_Surface   : in out Surface'Class;
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Wide_Fallback : access function (Item: Wide_Character) 
                                      return Character := null);
   -- Sets the background to the Fill_Character, with the style of the 
   -- Fill_Cursor.
   -- 
   -- If Fill_Cursor is not specified, The_Surface's Current Cursor is used.
   --
   -- The Set_Background subprogram group allows for special low-level calls to
   -- the binding which registers the background character and color, meaning 
   -- that it may operate completely differently from a Fill operation.
   --
   -- If implemented Set_Background is not cleared with a call to Clear, unlike
   -- for Fill.
   --
   -- Some care should be exercised if setting the Fill_Character to anything
   -- besides the default, as this may also replace all instances of the
   -- previously set "background character" with the new one. In most cases,
   -- this will replace all existing spaces with the new Fill_Character
   --
   -- Not all implementations of Surface'Class can support low-level 
   -- implementation of Set_Background. In these cases, Set_Background must be
   -- emulated by the Surface implementation, by dispatching to Fill, and
   -- re-dispatching to Fill appropriately after any Clear operation (including
   -- more specific Clear_* operations).
   --
   -- -- All Possible Exceptions --
   -- *  Assertion_Error    : Raised (by the Static_Preciate) if Fill_Character
   --                         is not a Graphic_Character
   -- *  Surface_Unavailable: Raised if Surface is not Available
   -- *  Curses_Library     : - Wide_Graphic_Character Set_Background not
   --                           supported, and no substitution fallback
   --                           specified; or,
   --                         - Unable to set background due to an error in the
   --                           Curses library; or,
   --                         - The implementation is not available for the
   --                           Surface.
   
   
   procedure Set_Border (The_Surface: in out Surface;
                         Use_Cursor : in     Cursor'Class)
     is abstract;
   
   
   procedure Set_Border (The_Surface: in out Surface'Class);
   
   
   procedure Set_Border (The_Surface: in out Surface;
                         Use_Cursor : in     Cursor'Class;
                         
                         Left_Side,
                         Right_Side,
                         Top_Side,
                         Bottom_Side,
                           
                         Top_Left_Corner,
                         Top_Right_Corner,
                         Bottom_Left_Corner,
                         Bottom_Right_Corner: in Graphic_Character)
     is abstract;
   
   procedure Set_Border (The_Surface: in out Surface'Class;
                         
                         Left_Side,
                         Right_Side,
                         Top_Side,
                         Bottom_Side,
                           
                         Top_Left_Corner,
                         Top_Right_Corner,
                         Bottom_Left_Corner,
                         Bottom_Right_Corner: in Graphic_Character);
   
   
   -- Wide_Character extension
   procedure Wide_Set_Border (The_Surface: in out Surface;
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
                                    return Character := null)
     is abstract;

   
   procedure Wide_Set_Border (The_Surface: in out Surface'Class;
                         
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
   
   -- Sets the (inner) border of The_Surface. If Use_Cursor is not specified,
   -- the Current_Cursor of The_Surface is used. If the specific characters for
   -- all sides and corners are not specified, the implementation-defined
   -- default is used.
   --
   -- Where possible, the implementation should attempt to apply proper line-
   -- drawing techniques, such as via the (n)curses library, or via
   -- Wide_String Put (if supported) using the unicode "box drawing" codepage
   -- (U2500-257F).
   --
   -- -- All Possible Exceptions --
   -- * Assertion_Error    : Raised (by the subtype predicate) if any of the
   --                        border drawing characters are not graphic
   -- * Surface_Unavailable: Raised if The_Surface is not Available
   -- * Curses_Library     : - Wide_Set_Border not supported,
   --                          and no substitution fallback specified; or,
   --                        - Any unexpected internal error or exception
   
   procedure Sample_Position
     (Source       : in out Surface;
      Position     : in     Cursor_Position;
      Content      :    out Graphic_Character;
      Styled_Cursor: in out Cursor'Class)
     is abstract;
   
   procedure Wide_Sample_Position 
     (Source       : in out Surface;
      Position     : in     Cursor_Position;
      Content      :    out Wide_Graphic_Character;
      Styled_Cursor: in out Cursor'Class)
     is abstract;
   
   function Sample_Position_Cursor (Source  : in out Surface;
                                    Position: in     Cursor_Position)
                                   return Cursor'Class
     is abstract;
   -- Samples a Character and it's style from a specific position on a Surface.
   --
   -- Sample_Cursor allows for the initialization of a Cursor'Class object to
   -- the most capable derrivation for the Surface.
   --
   -- If the Surface does not support Wide_Characters, Wide_Sample simply
   -- sets Character to a Wide_Character that corresponds to a Character.
   --
   -- Styled_Cursor is of mode in out since Sample_Position cannot be expected
   -- to have full knowledge of the Cursor type passed to is, and so cannot
   -- ensure full initialization. At a minimum Sample_Position sets
   -- Styled_Cursor.Position to Position, and Styled_Cursor.Style appropriately
   --
   -- If the particular override has knoweldge of additional Cursor types, it
   -- may configure those aspects separately
   --
   -- -- All Possible Exceptions --
   -- *  Surface_Unavailable: Source is not available
   -- *  Cursor_Excursion   : Position is beyond Source.Extents
   -- *  Curses_Library     : Any unexpected internal error
                              
   
     
   procedure Transcribe (Source   : in out Surface;
                         Target   : in out Surface'Class;
                         Source_TL: in     Cursor_Position;
                         Source_BR: in     Cursor_Position;
                         Target_TL: in     Cursor_Position;
                         Clip     : in     Boolean := False)
     is abstract
     with Pre'Class => Source_BR >= Source_TL
                       and then Source_BR <= Source.Extents
                       and then Target_TL <= Target.Extents;

   
   -- Copies the content from the Source Surface to the Target Surface,
   -- with the Source region identified with Source_TL (Top-Left), and
   -- Source_BR (Bottom-Right).
   --
   -- If Clip is True, and the Target cannot contain the full rectangle to be
   -- transcribed from source, the source rectangle is clipped to the largest
   -- size that can be accommodated by both Target. Note that this
   -- is only possible if Target_TL is within the Target's Extents. 
   -- If Clip is set to False, a Cursor_Excursion exception will be raised. 
   --
   -- If the Target Surface is not compatible with the Source Surface's
   -- implementation of Transcribe, Transcribe shall directly transfer the
   -- character content only, using the Current_Cursor of the Target Surface
   --
   -- -- All Possible Exceptions --
   -- * Assertion_Error    :  Precondition was violated
   -- * Surface_Unavailable: The From or To surface (or both) are not Available
   -- * Cursor_Excursion   : - The Source rectangle could not fit on the Target
   --                          and Clip was False
   --                        - Either Source or Target changed size during the
   --                          operation
   -- * Curses_Library     : Any unexpected internal error or exception
                         
   
   --
   -- Infrastructural Types
   --
   
   -----------------
   -- File_Handle --
   -----------------
   type File_Descriptor(<>) is limited private;
   type File_Handle(<>)     is limited private;
   type File_Mode(<>)       is limited private;
     
   
   function Handle_Valid (Handle: File_Handle) return Boolean
     with Inline;
   -- Verifies if the handle is valid.
   
   procedure Invalidate_Handle (Handle: in out File_Handle);
   -- Invalidates a handle
   
   function Invalid_Handle return File_Handle
     with Inline;
   -- Returns an invalid handle
   
      
   -----------------
   -- Line_Handle --
   -----------------
   type Line_Handle(<>) is limited private;
   -- Used by Terminal objects in conjunction with a Line_Device to obtain
   -- access to a line which can then be exchanged for a Terminal_Handle
   -- through the Binding package. This record represents the only
   -- "OS-dependent" data structure that is exported from the Binding package.
   --
   -- The default values for Line_Handle always represent an invalid handle.
   
   function "=" (Left, Right: Line_Handle) return Boolean
     with Inline;
   
   function Handle_Valid (Handle: Line_Handle) return Boolean
     with Inline;
   -- Verifies if the handle is valid.
   
   procedure Invalidate_Handle (Handle: in out Line_Handle);
   -- Invalidates a handle
   
   function Invalid_Handle return Line_Handle
     with Inline;
   -- Returns an invalid handle
   

   ---------------------
   -- Terminal_Handle --
   ---------------------
   type Terminal_Handle(<>) is limited private;  
   -- Carried by every Terminal object - refers to the initialized terminal
   -- actively managed by the (n)curses library
   
   function Handle_Valid (Handle: Terminal_Handle) return Boolean
     with Inline;
   -- Verifies if the handle is valid.
   
   procedure Invalidate_Handle (Handle: in out Terminal_Handle)
     with Inline;
   -- Invalidates a handle
   
   function Invalid_Handle return Terminal_Handle
     with Inline;
   -- Returns an invalid handle
   
   
   --------------------
   -- Surface_Handle --
   --------------------
   type Surface_Handle(<>) is limited private;
   -- Carried by every Curses_Surface object 
   
   function Handle_Valid (Handle: Surface_Handle) return Boolean
     with Inline;
   -- Verifies if the handle is valid.
   
   procedure Invalidate_Handle (Handle: in out Surface_Handle);
   -- Invalidates a handle
   
   function Invalid_Handle return Surface_Handle
     with Inline;
   -- Returns an invalid handle
   
   
private
   use Interfaces.C;
   use type System.Address;
   
   -------------
   -- Surface --
   -------------
   
   -- Surface Properties --
   ------------------------
   protected type Surface_Properties is
      function  Visible                 return Boolean;
      entry     Wait_Visible;
      
      procedure Arm                     (Armed: in Boolean);
      -- Has no effect if the Surface is currently Visible
      function  Armed                   return Boolean;
      entry     Wait_Armed;
      
      procedure Armed_To_Visible;
      -- Visible is set to True of Arm is True, Arm is then set to False.
      -- Otherwise, there is no effect.
      
      procedure Visible_To_Armed;
      -- Arm is set to True if Visible is True. Visible is then set to False.
      -- Otherwise, there is no effect.
      
      procedure Withdraw;
      -- Visible and Arm are both set to false
      
      procedure Clipped                 (Set: in Boolean);
      function  Clipped                 return Boolean;
      entry     Wait_Clipped;
      
      procedure Modified                (Set: in Boolean);
      function  Modified                return Boolean;
      entry     Wait_Modified           (Clear: in Boolean);
      
      procedure Extents                 (New_Extents: in Cursor_Position);
      function  Extents                 return Cursor_Position;
      
      function  Extents_Changed        return Boolean;
      procedure Clear_Extents_Changed;
      entry     Wait_Extents_Changed   (Clear: in Boolean);
      
      
      -- Refresh Hints --
      -------------------
      procedure Hint_Armed              (Changed: out Boolean);
      procedure Hint_Presented          (Changed: out Boolean);
      procedure Hint_Withdrawn          (Changed: out Boolean);
      procedure Hint_Modified           (Changed: out Boolean);
      procedure Hint_Extents            (Changed: out Boolean);
      procedure Hint_Clipping           (Changed: out Boolean);

      -- These properties are for internal use only, by the Terminal refresh
      -- process to more efficiently determine visibility and redraw 
      -- requirements. 
      --
      -- Hint_Armed is True if the Surface transitioned from not Visible and not
      -- Armed to Armed.
      --
      -- Hint_Presented is True if Armed_To_Visible was _successfully_ invoked 
      -- at any time since the last call to Hint_Presented - this means an
      -- actual transition from Armed to Visible
      --
      -- Hint_Withdrawn is True if Withdraw was called, while the Surface was 
      -- Visible, at any time since the last call to Hint_Withdrawn.
      --
      -- Hint_Modified is True if Modified was set to True at any time since the
      -- last call to Hint_Modified.
      --
      -- Hint_Extents is True if Extents_Changed was set to True at any time
      -- since the last call to Hint_Extents.
      --
      -- Hint_Moved is True if Set_Moved was called at any time since the last
      -- call to Hint_Moved
      --
      -- Hint_Clipping is set True if Clipped _changed_ at any time since the 
      -- last call to Hint_Clipping

      -- All hints properties are reset to False by a query on their value.
      
   private
      Is_Visible     : Boolean := False;
      Is_Armed       : Boolean := False;
      Is_Clipped     : Boolean := False;
      Is_Modified    : Boolean := False;
      Changed_Extents: Boolean := False;
      
      Extent         : Cursor_Position;
      
      Armed_Hint     : Boolean := False;
      Presented_Hint : Boolean := False;
      Withdrawn_Hint : Boolean := False;
      Modified_Hint  : Boolean := False;
      Extents_Hint   : Boolean := False;
      Clipping_Hint  : Boolean := False;
   end Surface_Properties;
   
   type Surface_Access is access all Surface'Class;
   type Surface is abstract limited new Limited_Controlled with
      record
         Properties: Surface_Properties;         
         
         -- Surface_Rack use only
         Next, Prev: aliased Surface_Access := null;
      end record;
   -- Next and Prev, while off-limits to anyone except designated Layer_Rack
   -- objects, should assumed to be potentially modified dynamically, and often
   -- through an Access type set via an Unchecked_Access attribute, thus these
   -- values must be explicitly aliased to ensure that objects of
   -- Surface'Class are always updated correctly. It should be noted,
   -- however, that the RM specifies that all formal parameters and objects of
   -- a tagged type shall have an aliased view. Applying aliased to Next and
   -- Prev certainly has no negative impact!
   

   -- Function Expression Completions --
   -------------------------------------
   function Visible (The_Surface: in out Surface) return Boolean is
      (The_Surface.Properties.Visible);
   
   function Armed (The_Surface: in out Surface) return Boolean is
      (The_Surface.Properties.Armed);
      
   function Clipped (The_Surface: in out Surface) return Boolean is
      (The_Surface.Properties.Clipped);
      
   function Modified (The_Surface: in out Surface) return Boolean is
      (The_Surface.Properties.Modified);
      
   function Extents (The_Surface: in out Surface'Class) 
                    return Cursor_Position is
      (The_Surface.Properties.Extents);
      
   function Extents_Changed (The_Surface: in out Surface) return Boolean is
      (The_Surface.Properties.Extents_Changed);
   

   --
   -- Handles
   -- 
   
   type File_Descriptor is new int;
   type File_Mode is
     (Read, 
      Write, 
      Write_Append,
      Random,
      Random_Append);
   
   type File_Handle     is new System.Address; -- FILE *
   type Terminal_Handle is new System.Address; -- SCREEN *
   type Surface_Handle  is new System.Address; -- WINDOW *
   -- Teaching Note --
   -- We fully exploit one key concept of the Ada type system:
   -- though all three of these handles are all regular address pointers, they
   -- represent different things, and cannot be mixed. This is a great lesson
   -- in the practical application of two types that have the exact same
   -- physical representation, but which represent two completely different
   -- things that are fundamentally incompatible. This is often not obvious
   -- without a practical example such as this.
   --
   -- Another interesting note, and common "criticism" of such a strong type
   -- system lies in the use of functions and procedures to check, create, and
   -- manage "null" handles. In fact, this again shows many strengths of Ada.
   -- The problem originally being: we can't declare constants for Terminal/
   -- Surface_Handles since we 1) know that null handles of these types are 
   -- physically equivalent to null pointers (System.Null_Address), and 
   -- therefore we'd need to invoke a type conversion, which would require a
   -- check at elaboration, which would then make this package
   -- non-preelaborable.
   --
   -- It is a better option to take advantage of Ada 2012 function expressions
   -- to inline validation tests in a way that does not require recompilation
   -- of the body. This way we can abstract null handle checking completely,
   -- and any re-implementation where handles are not simple pointers, would be
   -- trivial to implement.
   --
   -- The really neat thing is how overloading works. So at the end of the
   -- day, if we have a function which returns a Handle, and needs to return
   -- a "null" (Invalid) handle, one simply writes "return Invalid_Handle",
   -- viola.
   
   -----------------
   -- Line_Handle --
   -----------------
   type Line_Handle is
      record
         Input, Output: File_Handle;
      end record;
   
   function "=" (Left, Right: Line_Handle) return Boolean
   is ((Left.Input  = Right.Input) and then (Left.Output = Right.Output));
   
   
   ------------------
   -- Handle_Valid --
   ------------------
   function Handle_Valid(Handle: File_Handle) return Boolean
   is (Handle /= File_Handle(System.Null_Address));
   
   function Handle_Valid(Handle: Line_Handle) return Boolean
   is (Handle /= Line_Handle'(others => <>));
   
   function Handle_Valid(Handle: Terminal_Handle) return Boolean
   is (Handle /= Terminal_Handle(System.Null_Address));
   
   function Handle_Valid(Handle: Surface_Handle) return Boolean
   is (Handle /= Surface_Handle(System.Null_Address));
   
   
   --------------------
   -- Invalid_Handle --
   --------------------
   function Invalid_Handle return File_Handle
   is (File_Handle(System.Null_Address));
   
   function Invalid_Handle return Line_Handle
   is (Line_Handle'(others => <>));
   
   function Invalid_Handle return Terminal_Handle
   is (Terminal_Handle(System.Null_Address));
   
   function Invalid_Handle return Surface_Handle
   is (Surface_Handle(System.Null_Address));
   
   
end Curses;
