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

-- Notes about the implementation of color. --
----------------------------------------------
-- ** KEY NOTES **
--
-- ** Custom colors should be "Predefined" as constants, before the elaboration
--    of any Terminal objects within the partition. All Color_Swatches and 
--    Color_Style definitions are frozen once the first Terminal object is
--    elaborated.
--
-- ** Invoking Predefine_Swatch or Predefine_Color returns
--    Default_Swatch/_Style if any Terminal objects within the partition have
--    been initialized already.
--
-- ** Elaboration of Swatches and Styles is guaranteed to suppress any/all
--    exceptions.
--
-- Ancillary Notes
------------------
-- 1. Not all terminals support color. For terminals that don't, the default
--    color "Default_Color_Style" is always represented on the terminal as the
--    standard FG/BG pair. (In curses, this is COLOR_PAIR 0). If the 
--    Color_Style is set to any other style, somehow, on a terminal that does
--    not support color, it is interpreted automatically as Default_Color.
--
-- 2. Whenever the Inverted property is True for a Cursor_Style, this
--    automatically inverts the Color_Style of the Cursor. So any Color_Style
--    applied to a Colored_Cursor which is also Inverted, causes the Background
--    Color_Swatch to be used as the Foreground, and vice-versa.
--
-- 3. Some attributes, such as Bold can cause color changes when some default
--    colors are used, particularly with VGA displays.
--
-- 4. This package supports direct operation of multiple terminals. In order to
--    maintain maximum intuitiveness and usability, Color_Styles are common to
--    all terminals. This is not how the curses library naively handles color
--    styles. Therefore, this comes at a trade-off in the form of a hard limit
--    on the number of colors and color pairs that may be defined.
--
--    The default hard limits for user definable Swatches and Styles are user-
--    configurable, and set by the constants:
--    *  Swatch_Index_Limit (default 255), and
--    *  Style_Index_Limit  (default 255)
--
-- 5. Swatches and Styles cannot be deleted. If a user-defined Color_Swatch or
--    Color_Style object passes out of scope, and is not used elsewhere, it 
--    simply becomes unselectable, but remains defined.
--
-- Rationale
------------
-- The color system in curses is specific to the terminal. In order to abstract
-- this to be more global to the partition, and sequitur across any valid
-- Terminal object in the partition, significant abstraction machinery is
-- required.
--
-- There are many strategies which may be selected to implement this
-- abstraction machinery. One strategy involved dynamic updating of all active
-- Terminals once a new Swatch or Style was declared. This was decided to be
-- undesirable for three reasons:
--
--    1. This would encourage more disorganized, decentralized color control,
--       which if not properly disciplined could be unwieldy
--    2. Any user of the package could have global performance impacts merely
--       by defining new swatches.
--    3. Presenting the swatch/style definition facilities as unrestricted user
--       operations could lead to fast swatch/style depletion, unpredictable
--       behavior due to such depletion (similar to a memory-leak), and 
--       deteriorating performance overall.
--
-- It was therefore decided that Swatch/Style definition should be limited
-- "predefinitions" which must happen at partition elaboration, which must
-- precede the initialization of any Terminal objects within the partition.
--
-- Such a strategy forces better design, and creates more consistent
-- experience, and ensuring long-term stability and predictability throughout
-- the life of the partition.
--
-- It is recommended in the case of "user definable colors" to predefine a set
-- of global user color swatches/styles (e.g. User_Swatch_1), which can then be
-- locally overridden for a given Terminal.


with Interfaces.C;
with Curses.Indexed_Set;

private with Curses.Binding.Color;

package Curses.Terminals.Color is
   
   -----------------
   -- User Limits --
   -----------------
   Swatch_Index_Limit: constant := 255;
   Style_Index_Limit : constant := 255;
   -- The user limits represent the maximum indexed swatch and styles
   -- that may be Predefined or Referenced when declaring partition-global
   -- Color_Swatch'es and Color_Style's
   --
   -- Only Color_Swatch'es may be explicitly referenced by their underlying
   -- index value, and thus the following configuration values relate to that.
   -- The following values should not be modified.
   
   First_User_Swatch_Index: constant := 8;
   -- Indicates the first index that is not referred to by one of the default
   -- Color_Swatch'es defined in this package.
   
   subtype Swatch_Reference_Index is Integer 
     range First_User_Swatch_Index .. Swatch_Index_Limit;
   -- Swatch_References exclude the standard default colors, as this package
   -- includes defaults for those indexes.
   
   ---------------
   -- RGB_Value --
   ---------------
   type RGB_Value is range 0 .. 1_000;
   -- The Curses library specifically sets this range. See
   -- curs_color(3X)
   
   ------------------
   -- Color_Swatch --
   ------------------
   type Color_Swatch is private;
   
   function Predefine (Red, Green, Blue: in RGB_Value)
                      return Color_Swatch;
   -- ** Note **
   -- All Color_Swatch objects that are initialized through the evaluation of
   -- Predefine must be initialized before any Terminal objects are initialized
   -- anywhere in the partition.
   --
   -- The rationale behind this behavior is to enforce proper application of
   -- the Color package, and of Color_Swatch and Color_Style objects more
   -- generally. 
   --
   -- The goal of this implementation is to allow Color_Swatch values to be
   -- freely assignable to Cursor_Style's throughout the partition, which would
   -- be the naturally expected (ideal) behavior. However, the underlying
   -- implementation of colors in the (n)curses library does not operate in
   -- this fashion, especially when handling multiple terminals.
   --
   -- In order to provide the expected behavior, the one caveat is that there
   -- be a locally tracked, limited set of defined Color_Swatches. Allowing any
   -- "user" of the Color package to define colors could quickly cause
   -- exhaustion of the global palette.
   --
   -- This function should be used to define a set of global Swatches at
   -- partition elaboration, in order to provide the expected functionality
   -- reliably and consistently. This is enforced by the specific limitation
   -- that all Swatch definitions are frozen once the first Terminal object is
   -- initialized anywhere in the partition.
   --
   -- User-configurable colors on a per-terminal basis should be managed
   -- through the global definition of a set of constant "user" swatches, where
   -- are then appropriately Overridden on a per-Terminal basis.
   --
   -- Predefine selects the lowest available index value which has not yet been
   -- claimed through Predefine or Reference
   --
   -- Predefine has two possible failure modes: A frozen palette (Terminals
   -- have been elaborated), or exhaustion of available user swatch
   -- definitions. In both cases, Predefine will return Default_Swatch.
   --
   -- -- Suppresses All Exceptions --
   
   
   function Reserve (Index: Swatch_Reference_Index) return Color_Swatch;
   -- Reserves a specific reference index of an assumed pre-existent palette
   --
   -- Reservations exclude later Predefinitions, and fail in the event of
   -- earlier Predefinitions. Therefore:
   -- *** ALL RESERVATIONS SHOULD BE EXECUTED BEFORE ANY PREDEFINITIONS ***
   --
   -- Restrictions are same as for the direct RGB Predefine operation.
   --
   -- It should be of particular note that pre-defined swatches may have an
   -- inconsistent actual value across different terminals.
   --
   -- If the Swatch_Reference_Index is invalid at the underlying library level,
   -- has already been predefined, or reserved, Default_Swatch is returned.
   --
   -- -- Suppresses All Exceptions --
   
   
   
   procedure Decompose (Swatch          : in  Color_Swatch;
                        TTY             : in  Terminal;
                        Red, Green, Blue: out RGB_Value);
   -- Returns the actual RGB_Values as understood by the subject Terminal for
   -- the specified Swatch. This is useful to query the "native" representation
   -- of Default swatch compositions on any given terminal (if supported)
   -- 
   -- If the subject Terminal is unable to support all predefined swatches, and
   -- the subject Swatch is outside of the Terminal's limitation, the result
   -- will be as if Swatch was set to Default_Color 
   --
   -- -- All Possible Exceptions:
   -- *  Curses_Library   : The operation could not be executed at the
   --                       (n)curses library level, or other unexpected
   --                       condition
   
   
   procedure Override (Swatch          : in Color_Swatch;
                       TTY        : in Terminal;
                       Red, Green, Blue: in RGB_Value);
   -- Overrides a swatch for a specified Terminal only. 
   --
   -- If the terminal does not support color, the operation has no effect, and
   -- no exception is raised.
   --
   -- -- All Possible Exceptions:
   -- *  Curses_Library: An unexpected error occurred during the operation,
   --                    or an unexpected exception was raised.
   
   
   -- Default_Swatches --
   ----------------------
   Default_Color: constant Color_Swatch; -- Default Terminal color
   -- Note that the availability of Default_Color is dependent on the
   -- capabilities of the underlying terminal, and/or the capabilities
   -- of the underlying (n)curses library. If Default_Color is not
   -- available, the Terminal object will report a Degraded_Palette condition.
   
   -- The actual RGB values are set according to the typical xterm values
   
   Black  : constant Color_Swatch; -- COLOR_BLACK
   Red    : constant Color_Swatch; -- COLOR_RED
   Green  : constant Color_Swatch; -- COLOR_GREEN
   Yellow : constant Color_Swatch; -- COLOR_YELLOW
   Blue   : constant Color_Swatch; -- COLOR_BLUE
   Magenta: constant Color_Swatch; -- COLOR_MAGENTA
   Cyan   : constant Color_Swatch; -- COLOR_CYAN
   White  : constant Color_Swatch; -- COLOR_WHITE


   -- Additional Predefinitions are available through inclusions of child
   -- packages to this
   
   -----------------
   -- Color_Style --
   -----------------
   type Color_Style is private;
   -- the Color_Style object represents a Foreground-Background color pair
   
   
   function Predefine (Foreground, Background: Color_Swatch)
                      return Color_Style;
   -- Operates according to the same principals as Predefine.
   --
   -- If invoked after any Terminal object has been elaborated, Default_Style
   -- is returned.
   --
   -- -- Suppresses All Exceptions
   
   
   function  Background (Style: in Color_Style) return Color_Swatch;
   function  Foreground (Style: in Color_Style) return Color_Swatch;
   
   procedure Decompose (Style     : in     Color_Style;
                        Foreground:    out Color_Swatch;
                        Background:    out Color_Swatch);
   -- Background, Foreground and Decompose extract the registered Background or
   -- Foreground Color_Swatches for the referenced Color_Style
   
   procedure Override (Style     : in Color_Style;
                       TTY  : in Terminal;
                       Foreground: in Color_Swatch;
                       Background: in Color_Swatch);
   -- Overrides a color pair for the specified Terminal.
   --
   -- The Default_Style may not be overridden.
   -- 
   -- Unlike for Swatches, the Reference_Style must be Global, or Local to the
   -- terminal. 
   --
   -- If the Reference_Style parameter contains a Local_Style foreign to the
   -- target style, or the target style is Default_Style, a Constraint_Error is
   -- raised.
   --
   -- If the terminal does not support color, the operation has no effect, and
   -- no exception is raised.
   --
   -- If the terminal does not support the specific Color_Style, due to style 
   -- index limitations, the operation has no effect, and no exception is
   -- raised
   --
   -- -- All Possible Exceptions:
   -- *  Curses_Library  : An unexpected error occurred during the operation,
   --                      or an unexpected exception was raised.
   
   
   function Supports_Color_Style (TTY: Terminal'Class; Style: Color_Style)
                                 return Boolean;
   -- Returns True if Style is expected to be supported by the capabilities of
   -- Terminal.
   --
   -- This function also checks for general Color capability of the Terminal, 
   -- and thus returns False if the Terminal is monochrome.
   -- -- Suppresses All Exceptions --
   
   
   -- Default Color Styles --
   --------------------------
   Default_Color_Style: constant Color_Style;
   
   --
   -- Colored Cursor Extension
   --
   
   --------------------
   -- Colored_Cursor --
   --------------------
   type Colored_Cursor is new Curses.Cursor with
      record
         Color: Color_Style := Default_Color_Style;
      end record;
   -- The Colored_Cursor type extends the basic Cursor with an additional Color
   -- attribute. Where the parent Cursor type is used on a Terminal which
   -- supports Color, Default_Color_Style is assumed.
   
   
   ------------------------------
   -- Colored_Cursor_Container --
   ------------------------------
   -- This type creates a definite type for storing either a regular Cursor,
   -- or a Colored_Cursor.
   type Colored_Cursor_Container (Is_Colored: Boolean := False) is
      record
         case Is_Colored is
            when True =>
               Colored   : Terminals.Color.Colored_Cursor;
            when False =>
               Monochrome: Cursor := (others => <>);
         end case;
      end record;
   
   
   --
   -- Package internal support subprograms
   --
   
   ---------------------
   -- Install_Palette --
   ---------------------
   procedure Install_Palette (TTY: in out Terminal);
   -- Installs the complete set of Color_Swatches and Color_Styles that have
   -- been predefined for the partition via Predefine (if possible). Also
   -- updates the Terminal object's palette status as appropriate.
   --
   -- This procedure is invoked automatically at Terminal elaboration if a 
   -- Terminal object successfully receives command of a terminal device.
   --
   -- A package user should have no need to invoke this subprogram, however
   -- doing so is harmless.
   --
   -- -- Suppresses All Exceptions
   
   
   -----------------------
   -- Apply_Color_Style --
   -----------------------
   procedure Apply_Color_Style (Handle: in Surface_Handle;
                                Style : in Color_Style);
   -- Applies a specific color style for any characters added to the referenced
   -- Surface.
   --
   -- This procedure is only invoked internally, and is not usable by the
   -- user, since Surface_Handle objects are never exposed to the user.
   --
   -- Specifically, this is invoked by the Surface implementation whenever a
   -- Cursor's style is applied to a Surface under a Terminal which indicates
   -- support for color
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Unable to apply style, or unexpected error
   
   
   ------------------------------
   -- Apply_Colored_Background --
   ------------------------------
   procedure Apply_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Character;
      Reference_Cursor: in Colored_Cursor'Class);
   
   -- Wide_Character support
   procedure Wide_Apply_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Wide_Character;
      Reference_Cursor: in Colored_Cursor'Class);
   
   -- Dispatched from the Surface package (Set_Background) when the Surface's
   -- Terminal indicates support for color, and the Reference_Cursor was a 
   -- member of Colored_Cursor'Class.
   --
   -- This procedure is only invoked internally, and is not usable by the
   -- user, since Surface_Handle objects are never exposed to the user.
   --
   -- Calling this procedure with an invalid handle has no effect, and will not
   -- raise an exception
   --
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Unable to apply style, wide support not configured, 
   --                    or other unexpected error
   
   
   --------------------------
   -- Apply_Colored_Border --
   --------------------------
   procedure Apply_Colored_Border (Handle          : in Surface_Handle;
                                   Reference_Cursor: in Colored_Cursor'Class);
   -- Default border
   
   
   procedure Apply_Colored_Border (Handle: in Surface_Handle;
                                   Reference_Cursor: in Colored_Cursor'Class;
                                   LS, RS, TS, BS,
                                   TL, TR, BL, BR  : in Character);
   -- User-defined border
   
   -- Wide support
   procedure Wide_Apply_Colored_Border
     (Handle: in Surface_Handle;
      Reference_Cursor: in Colored_Cursor'Class;
      LS, RS, TS, BS,
      TL, TR, BL, BR  : in Wide_Character);
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Unable to apply style, wide support not configured, 
   --                    or other unexpected error

     
private
   use Curses.Binding.Color;
   
   ------------------
   -- Color_Swatch --
   ------------------
   subtype Valid_Swatch_Index is CURSES_Color
     range -1 .. CURSES_Color'Last;
   -- All possible swatches, according to the range of CURSES_Color
   
   subtype User_Swatch_Index is Valid_Swatch_Index
     range First_User_Swatch_Index .. Swatch_Index_Limit;
   -- All Predefinable Color_Swatches
   
   subtype Default_Swatch_Index is Valid_Swatch_Index
     range Valid_Swatch_Index'First .. (User_Swatch_Index'First - 1);
   -- All default swatches.
   -- This declaration ensures that Swatch_Index_Limit is not larger than the
   -- maximum range of CURSES_Color, without forcing us to expose CURSES_Color
   -- publicly!
   
   type Color_Swatch is
      record
         Index           : Valid_Swatch_Index;
         Red, Green, Blue: RGB_Value;
      end record;
   -- Notice how we don't have default values. This is OK since these objects
   -- cannot be declared without initialization by the user.
   
   -- Default Swatches
   -- RGB Values are as per xterm, but arn't actually used or referenced
   Default_Color: constant Color_Swatch   -- As per man pages
     := (Index => -1, Red => 127, Green => 127, Blue => 127); 
   
   Black  : constant Color_Swatch   -- COLOR_BLACK
     := (Index =>  0, Red =>   0, Green =>   0, Blue =>   0);
   Red    : constant Color_Swatch   -- COLOR_RED
     := (Index =>  1, Red => 205, Green =>   0, Blue =>   0);
   Green  : constant Color_Swatch   -- COLOR_GREEN
     := (Index =>  2, Red =>   0, Green => 205, Blue =>   0);
   Yellow : constant Color_Swatch   -- COLOR_YELLOW 3
     := (Index =>  3, Red => 205, Green => 205, Blue =>   0);
   Blue   : constant Color_Swatch   -- COLOR_BLUE
     := (Index =>  4, Red =>   0, Green =>   0, Blue => 238);
   Magenta: constant Color_Swatch   -- COLOR_MAGENTA
     := (Index =>  5, Red => 205, Green =>   0, Blue => 205);
   Cyan   : constant Color_Swatch   -- COLOR_CYAN
     := (Index =>  6, Red =>   0, Green => 205, Blue => 205);
   White  : constant Color_Swatch   -- COLOR_WHITE 7
     := (Index =>  7, Red => 229, Green => 229, Blue => 229);
   
   
   -- Swatch_Set --
   ----------------
   -- By instantiating this generic package here, in the package specification,
   -- we ensure that this package (specifically the User Limits) can be changed
   -- and recompiled without needing to recompile the package body.
   
   package Swatch_Set is new Curses.Indexed_Set
     (Unit         => Color_Swatch, 
      Unit_Index   => User_Swatch_Index,
      Default_Unit => Default_Color);
   
   
   -----------------
   -- Color_Style --
   -----------------
   subtype Valid_Style_Index is CURSES_Color_Pair
     range 0 .. CURSES_Color_Pair'Last;
   -- Same rationality as for the definition of Color_Swatch
   
   subtype Default_Style_Index is Valid_Style_Index
     range Valid_Style_Index'First .. 0;
   -- Represents the default styles
   
   subtype User_Style_Index is Valid_Style_Index
     range (Default_Style_Index'Last + 1) .. Style_Index_Limit;

   
   type Color_Style is
      record
         Index     : Valid_Style_Index := Valid_Style_Index'First;
         Foreground: Color_Swatch      := White;
         Background: Color_Swatch      := Black;
      end record;
   
   -- Inline Operations --
   -----------------------
   function  Background (Style: in Color_Style) return Color_Swatch is
      (Style.Background);
   function  Foreground (Style: in Color_Style) return Color_Swatch is
      (Style.Foreground);
   
   -- Default Style
   Default_Color_Style: constant Color_Style   -- Default colors pair
     := (others => <>);
   -- This is really speculative, but this is the most "pure"
   
   -- Style_Set --
   ---------------
   package Style_Set is new Curses.Indexed_Set
     (Unit         => Color_Style, 
      Unit_Index   => User_Style_Index,
      Default_Unit => Default_Color_Style);
   
   
end Curses.Terminals.Color;
