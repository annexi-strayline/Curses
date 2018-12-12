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
with Curses.Binding.Color;

package body Curses.Terminals.Color is
   
   --
   -- Internal Infrastructure
   -- 
   
   ------------------
   -- Palette_Lock --
   ------------------
   -- Locked by any Terminal object initialization procedure that finds the lock
   -- unlocked
   --
   -- This could be an Atomic variable, but we want to ensure portability.
   -- Atomic is defined in Annex C of the RM, and so is optional for an 
   -- implementation. However, the Palette_Lock is only checked when Terminals
   -- are elaborated, or when Predefine is invoked for a Swatch or Style. All of
   -- these conditions are (or should be) rare. There is not much of a
   -- performance worry here.
   
   protected Palette_Lock is
      function  Locked return Boolean;
      procedure Lock;
      
   private
      Lock_State: Boolean := False;
      
   end Palette_Lock;
   
   
   protected body Palette_Lock is
      function Locked return Boolean is (Lock_State);
      
      procedure Lock is begin
         Lock_State := True;
      end Lock;
      
   end Palette_Lock;
   
   
   --
   -- Generic Operations
   --
   

   -- Generic_Predefine --
   -----------------------
   -- Generic procedure to define a new set unit (Swatch or Style)
   
   generic
      with package Unit_Set is new Curses.Indexed_Set(<>);
      use Unit_Set;
      
      with function Initialize_Unit (Index: Unit_Index)
                                    return Unit;
   function Generic_Predefine return Unit;
   
   
   function Generic_Predefine return Unit is
      New_Index: Unit_Index;
      OK       : Boolean := False;
      
   begin
      Set.Reserve (Index   => New_Index,
                   Success => OK);
      
      if not OK then
         return Default_Unit;
      end if;
      
      -- We got one, Register it and return
      return New_Unit: Unit := Initialize_Unit (New_Index) do
        Set.Register (Index    => New_Index, 
                      New_Unit => New_Unit,
                      Success  => OK);
        
        if not OK then
           -- Shouldn't be possible
           New_Unit := Default_Unit;
        end if;
      end return;
      
   exception
      when others =>
         return Default_Unit;
         -- This should be impossible. It's ok if it isn't.
      
   end Generic_Predefine;
   
   
   -- Generic_Iterate_Registered --
   --------------------------------
   -- Iterates over the Registered_List
   generic
      with package Unit_Set is new Curses.Indexed_Set(<>);
      use Unit_Set;
      
      with procedure Process_Unit (The_Unit: in Unit);
   procedure Generic_Iterate_Registered;
   
   
   procedure Generic_Iterate_Registered is
      Index: Unit_Index;

   begin
      if not Set.Have_Registered then
         -- No Units registered
         return;
      end if;
      
      -- Set-up the loop (starting at the most recently registered, since
      -- Indexed_Sets always prepended)
      Index := Set.First_Registered;
      
      loop
         Process_Unit (Set.Query(Index));
         
         exit when Index = Unit_Index'First;
         -- Remember that Index_Sets are always "prepended", so
         -- the last item on the list has the lowest index
         
         Index := Set.Next_Registered (Index);
      end loop;
      
   end Generic_Iterate_Registered;
   
   
   --
   -- Color_Swatch and Color_Style Operations
   --
   
   ---------------
   -- Predefine --
   ---------------
   function Predefine (Red, Green, Blue: RGB_Value)
                   return Color_Swatch
   is
      function Initialize_Swatch (Index: User_Swatch_Index)
                                 return Color_Swatch is
        (Index => Index, Red => Red, Green => Green, Blue => Blue);
         
      function Define is new Generic_Predefine
        (Unit_Set => Swatch_Set, Initialize_Unit => Initialize_Swatch);
      
   begin
      return Define;
   end Predefine;
   
   ----------------------------------------
   function Predefine (Foreground, Background: in Color_Swatch)
                      return Color_Style
   is
      function Initialize_Style (Index: User_Style_Index)
                                return Color_Style is
        (Index => Index, Foreground => Foreground, Background => Background);
         
      function Define is new Generic_Predefine
        (Unit_Set => Style_Set, Initialize_Unit => Initialize_Style);
      
   begin
      return Define;
   end Predefine;
   
   
   -------------
   -- Reserve --
   -------------
   function Reserve (Index: Swatch_Reference_Index) return Color_Swatch
   is
      use Swatch_Set;
      
      Actual_Index: User_Swatch_Index; -- Specified at Swatch_Set instantiation
      OK          : Boolean;
   begin
      -- It shouldn't be possible to fail this conversion
      Actual_Index := User_Swatch_Index (Index);
      
      -- Attempt reservation
      Set.Claim (Index   => Actual_Index,
                 Success => OK);
      
      if not OK then
         -- Failed to claim!
         return Default_Color;
      end if;
         
      -- The only significant part of the swatch we return is the Index itself.
      -- The RBG values have no significance, and won't actually be used.
      return (Index => Actual_Index, others => 255);
      
   exception
      when others =>
         return Default_Color;
      
   end Reserve;


   
   ---------------
   -- Decompose --
   ---------------
   procedure Decompose (Swatch          : in Color_Swatch;
                        TTY             : in Terminal;
                        Red, Green, Blue: out RGB_Value)
   is
      R,G,B      : CURSES_RGB;
      
   begin
      -- This overload is quite different. We need to go down to the binding and
      -- make a direct inquiry
      
      -- First ensure the terminal is active
      if not TTY.Available then
         raise Curses_Library with "Terminal not available";
      end if;
      
      -- Binding call
      Decompose_Color (TTY   => TTY.Handle,
                       Color => Swatch.Index,
                       Red   => R,
                       Green => G,
                       Blue  => B);
      
      -- Raises Curses_Library if something went wrong with the library call
      
      Red   := RGB_Value(R);
      Green := RGB_Value(G);
      Blue  := RGB_Value(B);
      
   exception
      when Curses_Library =>
         raise;
         
      when Constraint_Error =>
         raise Curses_Library with "RGB Value conversion failed";
         
      when others =>
         raise Curses_Library with "Unexpected exception";
      
   end Decompose;
   
   
   ------------------------------------------------------------
   procedure Decompose (Style     : in     Color_Style;
                        Foreground:    out Color_Swatch;
                        Background:    out Color_Swatch)
   is
   begin
      Foreground := Style.Foreground;
      Background := Style.Background;
   end Decompose;
   
   
   --------------
   -- Override --
   --------------
   procedure Override (Swatch          : in Color_Swatch;
                       TTY             : in Terminal;
                       Red, Green, Blue: in RGB_Value)
   is
      R,G,B: CURSES_RGB;
      
   begin
      -- This is the real-deal: finally a call to the binding!
      
      -- First ensure the terminal is active
      if not TTY.Available then
         raise Curses_Library with "Terminal not available";
      end if;
      
      -- Convert colors
      R := CURSES_RGB (Red);
      G := CURSES_RGB (Green);
      B := CURSES_RGB (Blue);
      
      Set_Color (TTY   => TTY.Handle,
                 Color => Swatch.Index,
                 Red   => R,
                 Green => G,
                 Blue  => B);
      
   exception
      when Curses_Library =>
         raise;
         
      when Constraint_Error =>
         raise Curses_Library with
           "Color value/binding conversion failure.";
         
      when others =>
         raise Curses_Library with
           "Unexpected exception in Override";
      
   end Override;
   
   
   ------------------------------------------------------------
   procedure Override (Style     : in Color_Style;
                       TTY       : in Terminal;
                       Foreground: in Color_Swatch;
                       Background: in Color_Swatch)
   is
      Set_FG: Color_Swatch := Foreground;
      Set_BG: Color_Swatch := Background;
   begin
      
      if not TTY.Available then
         raise Curses_Library with "Terminal not available";
      end if;
      
      -- If the Terminal doesn't support Default_Colors, we need to switch-out
      -- and Default_Color with appropriate non-defaults (Black + White)
      if not TTY.Supports_Default_Color then
         if Set_FG = Default_Color then
           Set_FG := White;
           
         end if;
         
         if Set_BG = Default_Color then
            Set_BG := Black;
            
         end if;
      end if;
      
      Set_Pair (TTY        => TTY.Handle,
                Pair       => Style.Index,
                Foreground => Set_FG.Index,
                Background => Set_BG.Index);
      
      
   exception
      when Curses_Library =>
         raise;
         
      when others =>
         raise Curses_Library with
           "Unexpected exception in Override";
      
   end Override;
   
   
   --
   -- Terminal Compatibility Support Functions
   --
   
   function Supports_Color_Style (TTY: Terminal'Class; Style: Color_Style)
                                 return Boolean
   is
   begin
      if not TTY.Supports_Color then
         return False;
      end if;
      
      -- Next we match the style against various criterium, depending on the
      -- Style index value.
      
      if Style.Index > Valid_Style_Index (TTY.Max_Styles) then
         -- This index is beyond what is reported to be supported by this
         -- Terminal.
         return False;
         
      else
         -- Shouold be ok
         return True;
         
      end if;
      
   exception
      when others => return False;
         
   end Supports_Color_Style;
   
   
   --
   -- Package internal support subprograms
   --
   
   ---------------------
   -- Install_Palette --
   ---------------------
   procedure Install_Palette (TTY: in out Terminal)
   is
      procedure Install_Swatch (Swatch: in Color_Swatch) is
      begin
         Override (Swatch => Swatch,
                   TTY    => TTY,
                   Red    => Swatch.Red,
                   Green  => Swatch.Green,
                   Blue   => Swatch.Blue);
         
      exception
         when others =>
            -- Mark as degraded and move on
            if not TTY.Degraded_Palette then
               TTY.Status.Note_Palette_Degraded;
            end if;
            
      end Install_Swatch;
      
      
      procedure Install_Style (Style: in Color_Style) is
      begin
         Override (Style      => Style,
                   TTY        => TTY,
                   Foreground => Style.Foreground,
                   Background => Style.Background);
         
      exception
         when others =>
            -- Mark as degraded and move on
            if not TTY.Degraded_Palette then
               TTY.Status.Note_Palette_Degraded;
            end if;
      end Install_Style;
      
      
      procedure Install_Swatch_Set is new Generic_Iterate_Registered
        (Unit_Set => Swatch_Set, Process_Unit => Install_Swatch);
      
      procedure Install_Style_Set is new Generic_Iterate_Registered
        (Unit_Set => Style_Set, Process_Unit => Install_Style);
      
   begin
      -- First ensure the proper terminal preconditions, and set the
      -- appropriate status flags
      
      if not TTY.Available then
         -- Non-starter
         return;
      elsif not TTY.Color_Capable then
         -- Not even color capable!
         TTY.Status.Note_Palette_Limited;
         return;
         
      end if;
      
      
      if        not TTY.Supports_Default_Color
        or else not TTY.Supports_Custom_Colors
      then
         -- We can proceed. If we have no custom colors, we will only try to
         -- styles
         TTY.Status.Note_Palette_Limited;
         
      end if;
      
      
      -- Install all registered (Predefined) swatches (if possible) and styles 
      -- on the terminal
      if TTY.Supports_Custom_Colors then
         Install_Swatch_Set;
      end if;
      
      Install_Style_Set;
      
   exception
      when others =>
         TTY.Status.Note_Palette_Degraded;
   end Install_Palette;
   
   
   -----------------------
   -- Apply_Color_Style --
   -----------------------
   procedure Apply_Color_Style (Handle: in Surface_Handle;
                                Style : in Color_Style)
   is
   begin
      Apply_Pair (Handle => Handle, Pair => Style.Index);
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Apply_Color_Style: Unexpected exception: " &
           Exceptions.Exception_Information (e);

   end Apply_Color_Style;
   
   
   ------------------------------
   -- Apply_Colored_Background --
   ------------------------------
   procedure Apply_Colored_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Character;
      Reference_Cursor: in Colored_Cursor'Class)
   is
      use Binding.Color;
      
   begin
      Set_Colored_Background 
        (Handle           => Handle,
         Blank_Character  => Blank_Character,
         Reference_Cursor => Reference_Cursor,
         Color            => Reference_Cursor.Color.Index);
      
   exception
      when Curses_Library => 
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Apply_Colored_Background;
   
   
end Curses.Terminals.Color;
