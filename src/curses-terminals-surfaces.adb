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

with Ada.Exceptions; use Ada;
with Ada.Assertions;

with Curses.Put_Computer;
with Curses.Binding.Terminals;
with Curses.Binding.Render;
with Curses.Binding.Render.Wide;
with Curses.Terminals.Color;


package body Curses.Terminals.Surfaces is
   
   --
   -- Protected_Cursor Protected Type
   -- 
   
   protected body Protected_Cursor is
      
      procedure Set (New_Cursor: in Cursor'Class) is
         use Curses.Terminals.Color;
         
         Compare_Old: Cursor_Position;
      begin
         case Container.Is_Colored is
            when True =>
               Compare_Old := Container.Colored.Position;
               
            when False =>
               Compare_Old := Container.Monochrome.Position;
         end case;
         
         if Compare_Old /= New_Cursor.Position then
            -- Change in position
            Position_Changed_Hint := True;
         end if;
         
         if New_Cursor in Colored_Cursor'Class then
            Container := Colored_Cursor_Container'
              (Is_Colored => True,
               Colored    => Colored_Cursor (New_Cursor));
            
         else
            Container := Colored_Cursor_Container'
              (Is_Colored => False,
               Monochrome => Cursor (New_Cursor));
            
         end if;
      end Set;
      
      procedure Set_Position (New_Position: in Cursor_Position) is
         Last_Position: Cursor_Position;
      begin
         case Container.Is_Colored is
            when True =>
               Last_Position := Container.Colored.Position;
               Container.Colored.Position := New_Position;
               
            when False =>
               Last_Position := Container.Monochrome.Position;
               Container.Colored.Position := New_Position;
               
         end case;
         
         if Last_Position /= New_Position then
            Position_Changed_Hint := True;
         end if;
         
      end Set_Position;

      function Get return Cursor'Class is
      begin
         if Container.Is_Colored then
            return Container.Colored;
         else
            return Container.Monochrome;
         end if;
      end Get;
      
      
      procedure Position_Changed (Changed: out Boolean) is
      begin
         Changed := Position_Changed_Hint;
         Position_Changed_Hint := False;
      end Position_Changed;
      
   end Protected_Cursor;
   
   
   --
   -- Focus_State Protected Type
   --
   
   protected body Focus_State is
      function Has_Focus return Boolean is (Focused);
      
      procedure Has_Focus (Set: in Boolean) is
      begin
         Focused := Set;
      end Has_Focus;
      
      entry Wait_Focus when Focused is begin null;
      end Wait_Focus;
   end Focus_State;
   
   
   --
   -- Internal Subprograms
   --
   
   ------------------
   -- Set_Modified --
   ------------------
   -- Set_Modified is used in place of the direct 
   -- Surface.Properties.Set_Modified route, for the purpose of intercepting
   -- any members of Rendered_Surface'Class, which if Visible, need to trigger
   -- the Terminal's Refresh task.
   procedure Set_Modified (The_Surface: in out Terminal_Surface'Class) 
     with Inline is
   begin
      The_Surface.Properties.Modified (True);
      
      if The_Surface in Rendered_Surface'Class 
        and then (The_Surface.Visible or else The_Surface.Armed)
      then
         The_Surface.TTY.Refresh.Queue_Refresh;
      end if;
   end Set_Modified;
            
   
   ---------------
   -- Set_Style --
   ---------------
   -- Sets the style attributes, and color pair for the cursor
   -- -- All Possible Exceptions --
   -- *  Curses_Library
   
   procedure Set_Style (The_Surface: in out Terminal_Surface'Class;
                        Set_Cursor : in     Cursor'Class) 
   is
      use Binding.Render;
   begin
      -- First, we go ahead and set the regular attributes, these are set
      -- regardless of the colour capabilities of the Terminal
      
      Binding.Render.Set_Attributes (Handle => The_Surface.Handle,
                                     Style  => Set_Cursor.Style);
      
      -- Check for a Colored Cursor.
      -- Ensure that the selected Color_Style is actually supported by
      -- TTY behind The_Surface. This also checks general color capability
      -- of the terminal
      
      if Set_Cursor in Terminals.Color.Colored_Cursor'Class 
        and then Terminals.Color.Supports_Color_Style 
        (TTY   => The_Surface.TTY.all,
         Style => Terminals.Color.Colored_Cursor (Set_Cursor).Color)
      then
         Terminals.Color.Apply_Color_Style 
           (Handle => The_Surface.Handle,
            Style  => Terminals.Color.Colored_Cursor (Set_Cursor).Color);
      end if;
      
      
   exception
      when Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Set_Style - Unexpected exception: " & 
           Exceptions.Exception_Information (e);
      
   end Set_Style;
   
   
   -------------------------
   -- Limit_Add_Positions --
   -------------------------
   -- Blocks any possible (very unlikely) Ordinal overflow from adding two
   -- Cursor_Positions
   function Limit_Add_Positions (Left, Right: Cursor_Position) 
                                return Cursor_Position
     with Inline
   is
   begin
      return Result: Cursor_Position do
        declare begin
           Result.Row := Left.Row + Right.Row;
        exception
           when Constraint_Error =>
              Result.Row := Cursor_Ordinal'Last;
        end;
        
        declare begin
           Result.Column := Left.Column + Right.Column;
        exception
           when Constraint_Error =>
              Result.Column := Cursor_Ordinal'Last;
        end;
              
      end return;
   end Limit_Add_Positions;
   
   
   --
   -- Surface State and Properties
   -- 
   
   ------------------
   -- Wide_Support --
   ------------------
   function Wide_Support (The_Surface: Terminal_Surface) return Boolean
     is (Binding.Render.Wide.Wide_Support_Configured);
   -- Defer to the Binding package for Wide_Support
   
   --------------------
   -- Current_Cursor --
   --------------------
   -- Note that setting the current Cursor, does not, in of itself, set the 
   -- actual physical cursor on the terminal. Instead, this is handled by the 
   -- actual TTY refresh task, who ensures the physical cursor matches the 
   -- current cursor of the currently active surface.
   --
   -- This procedure simply updates the internal Surface state with the
   -- notional "Current_Cursor"
   
   overriding
   procedure Current_Cursor (The_Surface: in out Terminal_Surface;
                             New_Cursor : in     Cursor'Class)
   is
   begin
      if New_Cursor.Position > The_Surface.Extents then
         raise Cursor_Excursion with
           "New cursor would be beyond the edge of the surface";
      end if;
      
      The_Surface.Cursor_State.Set (New_Cursor);
      
   exception
      when Cursor_Excursion =>
         raise;
         
      when others =>
         null;
         
   end Current_Cursor;
   
   
   ---------------------
   -- Position_Cursor --
   ---------------------
   overriding
   procedure Position_Cursor(The_Surface: in out Terminal_Surface;
                             Position   : in     Cursor_Position)
   is
      Mod_Cursor: Cursor'Class := The_Surface.Cursor_State.Get;

   begin
      Mod_Cursor.Position := Position;
      The_Surface.Cursor_State.Set (Mod_Cursor);
      
   exception
      when Cursor_Excursion =>
         raise;
         
      when others =>
      -- Noting that, if the Surface is not Available, then any position is
      -- "ok", since it has no effect, thus we trap Surface_Unavailable
         null;
         
   end Position_Cursor;
   
   
   -----------
   -- Clear --
   -----------
   overriding
   procedure Clear (The_Surface: in out Terminal_Surface) is
      type T_O is new Tasking_Order with null record;
      
      procedure Execute (Order: in out T_O) is
         use Curses.Binding.Render;
      begin
         Clear_Surface (Handle => The_Surface.Handle);
      end Execute;
      
      Order: T_O;
      
   begin
      if not The_Surface.Available then
         return;
      end if;
      

      The_Surface.TTY.Liaison.Assign (Order);
      Set_Modified (The_Surface);
      
   exception
      when others => null;
      
   end Clear;
   
   
   ---------------
   -- Clear_Row --
   ---------------
   overriding
   procedure Clear_Row (The_Surface: in out Terminal_Surface;
                        Row        : in     Cursor_Ordinal)
   is
   begin
      -- Clear_To_End will handle Freezing the surface appropriately
      The_Surface.Clear_To_End 
        (From => Cursor'(Position => (Row    => Row, 
                                      Column => Cursor_Ordinal'First),
                         others   => <>));
      
   exception
      when others => null;
      
   end Clear_Row;
   
   
   ----------------
   -- Clear_Rows --
   ----------------
   overriding
   procedure Clear_Rows (The_Surface: in out Terminal_Surface;
                         First_Row  : in     Cursor_Ordinal;
                         Last_Row   : in     Cursor_Ordinal)
   is
   begin
      for I in First_Row .. Last_Row loop
         -- Note how if Last_Row is less than First_Row, we'll just have
         -- a null range, and therefore no loop
         The_Surface.Clear_Row (I);
      end loop;
      
   exception
      when others => null;
      
   end Clear_Rows;
   
   ------------------
   -- Clear_Column --
   ------------------
   overriding
   procedure Clear_Column (The_Surface: in out Terminal_Surface;
                           Column     : in     Cursor_Ordinal)
   is
      type T_O is new Tasking_Order with null record;
      
      procedure Execute (Order: in out T_O) is
         use Curses.Binding.Render;
         
         Last_Row: constant Cursor_Ordinal := The_Surface.Extents.Row;
      begin
         for I in Cursor_Ordinal'First .. Last_Row loop
            Place_Cursor (Handle => The_Surface.Handle,
                          Position => (Row => I, Column => Column));
            Clear_Character (Handle => The_Surface.Handle);
         end loop;
         
      end Execute;
      
      Order: T_O;
        
      
   begin
      if not The_Surface.Available then
         return;
      end if;
      
      The_Surface.TTY.Liaison.Assign (Order);
      Set_Modified (The_Surface);
      
   exception
      when others =>
         null;
      
   end Clear_Column;
   
   
   -------------------
   -- Clear_Columns --
   -------------------
   overriding
   procedure Clear_Columns (The_Surface : in out Terminal_Surface;
                            First_Column: in     Cursor_Ordinal;
                            Last_Column : in     Cursor_Ordinal)
   is
   begin
      for I in First_Column .. Last_Column loop
         The_Surface.Clear_Column (I);
      end loop;
      
   exception
      when others => null;
   end Clear_Columns;
   
   
   ------------------
   -- Clear_To_End --
   ------------------
   overriding
   procedure Clear_To_End (The_Surface: in out Terminal_Surface;
                           From       : in     Cursor'Class)
   is
      use Curses.Binding.Render;
      
      type T_O is new Tasking_Order with null record;
      
      procedure Execute (Order: in out T_O) is
         use Curses.Binding.Render;
      begin
         
         Place_Cursor (Handle   => The_Surface.Handle,
                       Position => From.Position);
         Clear_To_End_Of_Line (The_Surface.Handle);
         
      end Execute;
      
      Order: T_O;
   begin
      if not The_Surface.Available
        or else From.Position > The_Surface.Extents
      then
         -- Surface is not Available, or From is beyond extents. In either
         -- case, we can't do anything.
         return;
      end if;
      
      The_Surface.TTY.Liaison.Assign (Order);
      Set_Modified (The_Surface);
      
   exception
      when others => null;
         
   end Clear_To_End;
   
   
   ---------
   -- Put --
   ---------
   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
   
      with procedure Binding_Put_String (Handle: in Surface_Handle;
                                         Buffer: in String_Type);
   
   procedure Generic_Put (The_Surface   : in out Terminal_Surface'Class;
                          Set_Cursor    : in out Cursor'Class;
                          Content       : in     String_Type;
                          Justify       : in     Justify_Mode;
                          Overflow      : in     Overflow_Mode;
                          Advance_Cursor: in     Boolean);
   
   
   procedure Generic_Put (The_Surface   : in out Terminal_Surface'Class;
                          Set_Cursor    : in out Cursor'Class;
                          Content       : in     String_Type;
                          Justify       : in     Justify_Mode;
                          Overflow      : in     Overflow_Mode;
                          Advance_Cursor: in     Boolean)
   is
      Write_Head: Cursor_Position := Set_Cursor.Position;
      
      package Computer is new Put_Computer
        (Character_Type => Character_Type,
         String_Type    => String_Type,
      
         The_Surface    => Surface'Class (The_Surface),
         Write_Head     => Write_Head,
         Justify        => Justify,
         Overflow       => Overflow,
         Content        => Content);
      
      
      -- Tasking_Order --
      -------------------
      -- We position the actual cursor as per the justification before writing,
      -- and then write the Selected_Region to the position
      
      type T_O is new Tasking_Order with null record;
      
      procedure Execute (Order: in out T_O) is
         Select_First: Natural renames Computer.Select_First;
         Select_Last : Natural renames Computer.Select_Last;
         
      begin
         -- We are simply executing each computed line. The Put_Computer has
         -- already computed the Position of the Write_Cursor, so we move
         -- there, and execute the actual put. The purpose of the Tasking_Order
         -- is to ensure that the Place_Cursor (remember - that is
         -- terminal-wide), is atomically paired with Set_Style and Put_String.
           
         Set_Style 
           (The_Surface => The_Surface,
            Set_Cursor  => Set_Cursor);
         
         Binding.Render.Place_Cursor
           (Handle   => The_Surface.Handle,
            Position => Write_Head);
         
         Binding_Put_String 
           (Handle => The_Surface.Handle,
            Buffer => Content(Select_First .. Select_Last));
         
         Set_Modified (The_Surface);
         
      end Execute;
      
      Order: T_O;

   begin
      if not The_Surface.Available then
         raise Surface_Unavailable with
           "Surface not Available";
      end if;
      
      -- Most of the work is done by the Put_Computer. We simply pass it to the
      -- Tasking_Order
      
      while Computer.Compute_Line loop
         The_Surface.TTY.Liaison.Assign (Order);
      end loop;
      
      if Advance_Cursor then
         Set_Cursor.Position := Computer.After_Write;
      end if;
         
   exception
      when Surface_Unavailable | Cursor_Excursion | Curses_Library =>
         if Advance_Cursor then
            Set_Cursor.Position := Computer.After_Write;
         end if;
         
         raise;
         
      when e: others =>
         if Advance_Cursor then
            Set_Cursor.Position := Computer.After_Write;
         end if;
         
         raise Curses_Library with 
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Generic_Put;
   
   
   -- Standard String
   overriding
   procedure Put (The_Surface   : in out Terminal_Surface;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
   is 
      procedure Put_Actual is new Generic_Put
        (Character_Type     => Character,
         String_Type        => String,
         Binding_Put_String => Binding.Render.Put_String)
        with Inline;
   begin
      Put_Actual (The_Surface    => The_Surface,
                  Set_Cursor     => Set_Cursor,
                  Content        => Content,
                  Justify        => Justify,
                  Overflow       => Overflow,
                  Advance_Cursor => Advance_Cursor);
   end Put;
   
   
   -- Wide_Put
   overriding
   procedure Wide_Put (The_Surface   : in out Terminal_Surface;
                       Set_Cursor    : in out Cursor'Class;
                       Content       : in     Wide_String;
                       Justify       : in     Justify_Mode      := Left;
                       Overflow      : in     Overflow_Mode     := Truncate;
                       Advance_Cursor: in     Boolean           := False;
                       Wide_Fallback : access 
                         function (Item: Wide_String) return String := null)
   is
      procedure Put_Actual is new Generic_Put
        (Character_Type     => Wide_Character,
         String_Type        => Wide_String,
         Binding_Put_String => Binding.Render.Wide.Put_Wide_String)
        with Inline;
   begin
      if not Binding.Render.Wide.Wide_Support_Configured then
         if Wide_Fallback = null then
            raise Curses_Library with
              "Binding not configured with Wide_String support, " &
              "and no Wide_Fallback provided";
         else
            The_Surface.Put (Set_Cursor     => Set_Cursor,
                             Content        => Wide_Fallback (Content),
                             Justify        => Justify,
                             Overflow       => Overflow,
                             Advance_Cursor => Advance_Cursor);
         end if;
      else
         
         Put_Actual (The_Surface    => The_Surface,
                     Set_Cursor     => Set_Cursor,
                     Content        => Content,
                     Justify        => Justify,
                     Overflow       => Overflow,
                     Advance_Cursor => Advance_Cursor);
         
      end if;
   end Wide_Put;
   
   ----------
   -- Fill --
   ----------
   generic
      type Character_Type is (<>);
      type String_Type is array (Positive range <>) of Character_Type;
      
      with procedure Put_Pattern (The_Surface: in out Terminal_Surface'Class;
                                  Use_Cursor : in out Cursor'Class;
                                  Pattern    : in     String_Type);
      -- Should dispatch to Put with the following configuration:
      -- Justify        => Left,
      -- Overflow       => Wrap_Truncate
      -- Advance_Cursor => True
   
   procedure Generic_Fill (The_Surface: in out Terminal_Surface;
                           Pattern    : in     String_Type;
                           Fill_Cursor: in     Cursor'Class);
   
   
   procedure Generic_Fill (The_Surface: in out Terminal_Surface;
                           Pattern    : in     String_Type;
                           Fill_Cursor: in     Cursor'Class)
   is
      pragma Assertion_Policy (Pre'Class => Ignore);
      -- For the graphic character requirements to The_Surface.Put, which
      -- are already covered by the Pre'Class precondition for Fill
      
      Use_Cursor: Cursor'Class := Fill_Cursor;
      
   begin
      -- Check for silliness, don't waste any time with it
      if Pattern'Length < 1 then
         return;
      end if;
      
      Use_Cursor.Position := (1, 1);
      
      loop
         Put_Pattern (The_Surface => The_Surface,
                      Use_Cursor  => Use_Cursor,
                      Pattern     => Pattern);
         
         exit when Use_Cursor.Position >= The_Surface.Extents;
      end loop;
      
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with "Unexpected exception: " &
           Exceptions.Exception_Information (e);
         
   end Generic_Fill;
   ----------------------------------------
   
   
   overriding
   procedure Fill (The_Surface: in out Terminal_Surface;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class)
   is
      procedure Put_Pattern (The_Surface: in out Terminal_Surface'Class;
                             Use_Cursor : in out Cursor'Class;
                             Pattern    : in     String)
      with Inline is
      begin
         The_Surface.Put (Set_Cursor     => Use_Cursor,
                          Content        => Pattern,
                          Justify        => Left,
                          Overflow       => Wrap_Truncate,
                          Advance_Cursor => True);
         
      end Put_Pattern;
      
      procedure Fill_Actual is new Generic_Fill
        (Character_Type => Character,
         String_Type    => String,
         Put_Pattern    => Put_Pattern)
        with Inline;
   begin
      
      Fill_Actual (The_Surface => The_Surface,
                   Pattern     => Pattern,
                   Fill_Cursor => Fill_Cursor);
      
   end Fill;
   
   
   -- Wide_Fill
   overriding
   procedure Wide_Fill (The_Surface  : in out Terminal_Surface;
                        Pattern      : in     Wide_String;
                        Fill_Cursor  : in     Cursor'Class;
                        Wide_Fallback: access 
                          function (Item: Wide_String) return String := null)
   is
      procedure Put_Pattern (The_Surface: in out Terminal_Surface'Class;
                             Use_Cursor : in out Cursor'Class;
                             Pattern    : in     Wide_String)
      with Inline is
      begin
         The_Surface.Wide_Put (Set_Cursor     => Use_Cursor,
                               Content        => Pattern,
                               Justify        => Left,
                               Overflow       => Wrap_Truncate,
                               Advance_Cursor => True,
                               Wide_Fallback  => Wide_Fallback);
         
      end Put_Pattern;
      
      procedure Fill_Actual is new Generic_Fill
        (Character_Type => Wide_Character,
         String_Type    => Wide_String,
         Put_Pattern    => Put_Pattern)
        with Inline;
   begin
      Fill_Actual (The_Surface => The_Surface,
                   Pattern     => Pattern,
                   Fill_Cursor => Fill_Cursor);
   end Wide_Fill;
   
   
   --------------------
   -- Set_Background --
   --------------------
   generic
      type Character_Type is (<>);
   
      with procedure Apply_Colored_Background 
        (Handle          : in Surface_Handle;
         Blank_Character : in Character_Type;
         Reference_Cursor: in Terminals.Color.Colored_Cursor'Class);
   
      with procedure Apply_Monochrome_Background
        (Handle          : in Surface_Handle;
         Blank_Character : in Character_Type;
         Reference_Cursor: in Cursor'Class);
   
   procedure Generic_Set_Background 
     (The_Surface   : in out Terminal_Surface'Class;
      Fill_Character: in     Character_Type;
      Fill_Cursor   : in     Cursor'Class);
   
   
   procedure Generic_Set_Background
     (The_Surface   : in out Terminal_Surface'Class;
      Fill_Character: in     Character_Type;
      Fill_Cursor   : in     Cursor'Class)
   is begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      -- Similar to Set_Style, we need to consider if the Fill_Cursor is a
      -- Colored_Cursor, and if so, whether or not the TTY behind the Surface
      -- can actually handle that particular Color_Style.
      
      if Fill_Cursor in Terminals.Color.Colored_Cursor'Class
        and then Terminals.Color.Supports_Color_Style
        (TTY   => The_Surface.TTY.all,
         Style => Terminals.Color.Colored_Cursor (Fill_Cursor).Color)
      then
         Apply_Colored_Background
           (Handle           => The_Surface.Handle,
            Blank_Character  => Fill_Character,
            Reference_Cursor => Terminals.Color.Colored_Cursor (Fill_Cursor));
         -- Invokes the appropriate binding after processing the color
         -- information
         
      else
         -- Monochrome!
         Apply_Monochrome_Background
           (Handle           => The_Surface.Handle,
            Blank_Character  => Fill_Character,
            Reference_Cursor => Fill_Cursor);
         
      end if;
      
      Set_Modified (The_Surface);
         
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Set_Background: Unexpected exception: " &
           Exceptions.Exception_Information (e);
      
   end Generic_Set_Background;
   ----------------------------------------
   
   
   overriding
   procedure Set_Background (The_Surface   : in out Terminal_Surface;
                             Fill_Character: in     Graphic_Character := ' ';
                             Fill_Cursor   : in     Cursor'Class)
   is
      procedure Set_Background_Actual is new Generic_Set_Background
        (Character_Type 
           => Character,
         Apply_Colored_Background 
           => Terminals.Color.Apply_Colored_Background,
         Apply_Monochrome_Background 
           => Binding.Render.Set_Monochrome_Background);
   begin
      Set_Background_Actual (The_Surface    => The_Surface,
                             Fill_Character => Fill_Character,
                             Fill_Cursor    => Fill_Cursor);
   end Set_Background;
   
   
   -- Wide_Set_Background
   overriding
   procedure Wide_Set_Background
     (The_Surface   : in out Terminal_Surface;
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class;
      Wide_Fallback : access function (Item: Wide_Character) 
                                      return Character := null)
   is
      procedure Set_Background_Actual is new Generic_Set_Background
        (Character_Type 
           => Wide_Character,
         Apply_Colored_Background 
           => Terminals.Color.Wide_Apply_Colored_Background,
         Apply_Monochrome_Background 
           => Binding.Render.Wide.Wide_Set_Monochrome_Background);
   begin
      if not Binding.Render.Wide.Wide_Support_Configured then
         if Wide_Fallback = null then
            raise Curses_Library with
              "Binding not configured with Wide_String support, " &
              "and no Wide_Fallback provided";
         else
            The_Surface.Set_Background
              (Fill_Character => Wide_Fallback (Fill_Character),
               Fill_Cursor    => Fill_Cursor);
         end if;
      else
         
         Set_Background_Actual (The_Surface    => The_Surface,
                                Fill_Character => Fill_Character,
                                Fill_Cursor    => Fill_Cursor);
      end if;
   end Wide_Set_Background;
   
   
   ----------------
   -- Set_Border --
   ----------------
   overriding
   procedure Set_Border (The_Surface: in out Terminal_Surface;
                         Use_Cursor : in     Cursor'Class)
   is
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      if Use_Cursor in Terminals.Color.Colored_Cursor'Class
        and then Terminals.Color.Supports_Color_Style
          (TTY   => The_Surface.TTY.all,
           Style => Terminals.Color.Colored_Cursor (Use_Cursor).Color)
      then
         Terminals.Color.Apply_Colored_Border
           (Handle           => The_Surface.Handle,
            Reference_Cursor => Terminals.Color.Colored_Cursor (Use_Cursor));
      else
         Binding.Render.Set_Default_Monochrome_Border
           (Handle           => The_Surface.Handle,
            Reference_Cursor => Use_Cursor);
      end if;
      
      Set_Modified (The_Surface);
      
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Set_Background: Unexpected exception: " &
           Exceptions.Exception_Information (e);
      
   end Set_Border;
   ----------------------------------------
   
   
   generic
      type Character_Type is (<>);
      
      with procedure Apply_Colored_Border
        (Handle          : in Surface_Handle;
         Reference_Cursor: in Terminals.Color.Colored_Cursor'Class;
         LS, RS, TS, BS,
         TL, TR, BL, BR  : in Character_Type);
      
      with procedure Apply_Monochrome_Border
        (Handle           : in Surface_Handle;
         Reference_Cursor: in Cursor'Class;
         LS, RS, TS, BS, 
         TL, TR, BL, BR  : in Character_Type);
   
   procedure Generic_Set_Border
     (The_Surface: in out Terminal_Surface;
      Use_Cursor : in     Cursor'Class;
      
      Left_Side,
      Right_Side,
      Top_Side,
      Bottom_Side,
        
      Top_Left_Corner,
      Top_Right_Corner,
      Bottom_Left_Corner,
      Bottom_Right_Corner: in Character_Type);
   
   procedure Generic_Set_Border
     (The_Surface: in out Terminal_Surface;
      Use_Cursor : in     Cursor'Class;
      
      Left_Side,
        Right_Side,
        Top_Side,
        Bottom_Side,
        
        Top_Left_Corner,
        Top_Right_Corner,
        Bottom_Left_Corner,
        Bottom_Right_Corner: in Character_Type)
   is begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      if Use_Cursor in Terminals.Color.Colored_Cursor'Class
        and then Terminals.Color.Supports_Color_Style
        (TTY   => The_Surface.TTY.all,
         Style => Terminals.Color.Colored_Cursor (Use_Cursor).Color)
      then
         Apply_Colored_Border
           (Handle           => The_Surface.Handle,
            Reference_Cursor => Terminals.Color.Colored_Cursor (Use_Cursor),
            LS               => Left_Side,
            RS               => Right_Side,
            TS               => Top_Side,
            BS               => Bottom_Side,
            TL               => Top_Left_Corner,
            TR               => Top_Right_Corner,
            BL               => Bottom_Left_Corner,
            BR               => Bottom_Right_Corner);
         -- Invokes the appropriate binding after processing the color
         -- information
         
      else
         -- Monochrome!
         Apply_Monochrome_Border
           (Handle           => The_Surface.Handle,
            Reference_Cursor => Use_Cursor,
            LS               => Left_Side,
            RS               => Right_Side,
            TS               => Top_Side,
            BS               => Bottom_Side,
            TL               => Top_Left_Corner,
            TR               => Top_Right_Corner,
            BL               => Bottom_Left_Corner,
            BR               => Bottom_Right_Corner);
         
      end if;
      
      Set_Modified (The_Surface);
      
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Set_Background: Unexpected exception: " &
           Exceptions.Exception_Information (e);
   end Generic_Set_Border;
   ----------------------------------------
   
   
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
                           Bottom_Right_Corner: in Graphic_Character)
   is
      procedure Set_Border_Actual is new Generic_Set_Border
        (Character_Type          => Character,
         Apply_Colored_Border    => Terminals.Color.Apply_Colored_Border,
         Apply_Monochrome_Border => Binding.Render.Set_Monochrome_Border)
        with Inline;
   begin
      Set_Border_Actual (The_Surface         => The_Surface,
                         Use_Cursor          => Use_Cursor,
                         
                         Left_Side           => Left_Side,
                         Right_Side          => Right_Side,
                         Top_Side            => Top_Side,
                         Bottom_Side         => Bottom_Side,
                         
                         Top_Left_Corner     => Top_Left_Corner,
                         Top_Right_Corner    => Top_Right_Corner,
                         Bottom_Left_Corner  => Bottom_Left_Corner,
                         Bottom_Right_Corner => Bottom_Right_Corner);
                         
   end Set_Border;
   ----------------------------------------
   
   -- Wide_Character support
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
                                         return Character := null)
   is
      procedure Set_Border_Actual is new Generic_Set_Border
        (Character_Type          => Wide_Character,
         Apply_Colored_Border    => Terminals.Color.Wide_Apply_Colored_Border,
         Apply_Monochrome_Border 
           => Binding.Render.Wide.Wide_Set_Monochrome_Border)
        with Inline;
   begin
      if not Binding.Render.Wide.Wide_Support_Configured then
         if Wide_Fallback = null then
            raise Curses_Library with
              "Binding not configured with Wide_String support, " &
              "and no Wide_Fallback provided";
         else
            The_Surface.Set_Border
              (Use_Cursor          => Use_Cursor,
               
               Left_Side           => Wide_Fallback (Left_Side),
               Right_Side          => Wide_Fallback (Right_Side),
               Top_Side            => Wide_Fallback (Top_Side),
               Bottom_Side         => Wide_Fallback (Bottom_Side),
               
               Top_Left_Corner     => Wide_Fallback (Top_Left_Corner),
               Top_Right_Corner    => Wide_Fallback (Top_Right_Corner),
               Bottom_Left_Corner  => Wide_Fallback (Bottom_Left_Corner),
               Bottom_Right_Corner => Wide_Fallback (Bottom_Right_Corner));
         end if;
      else
         Set_Border_Actual (The_Surface         => The_Surface,
                            Use_Cursor          => Use_Cursor,
                            
                            Left_Side           => Left_Side,
                            Right_Side          => Right_Side,
                            Top_Side            => Top_Side,
                            Bottom_Side         => Bottom_Side,
                            
                            Top_Left_Corner     => Top_Left_Corner,
                            Top_Right_Corner    => Top_Right_Corner,
                            Bottom_Left_Corner  => Bottom_Left_Corner,
                            Bottom_Right_Corner => Bottom_Right_Corner);
      end if;
   end Wide_Set_Border;
   
   
   ----------------
   -- Transcribe --
   ----------------
   overriding
   procedure Transcribe (Source : in out Terminal_Surface;
                         Target : in out Surface'Class;
                         From   : in     Cursor'Class;
                         To     : in     Cursor'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False)
   is
      Source_Extents: Cursor_Position := Source.Extents;
      Target_Extents: Cursor_Position := Target.Extents;
      
      Region_Addendum: constant Cursor_Position 
        := (Row => Rows, Column => Columns);
      
      Source_Region_Extents: Cursor_Position 
        := Limit_Add_Positions (Left  => From.Position, 
                                Right => Region_Addendum);
      
      Target_Region_Extents: Cursor_Position 
        := Limit_Add_Positions (Left  => To.Position, 
                                Right => Region_Addendum);
      
      
      -- There are two possible permutations of Transcribe needed to satisfy to
      -- the Surface'Class specification, which requires that we are able to 
      -- Transcribe to any Target of Surface'Class, by using Surface'Class.Put 
      -- if necessary.
      --
      -- We will create two versions, one using a dispatching call to Put, the
      -- other for native Terminal_Surface'Class targets.
      
      -- For non Terminal_Surface'Class Targets
      procedure Indirect_Transcribe with Inline is
         type T_O is new Tasking_Order with null record;
         
         procedure Execute (Order: in out T_O) is
            use Binding.Render;
            
            Copy_String: 
              String (1 .. Natural 
                        (From.Position.Column - Source_Extents.Column));
            
            Paste_Cursor: Cursor'Class := To;
            
            Copy_Last: Natural;
            
         begin
            -- Once for each line, we read in a String from the Source, and
            -- distribute it to the Target.
            --
            -- Though in theory the regions are vetted for us already, in the
            -- rare case that 
            
            for I in From.Position.Row .. Source_Extents.Row loop
               Get_String (Handle => Source.Handle,
                           Buffer => Copy_String,
                           Last   => Copy_Last);
               
               if Copy_Last < Copy_String'Last then
                  -- Recovered string is not as long as it is supposed to be.
                  -- This should mean that the Source surface size has changed
                  raise Cursor_Excursion with
                    "Source Surface geometry changed during an indirect " & 
                    "transcribe operation";
               end if;
               
               declare
               begin
                  Target.Put (Set_Cursor => Paste_Cursor,
                              Content    => Copy_String,
                              Justify    => Left,
                              Overflow   => Error);
               exception
                  when Cursor_Excursion =>
                     raise Cursor_Excursion with
                       "Target Surface geometry changed during an indirect " & 
                       "transcribe operation";
                     
                  when others =>
                     raise;
               end;
               
               -- Move the "Paste_Cursor" down to the next row
               Paste_Cursor.Position.Row := Paste_Cursor.Position.Row + 1;
               
            end loop;
            
         end Execute;
         
         Order: T_O;
         
      begin
         Source.TTY.Liaison.Assign (Order);
      
      end Indirect_Transcribe;
         
   begin
      -- Verify that both surfaces are active
      if not Source.Available then
         raise Surface_Unavailable with
           "Source Surface not available.";
         
      elsif not Target.Available then
         raise Surface_Unavailable with
           "Target Surface not available.";
         
      end if;
      
      -- Make sure that the From and To Cursors are within the extents of
      -- their respective surfaces.
      if From.Position > Source_Extents then
         raise Cursor_Excursion with
           "From Cursor is out of bounds of Source Surface";
         
      elsif To.Position > Target_Extents then
         raise Cursor_Excursion with
           "To Cursor is out of bounds of Target Surface";
         
      end if;
      
      -- Now we check the extents of the region, and either raise an exception,
      -- or clip it to fit
      if Source_Region_Extents > Source_Extents then
         if Clip then

            
            if Source_Region_Extents.Row > Source_Extents.Row then
               Source_Region_Extents.Row := Source_Extents.Row;
            end if;
            
            if Source_Region_Extents.Column > Source_Extents.Column then
               Source_Region_Extents.Column := Source_Extents.Column;
            end if;
            
         else
            raise Cursor_Excursion with
              "Source region does not fit on Source Surface";
         end if;
         
      end if;
      
      -- If the specified area that we are copying from the Source Surface is 
      -- larger than what is actually available on the Surface, and we "clip" 
      -- the Source_Region_Extents, this needs to be carried through to our
      -- Target_Region_Extents. This is because the actual binding only
      -- receives the Target's region geometry.
      
      Target_Region_Extents 
        := Limit_Add_Positions 
             (Left  => To.Position,
              Right => (Source_Region_Extents - From.Position));
      
      
      if Target_Region_Extents > Target_Extents then
         if Clip then
            -- Only clip the parts that actually go out of bounds.
            -- Check each one separately.
            
            if Target_Region_Extents.Row > Target_Extents.Row then
               Target_Region_Extents.Row := Target_Extents.Row;
            end if;
            
            if Target_Region_Extents.Column > Target_Extents.Column then
               Target_Region_Extents.Column := Target_Extents.Column;
            end if;
            
         else
            raise Cursor_Excursion with
              "Target region does not fit on Target Surface";
         end if;
         
      end if;
      
      
      -- We've finally done all of our checks! This means we have two valid
      -- Regions (From + Source_Region_Extents) and (To +
      -- Target_Region_Extents) For "non-native" Target Surfaces, we can now
      -- safely dispatch to one of the two 
      if Target in Terminal_Surface'Class then
         Binding.Render.Copy_Area
           (From_Handle => Source.Handle,
            From_TL     => From.Position,
            
            To_Handle   => Terminal_Surface (Target).Handle,
            To_TL       => To.Position,
            To_BR       => Target_Region_Extents);
         
         Set_Modified (Terminal_Surface'Class (Target));
      else
         Indirect_Transcribe;
      end if;
      
   exception
      when Curses_Library | Cursor_Excursion =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Transcribe;
   
   ----------------------------------------
   overriding
   procedure Transcribe (Source : in out Terminal_Surface;
                         Target : in out Surface'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False)
   is
   begin
      Transcribe (Source  => Source,
                  Target  => Target,
                  From    => Source.Cursor_State.Get,
                  To      => Target.Current_Cursor,
                  Rows    => Rows,
                  Columns => Columns,
                  Clip    => Clip);
      
   end Transcribe;
   
   
   --------------
   -- Finalize --
   --------------
   overriding
   procedure Finalize (The_Surface: in out Terminal_Surface) is
      use Curses.Binding.Render;
   begin
      -- Note that we know we will have no windows still attached to us, since
      -- all windows require us as an access discriminant, so they can't go out
      -- of scope before we do!
      
      Destroy_Surface (The_Surface.Handle);
      -- Destroy_Window checks for invalid handles, so we don't need
      -- to check for that ourselves
      
      -- Parent type finalization
      Surface (The_Surface).Finalize;
         
   end Finalize;
   
   
   --
   -- Rendered_Surface (Input Facilities)
   --
   
   ----------
   -- Show --
   ----------
   procedure Show (The_Surface: in out Rendered_Surface) is
   begin
      The_Surface.Properties.Arm (True);
      The_Surface.TTY.Refresh.Queue_Refresh;
   end Show;
   
   
   ----------
   -- Hide --
   ----------
   procedure Hide (The_Surface: in out Rendered_Surface) is
   begin
      The_Surface.Properties.Withdraw;
      The_Surface.TTY.Refresh.Queue_Refresh;
   end Hide;
   
   
   ------------------
   -- Wait_Focused --
   ------------------
   procedure Wait_Focused (The_Surface: in out Rendered_Surface) is
   begin
      The_Surface.Focus.Wait_Focus;
   end Wait_Focused;
   
   ---------------
   -- Input_Key --
   ---------------
   
   -- To_Control --
   ----------------
   -- To_Control takes a CURSES_Character from the Binding.Terminals package,
   -- and returns the corresponding Control_Character. If there is no
   -- corresponding Control_Character, To_Control returns an Invalid class
   -- Control_Character
   function To_Control (Key: Binding.Terminals.CURSES_Character) 
                       return Control_Character
   is
      use Binding.Terminals;
   begin
      -- We first check for the most common case - No_Key, followed by regular 
      -- graphic characters
      
      if Key = CURSES_KEY_NOKEY then
         return Control_Character'(Class => No_Key);
      end if;
      
      -- Note that, unless creating some kind of ESC sequence (like val = 27),
      -- the Alt key always simply sets the 8th most significant bit.
      
      if Key in CURSES_KEYS_Graphic then
         return Control_Character'(Class => Graphic, 
                                   Alt   => False,
                                   Key   => Character'Val(Key));
         
      elsif (Key - CURSES_KEY_ALT_OFFSET) in CURSES_KEYS_Graphic then
         return Control_Character'
           (Class => Graphic,
            Alt   => True,
            Key   => Character'Val(Key - CURSES_KEY_ALT_OFFSET));
         
      end if;
      
      -- Next we check for Ctrl+ combinations, which will be below the 
      -- CURSES_KEY_CTRL_OFFSET value, and generally translate into graphic
      -- characters, in the lower-case range. If this doesn't work, we will
      -- assume it is something else and handle the special keys afterwards.
      -- Note that the Alt key works as normal, Ie Ctrl+` is 0, Ctrl+Alt+` is
      -- 128.
      
      if Key <= CURSES_KEY_CTRL_OFFSET then
         return Control_Character'
           (Class => Ctrl,
            Alt   => False,
            Key   => Character'Val(Key + CURSES_KEY_CTRL_OFFSET));
         
      elsif (Key - CURSES_KEY_ALT_OFFSET) in 0 .. CURSES_KEY_CTRL_OFFSET then
         return Control_Character'
           (Class => Ctrl,
            Alt   => True,
            Key   => Character'Val
              (Key - CURSES_KEY_ALT_OFFSET + CURSES_KEY_CTRL_OFFSET));
         
      end if;
      
      -- Next, check for the range of F-keys
      if Key in CURSES_KEY_F1 .. CURSES_KEY_F12 then
         return Control_Character'
           (Class    => F_Key,
            F_Number => F_Key_Range (Key - CURSES_KEY_F1 + 1));
      end if;
      
      -- Finally, we check for the last few specific cases, which do not accept
      -- Ctrl or Alt modifiers, anyways
      
      -- Unfortunately, the values for each key is taken from the curses library
      -- which means the constants are not "static values", but are assigned
      -- during elaboration of the Binding.Terminals package. This means we
      -- cannot use a case statement.
      
      -- We will do out best to order the following to put the most likely first
      
      if    Key = CURSES_KEY_ENTER then
         return Control_Character'(Class => Enter);
         
      elsif Key = CURSES_KEY_BACKSPACE then
         return Control_Character'(Class => Backspace);
         
      elsif Key = CURSES_KEY_LEFT then
         return Control_Character'(Class => Left_Key);
         
      elsif Key = CURSES_KEY_RIGHT then
         return Control_Character'(Class => Right_Key);
         
      elsif Key = CURSES_KEY_UP then
         return Control_Character'(Class => Up_Key);
         
      elsif Key = CURSES_KEY_DOWN then
         return Control_Character'(Class => Down_Key);
         
      elsif Key = CURSES_KEY_DELETE then
         return Control_Character'(Class => Delete_Key);
         
      elsif Key = CURSES_KEY_PAGEDN then
         return Control_Character'(Class => Page_Down);
         
      elsif Key = CURSES_KEY_PAGEUP then
         return Control_Character'(Class => Page_Up);
         
      elsif Key = CURSES_KEY_ESC then
         return Control_Character'(Class => Escape);
         
      elsif Key = CURSES_KEY_HOME then
         return Control_Character'(Class => Home_Key);
         
      elsif Key = CURSES_KEY_END then
         return Control_Character'(Class => End_Key);
         
      elsif Key = CURSES_KEY_INSERT then
         return Control_Character'(Class => Insert_Key);
         
      else
         return Control_Character'
           (Class             => Unknown, 
            Unknown_Character => Wide_Character'Val (Key));
         -- Note that this could conceivable cause a Constraint_Error, if the 
         -- CURSES_Character type is wider than Wide_Character. This is very
         -- unlikely for most in-the-wild Curses implementations. Even in that
         -- case, the exception handler will change it to an "Invalid" key.
         
      end if;
      
   exception
      when others =>
         return Control_Character'(Class => Invalid);
            
   end To_Control;
   
   
   ----------------------------------------
   function Input_Key (The_Surface  : in out Rendered_Surface;
                       Peek         : in     Boolean           := False;
                       Wait         : in     Boolean           := True)
                      return Control_Character
   is
      use Binding.Terminals;
      
      Raw_Input   : CURSES_Character;
      The_Key     : Control_Character;
      
      Scaled_Delay: Duration := 0.0;
   begin
      -- First ensure that the Surface is valid
      if not The_Surface.Available then
         return Control_Character'(Class => No_Key);
      end if;
      
      -- We need to be pulling from a "Focused" Surface that is currently
      -- Visible. However, we don't want to Wait on Visibility if the target
      -- Surface isn't even Focused!
      if not The_Surface.Focused or else not The_Surface.Visible then
         if Wait then
            -- Queue-up in order!
            while not The_Surface.Focused loop
               The_Surface.Wait_Focused;
               The_Surface.Wait_Visible;
            end loop;
            
         else
            -- No focus
            return Control_Character'(Class => Lost_Focus);
         
         end if;
      end if;
      
      
      
      loop
         -- We will keep trying periodically if Wait is true and we get a
         -- No_Key or we lose Focus
         
         -- Execute an input call and translate
         Raw_Input := Pop_Buffer (The_Surface.Handle);
         
         -- We have an actual key. If we are just peaking, we can put it
         -- back right away.
         
         if Peek then
            Push_Buffer (TTY  => The_Surface.TTY.Handle,
                         Char => Raw_Input);
            -- Honestly, we can't really do much if that didn't work, so we
            -- don't have any use of Check.
         end if;
         
         -- Convert the key
         The_Key := To_Control (Raw_Input);
         
         -- In theory, there should never be any resize keys coming through,
         -- since, in theory, the Binding package completely knocked-out the
         -- resize signal handler, which is normally what produces this. But
         -- just in case, we will use it's appearance to schedule an update
         if Raw_Input = CURSES_KEY_RESIZE then
            The_Surface.TTY.Refresh.Queue_Refresh;
            
         elsif The_Key.Class /= No_Key or else not Wait then
            -- Return the key if it is not a No_Key, unless we are not to wait,
            -- in which case we just return No_Key
            return The_Key;
            
         else
            -- Otherwise, try again in a bit (if we are still Focused), or wait
            -- until we regain focus
            if not The_Surface.Focused then
               while not The_Surface.Focused loop
                  The_Surface.Wait_Focused;
                  The_Surface.Wait_Visible;
               end loop;
            else
               if Scaled_Delay < 0.2 then
                  delay Scaled_Delay;
                  Scaled_Delay := Scaled_Delay + 0.01;
               else
                  delay 0.2;
               end if;
            end if;
         end if;
         
      end loop;
      
   exception
      when others =>
         return Control_Character'(Class => Invalid);
   end Input_Key;
   
end Curses.Terminals.Surfaces;
