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

with Ada.Assertions;
with Ada.Exceptions; use Ada;

with Curses.Put_Computer;

package body Curses.Frames is
   
   -- Use-like specific imports
   subtype Colored_Cursor is Terminals.Color.Colored_Cursor;
   
   Assertion_Error: exception renames Ada.Assertions.Assertion_Error;
   

   
   
   --
   -- Frame_State Implementation
   --
   
   protected body Frame_State is
      
      ---------------
      -- Available --
      ---------------
      function  Available return Boolean is (Active);
      
      procedure Available (Set: in Boolean) is
      begin
         Active := Set;
      end Available;
      
      
      --------------------
      -- Current_Cursor --
      --------------------
      function Current_Cursor return Cursor'Class is
      begin
         case Our_Cursor.Is_Colored is
            when True =>
               return Our_Cursor.Colored;
               
            when False =>
               return Our_Cursor.Monochrome;
         end case;
      end Current_Cursor;
      
      ----------------------------------------
      procedure Current_Cursor (Set: in Cursor'Class) is
      begin
         if Set in Colored_Cursor'Class then
            Our_Cursor := (Is_Colored => True, 
                           Colored    => Colored_Cursor (Set));
         else
            Our_Cursor := (Is_Colored => False, 
                           Monochrome => Cursor (Set));
         end if;
      end Current_Cursor;
      
      
      -----------------------
      -- Background_Cursor --
      -----------------------
      function Background_Cursor return Cursor'Class is
      begin
         case BG_Cursor.Is_Colored is
            when True =>
               return BG_Cursor.Colored;
               
            when False =>
               return BG_Cursor.Monochrome;
         end case;
      end Background_Cursor;
      
      ----------------------------------------
      procedure Background_Cursor (Set: in Cursor'Class) is
      begin
         if Set in Colored_Cursor'Class then
            BG_Cursor := (Is_Colored => True, Colored => Colored_Cursor (Set));
         else
            BG_Cursor := (Is_Colored => False, Monochrome => Cursor (Set));
         end if;
      end Background_Cursor;
      
      
      --------------------------
      -- Background_Character --
      --------------------------
      function  Background_Character return Graphic_Character is (BG_Char);
      
      procedure Background_Character (Set: in Graphic_Character) is
      begin
         BG_Char := Set;
      end Background_Character;
      
      -------------------------------
      -- Wide_Background_Character --
      -------------------------------
      function  Wide_Background_Character return Wide_Graphic_Character 
        is (Wide_BG_Char);
      
      procedure Wide_Background_Character (Set: in Wide_Graphic_Character) is
      begin
         Wide_BG_Char := Set;
      end Wide_Background_Character;
      
      
      ---------------
      -- Target_TL --
      ---------------
      function  Target_TL return Cursor_Position is (TL_Pos);
      
      procedure Target_TL (Set: in Cursor_Position) is
      begin
         TL_Pos := Set;
      end Target_TL;
      
      
      -----------------
      -- Auto_Assert --
      -----------------
      function  Auto_Assert return Boolean is (Assert_Config);
      
      procedure Auto_Assert (Set: in Boolean) is
      begin
         Assert_Config := Set;
      end Auto_Assert;
      
   end Frame_State;
   
   
   --
   -- Basic Infrastructure
   --
   function Target_Position (F: in Frame'Class;
                             P: in Cursor_Position)
                            return Cursor_Position
     is (P + F.State.Target_TL - (1,1))
     with Inline;

   
   
   --
   -- Type Extension Implementation
   --
   
   ---------------
   -- New_Frame --
   ---------------
   function  New_Frame (Target          : not null access Surface'Class;
                        Top_Left        : in Cursor_Position;
                        Proposed_Extents: in Cursor_Position)
                       return Frame
   is
   begin
      return New_Frame: Frame (Target) do
        
        New_Frame.Reframe (Top_Left         => Top_Left,
                           Proposed_Extents => Proposed_Extents);
        -- Reframe doesn't check the Frame Availability, only the Target's
        
      exception
         when others =>
            New_Frame.State.Available (False);
      end return;
   end New_Frame;
   
   ----------------------------------------
   function  New_Frame (Target          : not null access Surface'Class;
                        Margin          : in Cursor_Ordinal)
                       return Frame
   is
   begin
      return New_Frame: Frame (Target) do
      
        New_Frame.Reframe (Margin);
      
      exception
         when others =>
            New_Frame.State.Available (False);
      end return;
   end New_Frame;
   
   
   -------------
   -- Reframe --
   -------------
   procedure Reframe (The_Frame       : in out Frame;
                      Top_Left        : in     Cursor_Position;
                      Proposed_Extents: in     Cursor_Position)
   is
      T_Extents: Cursor_Position;
   begin
      if not The_Frame.Target.Available then
         -- We check the target here because Reframe may reactivate an inactive
         -- frame. We want to prevent reframing on inactive targets, however.
         raise Surface_Unavailable;
      end if;
      
      T_Extents := The_Frame.Target.Extents;
      
      if Top_Left > T_Extents then
         raise Cursor_Excursion with
           "Top_Left is beyond the Target's Extents";
         
      elsif (Top_Left + Proposed_Extents - (1,1)) > T_Extents then
         raise Cursor_Excursion with
           "Proposed_Extents cannot be accommodated by Target";
      end if;
      
      -- Remembering that this entire package promises to be Task-safe, we need
      -- to ensure that we update the state in the right way.
      --
      -- In fact, it is really bad form to be Reframing a Frame while other
      -- Tasks are operating on it, and it will probably have adverse results
      -- at the application level. We simply do the best job reasonable to
      -- try to help things go smoothly as possible where it otherwise
      -- wouldn't.
      
      The_Frame.Properties.Extents (Proposed_Extents);
      The_Frame.State.Target_TL (Top_Left);

      declare
         Init_Cursor: Cursor'Class := The_Frame.Current_Cursor;
      begin
         Init_Cursor.Position := (1,1);
         The_Frame.State.Current_Cursor    (Init_Cursor);
         The_Frame.State.Background_Cursor (Init_Cursor);
      end;
      
      The_Frame.State.Background_Character (' ');
      The_Frame.State.Available (True);
      
   exception
      when Surface_Unavailable | Cursor_Excursion =>
         The_Frame.State.Available (False);
         raise;
         
      when e: others =>
         The_Frame.State.Available (False);
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Reframe;
   
   ----------------------------------------
   procedure Reframe (The_Frame       : in out Frame;
                      Margin          : in     Cursor_Ordinal)
   is
      T_Extents: Cursor_Position;
   begin
      if not The_Frame.Target.Available then
         -- We check the target here because Reframe may reactivate an inactive
         -- frame. We want to prevent reframing on inactive targets, however.
         raise Surface_Unavailable;
      end if;
      
      T_Extents := The_Frame.Target.Extents;
      
      The_Frame.Reframe 
        (Top_Left         => (Margin + 1, Margin + 1),
         Proposed_Extents => 
           T_Extents - (Cursor_Position'(Margin * 2, Margin * 2)));
      
   exception
      when Surface_Unavailable =>
         The_Frame.State.Available (False);
         raise;
         
      when Cursor_Excursion | Curses_Library =>
         -- These must have come from the call to Reframe, so the
         -- Frame is already made unAvailable
         raise;
         
      when e: others =>
         The_Frame.State.Available (False);
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Reframe;
   
   
   -------------------
   -- Assert_Cursor --
   -------------------
   procedure Assert_Cursor (The_Frame: in out Frame) is
   begin
      if not The_Frame.Available or else not The_Frame.Target.Available then
         return;
      end if;
      
      declare
         Mod_Cursor: Cursor'Class := The_Frame.Current_Cursor;
      begin
         Mod_Cursor.Position 
           := Target_Position (The_Frame, Mod_Cursor.Position);
         
         The_Frame.Target.Current_Cursor (Mod_Cursor);
      end;
      
   exception
      when others => null;
   end Assert_Cursor;
   
   
   ------------------------
   -- Auto_Assert_Cursor --
   ------------------------
   procedure Auto_Assert_Cursor (The_Frame: in out Frame;
                                 Set      : in     Boolean := True)
   is
   begin
      The_Frame.State.Auto_Assert (Set);
   end Auto_Assert_Cursor;
   
   
   -------------------
   -- Derive_Cursor --
   -------------------
   procedure Derive_Cursor (The_Frame: in out Frame) is
   begin
      if not The_Frame.Available or else not The_Frame.Target.Available then
         return;
      end if;
      
      declare
         Mod_Cursor: Cursor'Class := The_Frame.Target.Current_Cursor;
         TL: constant Cursor_Position 
           := Target_Position (The_Frame, (1,1));
         BR: constant Cursor_Position 
           := Target_Position (The_Frame, The_Frame.Extents);
      begin
         -- Remember that the comparison operators for Cursor_Position are 
         -- based on the idea of a cursor being *either* to the right or below
         -- (i.e. an extent), we need to be a little more explicit here for
         -- seeing if the Target's Cursor is in the Frame
         
         if ((Mod_Cursor.Position.Row < TL.Row) or else
               (Mod_Cursor.Position.Column < TL.Column))
           
           or else Mod_Cursor.Position > BR
         then
            -- This means that the Target's Cursor is firmly outside
            -- the Frame, so we need to limit it
            
            if Mod_Cursor.Position.Row > BR.Row then
               Mod_Cursor.Position.Row := BR.Row;
            else
               -- Must be < TL.Row
               -- Else ensures there are no other possible execution paths here
               Mod_Cursor.Position.Row := TL.Row;
            end if;
            
            if Mod_Cursor.Position.Column > BR.Column then
               Mod_Cursor.Position.Column := BR.Column;
            else
               -- Must be < TL.Col
               Mod_Cursor.Position.Column := TL.Column;
            end if;
            
         end if;
         
         -- Remember to adjust it to the Frame-relative position
         Mod_Cursor.Position := Mod_Cursor.Position - TL + (1,1);
         
         The_Frame.Current_Cursor (Mod_Cursor);
      end;
      
   exception
      when others => null;
   end Derive_Cursor;
   
   
   --
   -- Surface type overrides
   --
   
   --------------------
   -- Current_Cursor --
   --------------------
   overriding
   procedure Current_Cursor (The_Surface: in out Frame;
                             New_Cursor : in     Cursor'Class)
   is
   begin
      if New_Cursor.Position > The_Surface.Extents then
         raise Cursor_Excursion;
      end if;
      
      The_Surface.State.Current_Cursor (New_Cursor);
      
      if The_Surface.State.Auto_Assert then
         declare
            Mod_Cursor: Cursor'Class := New_Cursor;
         begin
            Mod_Cursor.Position := Target_Position 
              (The_Surface, Mod_Cursor.Position);
            The_Surface.Target.Current_Cursor (Mod_Cursor);
         end;
      end if;
      
   exception
      when Cursor_Excursion =>
         raise;
         
      when others =>
         -- This should be impossible. If this happens, somehow, it could be a
         -- sign of problems too big for us to deal with. Our contract remains,
         -- however.
         null;
      
   end Current_Cursor;
   
   
   ---------------------
   -- Position_Cursor --
   ---------------------
   overriding
   procedure Position_Cursor(The_Surface: in out Frame;
                             Position   : in     Cursor_Position)
   is
   begin
      if Position > The_Surface.Extents then
         raise Cursor_Excursion;
      end if;
      
      declare
         Mod_Cursor: Cursor'Class := The_Surface.State.Current_Cursor;
      begin
         Mod_Cursor.Position := Position;
         The_Surface.State.Current_Cursor (Mod_Cursor);
      
         if The_Surface.State.Auto_Assert then
            Mod_Cursor.Position := Target_Position 
              (The_Surface, Mod_Cursor.Position);
            
            The_Surface.Target.Current_Cursor (Mod_Cursor);
         end if;
      end;
      
   exception
      when Cursor_Excursion => raise;
      when others           => null;
      
   end Position_Cursor;
   
   
   -- Clear Group Common Infrastructure --
   ---------------------------------------
   procedure Clear_Block (The_Surface: in out Frame'Class;
                          First_Row  : in     Cursor_Ordinal;
                          Last_Row   : in     Cursor_Ordinal)
     with Inline
   is
      Extents   : constant Cursor_Position := The_Surface.Extents;
      Use_Cursor: Cursor'Class := The_Surface.State.Background_Cursor;

   begin
      Use_Cursor.Position.Column := 1;
      
      if The_Surface.Wide_Support then
         declare
            Blanks  : constant Wide_String (1 .. Positive (Extents.Column))
              := (others => The_Surface.State.Wide_Background_Character);
         begin
            for I in First_Row .. Last_Row loop
               Use_Cursor.Position.Row := I;
               The_Surface.Wide_Put
                 (Set_Cursor    => Use_Cursor,
                  Content       => Blanks);
               -- Note that Put will do the actual Extents check for Row.
               -- This means that if Last_Row is beyond Extents, then we will
               -- still clear up to the actual last Row
            end loop;
         end;
         
      else
         declare
            Blanks  : constant String (1 .. Positive (Extents.Column))
              := (others => The_Surface.State.Background_Character);
         begin
            for I in First_Row .. Last_Row loop
               Use_Cursor.Position.Row := I;
               The_Surface.Put (Set_Cursor => Use_Cursor,
                                Content    => Blanks);
               -- Note that Put will do the actual Extents check for Row.
               -- This means that if Last_Row is beyond Extents, then we will
               -- still clear up to the actual last Row
            end loop;
         end;
      end if;
   end Clear_Block;
   
   
   -----------
   -- Clear --
   -----------
   overriding
   procedure Clear (The_Surface: in out Frame)
   is
   begin
      if not The_Surface.Available then
         return;
      end if;
      
      Clear_Block (The_Surface => The_Surface,
                   First_Row   => 1,
                   Last_Row    => The_Surface.Extents.Row);
      
   exception
      when others => null;
   end Clear;
   
   
   ---------------
   -- Clear_Row --
   ---------------
   overriding
   procedure Clear_Row (The_Surface  : in out Frame;
                        Row          : in     Cursor_Ordinal)
   is
   begin
      if not The_Surface.Available then
         return;
      end if;
      
      Clear_Block (The_Surface => The_Surface,
                   First_Row   => Row,
                   Last_Row    => Row);
      
   exception
      when others => null;
   end Clear_Row;
   
   
   ----------------
   -- Clear_Rows --
   ----------------
   overriding
   procedure Clear_Rows (The_Surface  : in out Frame;
                         First_Row    : in     Cursor_Ordinal;
                         Last_Row     : in     Cursor_Ordinal)
   is
   begin
      if not The_Surface.Available then
         return;
      end if;
      
      Clear_Block (The_Surface => The_Surface,
                   First_Row   => First_Row,
                   Last_Row    => Last_Row);
      
   exception
      when others => null;
   end Clear_Rows;
   
   
   ------------------
   -- Clear_Column --
   ------------------
   overriding
   procedure Clear_Column (The_Surface: in out Frame;
                           Column     : in     Cursor_Ordinal)
   is
   begin
      if not The_Surface.Available then
         return;
      end if;
      
      declare
         Extents   : constant Cursor_Position := The_Surface.Extents;
         Use_Cursor: Cursor'Class := The_Surface.State.Background_Cursor;
         Blank     : constant String (1..1) 
           := (others => The_Surface.State.Background_Character);
      begin
         if Column > Extents.Column then
            return;
         end if;
         
         Use_Cursor.Position.Column := Column;
         
         for I in Cursor_Ordinal'First .. Extents.Row loop
            Use_Cursor.Position.Row := I;
            The_Surface.Put (Set_Cursor => Use_Cursor,
                             Content    => Blank);
         end loop;
      end;
      
   exception
      when others => null;
   end Clear_Column;
   
   
   -------------------
   -- Clear_Columns --
   -------------------
   overriding
   procedure Clear_Columns (The_Surface : in out Frame;
                            First_Column: in     Cursor_Ordinal;
                            Last_Column : in     Cursor_Ordinal)
   is
   begin
      if not The_Surface.Available then
         return;
      end if;
      
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
   procedure Clear_To_End (The_Surface: in out Frame;
                           From       : in     Cursor'Class)
   is
      Extents: constant Cursor_Position := The_Surface.Extents;
   begin
      if not The_Surface.Available
        or else From.Position > The_Surface.Extents 
      then
         return;
      end if;
      
      declare
         Our_Cursor: Cursor'Class := The_Surface.State.Background_Cursor;
         Blank: constant String 
           (Positive (From.Position.Column) .. Positive (Extents.Column))
           := (others => The_Surface.State.Background_Character);
      begin
         Our_Cursor.Position := From.Position;
         The_Surface.Put (Set_Cursor => Our_Cursor,
                          Content    => Blank);
      end;
      
   exception
      when others => null;
   end Clear_To_End;
   
   
   ---------
   -- Put --
   ---------
   generic
      type Character_Type is (<>);
      type String_Type    is array (Positive range <>) of Character_Type;
   
      with procedure Target_Put (The_Surface: in out Surface'Class;
                                 Set_Cursor : in out Cursor'Class;
                                 Content    : in     String_Type;
                                 Justify    : in     Justify_Mode;
                                 Overflow   : in     Overflow_Mode);
      -- Should dispatch to Put with defaults as appropriate
   
   procedure Generic_Put (The_Surface   : in out Frame;
                          Set_Cursor    : in out Cursor'Class;
                          Content       : in     String_Type;
                          Justify       : in     Justify_Mode;
                          Overflow      : in     Overflow_Mode;
                          Advance_Cursor: in     Boolean);
   
   procedure Generic_Put (The_Surface   : in out Frame;
                          Set_Cursor    : in out Cursor'Class;
                          Content       : in     String_Type;
                          Justify       : in     Justify_Mode;
                          Overflow      : in     Overflow_Mode;
                          Advance_Cursor: in     Boolean)
   is
      Write_Position: Cursor_Position := Set_Cursor.Position;
      Write_Cursor  : Cursor'Class    := Set_Cursor;
      
      pragma Assertion_Policy (Pre'Class => Ignore);
      -- We have passed the class-wide precondition on Content on entry to this
      -- subprogram, So we disable it for the computer's dispatches to Put,
      -- since we know that the Precondition has been passed already if we got
      -- this far.
      
      package Computer is new Put_Computer
        (Character_Type => Character_Type,
         String_Type    => String_Type,
         
         The_Surface => Surface'Class (The_Surface),
         Write_Head  => Write_Position,
         Justify     => Justify,
         Overflow    => Overflow,
         Content     => Content);
      
      Select_First: Natural renames Computer.Select_First;
      Select_Last : Natural renames Computer.Select_Last;
      
   begin
      -- Really just a matter of translating the position and then passing it
      -- on to the Target
      
      if not The_Surface.Available then
         raise Surface_Unavailable;
      elsif Set_Cursor.Position > The_Surface.Extents then
         raise Cursor_Excursion;
      end if;
      
      while Computer.Compute_Line loop
         Write_Cursor.Position := Target_Position 
           (The_Surface, Write_Position);
         
         Target_Put (The_Surface => The_Surface.Target.all,
                     Set_Cursor  => Write_Cursor,
                     Content     => Content (Select_First .. Select_Last),
                     Justify     => Justify,
                     Overflow    => Overflow);
      end loop;
      
      if Advance_Cursor then
         Set_Cursor.Position := Computer.After_Write;
      end if;
      
   exception
      when 
        Surface_Unavailable |
        Cursor_Excursion    |
        Curses_Library      =>
         
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
   ----------------------------------------
   
   overriding 
   procedure Put (The_Surface   : in out Frame;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
   is
      procedure Target_Put (The_Surface: in out Surface'Class;
                            Set_Cursor : in out Cursor'Class;
                            Content    : in     String;
                            Justify    : in     Justify_Mode;
                            Overflow   : in     Overflow_Mode)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- This has already been checked
      begin
         The_Surface.Put (Set_Cursor => Set_Cursor,
                          Content    => Content,
                          Justify    => Justify,
                          Overflow   => Overflow);
      end Target_Put;
      
      procedure Put_Actual is new Generic_Put
        (Character_Type => Character,
         String_Type    => String,
         Target_Put     => Target_Put)
        with Inline;
   begin
      Put_Actual (The_Surface    => The_Surface,
                  Set_Cursor     => Set_Cursor,
                  Content        => Content,
                  Justify        => Justify,
                  Overflow       => Overflow,
                  Advance_Cursor => Advance_Cursor);
   end Put;
   
   
   --------------
   -- Wide_Put --
   --------------
   overriding
   procedure Wide_Put
     (The_Surface   : in out Frame;
      Set_Cursor    : in out Cursor'Class;
      Content       : in     Wide_String;
      Justify       : in     Justify_Mode          := Left;
      Overflow      : in     Overflow_Mode         := Truncate;
      Advance_Cursor: in     Boolean               := False;
      Wide_Fallback : access 
        function (Item: Wide_String) return String := null)
   is
      procedure Target_Put (The_Surface: in out Surface'Class;
                            Set_Cursor : in out Cursor'Class;
                            Content    : in     Wide_String;
                            Justify    : in     Justify_Mode;
                            Overflow   : in     Overflow_Mode)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- This has already been checked
      begin
         The_Surface.Wide_Put (Set_Cursor    => Set_Cursor,
                               Content       => Content,
                               Justify       => Justify,
                               Overflow      => Overflow,
                               Wide_Fallback => Wide_Fallback);
      end Target_Put;
      
      procedure Put_Actual is new Generic_Put
        (Character_Type => Wide_Character,
         String_Type    => Wide_String,
         Target_Put     => Target_Put)
        with Inline;
   begin
      Put_Actual (The_Surface    => The_Surface,
                  Set_Cursor     => Set_Cursor,
                  Content        => Content,
                  Justify        => Justify,
                  Overflow       => Overflow,
                  Advance_Cursor => Advance_Cursor);
   end Wide_Put;
   
   
   ----------
   -- Fill --
   ----------
   generic
      type Character_Type is (<>);
      type String_Type    is array (Positive range <>) of Character_Type;
   
      with procedure Dispatch_Put (The_Surface: in out Frame'Class;
                                   Set_Cursor : in out Cursor'Class;
                                   Content    : in     String_Type);
      -- Should dispatch to the appropriate Fill operation with the following
      -- configuration:
      -- Overflow => Wrap_Truncate
   
   procedure Generic_Fill (The_Surface: in out Frame'Class;
                           Pattern    : in     String_Type;
                           Fill_Cursor: in     Cursor'Class);
   
   procedure Generic_Fill (The_Surface: in out Frame'Class;
                           Pattern    : in     String_Type;
                           Fill_Cursor: in     Cursor'Class)
   is
      Extents: constant Cursor_Position := The_Surface.Extents;
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      declare
         Use_Cursor: Cursor'Class := Fill_Cursor;
         Expanded_Pattern: String_Type 
           (1 .. Positive (Extents.Row * Extents.Column));
      begin
         -- Home the cursor
         Use_Cursor.Position := (1,1);
         
         -- If Pattern is larger than the available area, then we truncate
         -- to fit, otherwise, we need to fill it out
         if Pattern'Length > Expanded_Pattern'Length then
            Expanded_Pattern 
              := Pattern (Pattern'First .. 
                            Pattern'First + Expanded_Pattern'Length - 1);
            
         else
            declare
               First: Positive := Expanded_Pattern'First;
               Last : Positive := First + Pattern'Length - 1;
            begin
               loop
                  if Last > Expanded_Pattern'Last then
                     -- This is the last bit of the pattern. Fit what we can.
                     Expanded_Pattern(First .. Expanded_Pattern'Last)
                       := Pattern(Pattern'First .. 
                                    Pattern'First + 
                                    (Expanded_Pattern'Last - First));
                     
                     exit;
                  end if;
                  
                  Expanded_Pattern(First .. Last) := Pattern;
                  
                  First := Last + 1;
                  
                  exit when First > Expanded_Pattern'Last;
                  
                  Last  := First + Pattern'Length - 1;
               end loop;
            end;
            
         end if;
         
         Dispatch_Put (The_Surface => The_Surface,
                       Set_Cursor  => Use_Cursor,
                       Content     => Expanded_Pattern);
         
      end;
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Generic_Fill;
   ----------------------------------------
   
   overriding
   procedure Fill (The_Surface: in out Frame;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class)
   is
      procedure Dispatch_Put (The_Surface: in out Frame'Class;
                              Set_Cursor : in out Cursor'Class;
                              Content    : in     String)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- This has already been checked
      begin
         The_Surface.Put (Set_Cursor => Set_Cursor,
                          Content    => Content,
                          Overflow   => Wrap_Truncate);
      end Dispatch_Put;
      
      procedure Put_Actual is new Generic_Fill
        (Character_Type => Character,
         String_Type    => String,
         Dispatch_Put   => Dispatch_Put);
   begin
      Put_Actual (The_Surface => The_Surface,
                  Pattern     => Pattern,
                  Fill_Cursor => Fill_Cursor);
   end Fill;
   
   
   ---------------
   -- Wide_Fill --
   ---------------
   overriding
   procedure Wide_Fill (The_Surface  : in out Frame;
                        Pattern      : in     Wide_String;
                        Fill_Cursor  : in     Cursor'Class;
                        Wide_Fallback: access 
                          function (Item: Wide_String) return String := null)
   is
      procedure Dispatch_Put (The_Surface: in out Frame'Class;
                              Set_Cursor : in out Cursor'Class;
                              Content    : in     Wide_String)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- This has already been checked
      begin
         The_Surface.Wide_Put (Set_Cursor    => Set_Cursor,
                               Content       => Content,
                               Overflow      => Wrap_Truncate,
                               Wide_Fallback => Wide_Fallback);
      end Dispatch_Put;
      
      procedure Fill_Actual is new Generic_Fill
        (Character_Type => Wide_Character,
         String_Type    => Wide_String,
         Dispatch_Put   => Dispatch_Put);
   begin
      Fill_Actual (The_Surface => The_Surface,
                   Pattern     => Pattern,
                   Fill_Cursor => Fill_Cursor);
   end Wide_Fill;
   
   
   --------------------
   -- Set_Background --
   --------------------
   overriding
   procedure Set_Background (The_Surface   : in out Frame;
                             Fill_Character: in     Graphic_Character := ' ';
                             Fill_Cursor   : in     Cursor'Class)
   is
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      -- Set the background information and then dispatch to Clear
      The_Surface.State.Background_Cursor    (Fill_Cursor);
      The_Surface.State.Background_Character (Fill_Character);
      
      The_Surface.Clear;
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Set_Background;
   
   
   -------------------------
   -- Wide_Set_Background --
   -------------------------
   overriding
   procedure Wide_Set_Background
     (The_Surface   : in out Frame;
      Fill_Character: in     Wide_Graphic_Character := ' ';
      Fill_Cursor   : in     Cursor'Class;
      Wide_Fallback : access function (Item: Wide_Character) 
                                      return Character := null)
   is
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      -- Set the background information and then dispatch to Clear
      The_Surface.State.Background_Cursor (Fill_Cursor);
      
      if The_Surface.Wide_Support then
         The_Surface.State.Wide_Background_Character (Fill_Character);
         
      else
         if Wide_Fallback = null then
            raise Curses_Library with
              "Wide support not enabled for the target surface";
         else
            The_Surface.State.Background_Character 
              (Wide_Fallback (Fill_Character));
         end if;
      end if;
      
      The_Surface.Clear;
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Wide_Set_Background;
   
   
   ----------------
   -- Set_Border --
   ----------------
   overriding
   procedure Set_Border (The_Surface: in out Frame;
                         Use_Cursor : in     Cursor'Class)
   is
      Vertical_Line  : constant Graphic_Character := '|';
      Horizontal_Line: constant Graphic_Character := '-';
      Corner         : constant Graphic_Character := '+';
      
      W_Vertical_Line  : constant Wide_Graphic_Character := '|';
      W_Horizontal_Line: constant Wide_Graphic_Character := '-';
      W_Corner         : constant Wide_Graphic_Character := '+';
      
      -- See Unicode U2500 codepage (Box Drawing)
      Wide_Vertical_Line: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#2502#);
      Wide_Horizontal_Line: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#2500#);
      
      Wide_Top_Left: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#250C#);
      Wide_Top_Right: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#2510#);
      Wide_Bottom_Left: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#2514#);
      Wide_Bottom_Right: constant Wide_Graphic_Character
        := Wide_Graphic_Character'Val (16#2518#);
      
   begin
      
      if The_Surface.Wide_Support then
         The_Surface.Wide_Set_Border
           (Use_Cursor => Use_Cursor,
            
            Left_Side           => Wide_Vertical_Line,
            Right_Side          => Wide_Vertical_Line,
            Top_Side            => Wide_Horizontal_Line,
            Bottom_Side         => Wide_Horizontal_Line,
            
            Top_Left_Corner     => Wide_Top_Left,
            Top_Right_Corner    => Wide_Top_Right,
            Bottom_Left_Corner  => Wide_Bottom_Left,
            Bottom_Right_Corner => Wide_Bottom_Right);
         
      else
         The_Surface.Set_Border
           (Use_Cursor          => Use_Cursor,
                                 
            Left_Side           => Vertical_Line,
            Right_Side          => Vertical_Line,
            Top_Side            => Horizontal_Line,
            Bottom_Side         => Horizontal_Line,
            
            Top_Left_Corner     => Corner,
            Top_Right_Corner    => Corner,
            Bottom_Left_Corner  => Corner,
            Bottom_Right_Corner => Corner);
      end if;
      
   end Set_Border;
   ----------------------------------------
   
   generic
      type Character_Type is (<>);
      type String_Type    is array (Positive range <>) of Character_Type;
   
      with procedure Dispatch_Put (The_Surface: in out Frame'Class;
                                   Set_Cursor : in out Cursor'Class;
                                   Content    : in     String_Type);
   
   procedure Generic_Set_Border (The_Surface: in out Frame'Class;
                                 Use_Cursor : in     Cursor'Class;
                                
                                 Left_Side,
                                 Right_Side,
                                 Top_Side,
                                 Bottom_Side,
                                    
                                 Top_Left_Corner,
                                 Top_Right_Corner,
                                 Bottom_Left_Corner,
                                 Bottom_Right_Corner: in Character_Type);
   
   procedure Generic_Set_Border (The_Surface: in out Frame'Class;
                                 Use_Cursor : in     Cursor'Class;
                                 
                                 Left_Side,
                                   Right_Side,
                                   Top_Side,
                                   Bottom_Side,
                                   
                                   Top_Left_Corner,
                                   Top_Right_Corner,
                                   Bottom_Left_Corner,
                                   Bottom_Right_Corner: in Character_Type)
   is
      Extents: constant Cursor_Position := The_Surface.Extents;
      
      Set_Cursor: Cursor'Class := Use_Cursor;
      
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      -- Corners take priority. If the surface is not large enough for the most
      -- minimal frame (at least one left, right, top, bottom side), it shall
      -- be populated instead with as many corner characters as possible,
      -- starting with the top-left corner.
      
      -- Top-left
      Set_Cursor.Position := (1,1);
      Dispatch_Put (The_Surface => The_Surface,
                    Set_Cursor  => Set_Cursor,
                    Content     => String_Type'(1 .. 1 => Top_Left_Corner));
      
      -- Top-right
      if Extents.Column > 1 then
         Set_Cursor.Position := (Row => 1, Column => Extents.Column);
         Dispatch_Put (The_Surface => The_Surface,
                       Set_Cursor  => Set_Cursor,
                       Content     => String_Type'(1 ..1 => Top_Right_Corner));
         
         -- Bottom-right
         if Extents.Row > 1 then
            Set_Cursor.Position := Extents;
            Dispatch_Put
              (The_Surface => The_Surface,
               Set_Cursor  => Set_Cursor,
               Content     => String_Type'(1 .. 1 => Bottom_Right_Corner));
         end if;
      end if;
      
      -- Bottom-left
      if Extents.Row > 1 then
         Set_Cursor.Position := (Row    => Extents.Row,
                                 Column => 1);
         Dispatch_Put
           (The_Surface => The_Surface,
            Set_Cursor  => Set_Cursor,
            Content     => String_Type'(1 .. 1 => Bottom_Left_Corner));
      end if;
      
      -- Now we can handle the sides, if there is room
      
      -- Horizontal sides
      if Extents.Column > 2 then
         declare
            Horizontal_Side: String_Type (1 .. Positive (Extents.Column - 2))
              := (others => Top_Side);
         begin
            Set_Cursor.Position := (Row => 1, Column => 2);
            Dispatch_Put (The_Surface => The_Surface,
                          Set_Cursor  => Set_Cursor,
                          Content     => Horizontal_Side);
            
            if Extents.Row > 1 then
               Horizontal_Side     := (others => Bottom_Side);
               Set_Cursor.Position := (Row => Extents.Row, Column => 2);
               Dispatch_Put (The_Surface => The_Surface,
                             Set_Cursor  => Set_Cursor,
                             Content     => Horizontal_Side);
            end if;
         end;
      end if;
      
      -- Vertical sides
      if Extents.Row > 2 then
         -- Left
         Set_Cursor.Position.Column := 1;
         for R in 2 .. Extents.Row - 1 loop
            Set_Cursor.Position.Row := R;
            Dispatch_Put (The_Surface => The_Surface,
                          Set_Cursor  => Set_Cursor,
                          Content     => String_Type'(1 .. 1 => Left_Side)); 
         end loop;
         
         -- Right
         if Extents.Column > 1 then
            Set_Cursor.Position.Column := Extents.Column;
            for R in 2 .. Extents.Row - 1 loop
               Set_Cursor.Position.Row := R;
               Dispatch_Put
                 (The_Surface => The_Surface,
                  Set_Cursor  => Set_Cursor,
                  Content     => String_Type'(1 .. 1 => Right_Side));
            end loop;
         end if;
      end if;
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with "Unexpected exception: "
           & Exceptions.Exception_Information (e);
   end Generic_Set_Border;
                                    
   
   ----------------------------------------
   overriding
   procedure Set_Border (The_Surface: in out Frame;
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
      procedure Target_Put (The_Surface: in out Frame'Class;
                            Set_Cursor : in out Cursor'Class;
                            Content    : in     String)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- Already checked for Put
      begin
         The_Surface.Put (Set_Cursor => Set_Cursor,
                          Content    => Content);
      end Target_Put;
      
      procedure Set_Border_Actual is new Generic_Set_Border
        (Character_Type => Character,
         String_Type    => String,
         Dispatch_Put   => Target_Put);
      
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
   
   
   ---------------------
   -- Wide_Set_Border --
   ---------------------
   overriding
   procedure Wide_Set_Border (The_Surface: in out Frame;
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
      procedure Target_Put (The_Surface: in out Frame'Class;
                            Set_Cursor : in out Cursor'Class;
                            Content    : in     Wide_String)
      with Inline is
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- Already checked for Put
      begin
         The_Surface.Wide_Put (Set_Cursor    => Set_Cursor,
                               Content       => Content);
      end Target_Put;
      
      procedure Set_Border_Actual is new Generic_Set_Border
        (Character_Type => Wide_Character,
         String_Type    => Wide_String,
         Dispatch_Put   => Target_Put);
      
   begin
      
      if The_Surface.Wide_Support then
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
         
      else
         -- No wide support
         -- Unfortunately, we can't just delegate this to Wide_Put via
         -- Target_Put since that expects a function returning a String, and
         -- we have one that returns a Character. We could "roll our own" and
         -- pass that in, but it seems too indirect.
         
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
         
   end Wide_Set_Border;
   
   
   ---------------------
   -- Sample_Position --
   ---------------------
   overriding
   procedure Sample_Position
     (Source       : in out Frame;
      Position     : in     Cursor_Position;
      Content      :    out Graphic_Character;
      Styled_Cursor: in out Cursor'Class)
   is begin
      Source.Target.Sample_Position
        (Position      => Target_Position (Source, Position),
         Content       => Content,
         Styled_Cursor => Styled_Cursor);
   end Sample_Position;
   
   
   ----------------------------------------
   procedure Wide_Sample_Position 
     (Source       : in out Frame;
      Position     : in     Cursor_Position;
      Content      :    out Wide_Graphic_Character;
      Styled_Cursor: in out Cursor'Class)
   is begin
      Source.Target.Wide_Sample_Position
        (Position      => Target_Position (Source, Position),
         Content       => Content,
         Styled_Cursor => Styled_Cursor);
   end Wide_Sample_Position;
   
   
   ----------------------------------------
   function Sample_Position_Cursor (Source  : in out Frame;
                                    Position: in     Cursor_Position)
                                   return Cursor'Class
   is begin
      return Source.Target.Sample_Position_Cursor 
        (Target_Position (Source, Position));
   end Sample_Position_Cursor;
   
   
   ----------------
   -- Transcribe --
   ----------------
   overriding
   procedure Transcribe (Source : in out Frame;
                         Target : in out Surface'Class;
                         From   : in     Cursor'Class;
                         To     : in     Cursor'Class;
                         Rows   : in     Cursor_Ordinal;
                         Columns: in     Cursor_Ordinal;
                         Clip   : in     Boolean := False)
   is
      Extents: constant Cursor_Position := Source.Extents;
      
   begin
      if not Source.Available then
         raise Surface_Unavailable with "Source not Available";
         
      elsif not Target.Available then
         raise Surface_Unavailable with "Target not Available";
         
      elsif From.Position > Extents
        or else 
        Cursor_Position'(Row    => From.Position.Row    + Rows    - 1,
                         Column => From.Position.Column + Columns - 1) 
        > Extents
      then
         raise Cursor_Excursion;
         -- This does not check the target extents, as this will be handled by
         -- the actual call to the Target's Transcribe
      end if;
      
      declare
         Frame_Offset: constant Cursor_Position := Source.State.Target_TL;
         Mod_From: Cursor'Class := From;
      begin
         Mod_From.Position := Mod_From.Position + Frame_Offset - (1,1);
         
         Source.Target.Transcribe (Target  => Target,
                                   From    => Mod_From,
                                   To      => To,
                                   Rows    => Rows,
                                   Columns => Columns,
                                   Clip    => Clip);
      end;
      
   exception
      when Surface_Unavailable | Cursor_Excursion | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Transcribe;
   
   
   ----------------------------------------
   overriding procedure Transcribe (Source : in out Frame;
                                    Target : in out Surface'Class;
                                    Rows   : in     Cursor_Ordinal;
                                    Columns: in     Cursor_Ordinal;
                                    Clip   : in     Boolean := False)
   is
   begin
      
      Source.Transcribe (Target  => Target,
                         From    => Source.Current_Cursor,
                         To      => Target.Current_Cursor,
                         Rows    => Rows,
                         Columns => Columns,
                         Clip    => Clip);
      
   exception
      when Surface_Unavailable | Cursor_Excursion | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Transcribe;
   
end Curses.Frames;
