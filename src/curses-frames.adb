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
      function  Background_Character return Character is (BG_Char);
      
      procedure Background_Character (Set: in Character) is
      begin
         BG_Char := Set;
      end Background_Character;
      
      
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
         raise Surface_Unavailable;
      end if;
      
      T_Extents := The_Frame.Target.Extents;
      
      The_Frame.Reframe 
        (Top_Left         => (Margin + 1, Margin + 1),
          Proposed_Extents => T_Extents - Cursor_Position'(Margin, Margin));
      
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
           := Mod_Cursor.Position + The_Frame.State.Target_TL - (1,1);
         
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
         TL: constant Cursor_Position := The_Frame.State.Target_TL;
         BR: constant Cursor_Position
           := TL + The_Frame.Extents - (1,1);
      begin
         -- Remember that the comparison operators for Cursor_Position are based
         -- on the idea of a cursor being *either* to the right or below (i.e.
         -- an extent), we need to be a little more explicit here for seeing if
         -- the Target's Cursor is in the Frame
         
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
            Mod_Cursor.Position := 
              Mod_Cursor.Position + The_Surface.State.Target_TL - (1,1);
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
            Mod_Cursor.Position := 
              Mod_Cursor.Position + The_Surface.State.Target_TL - (1,1);
            
            The_Surface.Target.Current_Cursor (Mod_Cursor);
         end if;
      end;
      
   exception
      when Cursor_Excursion => raise;
      when others           => null;
      
   end Position_Cursor;
   
   
   -- Clear Group Common Infrastructure --
   ---------------------------------------
   procedure Clear_Block (The_Surface: in out Frame;
                          First_Row  : in     Cursor_Ordinal;
                          Last_Row   : in     Cursor_Ordinal)
     with Inline
   is
      Extents : constant Cursor_Position := The_Surface.Extents;
      Blanks  : constant String (1 .. Positive (Extents.Column))
        := (others => The_Surface.State.Background_Character);
      
      Use_Cursor: Cursor'Class := The_Surface.State.Background_Cursor;

   begin
      Use_Cursor.Position.Column := 1;
      
      for I in First_Row .. Last_Row loop
         Use_Cursor.Position.Row := I;
         The_Surface.Put (Set_Cursor => Use_Cursor,
                          Content    => Blanks);
         -- Note that Put will do the actual Extents check for Row.
         -- This means that if Last_Row is beyond Extents, then we will
         -- still clear up to the last Row
      end loop;
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
   procedure Clear_To_End (The_Surface: in out Frame) is
      C: Cursor'Class := The_Surface.Current_Cursor;
   begin
      The_Surface.Clear_To_End (From => C);
      
   exception
      when others => null;
   end Clear_To_End;
   
   
   ----------------------------------------
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
   overriding 
   procedure Put (The_Surface   : in out Frame;
                  Set_Cursor    : in out Cursor'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
   is
      Write_Position: Cursor_Position := Set_Cursor.Position;
      Write_Cursor  : Cursor'Class    := Set_Cursor;
      
      pragma Assertion_Policy (Pre'Class => Ignore);
      -- We have passed the class-wide precondition on Content on entry to this
      -- subprogram, So we disable it for the computer's dispatches to Put,
      -- since we know that the Precondition has been passed already if we got
      -- this far.
      
      package Computer is new Put_Computer
        (The_Surface => Surface'Class (The_Surface),
         Write_Head  => Write_Position,
         Justify     => Justify,
         Overflow    => Overflow,
         Content     => Content);
      
      Select_First: Natural renames Computer.Select_First;
      Select_Last : Natural renames Computer.Select_Last;
      
      Offset: constant Cursor_Position := The_Surface.State.Target_TL;
      
   begin
      -- Really just a matter of translating the position and then passing it
      -- on to the Target
      
      if not The_Surface.Available then
         raise Surface_Unavailable;
      elsif Set_Cursor.Position > The_Surface.Extents then
         raise Cursor_Excursion;
      end if;
      
      while Computer.Compute_Line loop
         Write_Cursor.Position := Write_Position + Offset - (1,1);
         The_Surface.Target.Put 
           (Set_Cursor => Write_Cursor,
            Content    => Content (Select_First .. Select_Last),
            Justify    => Justify,
            Overflow   => Overflow);
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
   end Put;
   
   
   ----------
   -- Fill --
   ----------
   overriding
   procedure Fill (The_Surface: in out Frame;
                   Pattern    : in     String)
   is
   begin
      The_Surface.Fill (Pattern     => Pattern,
                        Fill_Cursor => The_Surface.Current_Cursor);
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Fill;
   
   ----------------------------------------
   overriding
   procedure Fill (The_Surface: in out Frame;
                   Pattern    : in     String;
                   Fill_Cursor: in     Cursor'Class)
   is
      Extents: constant Cursor_Position := The_Surface.Extents;
   begin
      if not The_Surface.Available then
         raise Surface_Unavailable;
      end if;
      
      declare
         Use_Cursor: Cursor'Class := Fill_Cursor;
         Expanded_Pattern: String 
           (1 .. Positive (Extents.Row * Extents.Column));
      begin
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
      end;
      
   exception
      when Assertion_Error | Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Fill;
   
   
   --------------------
   -- Set_Background --
   --------------------
   overriding
   procedure Set_Background (The_Surface   : in out Frame;
                             Fill_Character: in     Character := ' ')
   is
      pragma Assertion_Policy (Pre'Class => Ignore);
      -- We cleared this on entry to this subprogram
      
      Use_Cursor: Cursor'Class := The_Surface.Current_Cursor;
   begin
      The_Surface.Set_Background (Fill_Character => Fill_Character,
                                  Fill_Cursor    => Use_Cursor);
      
   exception
      when Surface_Unavailable | Curses_Library =>
         raise;
         
      when e: others =>
         raise Curses_Library with
           "Unexpected exception: " & Exceptions.Exception_Information (e);
      
   end Set_Background;
   
   
   ----------------------------------------
   overriding
   procedure Set_Background (The_Surface   : in out Frame;
                             Fill_Character: in     Character := ' ';
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
