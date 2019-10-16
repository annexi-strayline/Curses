------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

with Curses.Frames;

package body Curses.UI.Menus.Renderer is
   
   -----------
   -- Reset --
   -----------
   procedure Reset (Driver: in out Menu_Renderer) is
      This_Label: Cursor_Ordinal;
      
   begin
      Driver := (Ada.Finalization.Controlled with
                 
                 Canvas            => Driver.Canvas,
                 Input_Surface     => Driver.Input_Surface,
                 Branch            => Driver.Branch,
                 
                 Scrollbar_Enabled => Driver.Scrollbar_Enabled,
                 Marker            => Driver.Marker,
                 Bar               => Driver.Bar,
                 
                 others            => <>);
      
      Driver.Item_Count := Driver.Branch.Item_Count;
      
      for Item of Driver.Branch.all loop
         This_Label := Item.Label_Length;
         
         if This_Label > Driver.Longest_Label then
            Driver.Longest_Label := This_Label;
         end if;
         
         if not Driver.Have_Hotkeys
           and then Item.Hot_Key.Class not in No_Key
         then
            Driver.Have_Hotkeys := True;
         end if;
      end loop;
      
      Driver.Canvas_Extents := Driver.Canvas.Extents;
   end Reset;
   
   -----------------
   -- Wrap_Around --
   -----------------
   procedure Enable_Wrap_Around  (Driver: in out Menu_Renderer) is
   begin
      Driver.Wrap_Around := True;
   end Enable_Wrap_Around;
   
   procedure Disable_Wrap_Around (Driver: in out Menu_Renderer) is
   begin
      Driver.Wrap_Around := False;
   end Disable_Wrap_Around;
   
   
   ----------------------
   -- Enable_Scrollbar --
   ----------------------
   procedure Set_Scrollbar_Styles (Driver: in out Menu_Renderer'Class;
                                   Marker_Style: in Cursor'Class;
                                   Bar_Style: in Cursor'Class)
   with Inline is
      use Curses.Terminals.Color;
   begin
      -- Marker
      if Marker_Style in Colored_Cursor then
         Driver.Marker.Style := (Is_Colored => True,
                                 Colored    => Colored_Cursor (Marker_Style));
      else
         Driver.Marker.Style := (Is_Colored => False,
                                 Monochrome => Cursor (Marker_Style));
      end if;
      
      -- Bar
      if Bar_Style in Colored_Cursor then
         Driver.Bar.Style := (Is_Colored => True,
                              Colored    => Colored_Cursor (Bar_Style));
      else
         Driver.Bar.Style := (Is_Colored => False,
                              Monochrome => Cursor (Bar_Style));
      end if;
   end Set_Scrollbar_Styles;
   
   
   procedure Enable_Scrollbar
     (Driver          : in out Menu_Renderer;
      Marker_Style    : in     Cursor'Class;
      Marker_Character: in     Graphic_Character := ' ';
      Bar_Style       : in     Cursor'Class;
      Bar_Character   : in     Graphic_Character := ' ')
   is
      pragma Assertion_Policy (Static_Predicate => Check);
      
   begin
      Set_Scrollbar_Styles (Driver, Marker_Style, Bar_Style);
      
      Driver.Marker.Character  := (Wide => False, Char => Marker_Character);
      Driver.Bar.Character     := (Wide => False, Char => Bar_Character);
      Driver.Scrollbar_Enabled := True;
      
   end Enable_Scrollbar;
   
   
   ----------------------------------------------------------------------------
   procedure Wide_Enable_Scrollbar
     (Driver          : in out Menu_Renderer;
      Marker_Style    : in     Cursor'Class;
      Marker_Character: in     Wide_Graphic_Character := ' ';
      Bar_Style       : in     Cursor'Class;
      Bar_Character   : in     Wide_Graphic_Character := ' ';
      Wide_Fallback   : access function (Item: Wide_Graphic_Character) 
                                        return Graphic_Character := null)
   is
      pragma Assertion_Policy (Static_Predicate => Check);
   begin
      if Driver.Canvas.Wide_Support then
         Set_Scrollbar_Styles (Driver, Marker_Style, Bar_Style);
         
         Driver.Marker.Character  := (Wide      => True, 
                                      Wide_Char => Marker_Character);
         Driver.Bar.Character     := (Wide      => True, 
                                      Wide_Char => Bar_Character);
         Driver.Scrollbar_Enabled := True;
         
      elsif Wide_Fallback = null then
         raise Curses_Library with "Menu Canvas does not have Wide support "
           & "and no fallback provided.";
         
      else
         Driver.Enable_Scrollbar 
           (Marker_Style     => Marker_Style,
            Marker_Character => Wide_Fallback (Marker_Character),
            Bar_Style        => Bar_Style,
            Bar_Character    => Wide_Fallback (Bar_Character));
      end if;
   end Wide_Enable_Scrollbar;
   
   
   -----------------------
   -- Disable_Scrollbar --
   -----------------------
   procedure Disable_Scrollbar (Driver: in out Menu_Renderer) is
   begin
      Driver.Scrollbar_Enabled := False;
   end Disable_Scrollbar;
   
   
   ----------------------
   -- Interaction_Loop --
   ----------------------
   procedure Interaction_Loop 
     (Driver         : in out Menu_Renderer;
      Selected_Item  :    out Menu_Cursor_Type'Class;
      Update_Selected: in     Boolean := False;
      Last_Key       :    out Control_Character;
      Hotkey_Select  :    out Boolean)
   is
      subtype Frame is Curses.Frames.Frame;
      
      Iterator: constant Menu_Iterators.Reversible_Iterator'Class
        := Driver.Branch.Iterate;
      
      
      ---------------
      -- Frame_Row --
      ---------------
      -- Frams a given Row on Canvas for use by the menu item Render operation
      
      function Frame_Row (Row: Cursor_Ordinal) 
                         return Frame'Class with Inline 
      is
         use Curses.Frames;
         
         Adjusted_Width: constant Cursor_Ordinal
           := (if Driver.Scrollbar_Enabled then
                  Driver.Canvas_Extents.Column - 1
               else
                  Driver.Canvas_Extents.Column);
      begin
         return New_Frame 
           (Target           => Driver.Canvas,
            Top_Left         => (Row    => Row,
                                 Column => 1),
            Proposed_Extents => (Row    => 1,
                                 Column => Adjusted_Width));
      end Frame_Row;
      
      
      ----------------
      -- Render_All --
      ----------------
      -- Renders all visible items on the Canvas, according to the
      -- Scroll_Offset
      
      procedure Render_All is
         
         Item: Menu_Cursor_Type'Class := Iterator.First;
      begin
         Driver.Canvas.Clear;
         
         -- Skip items off the top (if Scroll_Offset is > 0)
         for I in 1 .. Driver.Scroll_Offset loop
            Item     := Iterator.Next (Item);
            exit when not Item.Has_Element;
         end loop;
         
         for R in 1 .. Driver.Canvas_Extents.Row loop
            exit when not Item.Has_Element;
            
            declare
               Row_Canvas: Frame'Class := Frame_Row (R);
            begin
               Driver.Branch.all(Item).Render_Label 
                 (Canvas   => Row_Canvas,
                  Selected => Item = Selected_Item);
               
               if Item = Selected_Item then
                  Driver.Selected_Row := R;
               end if;
            end;
            
            Item := Iterator.Next (Item);
         end loop;
         
         if Driver.Item_Count > Natural (Driver.Canvas_Extents.Row)
           and then Driver.Scrollbar_Enabled
         then
            -- Render the scroll position indicator
            -- What is the percentage of all unshown items which are above
            -- the current position
            declare
               Rows_Outside: constant Natural
                 := Driver.Item_Count - Natural (Driver.Canvas_Extents.Row);
               
               Rows_Above: Natural renames Driver.Scroll_Offset;
               
               Percent_Above: constant Float 
                 := Float (Rows_Above) 
                   / Float (Rows_Outside);
               
               Marker_Row: Natural 
                 := Natural (Float (Driver.Canvas_Extents.Row) 
                               * Percent_Above);
               
               procedure Do_Put 
                 (Use_Cursor: in out Cursor'Class;
                  Char      : in     Scrollbar_Component_Char) 
               is
               begin
                  Driver.Canvas.Put 
                    (Set_Cursor => Use_Cursor,
                     Content    => String'(1 .. 1 => Char.Char));
               end Do_Put;
               
               procedure Do_Wide_Put 
                 (Use_Cursor: in out Cursor'Class;
                  Char      : in     Scrollbar_Component_Char) 
               is
               begin
                  Driver.Canvas.Wide_Put 
                    (Set_Cursor => Use_Cursor,
                     Content    => Wide_String'(1 .. 1 => Char.Wide_Char));
               end Do_Wide_Put;
               
            begin
               if Marker_Row = 0 then
                  Marker_Row := 1;
               end if;
               
               -- First pain the bar itself
               declare
                  Bar_Cursor: Cursor'Class 
                    := (if Driver.Bar.Style.Is_Colored then
                           Driver.Bar.Style.Colored
                        else
                           Driver.Bar.Style.Monochrome);
                  
                  Put_Op: access procedure
                    (Use_Cursor: in out Cursor'Class;
                     Char      : in     Scrollbar_Component_Char)
                    := (if Driver.Bar.Character.Wide then
                           Do_Wide_Put'Access
                        else
                           Do_Put'Access);
               begin
                  for I in 1 .. Driver.Canvas_Extents.Row loop
                     Bar_Cursor.Position
                       := (Row    => I,
                           Column => Driver.Canvas_Extents.Column);
                     
                     Put_Op (Bar_Cursor, Driver.Bar.Character);
                  end loop;
               end;
               
               declare
                  Marker_Cursor: Cursor'Class
                    := (if Driver.Marker.Style.Is_Colored then
                           Driver.Marker.Style.Colored
                        else
                           Driver.Marker.Style.Monochrome);
               begin
                  Marker_Cursor.Position 
                    := (Row    => Cursor_Ordinal (Marker_Row),
                        Column => Driver.Canvas_Extents.Column);
                  
                  if Driver.Marker.Character.Wide then
                     Do_Wide_Put (Marker_Cursor, Driver.Marker.Character);
                  else
                     Do_Put (Marker_Cursor, Driver.Marker.Character);
                  end if;
               end;
            end;
         end if;
      end Render_All;
      
      ---------------------
      -- Change_Selected --
      ---------------------
      type Change_Hint is (Above, Below);
      -- New_Selection's position relative to Selected_Item
      
      procedure Change_Selected (New_Selection: in Menu_Cursor_Type'Class;
                                 Hint         : in Change_Hint) is
         Nominal_Row: Cursor_Ordinal;
         -- Nominal in this case means the row of an item if the menu was
         -- not scrolled, and thus the first item was always at row 1.
         
         Top_Nominal: constant Cursor_Ordinal
           := Cursor_Ordinal (Driver.Scroll_Offset + 1);
         -- The nominal row currently at the top of the menu
         
         Bottom_Nominal: constant Cursor_Ordinal
           := Top_Nominal + Driver.Canvas_Extents.Row - 1;
         -- The nominal row currently at the bottom of the menu
         
         pragma Assertion_Policy (Check);
      begin
         if New_Selection = Selected_Item then
            -- Do nothing if New_Selection is already selected
            return;
         end if;
         
         pragma Assert (New_Selection.Has_Element);
         
         -- Compute the current nominal row by seeking to the new selection,
         -- using the hint if possible
         
         Nominal_Row := Cursor_Ordinal 
           (Natural (Driver.Selected_Row) + Driver.Scroll_Offset);
         -- First set the Nominal_Row to that of the current item, and then we
         -- can manipulate it appropriately
         
         case Hint is
            when Above =>
               -- Reverse search from the current item
               declare
                  I: Menu_Cursor_Type'Class 
                    := Iterator.Previous (Selected_Item);
               begin
                  loop
                     pragma Assert (I.Has_Element);
                     -- This should not happen, but it would cause an infinite
                     -- loop. Safety first
                     
                     Nominal_Row := Nominal_Row - 1;
                     exit when I = New_Selection;
                     
                     I := Iterator.Previous (I);
                  end loop;
               end;
               
            when Below =>
               declare
                  I: Menu_Cursor_Type'Class := Iterator.Next (Selected_Item);
               begin
                  loop
                     pragma Assert (I.Has_Element);
                     -- This should not happen, but it would cause an infinite
                     -- loop. Safety first
                     
                     Nominal_Row := Nominal_Row + 1;
                     exit when I = New_Selection;
                     
                     I := Iterator.Next (I);
                  end loop;
               end;
         end case;
         
         
         if Nominal_Row in Top_Nominal .. Bottom_Nominal then
            -- Nice, we dont need to re-render, but we do need to
            -- un-select the selected item and re-select it, unless
            -- it is already selected
            
            -- Deselect
            declare 
               Row_Canvas: Frame'Class := Frame_Row (Driver.Selected_Row);
            begin
               Driver.Branch.all(Selected_Item).Render_Label 
                 (Canvas   => Row_Canvas,
                  Selected => False);
            end;
            
            Selected_Item := New_Selection;
            Driver.Selected_Row := Cursor_Ordinal 
              (Natural (Nominal_Row) - Driver.Scroll_Offset);
            
            declare 
               Row_Canvas: Frame'Class := Frame_Row (Driver.Selected_Row);
            begin
               Driver.Branch.all(Selected_Item).Render_Label 
                 (Canvas   => Row_Canvas,
                  Selected => True);
            end;

            
         elsif Nominal_Row < Top_Nominal then
            -- We need to scroll up such that the selected item
            -- becomes visible
            
            -- Selected_Row should be 1, so we need to calculate
            -- the correct Scroll_Offset to acheive that.
            
            -- We can do that by trying to get Top_Nominal to
            -- equal Nominal_Row, and then solving for
            -- Scroll_Offset
            Driver.Scroll_Offset := Natural (Nominal_Row) - 1;
            Driver.Selected_Row  := 1;
            Selected_Item := New_Selection;
            Render_All;
            
         elsif Nominal_Row > Bottom_Nominal then
            -- Similarily, we need to solve for Offset based
            -- on the desired Bottom_Nominal = Nominal_Row
            
            -- Top_Nominal = Scroll_Offset + 1
            -- Bottom_Nominal = Top_Nominal + Frame_Extents.Row - 1
            -- Bottom_Nominal = (Scroll_Offset + 1) 
            --                    + Frame_Extents.Row - 1
            -- Bottom_Nominal = Scroll_Offset + Frame_Extents.Row
            -- Scroll_Offset = Bottom_Nominal - Frame_Extents.Row
            -- Scroll_Offset = (Norminal_Row) - Frame_Extents.Row
            Driver.Scroll_Offset 
              := Natural (Nominal_Row - Driver.Canvas_Extents.Row);
            Driver.Selected_Row  := Driver.Canvas_Extents.Row;
            Selected_Item := New_Selection;
            Render_All;
            
         end if;
      end Change_Selected;
      
      
      
      ------------------------
      -- Select_With_Hotkey --
      ------------------------
      -- Called when we know we have hotkeys (Driver.Have_Hotkeys = True), and
      -- when we get a character that is not for usual navigation.
      
      procedure Select_With_Hotkey (Found: out Boolean) is 
         Select_Hint: Change_Hint := Above;
      begin
         if Last_Key.Class not in Graphic then
            Found := False;
            return;
         end if;
         
         for I in Iterator loop
            if I = Selected_Item then
               Select_Hint := Below;
            end if;
            
            if Driver.Branch.all(I).Hot_Key = Last_Key then
               -- Found a match, set it up!
               Change_Selected (New_Selection => I,
                                Hint          => Select_Hint);
               Found := True;
               return;
            end if;
         end loop;
         
         -- No match
         Found := False;
         return;
      end Select_With_Hotkey;
      
   begin
      Selected_Item := Iterator.First;
      -- We need this as a starting point in all cases, but this also ensures
      -- that we initialize this before we try to use it.
      
      -- Check for changes to Canvas or Branch which should trigger
      -- a reset
      if not Driver.Initial and then 
        (Driver.Item_Count /= Driver.Branch.Item_Count
           or else Driver.Canvas_Extents /= Driver.Canvas.Extents)
      then
         Driver.Reset;
      end if;
      
      if Driver.Item_Count = 0 then
         -- Nothing to work with
         Last_Key := (Class => No_Key, others => <>);
         Hotkey_Select := False;
         return;
      end if;
      
      -- Seek Selected_Item based on Scroll_Offset and Selected_Row
      for I in 1 .. Driver.Scroll_Offset loop
         Selected_Item := Iterator.Next (Selected_Item);
      end loop;
      
      -- Conceptually, Iterator.First is row one, so if Selected_Row = 1,
      -- we won't need to skip. That's why we start at 2
      for I in 2 .. Driver.Selected_Row loop
         Selected_Item := Iterator.Next (Selected_Item);
      end loop;
      
      if Driver.Initial then
         Render_All;
         Driver.Initial := False;
         
      elsif Update_Selected then
         declare
            Row_Canvas: Frame'Class := Frame_Row (Driver.Selected_Row);
         begin
            Driver.Branch.all(Selected_Item).Render_Label
              (Canvas   => Row_Canvas,
               Selected => True);
         end;
      end if;
      
      -- We're all set-up and ready to rock.
        Selection_Loop:
      loop
         Last_Key := Driver.Input_Surface.Input_Key (Wait => True);
         
         case Last_Key.Class is
            when Up_Key =>
               -- Check for backstop
               if Selected_Item /= Iterator.First then
                  Change_Selected 
                    (New_Selection => Iterator.Previous (Selected_Item),
                     Hint          => Above);
               elsif Driver.Wrap_Around then
                  Change_Selected
                    (New_Selection => Iterator.Last,
                     Hint          => Below);
               end if;
               
            when Down_Key =>
               if Selected_Item /= Iterator.Last then
                  Change_Selected 
                    (New_Selection => Iterator.Next (Selected_Item),
                     Hint          => Below);
               elsif Driver.Wrap_Around then
                  Change_Selected
                    (New_Selection => Iterator.First,
                     Hint          => Above);
               end if;
               
            when Page_Up =>
               if Selected_Item /= Iterator.First then
                  declare
                     New_Selection: Menu_Cursor_Type'Class := Selected_Item;
                     
                     pragma Assertion_Policy (Check);
                  begin
                     -- Two conceptual steps:
                     -- 1. Seek up to row 1
                     -- 2. Seek up to another canvas height - 1
                     -- If the offset is < canvas height, then we just
                     -- seek directly to the top.
                     --
                     -- After doing this, Change_Selected handles the rest
                     -- for us.
                     
                     if Driver.Scroll_Offset = 0 
                       or else Driver.Scroll_Offset 
                       < Natural (Driver.Canvas_Extents.Row)
                     then
                        -- This means there is less than an entire page
                        -- above us, so we can just jump directly to the top
                        New_Selection := Iterator.First;
                        
                     else
                        -- This means we have more than one "page" above us
                        -- So we want to land at the top row of the page above
                        
                        for I in 1 .. 
                          Driver.Selected_Row + Driver.Canvas_Extents.Row - 1 
                        loop
                           New_Selection := Iterator.Previous (New_Selection);
                        end loop;
                        
                        pragma Assert (New_Selection.Has_Element);
                     end if;
                     
                     Change_Selected (New_Selection => New_Selection,
                                      Hint          => Above);
                  end;
               end if;
               
            when Page_Down =>
               if Selected_Item /= Iterator.Last then
                  declare
                     New_Selection: Menu_Cursor_Type'Class := Selected_Item;
                     Next_Item    : Menu_Cursor_Type'Class := Selected_Item;
                     Seek_Count   : constant Natural
                       := Natural (Driver.Canvas_Extents.Row 
                                     - Driver.Selected_Row
                                     + Driver.Canvas_Extents.Row);
                     
                     pragma Assertion_Policy (Check);
                  begin
                     -- Two conceptual steps:
                     -- 1. Seek down to the last row
                     -- 2. Seek down one more page length
                     -- 3. Exit the seek as soon as we get to the end of the
                     --    list
                     --
                     -- If we are already on the last page, step number 1 will
                     -- reach the last item.
                     --
                     -- After doing this, Change_Selected handles the rest
                     -- for us.
                     
                     for I in 1 .. Seek_Count loop
                        Next_Item := Iterator.Next (New_Selection);
                        exit when not Next_Item.Has_Element;
                        New_Selection := Next_Item;
                     end loop;
                     
                     pragma Assert (New_Selection.Has_Element);
                     
                     if Next_Item.Has_Element then
                        -- This means the loop completed, but we are not at the
                        -- end of the list, so we should set the New_Selection
                        -- to Next
                        New_Selection := Next_Item;
                        -- This gets us to the top of the "next page"
                     end if;
                     
                     Change_Selected (New_Selection => New_Selection,
                                      Hint          => Below);
                  end;
               end if;
               
            when others =>
               -- Non movement item, check for Hotkey and return
               
               if Driver.Have_Hotkeys then
                  Select_With_Hotkey (Hotkey_Select);
               end if;
               
               return;
         end case;
      end loop Selection_Loop;
      
   end Interaction_Loop;
   
   
   ----------------
   -- Initialize --
   ----------------
   overriding
   procedure Initialize (Driver: in out Menu_Renderer) is
   begin
      Driver.Reset;
   end Initialize;
   
end Curses.UI.Menus.Renderer;
