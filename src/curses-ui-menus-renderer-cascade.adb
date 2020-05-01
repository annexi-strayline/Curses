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

with Curses.Frames; use Curses.Frames;

procedure Curses.UI.Menus.Renderer.Cascade
  (On_Screen : aliased in out Screen'Class;
   Branch    : aliased in out Menu_Type'Class;
   
   Min_Width : in Cursor_Ordinal := Default_Min_Width;
   Max_Width : in Cursor_Ordinal := Default_Max_Width;
   
   Min_Height: in Cursor_Ordinal := Default_Min_Height;
   Max_Height: in Cursor_Ordinal := Default_Max_Height;
   
   Scrollbar_Provision: in Natural := Default_Scrollbar_Provision)
is
   function Compute_Longest_Label (For_Branch: in out Menu_Type'Class)
                                  return Cursor_Ordinal 
   with Inline is begin
      return Longest_Label: Cursor_Ordinal := 1 do
         for Item of Branch loop
            if Item.Label_Length > Longest_Label then
               Longest_Label := Item.Label_Length;
            end if;
         end loop;
      end return;
   end Compute_Longest_Label;
   
   
   function Compute_Frame_Extents (For_Branch: in out Menu_Type'Class)
                                  return Cursor_Position
   with Inline is
      Item_Count: constant Natural := For_Branch.Item_Count;
      Longest_Label: constant Cursor_Ordinal
        := Compute_Longest_Label (For_Branch);
   begin
      
      if Item_Count = 0 then
         return (1,1);
      end if;
      
      return E: Cursor_Position do
         E.Row := Cursor_Ordinal (Item_Count);
         
         if E.Row < Min_Height then
            E.Row := Min_Height;
         elsif E.Row > Max_Height then
            E.Row := Max_Height;
         end if;
         
         E.Column := Cursor_Ordinal 
           (Natural (Longest_Label) + Scrollbar_Provision);
         
         if E.Column < Min_Width then
            E.Column := Min_Width;
         elsif E.Column > Max_Width then
            E.Column := Max_Width;
         end if;
      end return;
   end Compute_Frame_Extents;
   
   
   procedure Recursive_Open 
     (On_Screen     : aliased in out Screen'Class;
      Current_Branch: aliased in out Menu_Type'Class;
      Root_Item     :         in     Menu_Cursor_Type'Class;
      TL_Hint       :         in     Cursor_Position;
      Kill_Tree     :            out Boolean);
   
   procedure Recursive_Open 
     (On_Screen     : aliased in out Screen'Class;
      Current_Branch: aliased in out Menu_Type'Class;
      Root_Item     :         in     Menu_Cursor_Type'Class;
      TL_Hint       :         in     Cursor_Position;
      Kill_Tree     :            out Boolean)
   is
      subtype Control_Class     is Curses.Terminals.Surfaces.Control_Class;
      subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
      
      use all type Control_Class;
      
   begin
      if Current_Branch.Item_Count = 0 then
         Kill_Tree := False;
         return;
      end if;
      
      declare
         Master_Window: aliased Framed_Window'Class
           := Frame_And_Style 
             (On_Screen     => On_Screen,
              Root_Item     => Root_Item,
              Frame_Extents => Compute_Frame_Extents (Current_Branch),
              TL_Hint       => TL_Hint);
         
         Canvas: aliased Frame'Class := Master_Window.Get_Frame;
         
         Renderer: Menu_Renderer (Canvas        => Canvas'Access,
                                  Input_Surface => Master_Window'Access,
                                  Branch        => Current_Branch'Access);
         
         Iterator: constant Menu_Iterators.Reversible_Iterator'Class
           := Current_Branch.Iterate;
         
         Selected_Item  : Menu_Cursor_Type'Class := Iterator.First;
         Selected_Row   : Cursor_Ordinal;
         Update_Selected: Boolean                := False;
         Last_Key       : Control_Character;
         Hotkey_Select  : Boolean;
         Last_Directive : After_Execute_Directive;
      begin
         Configure_Renderer (Renderer);
         Master_Window.Show;
         
         loop
            Renderer.Interaction_Loop (Selected_Item   => Selected_Item,
                                       Canvas_Row      => Selected_Row,
                                       Update_Selected => Update_Selected,
                                       Last_Key        => Last_Key,
                                       Hotkey_Select   => Hotkey_Select);
            
            Update_Selected := False;
            
            -- Intercept Left_Right_Navigation keys if enabled
            if Left_Right_Navigation then
               
               if Last_Key.Class = Left_Key
                 and then Root_Item.Has_Element
               then
                  Last_Key := (Class => Escape, others => <>);
                  
               elsif Last_Key.Class = Right_Key
                 and then Current_Branch(Selected_Item).Submenu.Item_Count > 0
               then
                  Last_Key := (Class => Enter, others => <>);
               end if;
            end if;
            
            -- Intercept hotkey selects
            if Hotkey_Select then
               Last_Key := (Class => Enter, others => <>);

            end if;
            
            
            -- Should be a close or execute. Anything else is a no-action,
            -- and we'll go right back into the interaction loop;
            if Last_Key.Class = Escape then
               Kill_Tree := False;
               return;
               
               
            elsif Last_Key.Class = Enter then
               Current_Branch(Selected_Item).Execute (Last_Directive);
               
               case Last_Directive is
                  when Open_Submenu =>
                     declare
                        New_Hint: constant Cursor_Position
                          := Master_Window.Top_Left + Canvas.Top_Left - (1,1)
                            + (Row    => Selected_Row, 
                               Column => Canvas.Extents.Column)
                            - (1,1);
                        
                        Next_Branch: Menu_Type'Class
                          := Current_Branch(Selected_Item).Submenu;
                     begin
                        if Hide_Parent then
                           Master_Window.Hide;
                        end if;
                        
                        Recursive_Open 
                          (On_Screen      => On_Screen,
                           Current_Branch => Next_Branch,
                           Root_Item      => Selected_Item,
                           TL_Hint        => New_Hint,
                           Kill_Tree      => Kill_Tree);
                        
                        if Kill_Tree then
                           return;
                        end if;
                        
                        -- Otherwise simply go back to the menu!
                        if Hide_Parent then
                           Master_Window.Show;
                        end if;
                     end;
                     
                  when Update =>
                     Update_Selected := True;
                     
                  when Close_Menu =>
                     Kill_Tree := False;
                     return;
                     
                  when Close_Tree =>
                     Kill_Tree := True;
                     return;
               end case;
            end if;
         end loop;
      end;
   end Recursive_Open;
   
   
   KT_Discard: Boolean;
begin
   
   Recursive_Open (On_Screen      => On_Screen,
                   Current_Branch => Branch,
                   Root_Item      => Null_Menu_Cursor,
                   TL_Hint        => (1,1),
                   Kill_Tree      => KT_Discard);
   
end Curses.UI.Menus.Renderer.Cascade;
