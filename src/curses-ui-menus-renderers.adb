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

package body Curses.UI.Menus.Renderers is
   
   -----------------------------
   -- Generic_Style_And_Frame --
   -----------------------------
   function Generic_Style_And_Frame 
     (S                  : aliased in out Screen'Class;
      Main_Menu          :         in     Boolean;
      Conceptual_Top_Left:         in     Cursor_Position;
      Required_Extents   :         in     Cursor_Position;
      Frame_Top_Left     :            out Cursor_Position)
     return Window'Class
   is
      pragma Assertion_Policy (Check);
      
      Window_Extents: Cursor_Position;
      Window_TL     : Cursor_Position := Conceptual_Top_Left;
   begin
      -- First we need to calculate what the actual size should be
      Window_Extents := Required_Extents 
        + (Row    => Cursor_Ordinal (Vertical_Padding   + 1) * 2,
           Column => Cursor_Ordinal (Horizontal_Padding + 1) * 2);
      -- Border (1) + Padding on both sides
      
      
      -- Adjust for Main_Menu
      if Main_Menu then
         if Top_Clear_On_Main then
            Window_Extents.Row := Window_Extents.Row - 1;
         end if;
         
         -- Adjust Windows_TL for fly-outs (when not Main_Menu)
         Window_TL.Row := Cursor_Ordinal 
           (Integer (Window_TL.Row) + Vertical_Fly_Out_Offset);
         
         Window_TL.Column := Cursor_Ordinal 
           (Integer (Window_TL.Column) + Horizontal_Fly_Out_Offset);
      end if;
      
      -- Now we can create the Window and style it
      return W: Window'Class := S.New_Window 
        (Top_Left         => Window_TL,
         Proposed_Extents => Window_Extents)
      do
         Fill (W);
         Set_Border (The_Surface => W,
                     Main_Menu   => Main_Menu);
         
         -- Finally set frame extents properly
         Frame_Top_Left := Window_TL
           + (Row    => Cursor_Ordinal (Vertical_Padding + 1),
              Column => Cursor_Ordinal (Horizontal_Padding + 1));
         
         pragma Assert (Window_Extents = W.Extents);
         pragma Assert (Frame_Top_Left < Window_Extents);
         pragma Assert 
           (Frame_Top_Left + Required_Extents - (1,1) < Window_Extents);
                            
      end return;
   end Generic_Style_And_Frame;
   
   
   -----------------------
   -- Simple_Set_Border --
   -----------------------
   procedure Simple_Set_Border (The_Surface: in out Window'Class;
                                Main_Menu  : in     Boolean)
   is begin
      if The_Surface.Extents.Row < 2 then
         -- Not enough room for a (useful) border
         return;
      end if;
      
      The_Surface.Set_Border;
      
      
--      if Main_Menu then 
--         if The_Surface.Extents.Row < 2 then
--            -- Not enough space to do anything
--         end if;
--      else

--      end if;
   end Simple_Set_Border;
   
end Curses.UI.Menus.Renderers;
