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

package body Curses.Frames.Framed_Windows is
   
   ------------------------
   -- Compute_Frame_Size --
   ------------------------
   -- Computes the Top_Left and Proposed_Extents for the frame portion of a
   -- given Framed_Window
   procedure Compute_Frame_Size (FW: in out Framed_Window'Class;
                                 TL:    out Cursor_Position;
                                 PE:    out Cursor_Position)
   with Inline is begin
      
      TL := (1,1);
      PE := FW.Extents;
      
      if FW.Top_Padding > 0 then
         TL.Row := TL.Row + Cursor_Ordinal (FW.Top_Padding);
         PE.Row := PE.Row - Cursor_Ordinal (FW.Top_Padding);
      end if;
      
      if FW.Left_Padding > 0 then
         TL.Column := TL.Column + Cursor_Ordinal (FW.Left_Padding);
         PE.Column := PE.Column - Cursor_Ordinal (Fw.Left_Padding);
      end if;
      
      if FW.Bottom_Padding > 0 then
         PE.Row := PE.Row - Cursor_Ordinal (FW.Bottom_Padding);
      end if;
      
      if FW.Right_Padding > 0 then
         PE.Column := PE.Column - Cursor_Ordinal (FW.Right_Padding);
      end if;

   end Compute_Frame_Size;
   
   ----------------------------
   -- Compute_Window_Extents --
   ----------------------------
   -- Computes the required Extents for the host Window portion of a given
   -- Framed_Window's padding, as well as a Proposed_Extents of the desired
   -- Frame.
   function Compute_Window_Extents (FW           : in out Framed_Window'Class;
                                    Frame_Extents: in     Cursor_Position)
                                   return Cursor_Position
     is ((Row => Cursor_Ordinal 
            (Natural (Frame_Extents.Row)
               + FW.Top_Padding 
               + FW.Bottom_Padding),
          Column => Cursor_Ordinal 
            (Natural (Frame_Extents.Column)
               + FW.Left_Padding
               + FW.Right_Padding)))
     with Inline;
   
   
   -----------------------
   -- New_Framed_Window --
   -----------------------
   not overriding
   function New_Framed_Window 
     (On_Screen     : aliased in out Curses.Standard.Screen'Class;
      Top_Padding   :         in     Natural;
      Bottom_Padding:         in     Natural;
      Left_Padding  :         in     Natural;
      Right_Padding :         in     Natural;
      Frame_Extents :         in     Cursor_Position)
     return Framed_Window 
   is begin
      return FW: Framed_Window 
          := (Window with
              TTY            => On_Screen.TTY,
              Parent_Screen  => On_Screen'Access,
              
              Top_Padding    => Top_Padding,
              Bottom_Padding => Bottom_Padding,
              Left_Padding   => Left_Padding,
              Right_Padding  => Right_Padding)
      do
         FW.Activate_Window 
           (Proposed_Extents => Compute_Window_Extents 
                                  (FW            => FW,
                                   Frame_Extents => Frame_Extents));
      end return;
   end New_Framed_Window;

   ----------------------------------------------------------------------------
   
   not overriding
   function New_Framed_Window 
     (On_Screen      : aliased in out Curses.Standard.Screen'Class;
      Top_Padding    :         in     Natural;
      Bottom_Padding :         in     Natural;
      Left_Padding   :         in     Natural;
      Right_Padding  :         in     Natural;
      Window_Top_Left:         in     Cursor_Position;
      Frame_Extents  :         in     Cursor_Position)
     return Framed_Window
   is begin
      return FW: Framed_Window 
          := (Window with
              TTY           => On_Screen.TTY,
              Parent_Screen => On_Screen'Access,
              
              Top_Padding    => Top_Padding,
              Bottom_Padding => Bottom_Padding,
              Left_Padding   => Left_Padding,
              Right_Padding  => Right_Padding)
      do
         FW.Activate_Window 
           (Top_Left         => Window_Top_Left,
            Proposed_Extents => Compute_Window_Extents 
                                  (FW            => FW,
                                   Frame_Extents => Frame_Extents));

      end return;
   end New_Framed_Window;
   
   
   ---------------
   -- Get_Frame --
   ---------------
   not overriding
   function Get_Frame (FW: aliased in out Framed_Window)
                      return Frame'Class
   is 
      TL, PE: Cursor_Position;
   begin
      Compute_Frame_Size (FW => FW, TL => TL, PE => PE);
      
      return New_Frame (Target           => FW'Access,
                        Top_Left         => TL,
                        Proposed_Extents => PE);
   end Get_Frame;
   
   
   -----------
   -- Repad --
   -----------
   not overriding
   procedure Repad (The_Window    : in out Framed_Window;
                    Top_Padding   : in     Natural;
                    Bottom_Padding: in     Natural;
                    Left_Padding  : in     Natural;
                    Right_Padding : in     Natural)
   is 
      Frame_TL, Frame_Extents: Cursor_Position;
   begin
      -- We want to preserve the frame size across this change, though we
      -- cannot guaruntee the same position!
      Compute_Frame_Size (FW => The_Window,
                          TL => Frame_TL,
                          PE => Frame_Extents);
      
      The_Window.Top_Padding    := Top_Padding;
      The_Window.Bottom_Padding := Bottom_Padding;
      The_Window.Left_Padding   := Left_Padding;
      The_Window.Right_Padding  := Right_Padding;
      
      The_Window.Resize (Compute_Window_Extents 
                           (FW            => The_Window,
                            Frame_Extents => Frame_Extents));
   end Repad;
   
   
   ------------------
   -- Resize_Frame --
   ------------------
   not overriding
   procedure Resize_Frame (The_Window       : in out Framed_Window;
                           New_Frame_Extents: in     Cursor_Position)
   is begin
      The_Window.Resize (Compute_Window_Extents
                           (FW            => The_Window,
                            Frame_Extents => New_Frame_Extents));
   end Resize_Frame;
   
   
   -----------------
   -- Refit_Frame --
   -----------------
   procedure Refit_Frame (The_Frame: in out Frame'Class) is
      TL, PE: Cursor_Position;
   begin
      if The_Frame.Target.all not in Framed_Window'Class then
         return;
      end if;
      
      Compute_Frame_Size (FW => Framed_Window(The_Frame.Target.all),
                          TL => TL, PE => PE);
      
      The_Frame.Reframe (Top_Left         => TL,
                         Proposed_Extents => PE);
   end Refit_Frame;
   
   
   
end Curses.Frames.Framed_Windows;
