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

-- Framed_Window objects allow for the configuration of Window objects with a
-- distinct border and single Framed canvas of a predetermined size.

with Curses.Standard; use Curses.Standard;

package Curses.Frames.Framed_Windows is
   
   type Framed_Window (<>) is limited new Window with private;
   
   not overriding
   function New_Framed_Window 
     (On_Screen     : aliased in out Standard.Screen'Class;
      Top_Padding   :         in     Natural;
      Bottom_Padding:         in     Natural;
      Left_Padding  :         in     Natural;
      Right_Padding :         in     Natural;
      Frame_Extents :         in     Cursor_Position)
     return Framed_Window;
   -- Centered on On_Screen
   
   not overriding
   function New_Framed_Window 
     (On_Screen      : aliased in out Screen'Class;
      Top_Padding    :         in     Natural;
      Bottom_Padding :         in     Natural;
      Left_Padding   :         in     Natural;
      Right_Padding  :         in     Natural;
      Window_Top_Left:         in     Cursor_Position;
      Frame_Extents  :         in     Cursor_Position)
     return Framed_Window;
   -- Window is positioned at Window_Top_Left on On_Screen
   
   
   not overriding
   function Get_Frame (FW: aliased in out Framed_Window)
                      return Frame'Class;
   -- Returns a Frame sized according to the specified padding.
   
   
   not overriding
   procedure Repad (The_Window    : in out Framed_Window;
                    Top_Padding   : in     Natural;
                    Bottom_Padding: in     Natural;
                    Left_Padding  : in     Natural;
                    Right_Padding : in     Natural);
   -- Adjusts the padding of the of The_Window, and then Resizes it to maintain
   -- equal extents of the underlying frame. Refit_Frame should be called on
   -- any existing Frames after executing Repad
   
   
   not overriding
   procedure Resize_Frame (The_Window       : in out Framed_Window;
                           New_Frame_Extents: in     Cursor_Position);
   -- Resizes the Window to accomodate a new Frame size
   
   
   -- Package utilities
   procedure Refit_Frame (The_Frame : in out Frame'Class);
   -- Refits The_Frame appropriately if The_Window has been resized.
   -- If The_Frame does not target a Framed_Window, nothing is done.
   
   
private
   
   type Framed_Window is limited new Window with
      record
         Top_Padding   : Natural;
         Bottom_Padding: Natural;
         Left_Padding  : Natural;
         Right_Padding : Natural;
      end record;
   
end Curses.Frames.Framed_Windows;
