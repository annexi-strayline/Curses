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

with Ada.Exceptions;
with Ada.Assertions;
with Ada.Strings.Fixed; use Ada;

package body Curses is
   
   Assertion_Error: exception renames Assertions.Assertion_Error;
   
   -------------------------------
   -- Set_Library_Error_Message --
   -------------------------------
   procedure Set_Library_Error_Message 
     (Buffer : in out Library_Error_Message;
      Message: in     String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      
   begin
      Move
        (Source   => Message,
         Target   => Buffer,
         Drop     => Right,
         Justify  => Left,
         Pad      => Space);
      
   exception
      when others =>
         Buffer(1 .. 35) := "Set_Library_Error_Message Exception";
         Buffer(36 .. Buffer'Last) := String'(36 .. Buffer'Last => Space);
      
   end Set_Library_Error_Message;
   
   
   --
   -- Handle Operations
   --
   
   -----------------------
   -- Invalidate_Handle --
   -----------------------
   
   -- File_Handle --
   -----------------
   procedure Invalidate_Handle (Handle: in out File_Handle)
   is
   begin
      Handle := File_Handle(System.Null_Address);
   end Invalidate_Handle;
   
   -- Line_Handle --
   -----------------
   procedure Invalidate_Handle (Handle: in out Line_Handle)
   is
   begin
      Handle.Input  := Invalid_Handle;
      Handle.Output := Invalid_Handle;
      
   end Invalidate_Handle;
   
   -- Terminal_Handle --
   ---------------------
   procedure Invalidate_Handle (Handle: in out Terminal_Handle)
   is
   begin
      Handle := Terminal_Handle(System.Null_Address);
   end Invalidate_Handle;
   
   -- Surface_Handle --
   --------------------
   procedure Invalidate_Handle (Handle: in out Surface_Handle)
   is
   begin
      Handle := Surface_Handle(System.Null_Address);
   end Invalidate_Handle;   
   
   
   ------------------------
   -- Surface Properties --
   ------------------------
   protected body Surface_Properties is

      -- Visible --
      -------------
      function Visible return Boolean is (Is_Visible);
      
      entry Wait_Visible when Is_Visible is begin
         null;
      end Wait_Visible;
      
      
      -- Armed --
      -----------
      procedure Arm (Armed: in Boolean) is
      begin
         if Is_Visible then
            return;
         end if;
         
         Is_Armed   := Armed;
         Armed_Hint := True;
      end Arm;
      
      function Armed return Boolean is (Is_Armed);
      
      entry Wait_Armed when Is_Armed is begin
         null;
      end Wait_Armed;
      
      
      -- Armed <-> Visible Toggles --
      -------------------------------
      procedure Armed_To_Visible is
      begin
         if Is_Armed then
            Is_Armed       := False;
            Is_Visible     := True;
            Presented_Hint := True;
         end if;
      end Armed_To_Visible;
      
      procedure Visible_To_Armed is
      begin
         if Is_Visible then
            Is_Visible := False;
            Is_Armed   := True;
         end if;
      end Visible_To_Armed;
      
      
      procedure Withdraw is
      begin
         if Is_Visible then
            Withdrawn_Hint := True;
         end if;
         
         Is_Visible     := False;
         Is_Armed       := False;
      end Withdraw;
      
      
      -- Clipped --
      -------------
      procedure Clipped (Set: in Boolean) is
      begin
         if Clipping_Hint = False then
            Clipping_Hint := (Is_Clipped /= Set);
         end if;
         
         Is_Clipped := Set;
      end Clipped;
      
      function Clipped return Boolean is (Is_Clipped);
      
      entry Wait_Clipped when Is_Clipped is begin
         null;
      end Wait_Clipped;
      
      -- Modified --
      --------------
      procedure Modified (Set: in Boolean) is
      begin
         Is_Modified := Set;
         
         if Set then
            Modified_Hint := True;
         end if;
         
      end Modified;
      
      function Modified return Boolean is (Is_Modified);
      
      entry Wait_Modified (Clear: in Boolean) when Is_Modified is 
      begin
         if Clear then
            Is_Modified := False;
         end if;
      end Wait_Modified;
      
      
      -- Extents_Changed --
      ----------------------
      procedure Clear_Extents_Changed is
      begin
         Changed_Extents := False;
      end Clear_Extents_Changed;

      procedure Extents (New_Extents: in Cursor_Position) is
      begin
         if New_Extents /= Extent then
            Extent          := New_Extents;
            Changed_Extents := True;
            Extents_Hint    := True;
                      
         end if;
      end Extents;
      
      function Extents return Cursor_Position is (Extent);
      
      function Extents_Changed return Boolean is (Changed_Extents);
      
      entry Wait_Extents_Changed (Clear: in Boolean) when Changed_Extents is 
      begin
         if Clear then
            Changed_Extents := False;
         end if;
      end Wait_Extents_Changed;
      
      
      -- Hints --
      -----------
      procedure Hint_Armed (Changed: out Boolean) is
      begin
         Changed    := Armed_Hint;
         Armed_Hint := False;
      end Hint_Armed;
      
      procedure Hint_Presented (Changed: out Boolean) is
      begin
         Changed        := Presented_Hint;
         Presented_Hint := False;
      end Hint_Presented;
      
      procedure Hint_Withdrawn (Changed: out Boolean) is
      begin
         Changed        := Withdrawn_Hint;
         Withdrawn_Hint := False;
      end Hint_Withdrawn;
      
      procedure Hint_Modified (Changed: out Boolean) is
      begin
         Changed       := Modified_Hint;
         Modified_Hint := False;
      end Hint_Modified;
      
      procedure Hint_Extents (Changed: out Boolean) is
      begin
         Changed      := Extents_Hint;
         Extents_Hint := False;
      end Hint_Extents;
      
      procedure Hint_Clipping (Changed: out Boolean) is
      begin
         Changed       := Clipping_Hint;
         Clipping_Hint := False;
      end Hint_Clipping;
         
   end Surface_Properties;
   
   
   -- General User Access --
   -------------------------
   procedure Wait_Visible (The_Surface: in out Surface) is
   begin
      The_Surface.Properties.Wait_Visible;
   end Wait_Visible;
   
   
   procedure Wait_Armed (The_Surface: in out Surface) is
   begin
      The_Surface.Properties.Wait_Armed;
   end Wait_Armed;
   
   
   procedure Wait_Clipped (The_Surface: in out Surface) is
   begin
      The_Surface.Properties.Wait_Clipped;
   end Wait_Clipped;
   
   
   procedure Wait_Modified (The_Surface: in out Surface; 
                            Clear      : in     Boolean := True) is
   begin
      The_Surface.Properties.Wait_Modified (Clear);
   end Wait_Modified;
   
   
   procedure Clear_Extents_Changed (The_Surface: in out Surface) is
   begin
      The_Surface.Properties.Clear_Extents_Changed;
   end Clear_Extents_Changed;
   -- Unconditionally clears the Extents_Changed flag.
   
   
   procedure Wait_Extents_Changed (The_Surface: in out Surface;
                                   Clear      : in     Boolean  := True) is
   begin
      The_Surface.Properties.Wait_Extents_Changed (Clear);
   end Wait_Extents_Changed;
   
   
   --
   -- Surface Class-Wide Operations
   --
   
   ---------
   -- Put --
   ---------
   procedure Put (The_Surface   : in out Surface'Class;
                  Content       : in     String;
                  Justify       : in     Justify_Mode    := Left;
                  Overflow      : in     Overflow_Mode   := Truncate;
                  Advance_Cursor: in     Boolean         := False)
   is

   begin
      declare
         -- Block to contain any incorrectly implemented derrivations
         -- of Surface, which could raise unexpected exceptions
         
         pragma Assertion_Policy (Pre'Class => Ignore);
         -- We've already checked the Precondition on entry to this subprogram, 
         -- let's disable it for the later dispatch to Put.
         
         Use_Cursor: Cursor'Class := The_Surface.Current_Cursor;
      begin
         
         The_Surface.Put (Set_Cursor     => Use_Cursor,
                          Content        => Content,
                          Justify        => Justify,
                          Overflow       => Overflow,
                          Advance_Cursor => Advance_Cursor);
         
         if Advance_Cursor then
            The_Surface.Current_Cursor (Use_Cursor);
         end if;
      end;
      
   exception
      when 
        Assertion_Error     | 
        Surface_Unavailable | 
        Cursor_Excursion    | 
        Curses_Library      =>
         raise;
         
      when e: others =>
         raise Curses_Library with 
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Put;
   
   
   ----------------
   -- Set_Border --
   ----------------
   procedure Set_Border (The_Surface: in out Surface'Class) is
   begin
      declare
         Use_Cursor: Cursor'Class := The_Surface.Current_Cursor;
      begin
         The_Surface.Set_Border (Use_Cursor => Use_Cursor);
      end;
      
   exception
      when 
        Surface_Unavailable | 
        Curses_Library      =>
         raise;
         
      when e: others =>
         raise Curses_Library with 
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Set_Border;
   
   ----------------------------------------
   procedure Set_Border (The_Surface: in out Surface'Class;
                         
                         Left_Side,
                         Right_Side,
                         Top_Side,
                         Bottom_Side,
                           
                         Top_Left_Corner,
                         Top_Right_Corner,
                         Bottom_Left_Corner,
                         Bottom_Right_Corner: in Graphic_Character)
   is begin
      declare
         Use_Cursor: Cursor'Class := The_Surface.Current_Cursor;
      begin
         The_Surface.Set_Border (Use_Cursor          => Use_Cursor,
                                 Left_Side           => Left_Side,
                                 Right_Side          => Right_Side,
                                 Top_Side            => Top_Side,
                                 Bottom_Side         => Bottom_Side,
                                 Top_Left_Corner     => Top_Left_Corner,
                                 Top_Right_Corner    => Top_Right_Corner,
                                 Bottom_Left_Corner  => Bottom_Left_Corner,
                                 Bottom_Right_Corner => Bottom_Right_Corner);
      end;
      
   exception
      when 
        Surface_Unavailable | 
        Curses_Library      =>
         raise;
         
      when e: others =>
         raise Curses_Library with 
           "Unexpected exception: " & Exceptions.Exception_Information (e);
   end Set_Border;
   
end Curses;
