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

package body Curses.UI.Menus.Bounded_Item is
   
   ---------------
   -- Get_Label --
   ---------------
   overriding
   procedure Get_Label (Item : in     Bounded_Menu_Item_Type;
                        Value:    out String;
                        Key  :    out Natural)
   is
      Label: String renames 
        Item.Label (Item.Label'First .. Item.Label'First + Item.Label_Last);
   begin
      -- According to the spec for Menu_Item_Type, we should accept a bounds for
      -- Value that is either a null range, or larger than the actual Value.
      
      if Value'Length > Label'Length then
         -- We are supposed to pad the remaining with spaces
         Value (Value'First .. Value'First + Label'Length - 1) := Label;
         Value (Value'First + Label'Length .. Value'Last) := (others => ' ');
         
      elsif Value'Length < Label'Length then
         -- Truncate
         Value := Label (Label'First .. Label'First + Value'Length - 1);
         
         -- If Value'Length = 0, we will just end up "assigning" a null range
         -- since we'd have Label'First .. Label'First -1
         
      else
         -- They are equal!
         Value := Label;
            
         -- If Value'Length = 0, again, we'd end up assigning a null ranged
         -- String of Label'First .. Label'First - 1;
      end if;
      
      Key := Item.Key_Index;
      -- Note that Set_Label sets Key_Index to zero if the new label is empty
      
   end Get_Label;
   
   
   ---------------
   -- Set_Label --
   ---------------
   procedure Set_Label (Item : in out Bounded_Menu_Item_Type;
                        Value: in     String;
                        Key  : in     Natural := 0)
   is 
      -- There is contract that Item always fits, this simplifies things!
      New_Label: String renames
        Item.Label (Item.Label'First .. Value'Length - 1);
      
      -- We don't need to worry about any remaining, as we can just ignore it
      -- This is more efficient, obviously, than overwriting it
   begin
      if Value'Length = 0 then
         Item.Label_Last := Item.Label'First - 1;
         Item.Key_Index  := 0;
      end if;
      
      New_Label       := Value;
      Item.Label_Last := Value'Size;
      Item.Key_Index  := Key;
      
   end Set_Label;
   
   
   -------------
   -- Enabled --
   -------------
   procedure Enabled (Item: in out Bounded_Menu_Item_Type;
                      Set : in     Boolean)
   is 
   begin
      Item.Is_Enabled := Set;
   end Enabled;
   
   
   -------------
   -- Toggled --
   -------------
   procedure Toggled (Item: in out Bounded_Menu_Item_Type;
                      Set : in     Boolean)
   is 
   begin
      Item.Is_Toggled := Set;
   end Toggled;
   
end Curses.UI.Menus.Bounded_Item;
