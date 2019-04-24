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

-- No Wide support configuration

package body Curses.Binding.Render.Wide is
   
   pragma Assert (Wide_Support_Configured = False);
   
   ---------------------
   -- Put_Wide_String --
   ---------------------
   procedure Put_Wide_String (Handle: in Surface_Handle;
                              Buffer: in Wide_String)
   is begin
      raise Curses_Library with
        "Wide character/string support is not configured";
   end Put_Wide_String;
   
   
   ---------------------
   -- Get_Wide_String --
   ---------------------
   procedure Get_Wide_String (Handle: in     Surface_Handle;
                              Buffer:    out Wide_String;
                              Last  :    out Natural)
   is begin
      raise Curses_Library with
        "Wide character/string support is not configured";
   end Get_Wide_String;
   
   
   ------------------------------------
   -- Wide_Set_Monochrome_Background --
   ------------------------------------
   procedure Wide_Set_Monochrome_Background
     (Handle          : in Surface_Handle;
      Blank_Character : in Wide_Character;
      Reference_Cursor: in Cursor'Class)
   is
   begin
      raise Curses_Library with
        "Wide character/string support is not configured";
   end Wide_Set_Monochrome_Background;
   
   
   --------------------------------
   -- Wide_Set_Monochrome_Border --
   --------------------------------
   procedure Wide_Set_Monochrome_Border 
     (Handle          : in Surface_Handle;
      Reference_Cursor: in Cursor'Class;
      LS, RS, TS, BS, TL, TR, BL, BR: in Wide_Character)
   is begin
      raise Curses_Library with
        "Wide character/string support is not configured";
   end Wide_Set_Monochrome_Border;
   
end Curses.Binding.Render.Wide;
