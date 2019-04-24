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

-- The Put_Computer generic package provides a re-usable Overflow and 
-- Justification computation procedures that can be use to implement a typical 
-- multi-lined Put with justification, as required for all Surface derivations
--
-- The suggested implementation is to instantiate the package within the
-- declarative region of the Put implementation, passing in the appropriate
-- Put parameters to be acted on by the Put_Computer instance.
--
-- Put_Computer does not check the Active property of the Surface. This should
-- be done by the instantiating subprogram before invoking the computer's
-- subprograms.

generic
   type Character_Type is (<>);
   type String_Type is array (Positive range <>) of Character_Type;
   
   The_Surface: in out Surface'Class;
   Write_Head : in out Cursor_Position;
   Justify    : in     Justify_Mode;
   Overflow   : in     Overflow_Mode;
   Content    : in     String_Type;
   
package Curses.Put_Computer is
   
   Select_First: Natural         := Content'First;
   Select_Last : Natural         := Content'First - 1;
   -- Indicates the slice of Content which should be output from left-to-right
   -- starting at the Write_Head. All those values are computed by a call to
   -- Compute_Line
   
   After_Write : Cursor_Position := Write_Head;
   -- This is a computed position of where a cursor should be positioned after
   -- completing the write. This is dependent on the Justify mode, as well as
   -- Surface Extents as follows:
   --
   -- * For Left justify: One position past the end of the selected Content
   --   slice, if written starting at Write_Head, if possible Otherwise, this
   --   position will be on the last character of the selected Content slice.
   --
   -- * For Center and Right justify: Equal to the previous value of 
   --   Write_Head before invoking Compute_Line
   
   
   function Compute_Line return Boolean;
   -- Positions Write_Head and sets Select_First .. Select_Last such that
   -- a writing Content (Select_First .. Select_Last) at the Write_Head,
   -- from left-to-right, will achieve the correct result in consideration of
   -- 1. Justification mode
   -- 2. Available line space according to the justification mode
   --
   -- On the second and later invocations, Write_Head is automatically placed
   -- correctly on the next line (or else Cursor_Excursion is returned),
   -- depending on Justification and Overflow settings.
   --
   -- Returns False if there is no Content left to be written.
   --
   -- Compute_Line is designed to be directly used in a while loop statement.
   --
   -- For example, if Overflow = Error, the first call will give a truncated
   -- selection for the first row, returning True. The second call will raise
   -- a Cursor_Excursion.
   --
   -- Similarly for Wrap_Error, Compute_Line will return True until the last
   -- line is already computed. Subsequent calls to Compute_Line raise a 
   -- Cursor_Excursion.
   --
   -- -- All Possible Exceptions --
   -- *  Cursor_Excursion: Write_Head is already beyond the reported Extents
   --                      of The_Surface, or Overflow in Error | Wrap_Error,
   --                      and there was not enough available space
   -- *  Curses_Library  : Any unexpected internal error
   
   
end Curses.Put_Computer;
   
