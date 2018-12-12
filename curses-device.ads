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

package Curses.Device is
   
   -----------------
   -- Line_Device --
   -----------------
   type Line_Device is limited interface;
   type Line_Device_Access is access all Line_Device'Class;
   
   -- The Line_Device interface provides fully-prepared "Lines" (Line_Handle),
   -- which can then be initialized by the (n)curses library. For most UNIX
   -- systems, this must be some kind of underlying TTY device (often a pty).
   --
   -- Multiple Terminal objects may be managed through a single device.
   -- Terminals that cannot be accommodated by a device (Request_Line fails),
   -- are initialized as being Unavailable. A standard error message is to be
   -- left with the Terminal object of any failed attachments.
   --
   -- All Terminal objects are declared with a Line_Device_Access discriminant,
   -- and make calls to the Request_Line and Release_Line operations during
   -- Initialization and Finalization, respectively.
   
   
   procedure Request_Line (Device: in out Line_Device;
                           Handle:    out Line_Handle;
                           Error :    out Library_Error_Message)
     is abstract;
   -- If a Line cannot be allocated, Request_Line shall return
   -- Curses.Invalid_Handle
   --
   -- If a valid handle cannot be provided, an explanation shall be returned
   -- in Error
   --
   -- -- Suppresses All Exceptions --
   
   procedure Release_Line (Device: in out Line_Device;
                           Handle: in out Line_Handle)
      is abstract;
   -- If Handle is Invalid, Release_Line does nothing.
   --
   -- -- Suppresses All Exceptions --
   
end Curses.Device;
