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

-- The Environment_Terminal represents any terminal that may be associated with
-- attached to the partition. This is typically attached to the "STDIO"
-- descriptors for the partition, also known as the "controlling terminal".
--
-- The Environment_Terminal can only provide a single Handle. And therefore may
-- be attached to only a single Terminal object at any given time. Acquiring a
-- handle causes the curses library to be initialized for the "controlling
-- terminal" (if any) for the partition.


package Curses.Device.Environment is
   
   type Environment_Terminal_Device(<>) is limited new Line_Device
     with private;
   
   overriding
   procedure Request_Line (Device: in out Environment_Terminal_Device;
                           Handle:    out Line_Handle;
                           Error :    out Library_Error_Message);
   
   overriding
   procedure Release_Line (Device: in out Environment_Terminal_Device;
                           Handle: in out Line_Handle);
   
   function Environment_Terminal return Line_Device_Access;
   -- Should be used as the actual discriminant of a new Terminal object.
   
private
   
   type Environment_Terminal_Device is limited new Line_Device with
     null record;
   
   Environment_Terminal_Actual: aliased Environment_Terminal_Device 
     := (others => <>);
   
   function Environment_Terminal return Line_Device_Access 
     is (Environment_Terminal_Actual'Access);
   
end Curses.Device.Environment;
