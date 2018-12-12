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

--
-- UNIX/POSIX Implementation
--

-- This implementation is very simple, actually. We simply control assignment
-- of STDIO file descriptors to a maximum of one Terminal object per partition.

with Ada.Strings.Fixed;
with Curses.Binding; use Curses.Binding;

package body Curses.Device.Environment is
   
   --------------------------------
   -- UNIX/POSIX Standard Values --
   --------------------------------
   Standard_In : constant File_Descriptor := 0;
   Standard_Out: constant File_Descriptor := 1;
   
   
   ----------------------
   -- Exclusive_Access --
   ----------------------
   protected Exclusive_Access is
      procedure Acquire (Success: out Boolean);
      procedure Release;
   private
      Acquired: Boolean := False;
   end Exclusive_Access;
   
   protected body Exclusive_Access is
      procedure Acquire (Success: out Boolean) is
      begin
         if Acquired then 
            Success := False;
            
         else
            Acquired := True;
            Success  := True;
            
         end if;
      end Acquire;
      
      procedure Release is
      begin
         Acquired := False;
      end Release;
   
   end Exclusive_Access;
   
   
   ------------------
   -- Request_Line --
   ------------------
   overriding
   procedure Request_Line (Device: in out Environment_Terminal_Device;
                           Handle:    out Line_Handle;
                           Error :    out Library_Error_Message)
   is
      use Ada.Strings.Fixed;
      
      Acquired      : Boolean := False;
      Internal_Error: Library_Error_Message := (others => ' ');
      
   begin
      Error := Library_Error_Message'(others => ' ');
      
      Exclusive_Access.Acquire (Acquired);
      
      -- Only one Environment_Terminal_Device line may exist at any time.
      if not Acquired then
         Overwrite
           (Source   => Error,
            Position => Error'First,
            New_Item => "Environment Terminal Device already acquired.");
         
         Invalidate_Handle (Handle);
         return;
      end if;
      
      -- The binding requires actual File_Handles (FILE *), rather than
      -- File_Descriptors. We open those here, and close them in Release_Line
      
      -- STDIN
      Handle.Input := Open_Descriptor (Descriptor => Standard_In,
                                       Mode       => Read,
                                       Error      => Internal_Error);
      if not Handle_Valid (Handle.Input) then
         Error := Internal_Error;
         Invalidate_Handle (Handle);
         return;
         
      end if;
      
      -- STDOUT
      Handle.Output := Open_Descriptor (Descriptor => Standard_Out,
                                        Mode       => Write,
                                        Error      => Internal_Error);
      if not Handle_Valid (Handle.Output) then
         -- Since we got this far, we know that Handle.Input is valid.
         -- So we also need to close that
         Close_File (Handle.Input);
         
         Error := Internal_Error;
         Invalidate_Handle (Handle);
         return;
         
      end if;
      
   exception
      when others =>
         Overwrite
           (Source   => Error,
            Position => Error'First,
            New_Item => "Unexpected exception");
         
         Invalidate_Handle (Handle);
         
   end Request_Line;
   
   
   ------------------
   -- Release_Line --
   ------------------
   procedure Release_Line (Device: in out Environment_Terminal_Device;
                           Handle: in out Line_Handle)
   is
   begin
      if not Handle_Valid (Handle) then
         return;
      end if;
      
      Close_File (Handle.Input);
      Close_File (Handle.Output);
      
      Exclusive_Access.Release;
      Invalidate_Handle (Handle);
      
   exception
      when others =>
         Invalidate_Handle (Handle);
      
   end Release_Line;
   
end Curses.Device.Environment;
