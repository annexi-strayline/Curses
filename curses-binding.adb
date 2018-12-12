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

package body Curses.Binding is
   
   
   ----------------
   -- C Bindings --
   ----------------
   
   -- libc Direct --
   -----------------
   pragma Linker_Options ("-lc");
   
   function Sys_Open_File (fildes: File_Descriptor;
                           mode  : char_array) 
                          return File_Handle
     with
     Import        => True,
     Convention    => C,
     External_Name => "fdopen"; 
   -- POSIX fdopen(3).
   
   procedure Sys_Close_File (Handle    : in File_Handle)
     with
     Import        => True,
     Convention    => C,
     External_Name => "fclose"; 
   -- POSIX fclose(3)
   

   -- General Binding --
   ---------------------
   pragma Linker_Options ("curses-binding-sys.o");
   pragma Linker_Options ("-lcurses");
   -- Since the curses library makes extensive use of C preprocessor macros, we
   -- cannot directly import many of the routines, and as such have put some
   -- subroutines into a C language thin binding
   
   function Sys_errno return int
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_sys_errno";
   -- This was originally imported directly in the FreeBSD environment, but for
   -- portability it was moved to a more typical errno.h implementation through
   -- the general binding C file.
   
   
   function CURSES_set_term (Handle: Terminal_Handle) return Terminal_Handle
     with
     Import        => True,
     Convention    => C,
     External_Name => "__binding_curses_set_term"; 
   -- curs_initscr(3X)
   -- SCREEN *set_term(SCREEN *new)
   
   
   --
   -- General Binding Services
   --
   
   -- Note that these services don't need to be serialized, since they are
   -- system calls, which are already task-safe by default. Only calls to
   -- the actual (n)curses library are serialized in this package
   
   ---------------------
   -- Open_Descriptor --
   ---------------------
   function Open_Descriptor (Descriptor: in     File_Descriptor;
                             Mode      : in     File_Mode;
                             Error     :    out Library_Error_Message)
                            return File_Handle
   is
      Mode_Length: Positive;
      Mode_String: String (1 .. 2);
   begin
      Error := Library_Error_Message'(others => ' ');
      
      -- First we at least try to see if the descriptor is valid
      if Descriptor < 0 then
         Set_Library_Error_Message (Buffer  => Error,
                                    Message => "Descriptor is invalid");

         return Invalid_Handle;
            
      end if;
      
      -- Now we set-up the parameters to pass to libc
      case Mode is
         when Read =>
            Mode_String := "r ";
            Mode_Length := 1;
            
         when Write =>
            Mode_String := "w ";
            Mode_Length := 1;
            
         when Write_Append =>
            Mode_String := "a ";
            Mode_Length := 1;
            
         when Random =>
            Mode_String := "r+";
            Mode_Length := 2;
            
         when Random_Append =>
            Mode_String := "a+";
            Mode_Length := 2;
            
      end case;
      
      -- and off we go
      
      return Handle: File_Handle do
        Handle := Sys_Open_File 
          (fildes => Descriptor,
           mode => To_C(Mode_String(1 .. Mode_Length)));
        
        if not Handle_Valid (Handle) then
           declare
              UNIX_Error: String := int'Image(Sys_errno);
           begin
              Set_Library_Error_Message
                (Buffer  => Error,
                 Message => "Failed to open descriptor. UNIX errno =" &
                   UNIX_Error);
              
              Invalidate_Handle (Handle);
              return;
              
           end;
        end if;
                
      end return;
      
   exception
      when others =>
         Set_Library_Error_Message (Buffer  => Error,
                                    Message => "Unexpected Exception");
         
         return Invalid_Handle;
         
   end Open_Descriptor;
   
   
   ----------------
   -- Close_File --
   ----------------
   procedure Close_File (Handle: in out File_Handle)
   is
   begin
      if not Handle_Valid (Handle) then
         return;
         
      end if;
      
      Sys_Close_File (Handle);
      Invalidate_Handle (Handle);
      
   exception
      when others =>
         Invalidate_Handle (Handle);
      
   end Close_File;
   
   
   --------------------------------
   -- Binding Serialization Unit --
   --------------------------------
   protected body Serial is
      
      ----------
      -- Lock --
      ----------
      entry Lock when not Lock_Actual is
      begin
         Lock_Actual := True;
      end Lock;
      
      ------------
      -- Unlock --
      ------------
      procedure Unlock is
      begin
         Lock_Actual := False;
      end Unlock;   
      
      ------------
      -- Locked --
      ------------
      function Locked return Boolean is (Lock_Actual);
      
      
      ---------------------
      -- Active_Terminal --
      ---------------------
      function Active_Terminal return Terminal_Handle is (Terminal); 
      
      ------------------
      -- Set_Terminal --
      ------------------
      procedure Set_Terminal (Handle: in Terminal_Handle) is
      begin
         Terminal := Handle;
      end Set_Terminal;
      
   end Serial;
   
   
   -------------------
   -- Lock_Or_Panic --
   -------------------
   procedure Lock_Or_Panic
   is
   begin
      select
         Serial.Lock;
      or
         delay Deadlock_Wait;
         raise Curses_Library with "Serialization deadlock abort";
      end select;
      
   end Lock_Or_Panic;
   
   
   
   ---------------------
   -- Select_Terminal --
   ---------------------
   function Select_Terminal (Handle: in Terminal_Handle) return Boolean
   is
      Check : Terminal_Handle := Serial.Active_Terminal;
   begin
      if not Handle_Valid (Handle) then
         return False;
      end if;
      
      -- Check that the Active_Terminal is invalid or else not the same as the
      -- requested handle
      if Handle_Valid (Check) and then Check = Handle then
         -- No need to do anything here
         return True;
         
      end if;
      
      -- We need to attempt to set the new terminal
      Check := CURSES_set_term (Handle);
      
      if not Handle_Valid (Check) then
         Serial.Set_Terminal (Invalid_Handle);
         return False;
         
      else
         Serial.Set_Terminal (Check);
         return True;
      end if;
      
   exception
      when others =>
         Serial.Set_Terminal (Invalid_Handle);
         return False;
      
   end Select_Terminal;
     
   
end Curses.Binding;
