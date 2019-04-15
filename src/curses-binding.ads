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

with System;       use System;
with Interfaces.C; use Interfaces.C;

private package Curses.Binding is
   
   -------------------
   -- Configuration --
   -------------------
   Deadlock_Wait: constant Duration := 5.0;
   -- The maximum period the binding will wait to obtain the serialization
   -- mutex before aborting the operation.
   
   
   ------------------------------
   -- General Binding Services --
   ------------------------------
   -- Note, it shall be the responsibility of the Line_Device to manage
   -- File_Descriptors, and it is expected that the Line_Device operations may 
   -- use these General Binding Services to facilitate the generation of valid 
   -- Line_Handles.
   
   function Open_Descriptor (Descriptor: in     File_Descriptor;
                             Mode      : in     File_Mode;
                             Error     :    out Library_Error_Message)
                            return File_Handle;
   -- Returns a File_Handle associated with the provided File_Descriptor.
   -- If a File_Handle cannot be provided for the descriptor (including for
   -- invalid descriptors), an invalid File_Handle is returned, and the Error
   -- buffer is set appropriately.
   -- -- Suppresses All Exceptions --
   
   procedure Close_File (Handle: in out File_Handle);
   -- Closes Handle. Also invalidates Handle. Does not affect the underlying
   -- descriptor.
   -- -- Suppresses All Exceptions --
   
   
   -----------------------------
   -- Special Interface Types --
   -----------------------------
   
   -- 'bool' c Return Type --
   --------------------------
   type bool is (Bool_False, Bool_True);
   
   for bool      use (Bool_False => 0, Bool_True => 1);
   for bool'Size use unsigned_char'Size;
   -- Note. This type is not a direct casting of the type used by some of
   -- the curses functions which return bool.
   --
   -- LOGIC IN THE C-BINDING ENSURES A UNSIGNED CHAR WITH THE APPROPRIATE
   -- 0 OR 1 IS RETURNED.
   --
   -- For bindings that return an integer, the meaning of the return value
   -- is assigned through preprocessor #defines. The man page states that
   -- it is actually a binary result - error or no error. Therefore the C
   -- binding converts the response from some int-returning bindings in this
   -- package to this bool type.
   
private
   --------------------------------
   -- Binding Serialization Unit --
   --------------------------------
   -- The Binding Serialization Unit (Serial) ensures that no (n)curses library
   -- calls occur simultaneously. It is implemented as a simple mutex. The
   -- Lock_Or_Panic subprogram implements a timed entry call to guarantee no
   -- deadlock occurs.
   --
   -- Serial also internally manages Terminal_Handle switching in the library.
   
   protected Serial is
      entry     Lock;
      procedure Unlock;
      -- If Panic is true, the Lock queue is permanently unlocked,
      -- and State_OK is set to False. This would break the binding
      
      function  Locked return Boolean;
      -- Non-blocking check on lock state.
      
      -- Active Terminal --
      ---------------------
      function  Active_Terminal return Terminal_Handle;
      procedure Set_Terminal (Handle: in Terminal_Handle);
      -- Note how, in this view (as well as the implementations), 
      -- Terminal_Handle is not limited, so we can implement this quite easily.
      -- It's also safely hidden from any access outside of the Binding
      -- package, and it's children
      
   private
      Lock_Actual: Boolean         := False;
      Terminal   : Terminal_Handle := Terminal_Handle(System.Null_Address);
   end Serial;
   
   
   -- Common Private Binding Services --
   -------------------------------------
   -- These subprograms provide internal services for the children of this
   -- package.
   
   procedure Lock_Or_Panic;
   -- Attempts to acquire the Serial lock. If unable within Deadlock_Wait, 
   -- raises an exception.
   -- -- All Possible Exceptions --
   -- *  Curses_Library: Serial.Lock fails (panic condition), or any other
   --                    unexpected exception
   
   function Select_Terminal (Handle: in Terminal_Handle) return Boolean;
   -- Sets the current target terminal of the (n)curses library to the provided
   -- handle. If the requested Terminal_Handle is already the active terminal,
   -- no action is taken. If no change is made, or the change is successful,
   -- True is returned
   --
   -- If the handle is Invalid, False is returned, and no other action is
   -- taken.
   -- 
   -- If an an error occurs (set_term returns NULL) Active_Terminal is
   -- invalidated, and False is returned.
   --
   -- This procedure shall always called when Serial is locked.
   --
   -- Implementation note - It might seem, on the face of it, a good idea to
   -- implement this functionality from directly in Serial.Set_Terminal. 
   -- However, we'd then take a much larger risk of calling to an external
   -- library, which we don't control, from inside a protected object, thus
   -- opening us to the real possibility of a total deadlock.
   --
   -- -- Suppresses All Exceptions --
   
end Curses.Binding;
