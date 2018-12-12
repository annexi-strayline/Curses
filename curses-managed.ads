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

with Ada.Exceptions; use Ada;

package Curses.Surface.Managed is
   
   ----------------
   -- Exceptions --
   ----------------
   Managed_Retry: exception;
   -- If raised from the Manage procedure, the Manager task type deallocates
   -- and then reallocates the appropriate surface type, before passing it to
   -- subsequent call of Manage.
   --
   -- Any other exception propagated from Manage, or a normal exit of Manage,
   -- causes the Surface to be deallocated and the task to complete.
   
   
   ---------------------
   -- Null_Parameters --
   ---------------------
   -- Convenience for instantiation of Manager packages which do not require
   -- any special parameters to be passed to Manager task types
   type Null_Parameters is null record;
   Null_Default_Parameters: constant Null_Parameters;
   
   
   -------------------------------
   -- Generic_Resize_Management --
   -------------------------------
   -- The Generic_Resize_Management procedure provides a template for a Manage
   -- procedure which can be passed to a Window_Manager or Screen_Manager
   -- package instantiation, which manages dynamic screen/window resizing
   -- events by splitting management into Render and Interact components. When
   -- a Geometry_Changed exception
   
   
   ---------------------
   -- Managed_Surface --
   ---------------------
   
   generic
      Local_Screen: not null access Screen'Class;
      
      type Parameters     is limited private;
      type Client_Surface is new Surface'Class with private;
      
      Default_Parameters: in out Parameters;
      -- This allows for a Protected Type, to allow cross-window communication
      -- and synchronization schemes
      
      with procedure Render (The_Surface: in out Client_Surface'Class;
                             Custom     : in out Parameters);
      -- Render is called under two conditions
      -- 1. At execution of a new Manager task.
      -- 2. After any involuntary resize event.
      --
      -- Render will be called Synchronously from the Manage procedure
      -- at the beginning of any primitive operation of the Surface'Class type.
      -- At the beginning of such a call, the status of the Surface is queried
      
      with procedure Manage (The_Surface: in out Client_Surface'Class;
                             Custom     : in out Parameters);
      -- This is the core management procedure, and is first run after Render.
      
      -- Exiting Manage normally, or with any exception besides Managed_Retry
      -- will terminate the task, also destroying the window.
      --
      -- Propagating Managed_Retry from Manage will cause Render to be invoked,
      -- followed by a subsequent call to Manage.
     
      -- -- IMPORTANT --
      -- Manage will be synchronously interrupted by any resize event that
      -- occurs prior to the invocation any of Surface'Class
      --
      -- Managed_Retry should not be trapped by Manage or Render.
      
   package Managed_Surface is
      
      task type Manager (Custom: Parameters := Default_Parameters);
      
   end Surface_Manager;
   
   
end Curses.Surface.Managed;
