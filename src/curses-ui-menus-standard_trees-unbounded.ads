------------------------------------------------------------------------------
--                                                                          --
--                        UNIX Terminal Control Package                     --
--                             (n)curses Binding                            --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
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

-- This package provides a standard implementation Standard_Tree with bounded
-- storage.
--
-- Refer to the parent Standart_Trees package for a full interface listing of
-- the Standard_Tree interface.

with System.Storage_Elements;

private with Curses.UI.Menus.Standard_Trees.Implementation;
private with Curses.UI.Menus.Standard_Trees.Storage_Pools.Unbounded;

generic
   type Base_Item is limited new Menu_Item_Interface with private;
package Curses.UI.Menus.Standard_Trees.Unbounded is
   
   type Unbounded_Menu_Tree is limited new Standard_Tree
     with private;
   -- All Standard_Tree operations, including indexing, always refer to
   -- underlying items of Base_Item'Class
   
private
   
   package USP renames Standard_Trees.Storage_Pools.Unbounded;
   
   -- Implementation
   
   package GTI is new Standard_Trees.Implementation.Generic_Tree
     (Base_Item      => Base_Item,
      Subpool_Object => USP.Unbounded_Tree_Subpool);
   
   type Unbounded_Menu_Tree is limited new GTI.Menu_Tree
     with null record;
   
   
end Curses.UI.Menus.Standard_Trees.Unbounded;
