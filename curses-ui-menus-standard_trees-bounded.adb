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

package body Curses.UI.Menus.Standard_Trees.Bounded is
   
   use type GTE.Tree_Element;
   
   --------------
   -- Allocate --
   --------------
   function  Allocate (Pool: in out Item_Pool) return Index_Type is
      New_Item_Index: Index_Type;
   begin
      
      if Pool.Recycle_List /= Null_Index then
         -- Always Recycle first.
         New_Item_Index := Pool.Recycle_List;
         
         -- Move the next item up
         Pool.Recycle_List := Pool.Data(Pool.Recycle_List).State.Next;
         
      elsif Pool.Next_Fresh /= Null_Index then
         New_Item_Index := Pool.Next_Fresh;
         
         if Pool.Next_Fresh < Pool.Capacity then
            Pool.Next_Fresh := Pool.Next_Fresh + 1;
         else
            Pool.Next_Fresh := Null_Index;
         end if;
         
      else
         -- No new indexes available.
         New_Item_Index := Null_Index;
      end if;
      
      return New_Item_Index;
   end Allocate;
   
   ----------
   -- Free --
   ----------
   procedure Free (Pool : in out Item_Pool;
                   Index: in     Index_Type)
   is begin
      
      -- We are required to explicitly ignore Null_Index values
      if Index = Null_Index then
         return;
      end if;
      
      -- We always prepend freed items 
      Pool.Data(Index).State.Next (Pool.Recycle_List);
      Pool.Recycle_List := Index;
      -- The list is always linked forwards only
   end Free;
   
end Curses.UI.Menus.Standard_Trees.Bounded;
   
