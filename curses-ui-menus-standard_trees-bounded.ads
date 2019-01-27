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

-- This package provides a standard implementation Standard_Tree with bounded
-- storage.
--
-- Refer to the parent Standart_Trees package for a full interface listing of
-- the Standard_Tree interface.

private with Curses.UI.Menus.Standard_Trees.Logic;

generic
   type Base_Item is limited new Menu_Item_Type with private;
   
package Curses.UI.Menus.Standard_Trees.Bounded is
   
   -----------------
   -- Menu_Cursor --
   -----------------
   type Menu_Cursor is new Standard_Cursor with private;
   
   ---------------
   -- Menu_Item --
   ---------------
   type Menu_Item is limited new Base_Item with private;
   
   -----------------
   -- Menu_Branch --
   -----------------
   type Menu_Branch is new Menu_Type with private;
   -- References a particular Submenu of a tree.
   
   ------------------
   -- Bounded_Tree --
   ------------------
   type Bounded_Menu_Tree (Capacity: Positive) is limited new Standard_Tree
     with private;
   
private
   
   ---------------
   -- Item_Pool --
   ---------------
   subtype Index_Type    is Integer    range 0 .. Positive'Last;

   Null_Index: constant Index_Type := Index_Type'First;
   
   -- Generic element
   package GTE is new Logic.Generic_Tree_Element
     (Base_Item  => Base_Item,
      Index_Type => Index_Type,
      Null_Index => Null_Index);
   
   type Pool_Data is array (Positive range <>) of aliased GTE.Tree_Element;
   
   ----------------------------------------
   type Item_Pool (Capacity: Positive) is
      record
         Data: Pool_Data (1 .. Capacity);
         
         Next_Fresh: Index_Type := 1;
         -- First unused index in Data. Set to Null_Index when there are no
         -- more "Fresh" items available
         
         Recycle_List: Index_Type := Null_Index;
         -- Points to an Index which is the head of a list of free items.
      end record;
   -- Note that the Logic package promises that all calls to Free and Alocate
   -- are serialized, so we don't need to add further synchronization features
   -- to this type.
   
   
   -- Item_Pool operations used by Logic.Generic_Menu_Tree
   function  Allocate (Pool: in out Item_Pool) return Index_Type;
   
   procedure Free (Pool : in out Item_Pool;
                   Index: in     Index_Type);
   
   function  Lookup (Pool : in out Item_Pool;
                     Index: in Index_Type)
                    return Menu_Item_Reference_Type
     is (Menu_Item_Reference_Type'(Ref => Pool.Data(Index)'Access));

   -- Generic tree
   package GMT is new Logic.Generic_Menu_Tree
     (GTE            => GTE,
      Pool_Parameter => Positive,
      Item_Pool      => Item_Pool);
   
   type Menu_Item   is limited new GMT.Menu_Item   with null record;
   type Menu_Cursor is         new GMT.Menu_Cursor with null record;
   type Menu_Branch is         new GMT.Menu_Branch with null record;
   
   type Bounded_Menu_Tree (Capacity: Positive) is 
     limited new GMT.Menu_Tree (Capacity)
     with null record;
   
   
end Curses.UI.Menus.Standard_Trees.Bounded;
