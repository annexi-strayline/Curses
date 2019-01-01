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

-- This package provides a standard implementation Standard_Tree with bounded
-- storage.
--
-- Refer to the parent Standart_Trees package for a full interface listing of
-- the Standard_Tree interface.

private with Curses.UI.Menus.Standard_Trees.Core;

generic
   type Base_Item is limited new Menu_Item_Type with private;
   
package Curses.UI.Menus.Standard_Trees.Bounded with Preelaborate is
   
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
   
   overriding
   procedure Adjust (Branch: in out Menu_Branch);
   
   overriding
   procedure Finalize (Branch: in out Menu_Branch);
   
   overriding 
   function  Index (Menu  : Menu_Branch; 
                    Cursor: Menu_Cursor_Type'Class) 
                   return Menu_Item_Reference_Type;
   
   ------------------
   -- Bounded_Tree --
   ------------------
   type Bounded_Tree (Capacity: Positive) is limited new Standard_Tree
     with private;
   
private

   subtype Index_Type is Natural;
   Null_Index: constant Index_Type := 0;

   package Core renames Curses.UI.Menus.Standard_Trees.Core;
   
   -- Generic element
   package GTE is new Core.Generic_Tree_Element
     (Base_Item => Base_Item,
      Index_Type => Index_Type,
      Null_Index => Null_Index);
   
   use all type GTE.Tree_Element;
   
   type Pool_Data is array (Index_Type range <>) of aliased GTE.Tree_Element;
   
   
   ---------------
   -- Item_Pool --
   ---------------
   protected type Pool_Controller (Max_Index: Index_Type) is
      
      procedure Allocate (Index: out    Index_Type);
      procedure Free     (Index: in out Index_Type);
      
   private
      
      Have_Fresh  : Boolean    := True;
      Next_Fresh  : Index_Type := Null_Index;
      
      Recycle_Root: Index_Type := Null_Index;
      
   end Pool_Controller;
   
   ----------------------------------------
   type Item_Pool (Capacity: Positive) is
      record
         Controller: Pool_Controller (Capacity);
         Data      : Pool_Data (1 .. Capacity);
      end record;
   
   function  Allocate (Pool: in out Item_Pool) return Index_Type;
   
   procedure Free     (Pool: in out Item_Pool);
   
   procedure Modify   (Pool  : in out Item_Pool;
                       Index : in     Index_Type;
                       Action: not null access procedure
                         (Item: aliased in out GTE.Tree_Element'Class));
   
   
   -- Generic tree
   package GMT is new Core.Generic_Menu_Tree
     (GTE            => GTE,
      Pool_Parameter => Positive,
      Item_Pool      => Item_Pool);
   
   use all type GMT.Menu_Item;
   use all type GMT.Menu_Cursor;
   use all type GMT.Menu_Branch;
   use all type GMT.Menu_Tree;
   
   type Menu_Item   is limited new GMT.Menu_Item   with null record;
   type Menu_Cursor is         new GMT.Menu_Cursor with null record;
   type Menu_Branch is         new GMT.Menu_Branch with null record;
   
   type Bounded_Tree (Capacity: Positive) is 
     limited new GMT.Menu_Tree (Capacity)
     with null record;
   
   
end Curses.UI.Menus.Standard_Trees.Bounded;
