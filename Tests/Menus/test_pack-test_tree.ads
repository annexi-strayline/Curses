
with Curses;
with Curses.Terminals.Surfaces;
with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees;

private with Curses.UI.Menus.Standard_Trees.Bounded;

package Test_Pack.Test_Tree is
   
   ---------------
   -- Menu_Item --
   ---------------
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface
     with private;
   
   overriding
   function Label_Length (Item: Menu_Item)
                         return Curses.Cursor_Ordinal is (1);
   
   overriding
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Curses.Surface'Class;
                           Selected: in     Boolean);
   
   function Hot_Key (Item: Menu_Item) 
                    return Curses.Terminals.Surfaces.Control_Character
     is ((Class => Curses.Terminals.Surfaces.No_Key, others => <>));
   
   overriding
   procedure Execute 
     (Item     : in out Menu_Item;
      Directive:    out Curses.UI.Menus.After_Execute_Directive);

   
   ---------------
   -- Menu_Tree --
   ---------------
   type Menu_Tree is
     limited new Curses.UI.Menus.Standard_Trees.Standard_Tree with private;
   
   
private
   
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface with
      record
         Label: String (1 .. 60);
      end record;
   
   pragma Assertion_Policy (Check);
   
   package Bounded_Tree is new Curses.UI.Menus.Standard_Trees.Bounded
     (Base_Item => Menu_Item, Max_Items => 1_000_000);
   
   type Menu_Tree is
     limited new Bounded_Tree.Bounded_Menu_Tree with null record;
   
end Test_Pack.Test_Tree;
   
