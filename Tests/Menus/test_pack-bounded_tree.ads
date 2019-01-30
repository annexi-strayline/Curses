
with Curses;
with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees;

private with Curses.UI.Menus.Standard_Trees.Bounded;

package Test_Pack.Bounded_Tree is
   
   ---------------
   -- Menu_Item --
   ---------------
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface
     with private;
   
   overriding
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Curses.Surface'Class;
                           Selected: in     Boolean);
   
   overriding
   function Available (Item: Menu_Item) return Boolean is (True);
   
   overriding
   procedure Execute 
     (Item     : in out Menu_Item;
      Directive:    out Curses.UI.Menus.After_Execute_Directive);

   
   procedure Set_Label (Item: in out Menu_Item; Label: in String);
   
   
   ---------------
   -- Menu_Tree --
   ---------------
   type Menu_Tree (Capacity: Positive) is
     limited new Curses.UI.Menus.Standard_Trees.Standard_Tree with private;

     
private
   
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface with
      record
         Label: String (1 .. 60);
      end record;
   
   package Bounded_Tree is new Curses.UI.Menus.Standard_Trees.Bounded
     (Base_Item => Menu_Item);
   
   type Menu_Tree (Capacity: Positive) is
     limited new Bounded_Tree.Bounded_Menu_Tree (Capacity) with null record;
   
end Test_Pack.BoundeD_Tree;
   
