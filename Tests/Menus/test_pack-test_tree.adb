
package body Test_Pack.Test_Tree is
   
   procedure Set_Label (Item: in out Menu_Item; Label: in String)
   is
   begin
      Item.Label(Item.Label'First .. Item.Label'First + Label'Length - 1)
        := Label;
      
      Item.Label(Item.Label'First + Label'Length + 1 .. Item.Label'Last)
        := (others => ' ');
      
   end Set_Label;
   
   overriding
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Curses.Surface'Class;
                           Selected: in     Boolean)
   is
      use all type Curses.Surface;
   begin
      Canvas.Put (Item.Label);
   end Render_Label;
   
   overriding
   procedure Execute 
     (Item     : in out Menu_Item;
      Directive:    out Curses.UI.Menus.After_Execute_Directive)
   is
      use all type Curses.UI.Menus.After_Execute_Directive;
   begin
      Directive := Close_Menu;
   end Execute;
     
end Test_Pack.Test_Tree;
