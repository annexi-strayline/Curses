with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;
with Curses.Frames;    use Curses.Frames;
with Curses.Frames.Framed_Windows; use Curses.Frames.Framed_Windows;

with Curses.Terminals.Surfaces; use Curses.Terminals.Surfaces;
with Curses.Device.Environment;

with Curses.UI.Menus; use Curses.UI.Menus;
with Curses.UI.Menus.Renderer;
with Curses.UI.Menus.Renderer.Cascade;
with Curses.UI.Menus.Standard_Trees; use Curses.UI.Menus.Standard_Trees;
with Curses.UI.Menus.Standard_Trees.Unbounded;

with Ada.Text_IO;
with Ada.Exceptions;
with Debug; use Debug;

procedure Menu_Test is
   
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   
   
   function Frame_And_Style 
     (On_Screen    : aliased in out Screen'Class;
      Root_Item    :         in     Menu_Cursor_Type'Class;
      Frame_Extents:         in     Cursor_Position;
      TL_Hint      :         in     Cursor_Position)
     return Framed_Window'Class 
   is begin
      return FW: Framed_Window'Class 
          := New_Framed_Window (On_Screen      => On_Screen,
                                Top_Padding    => 1,
                                Bottom_Padding => 1,
                                Left_Padding   => 1,
                                Right_Padding  => 1,
                                Frame_Extents  => Frame_Extents)
      do
         FW.Set_Border;
      end return;
   end Frame_And_Style;
   
   
   procedure Open_Menu is new Curses.UI.Menus.Renderer.Cascade
     (Frame_And_Style => Frame_And_Style);
   
   
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface with
      record
         Label : String (1 .. 20) := (others => ' ');
         Hotkey: Control_Character := (Class => No_Key, others => <>);
         Has_Submenu: Boolean := False;
      end record;
   
   overriding
   function Label_Length (Item: Menu_Item)
                         return Cursor_Ordinal;
   
   overriding
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Surface'Class;
                           Selected: in     Boolean);
   
   overriding
   function Hot_Key (Item: Menu_Item) 
                    return Curses.Terminals.Surfaces.Control_Character;
   
   overriding
   procedure Execute (Item     : in out Menu_Item;
                      Directive:    out After_Execute_Directive);
   
   overriding
   function Label_Length (Item: Menu_Item)
                         return Cursor_Ordinal
     is (20);
   

   
   overriding
   function Hot_Key (Item: Menu_Item) 
                    return Curses.Terminals.Surfaces.Control_Character
     is (Item.Hotkey);
   
   
   overriding
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Surface'Class;
                           Selected: in     Boolean)
   is
      Use_Cursor: Cursor;
   begin
      Canvas.Clear;
      
      if Selected then
         Use_Cursor.Style.Inverted := True;
         Canvas.Fill (Pattern     => " ",
                      Fill_Cursor => Use_Cursor);
         Canvas.Position_Cursor ((1,1));
      end if;
      
      Canvas.Put (Set_Cursor => Use_Cursor,
                  Content    => Item.Label);
      
      if Item.Has_Submenu then
         Use_Cursor.Position.Column := Canvas.Extents.Column;
         Canvas.Put (Set_Cursor => Use_Cursor,
                     Content    => ">");
      end if;
      
   end Render_Label;
   
   
   overriding
   procedure Execute (Item     : in out Menu_Item;
                      Directive:    out After_Execute_Directive)
   is
   begin
      if Item.Has_Submenu then
         Directive := Open_Submenu;
      else
         Directive := Close_Tree;
      end if;
   end Execute;
   
   
   package Menu_Trees is new Curses.UI.Menus.Standard_Trees.Unbounded
     (Menu_Item);
   
   Tree: Menu_Trees.Unbounded_Menu_Tree;
   

   
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   
   
begin
   
   TTY.Attach;
   
   declare
      Root_Node: Standard_Cursor'Class := Tree.New_Item;
      Staging: aliased Menu_Type'Class := Tree(Root_Node).Submenu;
      
      S: aliased Screen (TTY'Access);


   begin
      S.Put (Content => "Initializing Menu...",
             Advance_Cursor => True);
      
      for I in 1 .. 10 loop
         declare
            Cursor: Standard_Cursor'Class := Tree.New_Item;
         begin
            declare
               New_Item: Menu_Item renames Menu_Item (Tree(Cursor).Ref.all);
               Target_Branch: Menu_Type'Class := Tree(Cursor).Submenu;
            begin
               New_Item.Label (1 .. 4) := "Item";
               New_Item.Has_Submenu := True;
               Int_IO.Put (To   => New_Item.Label (5 .. New_Item.Label'Last - 1),
                           Item => I);
               
               New_Item.Hotkey 
                 := (Class => Graphic, Alt => False, Key => Character'Val (I + 96));
               
               Tree.Append (Branch   => Staging,
                            Position => Cursor);
               
               for I in 1 .. 10 loop
                  -- Make submenu
                  declare
                     Sub_Cursor: Standard_Cursor'Class := Tree.New_Item;
                     
                  begin
                     declare
                        Sub_Item: Menu_Item renames
                          Menu_Item (Tree(Sub_Cursor).Ref.all);
                     begin
                        Sub_Item.Label (1 .. 3) := "Sub";
                     end;
                     Tree.Append (Branch   => Target_Branch,
                                  Position => Sub_Cursor);
                  end;
               end loop;
            end;
         end;
      end loop;
      
      
      S.Put (Content => "OK",
             Advance_Cursor => True);
      
      Open_Menu (On_Screen => S,
                 Branch    => Staging);
      
   end;
   
exception
   when e: others =>
      Debug_Line (Ada.Exceptions.Exception_Information (e));
end Menu_Test;
