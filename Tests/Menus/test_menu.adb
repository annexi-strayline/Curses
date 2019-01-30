with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;
with Curses.Frames;    use Curses.Frames;

with Curses.Terminals.Surfaces;
with Curses.Device.Environment;

with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees;
with Test_Item; use Test_Item;

with Debug; use Debug;

procedure Test_Menu is
   
   use all type Curses.UI.Menus.Menu_Type;
   
   package Stand_Trees renames Curses.UI.Menus.Standard_Trees;

   subtype Standard_Cursor is Stand_Trees.Standard_Cursor;
   
   subtype Menu_Cursor is Standard_Cursor;
   subtype Menu_Branch is Curses.UI.Menus.Menu_Type;
   
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   
   use all type Curses.Terminals.Surfaces.Control_Class;
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
   
   Key: Control_Character;
   
   Menu: Menu_Tree (21);
   
begin
   TTY.Attach;
   
   declare
      S: aliased Screen (TTY'Access);
      C: Cursor;
      
      
   begin
      loop
         S.Clear;
         C.Position := (Row => 2, Column => 2);
         C.Style := Underline_Style + Bold_Style;
         
         
         S.Put (C, "Creating New Submenu...");
         S.Show;
         
         declare
            Root_Item_Handle: Menu_Cursor'Class := Menu.New_Item;
         begin
            declare
               Branch: Menu_Branch'Class := Menu(Root_Item_Handle).Submenu;   
            begin
               
               C := (Position => (3, 2), Style => Normal_Style, others => <>);
               S.Current_Cursor (C);
               
               for I in 1 .. 10 loop
                  declare
                     Label_String: String := Integer'Image (I);
                     New_Item: Menu_Cursor'Class := Menu.New_Item;
                  begin
                     S.Put (Content => Label_String, Advance_Cursor => True);
                     Menu_Item (Menu(New_Item).Ref.all).Set_Label (Label_String);
                     
                     S.Put (Content => " - OK. Appending to branch..", 
                            Advance_Cursor => True);
                     
                     Menu.Append (Branch => Branch, Position => New_Item);
                     
                     S.Put (Content => "Done", 
                            Advance_Cursor => True);
                  end;
                  
                  delay 0.5;
                  
                  C.Position.Row := C.Position.Row + 1;
                  S.Position_Cursor (C.Position);
               end loop;
            end;
            
            C.Position.Row := C.Position.Row + 1;
            C.Style := Bold_Style;
            S.Current_Cursor (C);
            S.Put (Content => "Deleting all... ", Advance_Cursor => True);
            
            Menu.Delete (Root_Item_Handle);
            

            S.Put ("Done.");
            
            declare
               Running_Count: Positive := 1;
            begin
               null;
            end;
         end;
            
         Key := S.Input_Key;
         
         exit when Key.Class = Graphic and then Key.Key = 'x';
      end loop;
   end;
   
end Test_Menu;
