
with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;

with Curses.Terminals.Surfaces;
with Curses.Device.Environment;

with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees;

with Test_Pack.Recursive_Hammer;
with Test_Pack.Bounded_Tree;

with Debug; use Debug;

procedure Tree_Hammer is

   package TERM renames Curses.Device.Environment;

   TTY: aliased Terminal (TERM.Environment_Terminal);
   
   use all type Curses.Terminals.Surfaces.Control_Class;
   subtype C_Char is Curses.Terminals.Surfaces.Control_Character;
   
   package Hammer renames Test_Pack.Recursive_Hammer;
   
   use type Hammer.Stat_Pack;
   use type Hammer.Big_Counter;
   
   subtype Counter is Hammer.Big_Counter;
   
   Totals, Averages: Hammer.Stat_Pack;
   
   Key: C_Char;
   
begin
   
   TTY.Attach;
   
   declare
      S: aliased Screen (TTY'Access);
      
      Title_Cursor : Cursor := (Position => (Row    => 1,
                                             Column => S.Extents.Column / 2),
                                Style    => Bold_Style + Inverted_Style,
                                others   => <>); 
      
      Status_Cursor: Cursor := (Position => (Row    => S.Extents.Row,
                                             Column => 2),
                                Style => Inverted_Style, others => <>); 
      
      Label_Column: constant := 6;
      Data_Column : constant := 30;
      Start_Row   : constant := 5;
      
      Label_Cursor : Cursor := (Position => (Row    => Start_Row,
                                             Column => Label_Column),
                                Style    => Bold_Style,
                                others   => <>);
      
      Data_Cursor  : Cursor := (Position => (Row    => Start_Row,
                                             Column => Data_Column),
                                others   => <>);
      
   begin
      S.Clear;
      S.Position_Cursor (S.Extents);
      
      declare
         Clear_String: String (1 .. Positive (S.Extents.Column))
           := (others => ' ');
         Title_Temp: Cursor
           := (Position => (Row    => Title_Cursor.Position.Row,
                            Column => 1),
               Style    => Title_Cursor.Style,
               others   => <>);
         Status_Temp: Cursor
           := (Position => (Row    => Status_Cursor.Position.Row,
                            Column => 1),
               Style    => Status_Cursor.Style,
               others   => <>);
      begin
         S.Put (Set_Cursor => Title_Temp, Content => Clear_String);
         S.Put (Set_Cursor => Status_Temp, Content => Clear_String);
      end;
      
      S.Put (Set_Cursor => Title_Cursor,
             Content    => "Tree Hammer",
             Justify    => Center);
      
      S.Put (Set_Cursor => Status_Cursor,
             Content    => "Press any key to stop . . .");
      
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Created_Nodes      :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Created_Branches   :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 2;
      
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Deleted_Nodes      :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Deleted_Nodes      :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 2;
      
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Current_Depth      :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Max_Depth          :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 2;
      
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Append_New_Count   :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Prepend_New_Count  :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Insert_Before_Count:");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Insert_After_Count :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Up_Branch_Count    :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Down_Branch_Count  :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Delete_Item_Count  :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Delete_Branch_Count:");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 1;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Delete_All_Count   :");
      Label_Cursor.Position.Row := Label_Cursor.Position.Row + 3;
      
      S.Put (Set_Cursor => Label_Cursor,
             Content    => "Pool Available     :");
      
      S.Show;
      
      loop
         Data_Cursor.Position.Row := Start_Row;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Created_Nodes));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Created_Branches));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 2;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image(Totals.Deleted_Nodes));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Deleted_Nodes));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 2;
         
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Current_Depth));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Max_Depth));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 2;
         
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Append_New_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Prepend_New_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Insert_Before_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Insert_After_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Up_Branch_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Down_Branch_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Delete_Item_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Delete_Branch_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 1;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Counter'Image (Totals.Delete_All_Count));
         Data_Cursor.Position.Row := Data_Cursor.Position.Row + 3;
         
         S.Clear_To_End (Data_Cursor);
         S.Put (Set_Cursor => Data_Cursor,
                Content    => Natural'Image (Test_Pack.Bounded_Tree.Avail));
         
         exit when S.Input_Key (Wait => False).Class /= No_Key;
         
         delay 1.0;
         
         select
            Hammer.Totalizer.Global_Stats (Totals, Averages);
         or
            delay 5.0;
            exit;
         end select;
      end loop;
      
      Hammer.Shutdown;
      
      declare
         Clear_String: String (1 .. Positive (S.Extents.Column))
           := (others => ' ');

         Status_Temp: Cursor
           := (Position => (Row    => Status_Cursor.Position.Row,
                            Column => 1),
               Style    => Status_Cursor.Style,
               others   => <>);
      begin
         S.Put (Set_Cursor => Status_Temp, Content => Clear_String);
      end;
      
      S.Put (Set_Cursor => Status_Cursor,
             Content    => "Deleting entire Tree...");
      
      Debug_Line ("Going in: " & Natural'Image (Test_Pack.Bounded_Tree.Avail));
      
      declare
         use Curses.UI.Menus.Menu_Iterators;
         use Curses.UI.Menus.Standard_Trees;
         use Test_Pack.Recursive_Hammer;
         
         
         Iterator: Forward_Iterator'Class := Tree.Staging_Branch.Iterate;
         First: Standard_Cursor'Class := Standard_Cursor'Class(Iterator.First);
         Next : Standard_Cursor'Class := Standard_Cursor'Class(Iterator.Next (First));
      begin
         while First.Has_Element  loop
            Tree.Delete (First);
            Debug_Line ("*");
            
            exit when not Next.Has_Element;
            
            First := Next;
            Next  := Standard_Cursor'Class (Iterator.Next (First));
         end loop;
      end;
      
      Debug_Line ("Going out: " & Natural'Image (Test_Pack.Bounded_Tree.Avail));
      
      declare
         Clear_String: String (1 .. Positive (S.Extents.Column))
           := (others => ' ');
         
         Status_Temp: Cursor
           := (Position => (Row    => Status_Cursor.Position.Row,
                            Column => 1),
               Style    => Status_Cursor.Style,
               others   => <>);
      begin
         S.Put (Set_Cursor => Status_Temp, Content => Clear_String);
      end;
      
      S.Put (Set_Cursor => Status_Cursor,
             Content    => "Complete - Pool avail:" & 
               Natural'Image (Test_Pack.Bounded_Tree.Avail));
      
      Key := S.Input_Key;
      
   end;
   
end Tree_Hammer;
