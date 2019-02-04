
with Ada.Exceptions; use Ada;

with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees; use Curses.UI.Menus.Standard_Trees;
with Test_Pack.Bounded_Tree;

with Text_IO; use Text_IO;
with Debug; use Debug;

procedure Branch_Smasher is
   
   Tree: Test_Pack.Bounded_Tree.Menu_Tree (1_000_000);
   
   function Avail_Actual return Natural renames Test_Pack.Bounded_Tree.Avail;
   
   function Avail return String is (Integer'Image (Avail_Actual));
   
   Max_Depth: constant := 10;
   Nodes_Per_Branch: constant := 10;
   
   subtype Menu_Branch is Curses.UI.Menus.Menu_Type;
   
   task type Brancher;
   
   task body Brancher is
      Root: Standard_Cursor'Class := Tree.New_Item;
   begin
      
      declare
         Branch_Root: Standard_Cursor'Class := Root;
         Last_Item  : Standard_Cursor'Class := Root;
      begin
         
         for I in 1 .. Max_Depth loop
            declare
               Branch: Menu_Branch'Class := Tree(Branch_Root).Submenu;
            begin
               for K in 1 .. Nodes_Per_Branch loop
                  Last_Item := Tree.New_Item;
               
                  Tree.Append (Branch   => Branch,
                                Position => Last_Item);
                  
                  declare
                     Inner_Branch: Menu_Branch'Class 
                       := Tree(Last_Item).Submenu;
                  begin
                     for J in 1 .. Nodes_Per_Branch loop
                        declare
                           Temp_Item: Standard_Cursor'Class := Tree.New_Item;
                        begin
                           Tree.Append (Branch   => Inner_Branch,
                                        Position => Temp_Item);
                        end;
                     end loop;
                  end;
                  
               end loop;
            end;
            
            Branch_Root := Last_Item;
         end loop;
      end;
      
      Tree.Delete (Root);
      
   exception
      when e: others =>
      Debug_Line ("Brancher: " & Exceptions.Exception_Information (e));
   end Brancher;
   
   Avail_In : Natural;
   Avail_Out: Natural;
   
begin
   Put_Line ("Running...");
   
   loop
      Avail_In := Avail_Actual;
      declare
         Taskforce: array (1 .. 20) of Brancher;
      begin
         null;
      end;
      
      Avail_Out := Avail_Actual;
      
      if Avail_In /= Avail_Out then
         Put_Line ("Avail mismatch - In:" & Integer'Image (Avail_In) &
                     " Out:" & Integer'Image (Avail_Out));
      end if;

   end loop;
   
end Branch_Smasher;
