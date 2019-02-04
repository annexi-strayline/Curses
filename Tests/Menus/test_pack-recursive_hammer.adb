
with Ada.Exceptions; use Ada;

with Curses.UI.Menus;
with Curses.UI.Menus.Standard_Trees;

with Debug; use Debug;

package body Test_Pack.Recursive_Hammer is
   
   -----------------
   -- Total_Stack --
   -----------------
   procedure Total_Stack (Stack: in Stat_Stack; Total_Pack: out Stat_Pack)
   is begin
      Total_Pack := (others => 0);
      for Pack of Stack loop
         Total_Pack := Total_Pack + Pack;
      end loop;
   end Total_Stack;
   
   
   --------------
   -- Stat_Box --
   --------------
   protected body Stat_Box is
      
      function Ready return Boolean is (Avail);
   
      procedure Deposit (Pack: in Stat_Pack) is
      begin
         Box_Pack := Pack;
         Avail    := True;
      end Deposit;
   
      entry Collect (Pack: out Stat_Pack)
        when Avail is
      begin
         Pack  := Box_Pack;
         Avail := False;
      end Collect;
   
   end Stat_Box;
   
   ------------
   -- Hammer --
   ------------
   task body Hammer is
      use Curses.UI.Menus;
      use Curses.UI.Menus.Standard_Trees;
      
      use all type Directorate.Generator;
      
      Director         : Directorate.Generator;
      Stats            : Stat_Pack;
      Running_Directive: Random_Directive;
      Our_Index        : Positive;
      Kill_Order       : Boolean := False;
      
      procedure Inc (N: in out Big_Counter) with Inline is
      begin
         N := N + 1;
      end Inc;
      
      procedure Dec (N: in out Big_Counter) with Inline is
      begin
         N := N - 1;
      end Dec;
      
      -- The primary logic
      procedure Recurse_Branch (Root          : in     Standard_Cursor'Class;
                                Last_Directive:    out Random_Directive);
      
      procedure Recurse_Branch (Root          : in     Standard_Cursor'Class;
                                Last_Directive:    out Random_Directive)
      is
         This_Branch : Menu_Type'Class       := Tree(Root).Submenu;
         Current_Item: Standard_Cursor'Class := Tree.New_Item;
         
      begin
         Inc (Stats.Current_Depth);
         
         -- Check to see if we are "creating" a branch
         if not This_Branch.Iterate.First.Has_Element then
            Inc (Stats.Created_Branches);
         end if;
         
         -- We always append a new item to the branch
         Tree.Append (Branch   => This_Branch,
                      Position => Current_Item);
         Inc (Stats.Created_Nodes);

         
         if Stats.Max_Depth < Stats.Current_Depth then
            Stats.Max_Depth := Stats.Current_Depth;
         end if;
         
         loop
            Last_Directive := Random (Director);
            Debug_Line ("Hammer " & Positive'Image (Our_Index) & ": " &
                          Random_Directive'Image (Last_Directive));
            case Last_Directive is
               when Append_New =>
                  Current_Item := Tree.New_Item;
                  Tree.Append (Branch   => This_Branch,
                               Position => Current_Item);
                  Inc (Stats.Created_Nodes);
                  Inc (Stats.Append_New_Count);
               
               when Prepend_New =>
                  Current_Item := Tree.New_Item;
                  Tree.Prepend (Branch   => This_Branch,
                                Position => Current_Item);
                  Inc (Stats.Created_Nodes);
                  Inc (Stats.Prepend_New_Count);
                  
               when Insert_Before_Last =>
                  declare
                     Next_Item: Standard_Cursor'Class := Tree.New_Item;
                  begin
                     
                     Tree.Insert_Before (Branch   => This_Branch,
                                         Before   => Current_Item,
                                         Position => Next_Item);
                     Current_Item := Next_Item;
                  end;
             
                  Inc (Stats.Created_Nodes);
                  Inc (Stats.Insert_Before_Count);
               
               when Insert_After_Last =>
                  declare
                     Next_Item: Standard_Cursor'Class := Tree.New_Item;
                  begin
                     Tree.Insert_After (Branch   => This_Branch,
                                        After    => Current_Item,
                                        Position => Next_Item);
                     Current_Item := Next_Item;
                  end;
               
                  Inc (Stats.Created_Nodes);
                  Inc (Stats.Insert_After_Count);
               
               when Up_Branch =>
                  Inc (Stats.Up_Branch_Count);
                  exit;
               
               when Down_Branch =>
                  -- Go down the branch at whatever item we're pointed to
                  Inc (Stats.Down_Branch_Count);
                  
                  Recurse_Branch (Root           => Current_Item,
                                  Last_Directive => Last_Directive);
                  
                  if Last_Directive = Delete_All then
                     exit;
                     
                  elsif Last_Directive = Delete_Current_Branch then
                     -- We are executing the "Delete_Current_Branch" directive
                     -- on behalf of the branch we just came back from.
                     Tree.Delete (Current_Item);
                     Inc (Stats.Deleted_Branches);
                     Inc (Stats.Deleted_Nodes);
                     
                     Current_Item := Tree.New_Item;
                     Tree.Prepend (Branch   => This_Branch,
                                   Position => Current_Item);

                     Inc (Stats.Created_Nodes);
                  end if;
               
                  -- Otherwise, we just keep going as we were
               
               when Delete_Current_Item =>
                  Tree.Delete (Current_Item);
                  Inc (Stats.Deleted_Nodes);
                  
                  Current_Item := Tree.New_Item;
                  Tree.Prepend (Branch   => This_Branch,
                                Position => Current_Item);

                  Inc (Stats.Created_Nodes);
                  Inc (Stats.Delete_Item_Count);
               
               when Delete_Current_Branch  =>
                  Inc (Stats.Delete_Branch_Count);
                  exit;
               
               when Delete_All =>
                  Inc (Stats.Delete_All_Count);
                  exit;
               
            end case;
            
            -- Update the stat report if needed
            if not Hammer_Stats(Our_Index).Ready then
               Hammer_Stats(Our_Index).Deposit (Stats);
            end if;
            
         end loop;
         
         Dec (Stats.Current_Depth);
      end Recurse_Branch;
      
      
   begin
      
      accept Init (Index: in Positive) do
         Our_Index := Index;
      end Init;
      
      Reset (Director);
      
      delay 1.0;
      
      declare
         Root_Node: Standard_Cursor'Class := Tree.New_Item;
         
      begin
         loop
            Debug_Line ("--- Hammer " & Positive'Image (Our_Index) & " ---");
            Recurse_Branch (Root           => Root_Node,
                            Last_Directive => Running_Directive);
            
            -- Coming back from here means the Directive is either
            -- "Delete_Current_Branch" when refering to the Root branch, or is
            -- "Delete_All", both are logicially equivilient
            
            Debug_Line ("Hammer " & Positive'Image (Our_Index) & ": Deleting root...");
            --Tree.Delete (Root_Node);
            Debug_Line ("   Done - Hammer " & Positive'Image (Our_Index));
            
            select
               accept Kill;
               Debug_Line ("Hammer " & Positive'Image (Our_Index) & ": " & 
                             "Kill - exiting.");
               exit;
            else
               null;
               --Root_Node := Tree.New_Item;
            end select;
         end loop;
      end;
      
      
   exception
      when e: others =>
         Debug_Line ("Hammer " & Positive'Image (Our_Index) & ": " & 
                       Exceptions.Exception_Information (e));
   end Hammer;
   
   
   --------------------
   -- Group_Cruncher --
   --------------------
   task body Group_Cruncher is
      Group_First, Group_Last: Positive;
      Totals_Pack: Stat_Pack;
   begin
      
      accept Config_Group (First, Last: in Positive) do
         Group_First := First;
         Group_Last  := Last;
      end Config_Group;
      
      declare
         Stats: Stat_Stack (Group_First .. Group_Last);
         Our_Members: constant Positive := Group_Last - Group_First + 1;
      begin
         Main_Loop: loop
            for I in Stats'Range loop
--               select
                  Hammer_Stats(I).Collect (Stats(I));
--               or
--                  delay 10.0;
--                  Debug_Line ("Group Cruncher " & 
--                                Positive'Image (Group_First) & " -" &
--                                Positive'Image (Group_Last) & ": " &
--                                "Aborting due to Collect timeout");
--                  exit Main_Loop;
--               end select;
            end loop;
            
            Total_Stack (Stats, Totals_Pack);
            
            select
               accept Collect_Group (Totals : out Stat_Pack; 
                                     Members: out Positive)
               do
                  Totals  := Totals_Pack;
                  Members := Our_Members;
               end Collect_Group;
            or
               terminate;
            end select;
               
         end loop Main_Loop;
      end;
   exception
      when e: others =>
         Debug_Line ("Group Cruncher " & 
                       Positive'Image (Group_First) & " -" &
                       Positive'Image (Group_Last) & ": " &
                       Exceptions.Exception_Information (e));
   end Group_Cruncher;
   
   
   ---------------
   -- Totalizer --
   ---------------
   task body Totalizer is
      Member_Totals: Stat_Stack (Cruncher_Set'Range);
      Temp_Members, Total_Members: Natural;
      Total_Pack, Average_Pack: Stat_Pack;
   begin
      
      -- We also have the responsibility of setting-up the Hammers and 
      -- Crunchers
      
      for I in Hammer_Group'Range loop
         Hammer_Group(I).Init (I);
      end loop;
      
      declare
         Module: Positive := Hammer_Group'Length  /  Cruncher_Set'Length;
         Remain: Natural  := Hammer_Group'Length mod Cruncher_Set'Length;
         First : Positive := Hammer_Group'First;
         Last  : Positive := First + Module - 1;
      begin
         for I in Cruncher_Set'Range loop
            Cruncher_Set(I).Config_Group (First, Last);
            
            First := Last + 1;
            Last  := First + Module - 1;
            
            if I > Cruncher_Set'First and then I = Cruncher_Set'Last - 1 then
               Last := Last + Remain;
            end if;
         end loop;
      end;
      
      Main_Loop: loop
         Total_Members := 0;
         
         for I in Cruncher_Set'Range loop
--            select
               Cruncher_Set(I).Collect_Group (Totals  => Member_Totals(I),
                                              Members => Temp_Members);
--            or
--               delay 10.0;
--               Debug_Line ("Totalizer: Aborted due to Collect_Group timeout");
--               exit Main_Loop;
--            end select;
            
            Total_Members := Total_Members + Temp_Members;
         end loop;
         
         Total_Stack (Member_Totals, Total_Pack);
         Average_Pack := Total_Pack / Big_Counter (Total_Members);
         
         select
            accept Global_Stats (Totals, Averages: out Stat_Pack) do
               Totals   := Total_Pack;
               Averages := Average_Pack;
            end Global_Stats;
         or
            terminate;
         end select;
         
      end loop Main_Loop;
   exception
      when e: others =>
         Debug_Line ("Totalizer: " & Exceptions.Exception_Information (e));
   end Totalizer;
   
   
   procedure Shutdown is
   begin
      for T of Hammer_Group loop
         T.Kill;
      end loop;
      
   end Shutdown;
   
end Test_Pack.Recursive_Hammer;
