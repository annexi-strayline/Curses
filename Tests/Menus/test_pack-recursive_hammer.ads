
with Test_Pack.Bounded_Tree; use Test_Pack.Bounded_Tree;

private with Ada.Numerics.Discrete_Random;

package Test_Pack.Recursive_Hammer is
   
   pragma Assertion_Policy (Check);
   
   Hammers  : constant := 6;
   Crunchers: constant := 2;
   Tree_Size: constant := 1_000_000;
   
   Tree: Menu_Tree (Tree_Size);
   
   type Big_Counter is mod 2**64;
   
   type Stat_Pack is
      record
         -- Cumulative statistics
         Created_Nodes      : Big_Counter := 0;
         Created_Branches   : Big_Counter := 0;
         
         Deleted_Nodes      : Big_Counter := 0;
         Deleted_Branches   : Big_Counter := 0;
         
         Current_Depth      : Big_Counter := 0;
         Max_Depth          : Big_Counter := 0;
         
         Append_New_Count   : Big_Counter := 0;
         Prepend_New_Count  : Big_Counter := 0;
         Insert_Before_Count: Big_Counter := 0;
         Insert_After_Count : Big_Counter := 0;
         Up_Branch_Count    : Big_Counter := 0;
         Down_Branch_Count  : Big_Counter := 0;
         Delete_Item_Count  : Big_Counter := 0;
         Delete_Branch_Count: Big_Counter := 0;
         Delete_All_Count   : Big_Counter := 0;
      end record;
   
   function "+" (Left, Right: Stat_Pack) return Stat_Pack
     is ((Created_Nodes
            => Left.Created_Nodes       + Right.Created_Nodes,
          Created_Branches
            => Left.Created_Branches    + Right.Created_Branches,
          
          Deleted_Nodes
            => Left.Deleted_Nodes       + Right.Deleted_Nodes,
          Deleted_Branches
            => Left.Deleted_Branches    + Right.Deleted_Branches,
          
          Current_Depth
            => Left.Current_Depth       + Right.Current_Depth,
          Max_Depth
            => Left.Max_Depth           + Right.Max_Depth,
          
          Append_New_Count
            => Left.Append_New_Count    + Right.Append_New_Count,
          Prepend_New_Count
            => Left.Prepend_New_Count   + Right.Prepend_New_Count,
          Insert_Before_Count
            => Left.Insert_Before_Count + Right.Insert_Before_Count,
          Insert_After_Count
            => Left.Insert_After_Count  + Right.Insert_After_Count,
          Up_Branch_Count
            => Left.Up_Branch_Count     + Right.Up_Branch_Count,
          Down_Branch_Count
            => Left.Down_Branch_Count   + Right.Down_Branch_Count,
          Delete_Item_Count
            => Left.Delete_Item_Count   + Right.Delete_Item_Count,
          Delete_Branch_Count
            => Left.Delete_Branch_Count + Right.Delete_Branch_Count,
          Delete_All_Count
            => Left.Delete_All_Count + Right.Delete_All_Count))
   with Inline;
            
   function "/" (Left: Stat_Pack; Right: Big_Counter) return Stat_Pack
     is ((Created_Nodes       => Left.Created_Nodes       / Right,
          Created_Branches    => Left.Created_Branches    / Right,
          
          Deleted_Nodes       => Left.Deleted_Nodes       / Right,
          Deleted_Branches    => Left.Deleted_Branches    / Right,
          
          Current_Depth       => Left.Current_Depth       / Right,
          Max_Depth           => Left.Max_Depth           / Right,
          
          Append_New_Count    => Left.Append_New_Count    / Right,
          Prepend_New_Count   => Left.Prepend_New_Count   / Right,
          Insert_Before_Count => Left.Insert_Before_Count / Right,
          Insert_After_Count  => Left.Insert_After_Count  / Right,
          Up_Branch_Count     => Left.Up_Branch_Count     / Right,
          Down_Branch_Count   => Left.Down_Branch_Count   / Right,
          Delete_Item_Count   => Left.Delete_Item_Count   / Right,
          Delete_Branch_Count => Left.Delete_Branch_Count / Right,
          Delete_All_Count    => Left.Delete_All_Count    / Right))
     with Inline;
          
   
   type Stat_Stack is array (Positive range <>) of aliased Stat_Pack;
   
   procedure Total_Stack   (Stack: in Stat_Stack; Total_Pack  : out Stat_Pack);
   
   
   protected type Stat_Box is
      
      function Ready return Boolean;
      -- Returns True if a new Deposit is available for pickup
      
      procedure Deposit (Pack: in  Stat_Pack);
      entry     Collect (Pack: out Stat_Pack);
      
   private
      Avail     : Boolean := False;
      Box_Pack  : Stat_Pack;
   end;
   
   
   Hammer_Stats: array (1 .. Hammers) of Stat_Box;
   
   task type Hammer is
      entry Init (Index: in Positive);
      entry Kill;
   end Hammer;
   
   Hammer_Group: array (1 .. Hammers) of Hammer;
   
   
   task type Group_Cruncher is
      entry Config_Group  (First, Last: in Positive);
      entry Collect_Group (Totals: out Stat_Pack; Members: out Positive);
   end Group_Cruncher;
   
   Cruncher_Set: array (1 .. Crunchers) of Group_Cruncher;
   
   task Totalizer is
      entry Global_Stats (Totals, Averages: out Stat_Pack);
   end Totalizer;
   
   
   procedure Shutdown;
   
private
   
   type Random_Directive is
     (Append_New, 
      Prepend_New, 
      Insert_Before_Last, -- "Naked new" if no last
      Insert_After_Last,  -- Append to root if no last
      Up_Branch,          -- Ignore if current branch is root
      Down_Branch,        -- New item alwas appended
      Delete_Current_Item,  
      Delete_Current_Branch,
      Delete_All);
      
   package Directorate is new Ada.Numerics.Discrete_Random (Random_Directive);
   
end Test_Pack.Recursive_Hammer;
