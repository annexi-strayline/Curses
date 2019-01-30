
with Test_Pack.Bounded_Tree; use Test_Pack.Bounded_Tree;

private with Ada.Numerics.Discrete_Random;

package Test_Pack.Recursive_Hammer is
   
   Tree: Menu_Tree (1_000_000_000);
   
   type Big_Counter is mod 2**64;
   
   type Stat_Set is
      record
         Last   : Big_Counter := 0;
         Samples: Big_Counter := 0;
         Average: Big_Counter := 0;
      end record;
   
   type Stat_Pack is
      record
         -- Cumulative statistics
         Created_Nodes      : Stat_Set;
         Created_Branches   : Stat_Set;
         
         Deleted_Nodes      : Stat_Set;
         Deleted_Branches   : Stat_Set;
         
         -- High-water marks
         Max_Nodes          : Stat_Set;
         Max_Branches       : Stat_Set;
         Max_Depth          : Stat_Set;
         
         Append_New_Count   : Stat_Set;
         Prepend_New_Count  : Stat_Set;
         Insert_Before_Count: Stat_Set;
         Insert_After_Count : Stat_Set;
         Up_Branch_Count    : Stat_Set;
         New_Branch_Count   : Stat_Set;
         Delete_Item_Count  : Stat_Set;
         Delete_Branch_Count: Stat_Set;
      end record;
   
   
   procedure Crunch_Stat (Set: in out Stat_Set) with Inline;
   -- Crunch Last and compute Samples and Average
   
   procedure Crunch_Pack (Pack: in out Stat_Pack) with Inline;
   -- Crunch Pack, calling Crunch_Stat on each components
   
   
   task type Hammer is
      
      entry Current_Nodes (Current, Average, Max: out Natural);
      entry Current_Depth (Current, Average, Max: out Natural);
      
      entry Deleted_Branch_Depth (Last, Average, Max: out Natural);
      
      entry Directives (Append_New_Count,
                        Prepend_New_Count,
                        Insert_Before_Last_Count,
                        Insert_After_Last_Count,
                        Up_Branch_Count,
                        New_Branch_Count,
                        Delete_Last_Item_Count,
                        Delete_Current_Branch_Count:
                          out Natural);
   end Hammer;
   
   type Collective is array (Positive range <>) of Hammer;
   
   
   task type Collector (Group: not null access Collective) is
      
   end Collector;
   
private
   
   type Random_Directive is
     (Append_New, 
      Prepend_New, 
      Insert_Before_Last, -- "Naked new" if no last
      Insert_After_Last,  -- Append to root if no last
      Up_Branch,          -- Ignore if current branch is root
      New_Branch,         -- Down if already has a branch
      Delete_Last_Item,  
      Delete_Current_Branch);
   
   package Random is new Ada.Numerics.Discrete_Random (Random_Directive);
   
end Test_Pack.Recursive_Hammer;
