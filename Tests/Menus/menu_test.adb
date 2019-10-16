with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;
with Curses.Frames;    use Curses.Frames;
with Curses.Frames.Framed_Windows; use Curses.Frames.Framed_Windows;

with Curses.Terminals.Surfaces; use Curses.Terminals.Surfaces;
with Curses.Device.Environment;

with Curses.UI.Menus; use Curses.UI.Menus;
with Curses.UI.Menus.Renderer;
with Curses.UI.Menus.Standard_Trees; use Curses.UI.Menus.Standard_Trees;
with Curses.UI.Menus.Standard_Trees.Bounded;

with Ada.Text_IO;
with Debug; use Debug;

procedure Menu_Test is
   
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   
   
   type Menu_Item is limited new Curses.UI.Menus.Menu_Item_Interface with
      record
         Label : String (1 .. 20) := (others => ' ');
         Hotkey: Control_Character := (Class => No_Key, others => <>);
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
   procedure Render_Label (Item    : in out Menu_Item;
                           Canvas  : in out Surface'Class;
                           Selected: in     Boolean)
   is
      Use_Cursor: Cursor;
   begin
      Canvas.Clear;
      
      if Selected then
         Use_Cursor.Style.Inverted := True;
         Canvas.Position_Cursor ((1,1));
      end if;
      
      Canvas.Fill (Pattern => " ",
                   Fill_Cursor => Use_Cursor);

      
      Canvas.Put (Set_Cursor => Use_Cursor,
                  Content    => Item.Label);
   end Render_Label;
   
   overriding
   function Hot_Key (Item: Menu_Item) 
                    return Curses.Terminals.Surfaces.Control_Character
     is (Item.Hotkey);
   
   overriding
   procedure Execute (Item     : in out Menu_Item;
                      Directive:    out After_Execute_Directive)
   is
   begin
      Directive := Close_Tree;
   end Execute;
   
   
   package Menu_Trees is new Curses.UI.Menus.Standard_Trees.Bounded
     (Menu_Item);
   
   Tree: Menu_Trees.Bounded_Menu_Tree (25);
   

   
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   
   
   
begin
   
   TTY.Attach;
   
   declare
      Staging: aliased Menu_Type'Class := Tree.Staging_Branch;
      
      S: aliased Screen (TTY'Access);


   begin
      S.Put (Content => "Initializing Menu...",
             Advance_Cursor => True);
      
      for I in 1 .. 20 loop
         declare
            Cursor: Standard_Cursor'Class := Tree.Create;
         begin
            declare
               New_Item: Menu_Item renames
                 Menu_Item (Staging(Cursor).Ref.all);
            begin
               New_Item.Label (1 .. 4) := "Item";
               Int_IO.Put (To   => New_Item.Label (5 .. New_Item.Label'Last),
                           Item => I);
               
               New_Item.Hotkey 
                 := (Class => Graphic, Alt => False, Key => Character'Val (I + 96));
            end;
         end;
      end loop;
      
      S.Put (Content => "OK",
             Advance_Cursor => True);
      
      declare

         
--         FW: Framed_Window := New_Window (S, (1,1), (10,60));
--         FW: Framed_Window := New_Window (S, (1,1), (10,60));

         FW: Framed_Window := New_Framed_Window 
           (On_Screen => S,
            Top_Padding => 1,
            Bottom_Padding => 1,
            Left_Padding => 2,
              Right_Padding => 2,
            Frame_Extents => (Row => 10, Column => 60));
         
         F: aliased Frame'Class := FW.Get_Frame;
         

         
         M_Style: Cursor := (Style  => (Inverted => True, others => False),
                             others => <>);
         B_Style: Cursor := (others => <>);
      begin
         FW.Set_Border;
         FW.Show;
         
         declare
            R: Curses.UI.Menus.Renderer.Menu_Renderer
              (Canvas        => F'Access,
               Input_Surface => FW'Access,
               Branch        => Staging'Access);
            
            Itr: Menu_Iterators.Reversible_Iterator'Class := Staging.Iterate;
            S_Item: Menu_Cursor_Type'Class := Itr.First;
            Key: Control_Character;
            HK_Sel: Boolean;
         begin
            R.Enable_Wrap_Around;
            R.Enable_Scrollbar (Marker_Style     => M_Style,
                                Marker_Character => ' ',
                                Bar_Style        => B_Style,
                                Bar_Character    => ' ');
            
            
            loop
               R.Interaction_Loop (Selected_Item => S_Item,
                                   Last_Key => Key,
                                   Hotkey_Select => HK_Sel);
               
               if Key.Class in graphic and then Key.Key = 'M' then
                  FW.Move ((1,1));
               end if;
               
               exit when not HK_Sel
                 and then Key.Class = Graphic
                 and then Key.Key in 'q' | 'Q';
            end loop;
         end;
      end;
      
      
   end;
   
end Menu_Test;
