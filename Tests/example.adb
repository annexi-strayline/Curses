with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;

with Curses.Terminals.Surfaces;
with Curses.Device.Environment;

procedure Example is
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
   
   use all type Curses.Terminals.Surfaces.Control_Class;
begin
   TTY.Attach;

   declare
      Main_Screen: aliased Screen (TTY'Access);
      My_Window  : Window'Class 
        := Main_Screen.New_Window 
          (Proposed_Extents => (Row => 4, Column => 40));
      
      Input_Char: Control_Character;
      -- This will be centered on the screen, of size 3x40
      
      Fill_Cursor: Cursor := (Style    => (Inverted => True, others => <>),
                              others   => <>);
   begin
      
      My_Window.Set_Background (Fill_Cursor => Fill_Cursor);
      
      My_Window.Position_Cursor ( (Row    => 2,
                                   Column => (My_Window.Extents.Column / 2)) );
      
      My_Window.Put (Content        => "Type 'x' to exit.",
                     Justify        => Center,
                     Advance_Cursor => True);
      
      
      My_Window.Position_Cursor ( (Row => 3, Column => 2) );
      My_Window.Put (Content => ">",
                     Advance_Cursor => True);
      
      My_Window.Show;
      -- New windows are hidden by default
      
      loop
         Input_Char := My_Window.Input_Key (Poll_Period => 0.01);
         exit when Input_Char.Class = Graphic
           and then Input_Char.Key = 'x';
         
         if Input_Char.Class = Graphic 
           and then My_Window.Current_Cursor.Position < My_Window.Extents 
         then
            My_Window.Put (Content => String'(1..1 => Input_Char.Key),
                           Advance_Cursor => True);
         end if;
      end loop;
      
      -- That's it, the Curses package will automatically shut everything down
      -- for you!
   end; 
end Example;
    
