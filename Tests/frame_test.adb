with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;
with Curses.Frames;

with Curses.Terminals.Surfaces;
with Curses.Device.Environment;

procedure Frame_Test is
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
begin
   TTY.Attach;

   declare
      Main_Screen: aliased Screen (TTY'Access);
      My_Window  : aliased Window 
        := New_Window (On_Screen        => Main_Screen'Access,
                       Proposed_Extents => (Row => 10, Column => 60));
      
      Input_Char: Control_Character;
      -- This will be centered on the screen, of size 3x40
      
      Fill_Cursor: Cursor := (Style    => (Inverted => True, others => <>),
                              others   => <>);
   begin
      
      --      My_Window.Set_Background (Fill_Cursor => Fill_Cursor);
      
      My_Window.Set_Border;
      My_Window.Position_Cursor ( (Row    => 1,
                                   Column => (My_Window.Extents.Column / 2)) );
      
      My_Window.Put (Content        => "Hello, world!",
                     Justify        => Center,
                     Advance_Cursor => True);
      
      My_Window.Show;
      -- New windows are hidden by default
      
      Input_Char := My_Window.Input_Key;
      -- Wait for key press
      
      My_Window.Position_Cursor ( (Row    => 2,
                                   Column => 2));
      
      declare
         use Curses.Frames;
         
         My_Frame: Frame := New_Frame (Target => My_Window'Access, 
                                       Margin => 3);
      begin
         My_Frame.Assert_Cursor;
         My_Frame.Position_Cursor ((Row => 2, Column => 2));
         My_Frame.Put (Content => "Inside the frame", Advance_Cursor => True);
         Input_Char := My_Window.Input_Key;
         
         My_Frame.Set_Border;
         Input_Char := My_Window.Input_Key;
      end;
      
      -- That's it, the Curses package will automatically shut everything down
      -- for you!
   end; 
end Frame_Test;
