
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Ada.Exceptions; use Ada;

with Curses;                    use Curses;
with Curses.Terminal;           use Curses.Terminal;
with Curses.Terminal.Color;     use Curses.Terminal.Color;
with Curses.Device.Environment; use Curses.Device.Environment;
with Curses.Terminal.Surfaces;  use Curses.Terminal.Surfaces;
with Curses.Standard;           use Curses.Standard;

procedure Stress_Test is
   
   Blue_Fill: Color_Style := Predefine (Foreground => Blue_Color,
                                        Background => White_Color);
   
   Term: aliased TTY;
   
   task type Clock_Window (My_Screen: not null access Screen) is
      entry Open (Position, Size: Cursor_Position);
      entry Close;
   end Clock_Window;
   
   task body Clock_Window is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      
      TL, BR: Cursor_Position;
      
   begin
      
      loop
         
         select
            accept Open (Position, Size: Cursor_Position) do
               TL := Position;
               BR := Size;
            end Open;
         or
            terminate;
         end select;
         
         declare
            W: Window := New_Window (On_Screen        => My_Screen,
                                     Proposed_Extents => BR,
                                     Top_Left         => TL);
         begin
            
            W.Show;
            
            loop
               W.Put (Image (Clock));
               
               select
                  accept Close;
                  exit;
               or
                  delay 1.0;
               end select;
               

               
            end loop;
         end;
         
      end loop;
      
   exception
      when e: others =>
         Text_IO.Put_Line ("T: " & Exceptions.Exception_Information (e));
      
   end Clock_Window;
   

   
begin
   
   Term.Attach (Line_Driver => Environment_Terminal);
   
   declare
      S: aliased Screen (Term'Access);
   
      Next_Window:          Cursor_Position := (Row => 1, Column => 1);
      Window_Size: constant Cursor_Position := (Row => 4, Column => 20);
      
      Task_Group: array (1..80) of Clock_Window (S'Access);
      
      Start_Column: Cursor_Ordinal := 1;
   begin
      
      for T of Task_Group loop
         T.Open (Position => Next_Window, Size => Window_Size);
         delay 0.1;
         
         if Next_Window + Window_Size < S.Extents then
            Next_Window := Next_Window + (1,1);
            
         else
            Start_Column       := Start_Column + 1;
            Next_Window.Row    := 1;
            Next_Window.Column := Start_Column;
            
         end if;
         

      end loop;
      
      delay 30.0;
      
      for T of Task_Group loop
         delay 0.1;
         T.Close;
      end loop;
   end;
end Stress_Test;
