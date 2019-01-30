with Ada.Text_IO; use Ada.Text_IO;

package body Debug is
   
   Log_File: File_Type;
   
   procedure Debug_Line (Message: in String) is
   begin
      Put_Line (Log_File, Message);
      Flush (Log_File);
   end Debug_Line;
   
begin
   Create (File => Log_File, Mode => Out_File, Name => "debug.log");
end Debug;
