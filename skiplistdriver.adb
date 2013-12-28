with Skiplist;
with Ada.Streams.Stream_IO;
with Ada.Text_IO.Text_Streams;
use Ada.Text_IO;
procedure skiplistdriver is
   std_out : access Ada.Streams.Root_Stream_Type
   := Text_Streams.Stream (Standard_Output);
   type Key_T is new Integer;
   procedure Write_Int
      (stream : not null access Ada.Streams.Root_Stream_Type'Class;
      item : Key_T);
   for Key_T'Write use Write_Int;
   procedure Write_Int
      (stream : not null access Ada.Streams.Root_Stream_Type'Class;
      item : Key_T) is
   begin
      String'Write (stream, item'Img);
   end Write_Int;

   package IC_Skiplist is new Skiplist (
   Key_Type => Key_T,
   Value_Type => Key_T,
   No_Value => -1,
   Level => 9);
   the_skiplist : IC_Skiplist.Object;
   r : Boolean;
   v : Key_T;
begin
   for j in Key_T range 1 .. 2 ** 10 loop
      the_skiplist.Insert (j, (j - 2 ** 9) ** 2, r);
   end loop;
   IC_Skiplist.Put (std_out, the_skiplist);
   New_Line;
   Ada.Text_IO.Put_Line ("-----------------");
   for j in Key_T range 2 ** 6 .. 2 ** 9 loop
      the_skiplist.Remove (j, v);
      Ada.Text_IO.Put (v'Img);
   end loop;
   New_Line;
   Ada.Text_IO.Put_Line ("-----------------");
   IC_Skiplist.Put (std_out, the_skiplist);
   New_Line;
end skiplistdriver;
