Pragma Ada_2012;
with Skip_List;
with Ada.Text_IO;
procedure skiplistdriver is

   type Element is
      record
        K, V : Integer;
      end record;

   function Compare (Left, Right : Element) return Integer;
   function Compare (Left, Right : Element) return Integer is
   begin
      return Left.K - Right.K;
   end Compare;

   package SL is new Skip_List (Element);
   use SL;
   skiplist : List (9);

   r : Cursor;
begin
   for j in 1 .. 2 ** 10 loop
      skiplist.Insert (Element'(j, (j - 2 ** 9) ** 2));
   end loop;
   Ada.Text_IO.Put_Line ("Done");
   Ada.Text_IO.Flush;
end skiplistdriver;
