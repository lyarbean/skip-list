pragma Ada_2012;
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

   r, rr : Cursor;

begin
   for j in 1 .. 2 ** 10 loop
      skiplist.Insert (Element'(j, (j - 2 ** 9) ** 2));
   end loop;
   Ada.Text_IO.Put_Line ("Done Insert");
   for c of skiplist loop
      exit when c.K = 1000;
      Ada.Text_IO.Put_Line (c.K'Img & c.V'Img);
   end loop;

   r := skiplist.First;
   Ada.Text_IO.Put_Line ("Done Put");

   while r /= No_Cursor loop
      rr := Next (r);
      skiplist.Delete (r);
      r := rr;
   end loop;
   Ada.Text_IO.Put_Line ("Done Delete");

   for c of skiplist loop
      Ada.Text_IO.Put_Line (c.K'Img & c.V'Img);
   end loop;

   Ada.Text_IO.Put_Line ("Should no k v");
end skiplistdriver;
