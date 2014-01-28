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
      --  Ada.Text_IO.Put_Line (skiplist.Last_Element.K'Img);
   end loop;
   Ada.Text_IO.Put_Line ("Done Insert");
   --  for c : Constant_Reference of skiplist loop
   --     exit when c.K = 1000;
   --     Ada.Text_IO.Put_Line (c.K'Img & c.V'Img);
   --  end loop;
   Ada.Text_IO.Put_Line ("Find");

   r := skiplist.Find (Element'(256, 256 ** 2));
   if r /= No_Cursor then
      Ada.Text_IO.Put_Line (SL.Element(r).K'Img);
      Next (r);
   end if;
   Ada.Text_IO.Put_Line (SL.Element(r).K'Img);
   r := skiplist.First;

   while r /= No_Cursor loop
      rr := Next (r);
      exit when SL.Element (rr).K = 1000;
      skiplist.Delete (r);
      r := rr;
   end loop;
   Ada.Text_IO.Put_Line ("Done Delete");

   --  for c : Constant_Reference of skiplist loop
   --     Ada.Text_IO.Put_Line (c.K'Img & c.V'Img);
   --  end loop;

   Ada.Text_IO.Put_Line ("Iterate");
   for j in skiplist.Iterate loop
      Ada.Text_IO.Put_Line (SL.Element(j).K'Img & SL.Element(j).V'Img);
   end loop;
end skiplistdriver;
