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
   c : integer := 0;
begin
   for j in 1 .. 2 ** 9 loop
      skiplist.Insert (Element'(j, (j - 2 ** 9) ** 2));
      --  Ada.Text_IO.Put_Line (skiplist.Last_Element.K'Img);
   end loop;
   Ada.Text_IO.Put_Line ("Length " & skiplist.Length'Img);
   for j in -(2 ** 5) .. -1 loop
      skiplist.Insert (Element'(j, (j - 2 ** 9) ** 2));
   end loop;
   Ada.Text_IO.Put_Line ("Length " & skiplist.Length'Img);
   r := skiplist.First;
   Ada.Text_IO.Put_Line ("Done Insert");
   Ada.Text_IO.Put_Line (skiplist.First_Element.K'Img & skiplist.First_Element.V'Img);
   Ada.Text_IO.Flush;
   Ada.Text_IO.Put_Line ("Iterate 1");
   for j in skiplist.Iterate loop
      Ada.Text_IO.Put_Line (SL.Element (j).K'Img & SL.Element (j).V'Img);
   end loop;

   Ada.Text_IO.Put_Line ("Find");

   r := skiplist.Find (Element'(256, 256 ** 2));
   if r /= No_Cursor then
      Ada.Text_IO.Put_Line (SL.Element (r).K'Img);
      Next (r);
   end if;
   Ada.Text_IO.Put_Line (SL.Element (r).K'Img);
   r := skiplist.First;

   while r /= No_Cursor loop
      rr := Next (r);
      exit when SL.Element (r).K = 2000;
      skiplist.Delete (r);
      c := c + 1;
      r := rr;
   end loop;
   Ada.Text_IO.Put_Line ("Done Delete " & c'Img);

   Ada.Text_IO.Put_Line ("Iterate 2");
   for j in skiplist.Iterate loop
      Ada.Text_IO.Put_Line (SL.Element (j).K'Img & SL.Element (j).V'Img);
   end loop;

   Ada.Text_IO.Put_Line ("Length " & skiplist.Length'Img);
end skiplistdriver;
