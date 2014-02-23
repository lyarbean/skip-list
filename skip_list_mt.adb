pragma Ada_2012;
with Skip_List;
with Ada.Text_IO;
with System.Multiprocessors;
procedure Skip_List_MT is

   type E_Type is
      record
        K : Integer;
      end record;

   function Image (E : E_Type) return String;
   function Image (E : E_Type) return String is
   begin
      if E.K < 0 then
         return Integer'Image (abs (E.K)) & "_";
      end if;
      return Integer'Image (E.K);
   end Image;

   function Compare (Left, Right : E_Type) return Integer;
   function Compare (Left, Right : E_Type) return Integer is
   begin
      return Left.K - Right.K;
   end Compare;

   package SL is new Skip_List (E_Type);
   use SL;

   --  23 : ~ 1 GB !!
   L : constant := 20;

   skiplist : List (L + 2);

   task type Worker (Which : System.Multiprocessors.CPU_Range)
      with CPU => Which is
      entry Start (Sep : Integer);
   end Worker;

   task body Worker is
      S : Integer;
   begin
      select
         accept Start (Sep : Integer) do
            S := Sep;
         end Start;

         for j in 1 .. 2 ** L loop
            if j mod S = 0 then
               skiplist.Insert (E_Type'(K => j));
            end if;
         end loop;
      or
         terminate;
      end select;
   end Worker;
   Iterate_Check : Boolean := False;
   Verbose : Boolean := False;
   R, RR : Cursor;
   C : Integer := 0;
   pragma Atomic (C);
   P : array (1 .. 16) of Integer
      := (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53);
   W1 : Worker (1);
   W2 : Worker (2);
   W3 : Worker (3);
   W4 : Worker (4);
   W5 : Worker (5);
   W6 : Worker (6);
   W7 : Worker (7);
   W8 : Worker (8);
   W9 : Worker (1);
   W10 : Worker (2);
   W11 : Worker (3);
   W12 : Worker (4);
   W13 : Worker (5);
   W14 : Worker (6);
   W15 : Worker (7);
   W16 : Worker (8);
begin

   W1.Start (P (1));
   W2.Start (P (2));
   W3.Start (P (3));
   W4.Start (P (4));
   W5.Start (P (5));
   W6.Start (P (6));
   W7.Start (P (7));
   W8.Start (P (8));
   W9.Start (P (9));
   W10.Start (P (10));
   W11.Start (P (11));
   W12.Start (P (12));
   W13.Start (P (13));
   W14.Start (P (14));
   W15.Start (P (15));
   W16.Start (P (16));

   loop
      exit when W1'Terminated;
   end loop;
   if Iterate_Check then
      R := skiplist.First;
      for j in skiplist.Iterate loop
         if Verbose then
            Ada.Text_IO.Put_Line (Element (j).K'Img);
         end if;
      end loop;

      R := skiplist.Find (E_Type'(K => 256));
      if R /= No_Cursor then
         Next (R);
      end if;

      R := skiplist.First;

      while R /= No_Cursor loop
         RR := Next (R);
         exit when Element (R).K > 100;
         skiplist.Delete (R);
         C := C + 1;
         R := RR;
      end loop;

      for j in skiplist.Iterate loop
         if Verbose then
            Ada.Text_IO.Put_Line (Element (j).K'Img);
         end if;
      end loop;
   end if;
   loop
      exit when W2'Terminated;
   end loop;
   loop
      exit when W3'Terminated;
   end loop;
   loop
      exit when W4'Terminated;
   end loop;
   loop
      exit when W5'Terminated;
   end loop;
   loop
      exit when W6'Terminated;
   end loop;
   loop
      exit when W7'Terminated;
   end loop;
   loop
      exit when W8'Terminated;
   end loop;
   loop
      exit when W9'Terminated;
   end loop;
   loop
      exit when W10'Terminated;
   end loop;
   loop
      exit when W11'Terminated;
   end loop;
   loop
      exit when W12'Terminated;
   end loop;
   loop
      exit when W13'Terminated;
   end loop;
   loop
      exit when W14'Terminated;
   end loop;
   loop
      exit when W15'Terminated;
   end loop;
   loop
      exit when W16'Terminated;
   end loop;
   skiplist.Vet (Image'Access);

   --  Dot may not be able to handle so many nodes
   if L < 12 then
      skiplist.Draw (Image'Access);
   end if;
end Skip_List_MT;
