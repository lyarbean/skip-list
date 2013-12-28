pragma License (GPL);
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Ada.Streams;
with Ada.Text_IO;
package body Skiplist is
   procedure Write
      (stream : not null access Ada.Streams.Root_Stream_Type'Class;
      item : Object);
   for Object'Write use Write;
   type Node_Access_Array is array (Positive range <>) of Node_Access;
   type Nodes_Access is access Node_Access_Array;
   type Node_Type is record
      K : Key_Type;
      V : Value_Type;
      Forward : Nodes_Access;
   end record;

   subtype Level_Type is Integer range 1 .. Level;
   package Randomizer is new Ada.Numerics.Discrete_Random (Level_Type);
   g : Randomizer.Generator;
   function Random_Level return Integer;
   function Random_Level return Integer is
      l : Integer := 1;
   begin
      while Randomizer.Random (g) < Level / 2 loop
         exit when l >= Level;
         l := l + 1;
      end loop;
      return l;
   end Random_Level;

   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Access_Array, Nodes_Access);

   procedure Initialize (o : in out Object) is
   begin
      Randomizer.Reset (g);
      o.Header             := new Node_Type;
      o.Header.V           := No_Value;
      o.Header.Forward     := new Node_Access_Array (1 .. Level);
      o.Header.Forward.all := (others => null);
      o.Level              := 1;
      o.Size               := 0;
   end Initialize;

   procedure Finalize (o : in out Object) is
      x, y : Node_Access;
   begin
      x := o.Header.Forward (1);
      while x /= null loop
         y := x.Forward (1);
         if x.Forward /= null then
            Free (x.Forward);
         end if;
         Free (x);
         x := y;
      end loop;
      Free (o.Header.Forward);
      Free (o.Header);
   end Finalize;

   procedure Insert
      (o : in out Object; k : Key_Type; v : Value_Type; r : out Boolean) is
      x : Node_Access;
      new_level : Integer;
      update : Node_Access_Array (1 .. Level) := (others => null);
   begin
      r := False;
      x := o.Header;
      if x = null then
         raise Program_Error;
      end if;

      for j in reverse 1 .. o.Level loop
         loop
            exit when x.Forward (j) = null;
            exit when not (x.Forward (j).K < k);
            x := x.Forward (j);
         end loop;
         update (j) := x;
      end loop;

      x := x.Forward (1);
      if x /= null and then x.K = k then
         r := True;
         return;
      end if;
      --  fixme update x.v := v
      new_level := Random_Level;
      if new_level > o.Level then
         update (o.Level + 1 .. new_level) := (others => o.Header);
         o.Level := new_level;
      end if;

      --  Insert
      x := new Node_Type;
      x.K := k;
      x.V := v;
      x.Forward := new Node_Access_Array (1 .. new_level);
      for j in 1 .. new_level loop
         x.Forward (j) := update (j).Forward (j);
         update (j).Forward (j) := x;
      end loop;
      r :=  True;
      o.Size := o.Size + 1;
   end Insert;

   procedure Remove (o : in out Object; k : Key_Type; r : out Boolean) is
      x : Node_Access;
      update : Node_Access_Array (1 .. Level) := (others => null);
   begin
      x := o.Header;
      for j in reverse 1 .. o.Level loop
         loop
            exit when x.Forward (j) = null;
            exit when not (x.Forward (j).K < k);
            x := x.Forward (j);
         end loop;
         update (j) := x;
      end loop;
      x := x.Forward (1);
      r := False;
      if x.K = k then
         for j in 1 .. o.Level loop
            exit when update (j).Forward (j) /= x;
            update (j).Forward (j) := x.Forward (j);
         end loop;
         Free (x.Forward);
         Free (x);
         while o.Level > 0 and then o.Header.Forward (o.Level) = o.Header loop
            o.Level := o.Level - 1;
         end loop;
         r := True;
         o.Size := o.Size - 1;
      end if;
   end Remove;

   function Search (o : Object; k : Key_Type) return Value_Type is
      x : Node_Access;
   begin
      x := o.Header;
      for j in reverse 1 .. o.Level loop
         loop
            exit when x.Forward (j) = null;
            exit when not (x.Forward (j).K < k);
            x := x.Forward (j);
         end loop;
      end loop;
      x := x.Forward (1);
      if x.K = k then
         return x.V;
      end if;
      return No_Value;
   end Search;

   function Size (o : Object) return Integer is
   begin
      return o.Size;
   end Size;

   procedure Write
      (stream : not null access Ada.Streams.Root_Stream_Type'class;
      item : Object) is
      x : Node_Access;
   begin
      for j in reverse 1 .. item.Level loop
         x := item.Header.Forward (j);
         String'Write (stream, ASCII.LF & "-- level " & j'Img & ASCII.LF);
         loop
            exit when x = null;
            String'Write (stream, "[");
            Key_Type'Write (stream, x.K);
            String'Write (stream, " :");
            Value_Type'Write (stream, x.V);
            String'Write (stream, " ] ");
            x := x.Forward (j);
         end loop;
      end loop;
   end Write;
end Skiplist;
