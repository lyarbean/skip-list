pragma License (GPL);
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Ada.Streams;

package body Skiplist is
   -------------
   --  Types  --
   -------------
   type Node_Array is array (Positive range <>) of Node_Access;
   type Node_Array_Access is access Node_Array;
   type Node_Type is record
      K : Key_Type;
      V : Value_Type;
      Forward : Node_Array_Access;
   end record;

   subtype Level_Type is Integer range 1 .. Level;
   ----------------------
   --  Inner Routines  --
   ----------------------
   package Randomizer is new Ada.Numerics.Discrete_Random (Level_Type);
   g : Randomizer.Generator;
   function Random_Level return Integer with Inline;
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
      new Ada.Unchecked_Deallocation (Node_Array, Node_Array_Access);

   procedure Initialize (o : in out Object) is
   begin
      Randomizer.Reset (g);
      o.Header             := new Node_Type;
      o.Header.V           := Null_Value;
      o.Header.Forward     := new Node_Array (1 .. Level);
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

   -------------------------
   --  External Routines  --
   -------------------------

   -----------------------------------------
   --  Insert a <k, v>, r = true if done  --
   -----------------------------------------
   procedure Insert
      (o : in out Object; k : Key_Type; v : Value_Type; r : out Boolean) is
      x : Node_Access;
      new_level : Integer;
      update : Node_Array (1 .. Level) := (others => null);
   begin
      r := False;
      x := o.Header;
      --  Should never happen, just a check
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

      new_level := Random_Level;
      if new_level > o.Level then
         update (o.Level + 1 .. new_level) := (others => o.Header);
         o.Level := new_level;
      end if;

      --  Insert
      x := new Node_Type;
      x.K := k;
      x.V := v;
      x.Forward := new Node_Array (1 .. new_level);
      for j in 1 .. new_level loop
         x.Forward (j) := update (j).Forward (j);
         update (j).Forward (j) := x;
      end loop;
      r :=  True;
      o.Size := o.Size + 1;
   end Insert;
   ----------------------------------------------------------------
   --  Remove `a' node whose Key is k, if such a node is found,  --
   --  set v to its value, otherwise set to Null_Value            --
   ----------------------------------------------------------------
   procedure Remove (o : in out Object; k : Key_Type; v : out Value_Type) is
      x : Node_Access;
      update : Node_Array (1 .. Level) := (others => null);
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
      v := Null_Value;
      if x.K = k then
         for j in 1 .. o.Level loop
            exit when update (j).Forward (j) /= x;
            update (j).Forward (j) := x.Forward (j);
         end loop;
         while o.Level > 0 and then o.Header.Forward (o.Level) = o.Header loop
            o.Level := o.Level - 1;
         end loop;
         o.Size := o.Size - 1;
         v := x.V;
         Free (x.Forward);
         Free (x);
      end if;
   end Remove;
   --------------------------------------------------------------------------
   --  Replace a node whose Key is k with Value v and set old Value to r,  --
   --  if no such node, set r to Null_Value                                --
   --------------------------------------------------------------------------
   procedure Replace
      (o : in out Object; k : Key_Type; v : Value_Type; r : out Value_Type) is
      x : Node_Access;
   begin
      r := Null_Value;
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
         x.V := v;
         r := v;
      end if;
   end Replace;

   --------------------------------------------------------------------------
   --  Try to replace a node whose Key is k with Value v and set old Value --
   --  to r,  if no such node, insert a new node with <k, v>               --
   --------------------------------------------------------------------------

   procedure Replace_Or_Insert
      (o : in out Object; k : Key_Type; v : Value_Type; r : out Value_Type) is
      x : Node_Access;
      new_level : Integer;
      update : Node_Array (1 .. Level) := (others => null);
   begin
      r := Null_Value;
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
      if x /= null and then x.K = k then
         r := v;
         x.V := v;
         return;
      end if;

      new_level := Random_Level;
      if new_level > o.Level then
         update (o.Level + 1 .. new_level) := (others => o.Header);
         o.Level := new_level;
      end if;

      --  Insert
      x := new Node_Type'(k, v, new Node_Array (1 .. new_level));

      for j in 1 .. new_level loop
         x.Forward (j) := update (j).Forward (j);
         update (j).Forward (j) := x;
      end loop;
      o.Size := o.Size + 1;
      r := v;
   end Replace_Or_Insert;
   ---------------------------------------------------------------
   --  Search a Value with Key k, Null_Value returns not found  --
   ---------------------------------------------------------------
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
      return Null_Value;
   end Search;

   function Size (o : Object) return Integer is
   begin
      return o.Size;
   end Size;

   procedure Put (stream : not null access Ada.Streams.Root_Stream_Type'class;
      item : Object) is
      x : Node_Access;
   begin
      String'Write (stream,  "Size :" & item.Size'Img & ASCII.LF);
      if item.Size /= 0 then
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
      end if;
   end Put;
end Skiplist;
