pragma License (GPL);
pragma Ada_2012;
with Ada.Streams;
with Ada.Finalization;
generic
   type Key_Type is private;
   type Value_Type is private;
   No_Value : Value_Type;
   Level : Positive;
   with function "=" (l, r : Key_Type) return Boolean is <>;
   with function ">" (l, r : Key_Type) return Boolean is <>;
   with function "<" (l, r : Key_Type) return Boolean is <>;
package Skiplist is
   pragma Elaborate_Body;
   type Object is new Ada.Finalization.Limited_Controlled with private;
   procedure Insert
      (o : in out Object; k : Key_Type; v : Value_Type; r : out Boolean);
   procedure Remove (o : in out Object; k : Key_Type; v : out Value_Type);
   function Search (o : Object; k : Key_Type) return Value_Type;
   function Size (o : Object) return Integer;
   procedure Put (stream : not null access Ada.Streams.Root_Stream_Type'Class;
      item : Object);
private
   type Node_Type;
   type Node_Access is access Node_Type;
   type Object is new Ada.Finalization.Limited_Controlled with record
      Header : Node_Access;
      Size   : Integer;
      Level  : Integer;
   end record;
   overriding procedure Initialize (o : in out Object);
   overriding procedure Finalize   (o : in out Object);
end Skiplist;
