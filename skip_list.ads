pragma License (GPL);
pragma Ada_2012;
with Ada.Iterator_Interfaces;
private with Ada.Streams;
private with Ada.Finalization;

generic
   type Element_Type is private;
   with function Compare (Left, Right : Element_Type) return Integer is <>;

package Skip_List is
   type List (Level : Positive) is tagged limited private with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_List : constant List;

   No_Cursor : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package List_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   function "=" (Left, Right : List) return Boolean;

   function Length (Container : List) return Natural;

   function Is_Empty (Container : List) return Boolean;

   procedure Clear (Container : in out List);

   function Element (Position : Cursor) return Element_Type;

   procedure Replace_Element
     (Container : in out List;
      Position  : Cursor;
      New_Item  : Element_Type) is null;

   procedure Query_Element
     (Position : Cursor;
      Process  : not null access procedure (Element : Element_Type)) is null;

   procedure Update_Element
     (Container : in out List;
      Position  : Cursor;
      Process   : not null access procedure
                  (Element : in out Element_Type)) is null;

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference => Element;

   type Reference_Type
     (Element : not null access Element_Type) is private
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased List;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out List;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   procedure Insert (Container : in out List; New_Item : Element_Type);

   procedure Insert
      (Container : in out List;
       New_Item  : Element_Type;
       Position  : out Cursor);

   procedure Delete (Container : in out List; Position : in out Cursor);

   --  procedure Delete_Last (Container : in out List; Count_Type := 1);

   --  Currently only Forward_Iterator considered
   function Iterate (Container : List)
      return List_Iterator_Interfaces.Forward_Iterator'class;
      --  return List_Iterator_Interfaces.Reversible_Iterator'class;

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Forward_Iterator'class;

   --  TODO Splice

   function First (Container : List) return Cursor;

   --  function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   --  function Last_Element (Container : List) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Find (Container : List; Item : Element_Type) return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   --  procedure Iterate
   --     (Container : List;
   --     Process : not null access procedure (Position : Cursor));

private
   pragma Inline (Next);
   --  pragma Inline (Previous);

   use Ada.Streams;
   use Ada.Finalization;
   type Node_Type;
   type Node_Access is access Node_Type;
   type Node_Array is array (Positive range <>) of Node_Access;
   type Node_Array_Access is access Node_Array;
   type Node_Type is record
      --  Backward
      Forward : Node_Array_Access;
      Element : aliased Element_Type;
   end record;

   --  Non-copyable
   type List (Level : Positive) is new Limited_Controlled with
      record
         Header : Node_Access;
         Length : Natural;
         Current_Level   : Natural;
         pragma Atomic (Current_Level);
         Busy   : Natural;
         pragma Atomic (Busy);
         Lock   : Natural;
         pragma Atomic (Lock);
      end record;

   overriding procedure Initialize (Container : in out List);
   overriding procedure Finalize   (Container : in out List);

   type List_Access is access all List;
   for List_Access'Storage_Size use 0;

   type Cursor is
      record
         Container : List_Access;
         Node      : Node_Access;
      end record;

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is null record;
   type Reference_Type
      (Element : not null access Element_Type) is null record;

   Empty_List : constant List := (Limited_Controlled with 1, null, 0, 0, 0, 0);
   No_Cursor  : constant Cursor := Cursor'(null, null);

   type Iterator is new Limited_Controlled
      and List_Iterator_Interfaces.Forward_Iterator with
      record
         Container : List_Access;
         Node      : Node_Access;
      end record;

   --  overriding procedure Finalize (Object : in out Iterator);
   overriding function First (Object : Iterator) return Cursor;
   overriding function Next
      (Object : Iterator; Position : Cursor) return Cursor;

end Skip_List;
