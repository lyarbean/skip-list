pragma License (GPL);
pragma Ada_2012;
with Ada.Iterator_Interfaces;
private with Ada.Finalization;
private with Atomic_Value;
generic
   type Element_Type is private;
   with function Compare (Left, Right : Element_Type) return Integer is <>;

package Skip_List is
   subtype Level_type is Integer range 1 .. 63;
   type List (Max_Level : Level_type := 1) is tagged limited private with
      Constant_Indexing => Constant_Reference,
      --  Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Element_Type;

   type Cursor is private;
   pragma Preelaborable_Initialization (Cursor);

   Empty_List : constant List;

   No_Cursor : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;
   function Is_Valid (Position : Cursor) return Boolean;

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

   procedure Insert (Container : in out List; New_Item : Element_Type);

   procedure Insert
      (Container : in out List;
       New_Item  : Element_Type;
       Position  : out Cursor);

   procedure Delete (Container : in out List; Position : in out Cursor);

   procedure Delete_Last (Container : in out List; Count : Positive := 1);

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'class;

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'class;

   --  TODO Splice

   function First (Container : List) return Cursor;

   function First_Element (Container : List) return Element_Type;

   function Last (Container : List) return Cursor;

   function Last_Element (Container : List) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Previous (Position : Cursor) return Cursor;

   procedure Previous (Position : in out Cursor);

   function Find (Container : List; Item : Element_Type) return Cursor;

   function Contains (Container : List; Item : Element_Type) return Boolean;

   --  procedure Iterate
   --     (Container : List;
   --     Process : not null access procedure (Position : Cursor));

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
      with Implicit_Dereference => Element;

   --  type Reference_Type
   --     (Element : not null access Element_Type) is private
   --     with Implicit_Dereference => Element;

   function Constant_Reference
      (Container : aliased List; Position : Cursor)
      return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   --  function Reference (Container : aliased in out List; Position : Cursor)
   --     return Reference_Type;
   --  pragma Inline (Reference);

   procedure Vet (Container : List;
      To_String : access function (E : Element_Type) return String);
   procedure Draw (Container : List;
      To_String : access function (E : Element_Type) return String);
private
   pragma Inline (First);
   pragma Inline (Last);
   pragma Inline (Next);
   pragma Inline (Previous);

   use Ada.Finalization;
   use Atomic_Value;
   type Node_Type;
   type Node_Access is access Node_Type;

   type Node_Array is array (Natural range <>) of Node_Access;
   pragma Atomic_Components (Node_Array);

   type Node_Array_Access is access all Node_Array;

   type Node_Type is record
      Visited : aliased B4;
      Forward : Node_Array_Access;
      Element : aliased Element_Type;
   end record;

   type List (Max_Level : Level_type := 1) is new Limited_Controlled with
      record
         Skip          : Node_Array_Access;
         Length        : Natural := 0;
         pragma Atomic (Length);
         Current_Level : Natural := 0;
         pragma Atomic (Current_Level);
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

   type Iterator is new Limited_Controlled
      and List_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : List_Access;
         Node      : Node_Access;
      end record;

   overriding procedure Finalize (Object : in out Iterator);
   overriding function First (Object : Iterator) return Cursor;
   overriding function Last (Object : Iterator) return Cursor;
   overriding function Next
      (Object : Iterator; Position : Cursor) return Cursor;
   overriding function Previous
      (Object : Iterator; Position : Cursor) return Cursor;

   Empty_List : constant List :=
               (Limited_Controlled with 1, null, 0, 0);
   No_Cursor  : constant Cursor := Cursor'(null, null);

   -----------------
   --  Reference  --
   -----------------

   type Reference_Control_Type is new Controlled with
      record
          --  Container : List_Access;
          Node : Node_Access;
      end record;
   overriding procedure Adjust (Control : in out Reference_Control_Type);
   pragma Inline (Adjust);

   overriding procedure Finalize (Control : in out Reference_Control_Type);
   pragma Inline (Finalize);

   type Constant_Reference_Type
      (Element : not null access constant Element_Type) is
      record
         Control : Reference_Control_Type;
      end record;

   --  type Reference_Type
   --     (Element : not null access Element_Type) is
   --     record
   --        Control : Reference_Control_Type;
   --     end record;
end Skip_List;
