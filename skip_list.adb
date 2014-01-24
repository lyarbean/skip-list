pragma License (GPL);
pragma Ada_2012;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
package body Skip_List is
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Array, Node_Array_Access);

   package NFR renames Ada.Numerics.Float_Random;
   G : NFR.Generator;

   procedure Initialize (Container : in out List) is
   begin
      NFR.Reset (G, Container.Level);
      Container.Header        := null;
      Container.Current_Level := 0;
      Container.Length        := 0;
      Container.Busy          := 0;
   end Initialize;

   procedure Finalize (Container : in out List) is
   begin
      Container.Clear;
   end Finalize;

   ----------------
   --  External  --
   ----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         return False;
      end if;
      return True;
   end Has_Element;

   function "=" (Left, Right : List) return Boolean is
   begin
      return Left.Header = Right.Header;
   end "=";

   function Length (Container : List) return Natural is
   begin
      return Container.Length;
   end Length;

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Length = 0;
   end Is_Empty;

   procedure Clear (Container : in out List) is
      X, Y : Node_Access := Container.Header;
   begin
      if Container.Length = 0 then
         pragma Assert (Container.Header = null);
         pragma Assert (Container.Tail = null);
         pragma Assert (Container.Busy = 0);
         return;
      end if;
      if Container.Busy > 0 then
         raise Program_Error with "Attemp to clear while list is busy";
      end if;
      while X /= null loop
         Y := X.Forward (1);
         pragma Assert (System.Atomic_Counters.Is_One (X.Lock));
         Free (X.Forward);
         Free (X);
         X := Y;
      end loop;
      Initialize (Container);
   end Clear;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = null then
         raise Program_Error with "Bad Cursor on Element";
      end if;
      return Position.Node.Element;
   end Element;

   --  Replace
   --  Query
   --  Update

   -----------------
   --  Reference  --
   -----------------

   --  FIXME References are allocated on secondary stack, but not got freed
   --        It seems like a bug in GNAT

   function Constant_Reference
     (Container : aliased List;
      Position  : Cursor) return Constant_Reference_Type is
   begin
      if Position.Container /= Container'Unrestricted_Access
         or Position.Node = null then
         raise Program_Error with "Bad Cursor on Constant_Reference";
      end if;
      return R : Constant_Reference_Type :=
         (Element => Position.Node.Element'Access,
         Control => (Controlled with Position.Node)) do
            System.Atomic_Counters.Increment (Position.Node.Lock);
         end return;
   end Constant_Reference;

   function Reference
     (Container : aliased in out List;
      Position  : Cursor) return Reference_Type is
   begin
      if Position.Container /= Container'Unrestricted_Access
         or Position.Node = null then
         raise Program_Error with "Bad Cursor on Reference";
      end if;
      return R : Reference_Type :=
         (Element => Position.Node.Element'Access,
         Control => (Controlled with position.Node)) do
            System.Atomic_Counters.Increment (Position.Node.Lock);
         end return;
   end Reference;

   procedure Insert (Container : in out List; New_Item : Element_Type) is
      Pos : Cursor;
      pragma Unreferenced (Pos);
   begin
      Insert (Container, New_Item, Pos);
   end Insert;

   procedure Insert
      (Container : in out List; New_Item : Element_Type; Position : out Cursor)
   is
   begin
      if Container.Header = null then
         if not Container.Is_Empty then
            raise Program_Error
            with "Container.Length /= 0" & Container.Length'Img;
         end if;
         Container.Header := new Node_Type'(null, New_Item, Lock => <>);
         Container.Header.Forward := new Node_Array (0 .. Container.Level);
         Container.Header.Forward.all := (others => null);
         Container.Tail := Container.Header;
         Container.Length := 1;
         Container.Current_Level := 1;
         Position := Container.First;
         return;
      end if;

      declare
         X, Y : Node_Access := Container.Header;
         New_Level : Integer := 1;
         Update : Node_Array (1 .. Container.Level) := (others => null);
      begin
         Random_Level :
         while NFR.Random (G) < 0.5 loop
            exit Random_Level when New_Level >= Container.Level;
            New_Level := New_Level + 1;
         end loop Random_Level;
         --  TODO Atomic access and lock nodes
         for j in reverse 1 .. Container.Current_Level loop
            loop
               exit when X.Forward (j) = null;
               exit when Compare (X.Forward (j).Element, New_Item) > 0;
               X := X.Forward (j);
            end loop;
            Update (j) := X;
         end loop;

         --  Already presented
         if X.Forward (1) /= null and then
            X.Forward (1).Element = New_Item then
            Position := (Container'Unchecked_Access, X.Forward (1));
            return;
         end if;

         if New_Level > Container.Current_Level then
            Container.Current_Level := New_Level;
         end if;

         Y := new Node_Type'(null, New_Item, Lock => <>);
         Y.Forward := new Node_Array (0 .. New_Level);
         for j in 1 .. New_Level loop
            exit when Update (j) = null;
            Y.Forward (j) := Update (j).Forward (j);
            Update (j).Forward (j) := Y;
         end loop;
         Y.Forward (0) := X;
         X.Forward (1) := Y;
         if Y.Forward (1) = null then
            Container.Tail := Y;
         end if;
         Container.Length := Container.Length + 1;
         Position := (Container'Unchecked_Access, Y);
      end;
   end Insert;

   --  Helper
   procedure Remove (Container : in out List; Item : Element_Type);
   --  TODO  Reset Container.Tail
   procedure Remove (Container : in out List; Item : Element_Type) is
      X : Node_Access;
      Update : Node_Array (1 .. Container.Level) := (others => null);
   begin
      X := Container.Header;
      if Container.Header.Element = Item then
         Container.Header := X.Forward (1);
         --  FIXME ???
         --  Swap Forwards
         if Container.Header /= null then
            X.Forward (X.Forward (1).Forward'Range) :=
               Container.Header.Forward.all;
            Free (Container.Header.Forward);
            Container.Header.Forward := X.Forward;
         else
            Free (X.Forward);
         end if;
         Free (X);
         Container.Length := Container.Length - 1;
         return;
      end if;

      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, Item) > 0;
            X := X.Forward (j);
         end loop;
         Update (j) := X;
      end loop;
      --  TODO and FIXME
      --  A node with Lock > 1 means there are visitor, don't remove it,
      --  Decrement lock instead. If Is_One then is fine to deallocate
      if X.Forward (1) /= null and then X.Forward (1).Element = Item then
         X.Forward (1).Forward (0) := X;
         for j in 1 .. Container.Current_Level  loop
            exit when Update (j).Forward (j) /= X;
            Update (j).Forward (j) := X.Forward (j);
         end loop;

         while Container.Current_Level > 0 and then
            Container.Header.Forward (Container.Current_Level) = null loop
               Container.Current_Level := Container.Current_Level - 1;
         end loop;

         Container.Length := Container.Length - 1;
         if X.Forward /= null then
            Free (X.Forward);
         end if;
         Free (X);
      end if;
   end Remove;

   procedure Delete (Container : in out List; Position : in out Cursor) is
   begin
      if Position.Node = null then
         Position := No_Cursor;
         return;
      end if;
      Remove (Container, Position.Node.Element);
      Position := No_Cursor;
   end Delete;

   --  TODO
   procedure Delete_Last (Container : in out List; Count : Positive := 1) is
   begin
         if Count > Container.Length then
            Container.Clear;
         end if;

   end Delete_Last;

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'class is
      B : Natural renames Container'Unrestricted_Access.all.Busy;
   begin
      return It : constant Iterator :=
         Iterator'(Limited_Controlled with
         Container => Container'Unrestricted_Access,
         Node      => Container.Header)
         do
            B := B + 1;
         end return;
   end Iterate;

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'class is
      B : Natural renames Container'Unrestricted_Access.all.Busy;
   begin
      if Start = No_Cursor then
         raise Constraint_Error with
         "Start position for iterator equals No_Cursor";
      elsif Start.Container /= Container'Unrestricted_Access then
         raise Program_Error with
         "Start cursor of Iterate designates wrong list";
      else
         return It : constant Iterator :=
            Iterator'(Limited_Controlled with
            Container => Container'Unrestricted_Access,
            Node      => Start.Node)
            do
               B := B + 1;
            end return;
      end if;
   end Iterate;

   function First (Container : List) return Cursor is
   begin
      if Container.Header = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Header while Length > 0";
         end if;
         return No_Cursor;
      end if;
      return Cursor'(Container'Unrestricted_Access, Container.Header);
   end First;

   function First_Element (Container : List) return Element_Type is
   begin
      if Container.Header = null then
         raise Constraint_Error with "List is empty";
      end if;
      return Container.Header.Element;
   end First_Element;

   function Last (Container : List) return Cursor is
   begin
      if Container.Tail = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Tail while Length > 0";
         end if;
         return No_Cursor;
      end if;
      return (Container'Unrestricted_Access, Container.Tail);
   end Last;

   function Last_Element (Container : List) return Element_Type is
   begin
      if Container.Tail = null then
         raise Constraint_Error with "List is empty";
      end if;
      return Container.Tail.Element;
   end Last_Element;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Node /= null and then
         Position.Node.Forward (1) /= null then
         return Cursor'(Position.Container, Position.Node.Forward (1));
      end if;
      return No_Cursor;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Node /= null and then
         Position.Node.Forward (1) /= null then
         Position.Node := Position.Node.Forward (1);
         return;
      end if;
      Position := No_Cursor;
   end Next;

   function Previous (Position : Cursor) return Cursor is
   begin
      if Position.Node /= null and then
         Position.Node.Forward (0) /= null then
         return Cursor'(Position.Container, Position.Node.Forward (0));
      end if;
      return No_Cursor;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      if Position.Node /= null and then
         Position.Node.Forward (0) /= null then
         Position.Node := Position.Node.Forward (0);
         return;
      end if;
      Position := No_Cursor;
   end Previous;

   function Find (Container : List; Item : Element_Type) return Cursor is
      X : Node_Access;
   begin
      if Container.Header = null then
         raise Constraint_Error with "List is empty";
      end if;
      X := Container.Header;
      if X.Element = Item then
         return Container.First;
      end if;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, Item) > 0;
            X := X.Forward (j);
         end loop;
      end loop;
      if X.Element = Item then
         return Cursor'(Container'Unrestricted_Access, X);
      end if;
      return No_Cursor;
   end Find;

   function Contains (Container : List; Item : Element_Type) return Boolean is
      X : Node_Access;
   begin
      X := Container.Header;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, Item) > 0;
            X := X.Forward (j);
         end loop;
      end loop;
      if X.Element = Item then
         return True;
      end if;
      return False;
   end Contains;

   procedure Adjust (Control : in out Reference_Control_Type) is
   begin
      if Control.Node /= null then
         System.Atomic_Counters.Increment (Control.Node.Lock);
      end if;
   end Adjust;

   procedure Finalize (Control : in out Reference_Control_Type) is
      Is_Zero : Boolean;
      pragma Unreferenced (Is_Zero);
   begin
      if Control.Node /= null then
         Is_Zero := System.Atomic_Counters.Decrement (Control.Node.Lock);
      end if;
   end Finalize;

   procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Container /= null then
         declare
            B : Natural renames Object.Container.all.Busy;
         begin
            B := B - 1;
         end;
      end if;
   end Finalize;

   function First (Object : Iterator) return Cursor is
   begin
      return First (Object.Container.all);
   end First;

   function Last (Object : Iterator) return Cursor is
   begin
      return Last (Object.Container.all);
   end Last;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Object.Container /= Position.Container then
         return No_Cursor;
      end if;
      return  Next (Position);
   end Next;

   function Previous (Object : Iterator; Position : Cursor) return Cursor is
   begin
      if Object.Container /= Position.Container then
         return No_Cursor;
      end if;
      return  Previous (Position);
   end Previous;

end Skip_List;
