pragma License (GPL);
pragma Ada_2012;
with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
package body Skip_List is
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Array, Node_Array_Access);

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position = No_Element then
         return False;
      end if;
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
      x, y : Node_Access;
   begin
      if Container.Header /= null then
         x := Container.Header.Forward (1);
         while x /= null loop
            y := x.Forward (1);
            if x.Forward  /= null then
               Free (x.Forward);
            end if;
            Free (x);
            x := y;
         end loop;
         Free (Container.Header.Forward);
         Free (Container.Header);
      end if;
      Initialize (Container);
   end Clear;

   function Element (Position : Cursor) return Element_Type is
   begin
      if Position.Node = null then
         raise Program_Error with "Bad Cursor on Element";
      end if;
      return Position.Node.Element;
   end Element;

   function Constant_Reference
     (Container : aliased List;
      Position  : Cursor) return Constant_Reference_Type is
   begin
      if Container /= Position.Container.all
         or Position.Node = null then
         raise Program_Error with "Bad Cursor on Constant_Reference";
      end if;
      return Constant_Reference_Type'(Element => Position.Node.Element'Access);
   end Constant_Reference;

   function Reference
     (Container : aliased in out List;
      Position  : Cursor) return Reference_Type is
   begin
      if Container /= Position.Container.all
         or Position.Node = null then
         raise Program_Error with "Bad Cursor on Reference";
      end if;
      return Reference_Type'(Element => Position.Node.Element'Access);
   end Reference;

   type Level_Type is mod 2 ** 64;
   package Level_Random is new Ada.Numerics.Discrete_Random (Level_Type);

   g : Level_Random.Generator;

   function Random_Level (Container : List) return Integer with Inline;
   function Random_Level (Container : List) return Integer is
      l : Integer := 1;
   begin
      while Level_Random.Random (g) < 2 ** 32 loop
         exit when l >= Container.Level;
         l := l + 1;
      end loop;
      return l;
   end Random_Level;

   procedure Initialize (Container : in out List) is
   begin
      Level_Random.Reset (g);
      Container.Header             := null;
      Container.Current_Level      := 0;
      Container.Length             := 0;
      Container.Busy               := 0;
      Container.Lock               := 0;
   end Initialize;

   procedure Finalize (Container : in out List) is
   begin
      Container.Clear;
   end Finalize;

   procedure Insert (Container : in out List; New_Item : Element_Type) is
      Pos : Cursor;
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
         Container.Header := new Node_Type'(null, New_Item);
         Container.Header.Forward := new Node_Array (1 .. 1);
         Container.Header.Forward.all := (others => null);
         Container.Length := 1;
         Container.Current_Level := 1;
         Position := Container.First;
         return;
      end if;
      declare
         x : Node_Access := Container.Header;
         new_level : Integer;
         update : Node_Array (1 .. Container.Level) := (others => null);
      begin
         for j in reverse 1 .. Container.Current_Level loop
            loop
               exit when x.Forward (j) = null;
               exit when Compare (x.Forward (j).Element, New_Item) < 0;
               x := x.Forward (j);
            end loop;
            update (j) := x;
         end loop;

         x := x.Forward (1);
         --  Already presented
         if x /= null and then x.Element = New_Item then
            Position := (Container'Unchecked_Access, x);
            return;
         end if;

         new_level := Random_Level (Container);
         if new_level > Container.Current_Level then
            Container.Current_Level := new_level;
         end if;

         x := new Node_Type'(null, New_Item);
         x.Forward := new Node_Array (1 .. new_level);
         for j in 1 .. new_level loop
            x.Forward (j) := update (j).Forward (j);
            update (j).Forward (j) := x;
         end loop;
         Container.Length := Container.Length + 1;
         Position := (Container'Unchecked_Access, x);
      end;
   end Insert;

   procedure Remove (Container : in out List; Item : Element_Type);
   procedure Delete (Container : in out List; Position : in out Cursor) is
   begin
      --  In case when Position set to null Container.Header,
      --  which does not equal to No_Element
      if Position.Node = null then
         Position := No_Element;
         return;
      end if;
      --  TODO check if cursor valid
      Remove (Container, Position.Node.Element);
      Next (Position);
   end Delete;

   procedure Remove (Container : in out List; Item : Element_Type) is
      x : Node_Access;
      update : Node_Array (1 .. Container.Level) := (others => null);
      --  Container.Header);
   begin
      x := Container.Header;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when x.Forward (j) = null;
            exit when Compare (x.Forward (j).Element, Item) < 0;
            x := x.Forward (j);
         end loop;
         update (j) := x;
      end loop;
      x := x.Forward (1);
      if x /= null and then x.Element = Item then
         --  FIXME
         for j in 1 .. Container.Current_Level  loop
            exit when update (j).Forward (j) /= x;
            update (j).Forward (j) := x.Forward (j);
         end loop;
         while Container.Current_Level > 0 and then
            Container.Header.Forward (Container.Current_Level) = null loop
               Container.Current_Level := Container.Current_Level - 1;
         end loop;
         Container.Length := Container.Length - 1;
         if x.Forward /= null then
            Free (x.Forward);
         end if;
         Free (x);
      end if;
   end Remove;

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Forward_Iterator'class is
      B : Natural renames Container'Unrestricted_Access.all.Busy;
   begin
      return It : constant Iterator :=
         Iterator'(Limited_Controlled with
         Container => Container'Unrestricted_Access,
         Node      => null)
         do
            B := B + 1;
         end return;
   end Iterate;

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Forward_Iterator'class is
      B : Natural renames Container'Unrestricted_Access.all.Busy;
   begin
      if Start = No_Element then
         raise Constraint_Error with
         "Start position for iterator equals No_Element";
      elsif Start.Container /= Container'Unrestricted_Access then
         raise Program_Error with
         "Start cursor of Iterate designates wrong list";
      else
         --  pragma Assert (Vet (Start), "Start cursor of Iterate is bad");
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
      return Cursor'(Container'Unrestricted_Access,
                     Container.Header);
   end First;

   function Last (Container : List) return Cursor is
      x : Node_Access;
   begin
      if Container.Is_Empty then
         return No_Element;
      end if;
      x := Container.Header;
      while x.Forward (1) /= null loop
         x := x.Forward (1);
      end loop;
      return (Container'Unrestricted_Access, x);
   end Last;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position = No_Element or
         Position.Node.Forward (1) = Position.Container.Header then
         Ada.Text_IO.Put_Line ("Why I am here?-2");
         return No_Element;
      end if;
      return Cursor'(Position.Container, Position.Node.Forward (1));

   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      if Position.Node = null then
         Position := No_Element;
         return;
      end if;
      if Position.Node.Forward (1) /= null then
         Position.Node := Position.Node.Forward (1);
         return;
      end if;
      Position := No_Element;
   end Next;

   function Find (Container : List; Item : Element_Type) return Cursor is
      x : Node_Access;
   begin
      x := Container.Header;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when x.Forward (j) = null;
            exit when Compare (x.Forward (j).Element, Item) < 0;
            x := x.Forward (j);
         end loop;
      end loop;
      x := x.Forward (1);
      if x.Element = Item then
         return Cursor'(Container'Unrestricted_Access, x);
      end if;
      return No_Element;
   end Find;

   function Contains (Container : List; Item : Element_Type) return Boolean is
      x : Node_Access;
   begin
      x := Container.Header;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when x.Forward (j) = null;
            exit when Compare (x.Forward (j).Element, Item) < 0;
            x := x.Forward (j);
         end loop;
      end loop;
      x := x.Forward (1);
      if x.Element = Item then
         return True;
      end if;
      return False;
   end Contains;

   procedure Finalize (Object : in out Iterator) is
   begin
      null;
   end Finalize;

   function First (Object : Iterator) return Cursor is
   begin
      return First (Object.Container.all);
   end First;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
   begin
      --  Iterator.Container = Cursor.container
      return  Next (Position);
   end Next;

end Skip_List;
