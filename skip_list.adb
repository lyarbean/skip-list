pragma License (GPL);
pragma Ada_2012;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;

package body Skip_List is
   procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   procedure Free is
      new Ada.Unchecked_Deallocation (Node_Array, Node_Array_Access);

   package NFR renames Ada.Numerics.Float_Random;
   G : NFR.Generator;

   ----------------------------------
   --  Atomic Compare And Exchange --
   ----------------------------------
   function Compare_Exchange
      (Dest : access Node_Access; Expetected, Desired : Node_Access)
      return Boolean with Inline_Always => True;

   function Compare_Exchange
      (Dest : access Node_Access; Expetected, Desired : Node_Access)
   return Boolean is
      function To is new Ada.Unchecked_Conversion (Node_Access, B8);
   begin
      return Atomic_Compare_Exchange_8
         (To (Dest.all)'Unrestricted_Access,
         To (Expetected)'Unrestricted_Access, To (Desired));
   end Compare_Exchange;

   procedure Initialize (Container : in out List) is
   begin
      NFR.Reset (G);
      Container.Skip          := new Node_Array (0 .. Container.Level);
      Container.Skip.all      := (others => null);
      Container.Current_Level := 0;
      Container.Length        := 0;
   end Initialize;

   procedure Finalize (Container : in out List) is
      X, Y : Node_Access;
   begin
      --  If Empty_List
      if Container.Skip = null then
         return;
      end if;

      Ada.Text_IO.Put_Line ("Finalize");
      Clear (Container);
      Atomic_Thread_Fence;
      X := Container.Skip (1);
      while X /= null loop
         Y := X.Forward (1);
         pragma Assert (Atomic_Load_4 (X.Visited'Access) = 0);
         Free (X.Forward);
         Free (X);
         X := Y;
      end loop;
      Free (Container.Skip);
      Ada.Text_IO.Put_Line ("Finalized");
   end Finalize;

   --  TODO Atomic
   procedure Link (Container : in out List; Node : not null Node_Access);
   procedure Link (Container : in out List; Node : not null Node_Access) is
      X : Node_Access := null;
   begin
      Atomic_Thread_Fence;
      for j in reverse 2 .. Container.Current_Level loop
            if Container.Skip (j) = null and then j in Node.Forward'Range then
               Container.Skip (j) := Node;
            else
               if X = null then
                  X := Container.Skip (j);
               end if;
               loop
                  exit when X.Forward (j) = null;
                  exit when Compare (X.Forward (j).Element, Node.Element) > 0;
                  X := X.Forward (j);
               end loop;
               if j in Node.Forward'Range then
                  Node.Forward (j) := X.Forward (j);
                  X.Forward (j) := Node;
               end if;
            end if;
      end loop;
      Atomic_Thread_Fence;
   end Link;
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

   function Is_Valid (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         return False;
      end if;
      Atomic_Thread_Fence;
      return Atomic_Load_4 (Position.Node.Visited'Access) > 0;
   end Is_Valid;

   function "=" (Left, Right : List) return Boolean is
   begin
      return Left.Skip = Right.Skip;
   end "=";

   function Length (Container : List) return Natural is
   begin
      return Container.Length;
   end Length;

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Skip (1) = null;
   end Is_Empty;

   --  No Finalization, just reset all Visited
   procedure Clear (Container : in out List) is
      X : Node_Access;
   begin
      Atomic_Thread_Fence;
      pragma Assert (Container.Skip /= null);
      X := Container.Skip (1);
      while X /= null loop
         Atomic_Store_4 (X.Visited'Access, 0);
         --  X.Visited := 0;
         X := X.Forward (1);
      end loop;
      Container.Length := 0;
   end Clear;

   --  User is responsible to call Is_Valid against Position
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

   procedure Insert (Container : in out List; New_Item : Element_Type) is
      Pos : Cursor;
      pragma Unreferenced (Pos);
   begin
      Insert (Container, New_Item, Pos);
   end Insert;

   procedure Insert
      (Container : in out List; New_Item : Element_Type; Position : out Cursor)
   is
      R : Boolean;
      B : B4;
      pragma Unreferenced (B);
      X, Y, Z : Node_Access;
      New_Level : Integer := 1;
   begin
      <<Start>>
      Atomic_Thread_Fence;
      X := Container.Skip (1);
      --  When Container is empty
      if X = null then
         Z := new Node_Type'(1, null, New_Item);
         Z.Forward := new Node_Array (0 .. 1);
         Z.Forward.all := (others => null);

         if not Compare_Exchange
            (Container.Skip (1)'Unrestricted_Access, X, Z) then
            Free (Z.Forward);
            Free (Z);
            Ada.Text_IO.Put_Line ("Retry");
            goto Start;
         end if;
         --  If Forward is set, then set Backward, for Skip
         R := Compare_Exchange (Container.Skip (0)'Unrestricted_Access, X, Z);
         if not R then
            raise Program_Error with "Fail one CAS Z";
         end if;
         Ada.Text_IO.Put_Line ("Skip");
         Container.Length := 1;
         Container.Current_Level := 1;
         Position := Cursor'(Container'Unrestricted_Access, Z);
         return;
      end if;

      --  Pre-allocate
      Random_Level :
      while NFR.Random (G) < 0.5 loop
         exit Random_Level when New_Level >= Container.Level;
         New_Level := New_Level + 1;
         --  Ada.Text_IO.Put_Line ("Random_Level");
      end loop Random_Level;
      Z := new Node_Type'(1, null, New_Item);
      Z.Forward := new Node_Array (0 .. New_Level);
      Z.Forward.all := (others => null);

      X := Container.Skip (1);
      --  (Z = X)
      if Compare (X.Element, New_Item) = 0 then
         if Atomic_Load_4 (X.Visited'Access) = 0 then
            B := Atomic_Fetch_Add_4 (X.Visited'Access, 1);
         end if;
         Position := (Container'Unchecked_Access, X);
         goto FreeZ;
      end if;

      X := Container.Skip (1);
      --  Front_push
      --  Link Forwards in place
      --  Z --> X
      if Compare (X.Element, New_Item) > 0 then
         Z.Forward (1) := X;
         if not Compare_Exchange
            (Container.Skip (1)'Unrestricted_Access, X, Z) then
            Free (Z.Forward);
            Free (Z);
            goto Start;
         end if;
         X.Forward (0) := Z;
         Z.Forward (2 .. Z.Forward'Last) := Container.Skip (2 .. Z.Forward'Last);
         for j in 2 .. X.Forward'Last loop
            R := Compare_Exchange
            (Container.Skip (j)'Unrestricted_Access, X, Z);
         end loop;

         Container.Length := Container.Length + 1;
         if New_Level > Container.Current_Level then
            Container.Current_Level := New_Level;
         end if;
         Position := Cursor'(Container'Unrestricted_Access, Z);
         return;
      end if;

      --  X ----------> Y := X.F
      --  X -- > Z -- > Y
      --  X := First;
      --  Try :
      --  Move on X while X < Z, until X.F is null or X.F > Z and
      --  Z.F := Y
      --  if not CAS (X.F, Y) then goto Try; end if;

      <<TrySkip>>
      pragma Assert (X /= null);
      for j in reverse 1 .. Container.Current_Level loop
         loop
            --  In case the Link is not done
            exit when j not in X.Forward'Range;
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, New_Item) > 0;
            X := X.Forward (j);
         end loop;
      end loop;
      pragma Assert (X /= null);

      --  (X = Z) --> Y
      --  Activate X
      if Compare (X.Element, New_Item) = 0 then
         --  if X.Visited = 0 then
         if Atomic_Load_4 (X.Visited'Access) = 0 then
            B := Atomic_Fetch_Add_4 (X.Visited'Access, 1);
         end if;
         Position := (Container'Unchecked_Access, X);
         goto FreeZ;
      end if;

      --  X --> Z --> Y (*|null)
      Y := X.Forward (1);
      Z.Forward (1) := Y;
      R := Compare_Exchange (X.Forward (1)'Unrestricted_Access, Y, Z);
      if R then
         if Y /= null then
            R := Compare_Exchange (Y.Forward (0)'Unrestricted_Access, X, Z);
            --  if not R, then there is a node inserted between X and Y,
            --  X    N -- Y
            --  |         |
            --  | -- Z -- |
            --  TODO When does this case occur?
            --
            if not R then
               raise Program_Error with "Racing";
            end if;
         end if;
         Z.Forward (0) := X;
         if New_Level > Container.Current_Level then
            Container.Current_Level := New_Level;
         end if;
         Container.Length := Container.Length + 1;
         Position := Cursor'(Container'Unrestricted_Access, Z);
         Link (Container, Z);
         return;
      else
         --  A node has been inserted as X.Forward (1),
         --  So go to <<TrySkip>>, and locate a right place
         Ada.Text_IO.Put_Line ("TrySkip");
         goto TrySkip;
      end if;

      <<FreeZ>>
      Free (Z.Forward);
      Free (Z);
   end Insert;

   function Delete (Node : Node_Access) return Boolean;
   function Delete (Node : Node_Access) return Boolean is
      B : aliased B4;
      R : Boolean;
   begin
      <<Try>>
      B := Atomic_Load_4 (Node.Visited'Access);
      if B > 0 then
         R := Atomic_Compare_Exchange_4
            (Node.Visited'Access, B'Access, B - 1);
         if R then
            return True;
         end if;
         goto Try;
      end if;
      return False;
   end Delete;

   procedure Delete (Container : in out List; Position : in out Cursor) is
   begin
      if Position.Node /= null then
         if Delete (Position.Node) then
            Container.Length := Container.Length - 1;
         end if;
      end if;
      Position := No_Cursor;
   end Delete;

   procedure Delete_Last (Container : in out List; Count : Positive := 1) is
      C : Positive := Count;
      X : Node_Access;
   begin
      loop
         X := Container.Skip (0);
         exit when X = null;
         if Delete (X) then
            Container.Length := Container.Length - 1;
            C := C - 1;
         end if;
         exit when C = 0;
      end loop;
   end Delete_Last;

   function Iterate (Container : List)
      return List_Iterator_Interfaces.Reversible_Iterator'class is
      B : B4;
      pragma Unreferenced (B);
   begin
      return It : constant Iterator :=
         Iterator'(Limited_Controlled with
         Container => Container'Unrestricted_Access,
         Node      => null);
   end Iterate;

   function Iterate (Container : List; Start : Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'class is
      B : B4;
      pragma Unreferenced (B);
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
               --  Atomic_Thread_Fence;
               B := Atomic_Fetch_Add_4 (Start.Node.Visited'Access, 1);
               --  Atomic_Thread_Fence;
            end return;
      end if;
   end Iterate;

   function First (Container : List) return Cursor is
   begin
      if Container.Skip (1) = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Skip (1) while Length > 0 ";
         end if;
         return No_Cursor;
      end if;
      declare
         X : Node_Access := Container.Skip (1);
         --  B : B4;
      begin
         while Atomic_Load_4 (X.Visited'Access) = 0 loop
            X := X.Forward (1);
            exit when X = null;
         end loop;
         --  TODO Visit
         --  B := Atomic_Fetch_Add_4 (X.Visited'Access, 1);
         return Cursor'(Container'Unrestricted_Access, X);
      end;
   end First;

   function First_Element (Container : List) return Element_Type is
   begin
      if Container.Skip (1) = null then
         raise Program_Error with "List is Empty";
      end if;

      declare
         X : Node_Access := Container.Skip (1);
      begin
         loop
            exit when Atomic_Load_4 (X.Visited'Access) /= 0;
            X := X.Forward (1);
            exit when X = null;
         end loop;
         if X = null then
            raise Program_Error with "No element valid";
         end if;
         return X.Element;
      end;
   end First_Element;

   function Last (Container : List) return Cursor is
   begin
      if Container.Skip (0) = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Tail with Length > 0";
         end if;
         return No_Cursor;
      end if;
      declare
         X : Node_Access := Container.Skip (0);
      begin
         while Atomic_Load_4 (X.Visited'Access) = 0 loop
            X := X.Forward (0);
            exit when X = null;
         end loop;
         return Cursor'(Container'Unrestricted_Access, X);
      end;
   end Last;

   function Last_Element (Container : List) return Element_Type is
   begin
      if Container.Skip (0) = null then
         raise Program_Error with "List is Empty";
      end if;

      declare
         X : Node_Access := Container.Skip (0);
      begin
         while Atomic_Load_4 (X.Visited'Access) = 0 loop
            X := X.Forward (0);
            exit when X = null;
         end loop;
         if X = null then
            raise Program_Error with "No element valid";
         end if;
         return X.Element;
      end;
   end Last_Element;

   function Next (Position : Cursor) return Cursor is
      X : Node_Access := Position.Node;
   begin
      if X = null then
         return No_Cursor;
      end if;

      X := X.Forward (1);

      while X /= null loop
         exit when Atomic_Load_4 (X.Visited'Access) /= 0;
         X := X.Forward (1);
      end loop;

      if X = null then
         return No_Cursor;
      end if;

      if Atomic_Load_4 (X.Visited'Access) /= 0 then
         return Cursor'(Position.Container, X);
      end if;

      return No_Cursor;
   end Next;

   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end Next;

   function Previous (Position : Cursor) return Cursor is
      X : Node_Access := Position.Node;
   begin
      if X = null then
         return No_Cursor;
      end if;

      X := X.Forward (0);

      while X /= null loop
         exit when Atomic_Load_4 (X.Visited'Access) /= 0;
         X := X.Forward (0);
      end loop;

      if X = null then
         return No_Cursor;
      end if;

      if Atomic_Load_4 (X.Visited'Access) /= 0 then
         return Cursor'(Position.Container, X);
      end if;

      return No_Cursor;
   end Previous;

   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end Previous;

   function Find (Container : List; Item : Element_Type) return Cursor is
      X : Node_Access;
      B : B4;
      pragma Unreferenced (B);
   begin
      if Container.Skip (1) = null then
         raise Constraint_Error with "List is empty";
      end if;
      X := Container.Skip (1);
      if Compare (X.Element, Item) > 0 then
         return No_Cursor;
      end if;

      if X.Element = Item then
         if  Atomic_Load_4 (X.Visited'Access) > 0 then
            --  TODO Visit
            --  B := Atomic_Fetch_Add_4 (X.Visited'Access, 1);
            return Cursor'(Container'Unrestricted_Access, X);
         else
            --  Node is tired
            return No_Cursor;
         end if;
      end if;
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when j not in X.Forward'Range;
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, Item) > 0;
            X := X.Forward (j);
         end loop;
      end loop;

      if X.Element = Item and then Atomic_Load_4 (X.Visited'Access) > 0 then
         return Cursor'(Container'Unrestricted_Access, X);
      end if;
      return No_Cursor;
   end Find;

   function Contains (Container : List; Item : Element_Type) return Boolean is
   begin
      return  Find (Container, Item) /= No_Cursor;
   end Contains;

   procedure Finalize (Object : in out Iterator) is
   begin
      if Object.Container /= null then
         declare
            B : B4;
            pragma Unreferenced (B);
         begin
            --  FIXME
            if Object.Node /= null then
               B := Atomic_Fetch_Sub_4 (Object.Node.Visited'Access, 1);
            end if;
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
