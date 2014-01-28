pragma License (GPL);
pragma Ada_2012;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with System;
use type System.Address;

---------------------------
--  ISSUES
--  1. Are FENCES required?
---------------------------
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
      return Boolean;

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
      NFR.Reset (G, Container.Level);
      Container.Header        := null;
      Container.Current_Level := 0;
      Container.Length        := 0;
   end Initialize;

   --  One cannot reference any node of this list, as all refs get invalid.
   procedure Finalize (Container : in out List) is
      X, Y : Node_Access;
   begin
      if Container.Length = 0 then
         pragma Assert (Container.Header = null);
         pragma Assert (Container.Tail = null);
         return;
      end if;
      Container.Clear;
      Atomic_Thread_Fence;
      X := Container.Header;
      while X /= null loop
         Y := X.Forward (1);
         pragma Assert (Atomic_Load_4 (X.Visited'Access) = 0);
         Free (X.Forward);
         Free (X);
         X := Y;
      end loop;
   end Finalize;

   ----------------
   --  External  --
   ----------------

   --  FIXME Is this correct?
   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Node = null then
         return False;
      end if;
      return True;
   end Has_Element;

   function Is_Valid (Position : Cursor) return Boolean is
      B : B4;
   begin
      Atomic_Thread_Fence;
      B := Atomic_Load_4 (Position.Node.Visited'Access);
      return B > 0;
   end Is_Valid;

   function "=" (Left, Right : List) return Boolean is
   begin
      return Left'Address = Right'Address;
   end "=";

   function Length (Container : List) return Natural is
   begin
      return Container.Length;
   end Length;

   function Is_Empty (Container : List) return Boolean is
   begin
      return Container.Header = null;
   end Is_Empty;

   --  No Finalization, just reset all Visited
   procedure Clear (Container : in out List) is
      X, Y : Node_Access;
   begin
      if Container.Length = 0 then
         pragma Assert (Container.Header = null);
         pragma Assert (Container.Tail = null);
         return;
      end if;
      Atomic_Thread_Fence;
      X := Container.Header;
      while X /= null loop
         Y := X.Forward (1);
         pragma Assert
            (Atomic_Load_4 (X.Visited'Access) = 1
            or Atomic_Load_4 (X.Visited'Access) = 0);
         Atomic_Store_4 (X.Visited'Access, 0);
         Atomic_Thread_Fence;
         --  Don't reset X.Forward;
         X := Y;
      end loop;
      Container.Length := 0;
   end Clear;

   --  User is responsible to call Is_Valid against Position for a valid node
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
   begin
      <<Start>>
      Atomic_Thread_Fence;
      declare
         H : Node_Access := Container.Header;
         B : Boolean;
         pragma Unreferenced (B);
      begin
         if H = null then
            --  Prepare to insert as Header, but may not success
            --
            H := new Node_Type'(null, New_Item, 1);
            H.Forward := new Node_Array (0 .. Container.Level);
            H.Forward.all := (others => null);

            --  Check if Header is not present use CAS
            if not Compare_Exchange
               (Container.Header'Unrestricted_Access, null, H) then
               Free (H.Forward);
               Free (H);
               Ada.Text_IO.Put_Line ("Retry");
               goto Start;
            end if;
            B := Compare_Exchange
               (Container.Tail'Unrestricted_Access, null, H);
            Container.Length := 1;
            Container.Current_Level := 1;
            Position := Container.First;
            return;
         end if;
      end;

      --  Find the position first and insert a node with null forwards
      declare
         X, Y : Node_Access := Container.Header;
         New_Level : Integer := 1;
         B : B4;
      begin

         Random_Level :
         while NFR.Random (G) < 0.5 loop
            exit Random_Level when New_Level >= Container.Level;
            New_Level := New_Level + 1;
         end loop Random_Level;

         if New_Level > Container.Current_Level then
            Container.Current_Level := New_Level;
         end if;

         --  TODO Check Atomic ops
         for j in reverse 1 .. Container.Current_Level loop
            loop
               Y := X.Forward (j);
               exit when Y = null;
               exit when Compare (Y.Element, New_Item) > 0;
               X := Y;
            end loop;
         end loop;
         Y := X.Forward (1);

         --  Already presented
         if Y /= null and then Compare (Y.Element, New_Item) = 0 then
            <<Try>>
            B := Atomic_Load_4 (Y.Visited'Access);
            if B = 0 then
               --  Attach
               --  TODO Visit
               if not Atomic_Compare_Exchange_4
                     (Y.Visited'Access, B'Unrestricted_Access, 1) then
                  goto Try;
               end if;
            end if;

            Position := (Container'Unchecked_Access, Y);
            return;
         end if;

         --  New Node
         --  TODO Atomic
         Y := new Node_Type'(null, New_Item, 1);
         Y.Forward := new Node_Array (0 .. New_Level);
         Y.Forward.all := (0 => X, 1 => X.Forward (1), others => null);

         --  FIXME Link to List
         X.Forward (1) := Y;

         if Y.Forward (1) = null then
            Container.Tail := Y;
         end if;
         --  Call AUX Task to Link Y' Forwards
         --  Link (Y);
         Container.Length := Container.Length + 1;
         Position := (Container'Unchecked_Access, Y);
      end;
   end Insert;

   procedure Delete (Node : in out Node_Access);
   procedure Delete (Node : in out Node_Access) is
   begin
      declare
         B : B4;
      begin
         Atomic_Thread_Fence;
         if Node.Visited > 0 then
            B := Atomic_Fetch_Sub_4 (Node.Visited'Access, 1);
         end if;
      end;
   end Delete;

   procedure Delete (Container : in out List; Position : in out Cursor) is
   begin
      if Position.Node /= null then
         Delete (Position.Node);
      end if;
      Position := No_Cursor;
   end Delete;

   --  TODO
   procedure Delete_Last (Container : in out List; Count : Positive := 1) is
      C : Positive := Count;
      X : Node_Access;
   begin
      loop
         X := Container.Tail;
         exit when X = null;
         if Compare_Exchange
            (Container.Tail'Unrestricted_Access, X, X.Forward (0)) then
            Delete (X);
            exit when C = 1;
            C := C - 1;
         end if;
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
         Node      => Container.Header)
         do
            null;
            --  Atomic_Thread_Fence;
            --  B := Atomic_Fetch_Add_4 (Container.Header.Visited'Access, 1);
            --  Atomic_Thread_Fence;
         end return;
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
      if Container.Header = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Header while Length > 0";
         end if;
         return No_Cursor;
      end if;
      declare
         X : Node_Access := Container.Header;
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
      if Container.Header = null then
         raise Program_Error with "List is Empty";
      end if;

      declare
         X : Node_Access := Container.Header;
      begin
         while Atomic_Load_4 (X.Visited'Access) = 0 loop
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
      if Container.Tail = null then
         if Container.Length > 0 then
            raise Program_Error with "Null Tail with Length > 0";
         end if;
         return No_Cursor;
      end if;
      declare
         X : Node_Access := Container.Tail;
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
      if Container.Tail = null then
         raise Program_Error with "List is Empty";
      end if;

      declare
         X : Node_Access := Container.Tail;
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
      if Container.Header = null then
         raise Constraint_Error with "List is empty";
      end if;
      X := Container.Header;
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
      --  TODO Atomic
      for j in reverse 1 .. Container.Current_Level loop
         loop
            exit when X.Forward (j) = null;
            exit when Compare (X.Forward (j).Element, Item) > 0;
            X := X.Forward (j);
         end loop;
      end loop;

      if X.Element = Item and then Atomic_Load_4 (X.Visited'Access) > 0 then
         --  TODO Visit
         --  B := Atomic_Fetch_Add_4 (X.Visited'Access, 1);
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
            B := Atomic_Fetch_Sub_4 (Object.Node.Visited'Access, 1);
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
