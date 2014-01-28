package body Atomic_Value is
   function Test_And_Set (B : access B4; Position : Integer) return Boolean is
      A, C : aliased B4;
   begin
      Atomic_Thread_Fence;
      A := B.all;
      C := A or (2 ** Position);
      if A = C  then
         return False;
      end if;
      Atomic_Thread_Fence;
      return Atomic_Compare_Exchange_4 (B, A'Access, C);
   end Test_And_Set;

   function Test_And_Set (B : access B8; Position : Integer) return Boolean is
      A, C : aliased B8;
   begin
      Atomic_Thread_Fence;
      A := B.all;
      C := A or (2 ** Position);
      if A = C  then
         return False;
      end if;
      Atomic_Thread_Fence;
      return Atomic_Compare_Exchange_8 (B, A'Access, C);
   end Test_And_Set;

end Atomic_Value;
