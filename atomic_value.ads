package Atomic_Value is
   type B4 is mod 2 ** 32 with Size => 32;
   --  pragma Atomic (B4);
   type B8 is mod 2 ** 64 with Size => 64;
   --  pragma Atomic (B8);
   Relaxed : constant := 0;
   Consume : constant := 1;
   Acquire : constant := 2;
   Release : constant := 3;
   Acq_Rel : constant := 4;
   Seq_Cst : constant := 5;
   Last    : constant := 6;
   subtype Mem_Model is Integer range Relaxed .. Last;
   -------------
   --  Fence  --
   -------------
   procedure Atomic_Thread_Fence (Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Thread_Fence, "__atomic_thread_fence");

   -------------
   --  Loads  --
   -------------
   function Atomic_Load_4
      (Ptr : access B4;  Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Load_4, "__atomic_load_4");

   function Atomic_Load_8
      (Ptr : access B4;  Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Load_8, "__atomic_load_8");

   --------------
   --  Stores  --
   --------------
   procedure Atomic_Store_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Store_4, "__atomic_store_4");

   procedure Atomic_Store_8
      (Ptr : access B4; Value : B8; Model : Mem_Model := Seq_Cst);
   pragma Import (Intrinsic, Atomic_Store_8, "__atomic_store_8");

   ----------------
   --  Exchange  --
   ----------------
   function Atomic_Exchange_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Exchange_4, "__atomic_exchange_4");

   function Atomic_Exchange_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Exchange_8, "__atomic_exchange_8");

   ------------------------
   --  Compare Exchange  --
   ------------------------
   function Atomic_Compare_Exchange_4
      (Ptr      : access B4;
      Expected  : access B4;
      Desired   : B4;
      Weak      : Boolean := False;
      Success_Model : Mem_Model := Seq_Cst;
      Failure_Model : Mem_Model := Seq_Cst) return Boolean;
   pragma Import
      (Intrinsic, Atomic_Compare_Exchange_4, "__atomic_compare_exchange_4");

   function Atomic_Compare_Exchange_8
      (Ptr      : access B8;
      Expected  : access B8;
      Desired   : B8;
      Weak      : Boolean := False;
      Success_Model : Mem_Model := Seq_Cst;
      Failure_Model : Mem_Model := Seq_Cst) return Boolean;
   pragma Import
      (Intrinsic, Atomic_Compare_Exchange_8, "__atomic_compare_exchange_8");

   -------------------------
   --  Fetch and then Ops --
   -------------------------
   function Atomic_Fetch_Add_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_Add_4, "__atomic_fetch_add_4");

   function Atomic_Fetch_Sub_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_Sub_4, "__atomic_fetch_sub_4");

   function Atomic_Fetch_And_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_And_4, "__atomic_fetch_and_4");

   function Atomic_Fetch_Xor_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_Xor_4, "__atomic_fetch_xor_4");

   function Atomic_Fetch_Or_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_Or_4, "__atomic_fetch_or_4");

   function Atomic_Fetch_Nand_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Fetch_Nand_4, "__atomic_fetch_nand_4");

   function Atomic_Fetch_Add_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_Add_8, "__atomic_fetch_add_8");

   function Atomic_Fetch_Sub_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_Sub_8, "__atomic_fetch_sub_8");

   function Atomic_Fetch_And_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_And_8, "__atomic_fetch_and_8");

   function Atomic_Fetch_Xor_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_Xor_8, "__atomic_fetch_xor_8");

   function Atomic_Fetch_Or_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_Or_8, "__atomic_fetch_or_8");

   function Atomic_Fetch_Nand_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Fetch_Nand_8, "__atomic_fetch_nand_8");
   -------------------------
   -- Ops and then Fetch  --
   -------------------------
   function Atomic_Add_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Add_Fetch_4, "__atomic_add_fetch_4");

   function Atomic_Sub_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Sub_Fetch_4, "__atomic_sub_fetch_4");

   function Atomic_And_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_And_Fetch_4, "__atomic_and_fetch_4");

   function Atomic_Xor_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Xor_Fetch_4, "__atomic_xor_fetch_4");

   function Atomic_Or_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Or_Fetch_4, "__atomic_or_fetch_4");

   function Atomic_Nand_Fetch_4
      (Ptr : access B4; Value : B4; Model : Mem_Model := Seq_Cst) return B4;
   pragma Import (Intrinsic, Atomic_Nand_Fetch_4, "__atomic_nand_fetch_4");

   function Atomic_Add_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Add_Fetch_8, "__atomic_add_fetch_8");

   function Atomic_Sub_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Sub_Fetch_8, "__atomic_sub_fetch_8");

   function Atomic_And_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_And_Fetch_8, "__atomic_and_fetch_8");

   function Atomic_Xor_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Xor_Fetch_8, "__atomic_xor_fetch_8");

   function Atomic_Or_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Or_Fetch_8, "__atomic_or_fetch_8");

   function Atomic_Nand_Fetch_8
      (Ptr : access B8; Value : B8; Model : Mem_Model := Seq_Cst) return B8;
   pragma Import (Intrinsic, Atomic_Nand_Fetch_8, "__atomic_nand_fetch_8");

   ----------------
   --  Routines  --
   ----------------
   function Test_And_Set (B : access B4; Position : Integer)
      return Boolean  with Inline_Always => True, Pre => Position < 32;

   function Test_And_Set (B : access B8; Position : Integer)
      return Boolean  with Inline_always => True, Pre => Position < 64;

end Atomic_Value;
