with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Calendar;
with Ada.Real_Time;        use Ada.Real_Time;

procedure Tambo is
   -------------------------------------------------------------------------
   -- CONFIG
   -------------------------------------------------------------------------
   Num_Cows     : constant Positive := 100;
   Cap_Milking  : constant Positive := 15;  -- capacidad sala de ordenie
   Cap_Vacc     : constant Positive := 5;   -- capacidad area vacunacion
   Cap_Truck    : constant Positive := 50;  -- capacidad por camion

   -------------------------------------------------------------------------
   -- LOG protegido para ordenar todas las salidas en pantalla
   -------------------------------------------------------------------------
   protected Log is
      procedure Print (S : String);
   private
   end Log;

   protected body Log is
      procedure Print (S : String) is
      begin
         Put_Line (S);
      end Print;
   end Log;

   -------------------------------------------------------------------------
   -- PASILLO (solo 1 vaca a la vez)
   -- Secuencia que deben seguir las vacas:
   --   1) Pasillo.Enter (se imprime "entrando al pasillo")
   --   2) Pasillo.Leave (se imprime "saliendo del pasillo")
   --   3) Entrar al area (Milking.Enter o Vacc.Enter) -> imprime "entrando al area..."
   -------------------------------------------------------------------------
   protected Pasillo is
      entry Enter_Pasillo (Id : Positive);
      entry Leave_Pasillo (Id : Positive);
   private
      Busy : Boolean := False;
   end Pasillo;

   protected body Pasillo is
      entry Enter_Pasillo (Id : Positive)
        when not Busy
      is
      begin
         Busy := True;
         Log.Print ("La vaca " & Id'Image & " esta entrando al pasillo");
      end Enter_Pasillo;

      entry Leave_Pasillo (Id : Positive)
        when Busy
      is
      begin
         -- la vaca libera el pasillo
         Busy := False;
         Log.Print ("La vaca " & Id'Image & " esta saliendo del pasillo");
      end Leave_Pasillo;
   end Pasillo;

   -------------------------------------------------------------------------
   -- AREA DE ORDENIE (semáforo con capacidad Cap_Milking)
   -------------------------------------------------------------------------
   protected Milking is
      entry Enter_Milking (Id : Positive);
      entry Leave_Milking (Id : Positive);
      function Occupied return Integer;
   private
      Count : Integer := 0;
   end Milking;

   protected body Milking is
      entry Enter_Milking (Id : Positive)
        when Count < Cap_Milking
      is
      begin
         Count := Count + 1;
         Log.Print ("La vaca " & Id'Image & " esta entrando al area de ordenie");
      end Enter_Milking;

      entry Leave_Milking (Id : Positive)
        when Count > 0
      is
      begin
         Count := Count - 1;
         Log.Print ("La vaca " & Id'Image & " esta saliendo del area de ordenie");
      end Leave_Milking;

      function Occupied return Integer is
      begin
         return Count;
      end Occupied;
   end Milking;

   -------------------------------------------------------------------------
   -- AREA DE VACUNACION (semáforo con capacidad Cap_Vacc)
   -------------------------------------------------------------------------
   protected Vaccination is
      entry Enter_Vacc (Id : Positive);
      entry Leave_Vacc (Id : Positive);
      function Occupied return Integer;
   private
      Count : Integer := 0;
   end Vaccination;

   protected body Vaccination is
      entry Enter_Vacc (Id : Positive)
        when Count < Cap_Vacc
      is
      begin
         Count := Count + 1;
         Log.Print ("La vaca " & Id'Image & " esta entrando al area de vacunacion");
      end Enter_Vacc;

      entry Leave_Vacc (Id : Positive)
        when Count > 0
      is
      begin
         Count := Count - 1;
         Log.Print ("La vaca " & Id'Image & " esta saliendo del area de vacunacion");
      end Leave_Vacc;

      function Occupied return Integer is
      begin
         return Count;
      end Occupied;
   end Vaccination;

   -------------------------------------------------------------------------
   -- CAMIONES: dos camiones con capacidad Cap_Truck cada uno
   -------------------------------------------------------------------------
   protected Trucks is
      entry Board (Id : Positive);
      function Total_Load return Integer;
   private
      T1 : Integer := 0;
      T2 : Integer := 0;
   end Trucks;

   protected body Trucks is
      entry Board (Id : Positive)
        when (T1 < Cap_Truck) or (T2 < Cap_Truck)
      is
      begin
         if T1 < Cap_Truck then
            T1 := T1 + 1;
            Log.Print ("La vaca " & Id'Image & " esta subiendo al Camion 1");
         else
            T2 := T2 + 1;
            Log.Print ("La vaca " & Id'Image & " esta subiendo al Camion 2");
         end if;
      end Board;

      function Total_Load return Integer is
      begin
         return T1 + T2;
      end Total_Load;
   end Trucks;

   -------------------------------------------------------------------------
   -- GENERADOR ALEATORIO (Discrete_Random)
   -------------------------------------------------------------------------
   subtype Rand_Range is Integer range 0 .. 1000;
   package Rand_Int is new Ada.Numerics.Discrete_Random (Rand_Range);
   Gen : Rand_Int.Generator;

   function Rand_Time (Max : Float) return Duration is
      R : Rand_Range := Rand_Int.Random(Gen);
   begin
      return Duration (Float(R) / 1000.0 * Max);
   end Rand_Time;

   -------------------------------------------------------------------------
   -- TASK VACA
   -------------------------------------------------------------------------
   task type Cow (Id : Positive);

   task body Cow is
      -- Decide una vez si vacuna primero
      R : Rand_Range := Rand_Int.Random(Gen);
      Vacc_First : Boolean := (R mod 2 = 0);
   begin
      -- Realizamos el movimiento por pasillo ANTES de entrar al area
      if Vacc_First then
         -- Pasillo -> Vacunacion -> Ordeñe
         Pasillo.Enter_Pasillo (Id);
         -- simulamos movimiento por pasillo (pequeña demora)
         delay 0.001;
         Pasillo.Leave_Pasillo (Id);

         -- ahora intentar entrar al area de vacunacion (si esta llena espera)
         Vaccination.Enter_Vacc (Id);
         delay Rand_Time (1.5);  -- tiempo de vacunacion aleatorio
         Vaccination.Leave_Vacc (Id);

         -- luego pasillo otra vez para moverse a ordenie
         Pasillo.Enter_Pasillo (Id);
         delay 0.001;
         Pasillo.Leave_Pasillo (Id);

         Milking.Enter_Milking (Id);
         delay Rand_Time (2.5);  -- tiempo de ordenie aleatorio
         Milking.Leave_Milking (Id);

      else
         -- Pasillo -> Ordeñe -> Vacunacion
         Pasillo.Enter_Pasillo (Id);
         delay 0.001;
         Pasillo.Leave_Pasillo (Id);

         Milking.Enter_Milking (Id);
         delay Rand_Time (2.5);
         Milking.Leave_Milking (Id);

         Pasillo.Enter_Pasillo (Id);
         delay 0.001;
         Pasillo.Leave_Pasillo (Id);

         Vaccination.Enter_Vacc (Id);
         delay Rand_Time (1.5);
         Vaccination.Leave_Vacc (Id);
      end if;

      -- Finalmente, ir al embarque (camiones)
      Trucks.Board (Id);
   end Cow;

   -------------------------------------------------------------------------
   -- CREACION DINAMICA DE LAS VACAS (evita problemas con arrays de tasks)
   -------------------------------------------------------------------------
   type Cow_Access is access Cow;
   Cows : array (1 .. Num_Cows) of Cow_Access;

begin
   -- Inicializar RNG
   Rand_Int.Reset (Gen);

   -- Crear y lanzar las tasks vaca
   for I in Cows'Range loop
      Cows (I) := new Cow (I);
   end loop;

   -- Esperar hasta que todos terminen (simplemente esperar que todos se hayan subido)
   -- Una forma simple: busy-wait hasta que Trucks.Total_Load = Num_Cows
   loop
      exit when Trucks.Total_Load = Num_Cows;
      delay 0.1;
   end loop;

   Log.Print ("Simulacion finalizada: total vacas en camiones = " &
              Integer'Image (Trucks.Total_Load));
end Tambo;
