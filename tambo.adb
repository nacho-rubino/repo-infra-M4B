with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;

procedure Tambo is

   --------------
   -- RANDOM ---
   --------------
   package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);
   Gen : Rand_Int.Generator;

   function Random_Delay (Max : Integer) return Duration is
   begin
      return Duration ((Rand_Int.Random (Gen) mod Max) + 1);
   end Random_Delay;


   --------------------------
   -- AREA DE ORDENE (15) --
   --------------------------
   protected Ordene is
      entry Entrar;
      procedure Salir;
   private
      Capacidad : Integer := 15;
   end Ordene;

   protected body Ordene is
      entry Entrar when Capacidad > 0 is
      begin
         Capacidad := Capacidad - 1;
      end Entrar;

      procedure Salir is
      begin
         Capacidad := Capacidad + 1;
      end Salir;
   end Ordene;


   -------------------------------
   -- PASILLO (DE 1 A LA VEZ)  ---
   -------------------------------
   protected Pasillo is
      entry Entrar;
      procedure Salir;
   private
      Libre : Boolean := True;
   end Pasillo;

   protected body Pasillo is
      entry Entrar when Libre is
      begin
         Libre := False;
      end Entrar;

      procedure Salir is
      begin
         Libre := True;
      end Salir;
   end Pasillo;


   ----------------------------
   -- MANGAS (Cupo = 5)     --
   ----------------------------
   protected Mangas is
      entry Entrar;
      procedure Salir;
   private
      Cupo : Integer := 5;
   end Mangas;

   protected body Mangas is
      entry Entrar when Cupo > 0 is
      begin
         Cupo := Cupo - 1;
      end Entrar;

      procedure Salir is
      begin
         Cupo := Cupo + 1;
      end Salir;
   end Mangas;


   -------------------------------
   -- DOS CAMIONES DE 50 VACAS --
   -------------------------------
   protected Camiones is
      entry Subir (V : Integer);
      function Llenos return Boolean;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is
      entry Subir (V : Integer) when C1 < 50 or C2 < 50 is
      begin
         if C1 < 50 then
            C1 := C1 + 1;
            Put_Line ("La vaca" & Integer'Image(V) & " esta entrando al Camion 1");
         else
            C2 := C2 + 1;
            Put_Line ("La vaca" & Integer'Image(V) & " esta entrando al Camion 2");
         end if;
      end Subir;

      function Llenos return Boolean is
      begin
         return C1 = 50 and C2 = 50;
      end Llenos;
   end Camiones;


   -----------
   -- VACA --
   -----------
   task type Vaca (ID : Integer := 0);

   task body Vaca is
      Orden : Integer := Rand_Int.Random (Gen) mod 2;
   begin

      ---------------------------
      -- ORDEN = 0 ORDEÑE→VACUNA
      -- ORDEN = 1 VACUNA→ORDEÑE
      ---------------------------

      if Orden = 0 then
         Put_Line ("La vaca" & Integer'Image(ID) & " esta entrando al area de ordene");
         Ordene.Entrar;
         delay Random_Delay(3);
         Put_Line ("La vaca" & Integer'Image(ID) & " esta saliendo del area de ordene");
         Ordene.Salir;

         Put_Line ("La vaca" & Integer'Image(ID) & " esta entrando al area de vacunacion");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;
         delay Random_Delay(2);
         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;
         Put_Line ("La vaca" & Integer'Image(ID) & " esta saliendo del area de vacunacion");

      else
         Put_Line ("La vaca" & Integer'Image(ID) & " esta entrando al area de vacunacion");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;
         delay Random_Delay(2);
         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;
         Put_Line ("La vaca" & Integer'Image(ID) & " esta saliendo del area de vacunacion");

         Put_Line ("La vaca" & Integer'Image(ID) & " esta entrando al area de ordene");
         Ordene.Entrar;
         delay Random_Delay(3);
         Put_Line ("La vaca" & Integer'Image(ID) & " esta saliendo del area de ordene");
         Ordene.Salir;
      end if;

      Camiones.Subir(ID);

   end Vaca;


   -----------------------
   -- CREAR 100 VACAS  --
   -----------------------
   type Vaca_Access is access Vaca;
   Vacas : array (1 .. 100) of Vaca_Access;

begin
   Rand_Int.Reset(Gen);

   -- Crear cada vaca con su ID
   for I in 1 .. 100 loop
      Vacas(I) := new Vaca(ID => I);
   end loop;

   -- Esperar a que se llenen los camiones
   loop
      exit when Camiones.Llenos;
      delay 0.2;
   end loop;

   Put_Line ("Los dos camiones estan llenos. Fin del proceso.");
end Tambo;
