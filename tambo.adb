with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Tambo is

   -----------------------------------------------------------------------
   -- CONFIGURACIÓN GENERAL
   -----------------------------------------------------------------------
   Cantidad_Vacas     : constant Integer := 100;
   Capacidad_Ordenie  : constant Integer := 15;
   Capacidad_Vacunas  : constant Integer := 5;
   Capacidad_Camion   : constant Integer := 50;

   subtype Rango_Azar is Integer range 0 .. 1000;
   package Azar_Entero is new Ada.Numerics.Discrete_Random(Rango_Azar);
   Generador_Global : Azar_Entero.Generator;

   function Tiempo_Aleatorio (Max : Float) return Duration is
      Valor : Rango_Azar := Azar_Entero.Random(Generador_Global);
   begin
      return Duration (Float(Valor) / 1000.0 * Max);
   end Tiempo_Aleatorio;

   -----------------------------------------------------------------------
   -- PASILLO (solo 1 vaca a la vez)
   -----------------------------------------------------------------------
   protected Pasillo is
      entry Entrar (Vaca_Id : Integer);
      entry Salir  (Vaca_Id : Integer);
   private
      Ocupado : Boolean := False;
   end Pasillo;

   protected body Pasillo is

      entry Entrar (Vaca_Id : Integer)
        when not Ocupado
      is
      begin
         Ocupado := True;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta entrando al pasillo");
      end Entrar;

      entry Salir (Vaca_Id : Integer)
        when Ocupado
      is
      begin
         Ocupado := False;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta saliendo del pasillo");
      end Salir;

   end Pasillo;


   -----------------------------------------------------------------------
   -- ORDEÑE (capacidad 15)
   -----------------------------------------------------------------------
   protected Ordenie is
      entry Entrar (Vaca_Id : Integer);
      entry Salir  (Vaca_Id : Integer);
   private
      Ocupacion : Integer := 0;
   end Ordenie;

   protected body Ordenie is

      entry Entrar (Vaca_Id : Integer)
        when Ocupacion < Capacidad_Ordenie
      is
      begin
         Ocupacion := Ocupacion + 1;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta entrando al area de ordenie");
      end Entrar;

      entry Salir (Vaca_Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta saliendo del area de ordenie");
      end Salir;

   end Ordenie;


   -----------------------------------------------------------------------
   -- VACUNACION (capacidad 5)
   -----------------------------------------------------------------------
   protected Vacunacion is
      entry Entrar (Vaca_Id : Integer);
      entry Salir  (Vaca_Id : Integer);
   private
      Ocupacion : Integer := 0;
   end Vacunacion;

   protected body Vacunacion is

      entry Entrar (Vaca_Id : Integer)
        when Ocupacion < Capacidad_Vacunas
      is
      begin
         Ocupacion := Ocupacion + 1;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta entrando al area de vacunacion");
      end Entrar;

      entry Salir(Vaca_Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta saliendo del area de vacunacion");
      end Salir;

   end Vacunacion;


   -----------------------------------------------------------------------
   -- CAMIONES (productor/consumidor)
   -----------------------------------------------------------------------
   protected Camiones is
      entry Subir (Vaca_Id : Integer);
      function Total return Integer;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is

      entry Subir (Vaca_Id : Integer)
        when (C1 < Capacidad_Camion) or else (C2 < Capacidad_Camion)
      is
      begin
         if C1 < Capacidad_Camion then
            C1 := C1 + 1;
            Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta subiendo al Camion 1");
         else
            C2 := C2 + 1;
            Put_Line("La vaca" & Integer'Image(Vaca_Id) & " esta subiendo al Camion 2");
         end if;
      end Subir;

      function Total return Integer is
      begin
         return C1 + C2;
      end Total;

   end Camiones;


   -----------------------------------------------------------------------
   -- TAREA VACA
   -----------------------------------------------------------------------
   task type Vaca (Id : Integer);

   task body Vaca is
      Valor : Rango_Azar := Azar_Entero.Random(Generador_Global);
      Primero_Vacuna : Boolean := (Valor mod 2 = 0);
   begin
      if Primero_Vacuna then
         ----------------------------------------------------------------
         -- VACUNACION
         ----------------------------------------------------------------
         Pasillo.Entrar(Id);
         Vacunacion.Entrar(Id);
         Pasillo.Salir(Id);

         delay Tiempo_Aleatorio(2.0);

         Vacunacion.Salir(Id);

         ----------------------------------------------------------------
         -- ORDEÑE
         ----------------------------------------------------------------
         Pasillo.Entrar(Id);
         Ordenie.Entrar(Id);
         Pasillo.Salir(Id);

         delay Tiempo_Aleatorio(3.0);

         Ordenie.Salir(Id);

      else
         ----------------------------------------------------------------
         -- ORDEÑE
         ----------------------------------------------------------------
         Pasillo.Entrar(Id);
         Ordenie.Entrar(Id);
         Pasillo.Salir(Id);

         delay Tiempo_Aleatorio(3.0);

         Ordenie.Salir(Id);

         ----------------------------------------------------------------
         -- VACUNACION
         ----------------------------------------------------------------
         Pasillo.Entrar(Id);
         Vacunacion.Entrar(Id);
         Pasillo.Salir(Id);

         delay Tiempo_Aleatorio(2.0);

         Vacunacion.Salir(Id);

      end if;

      ----------------------------------------------------------------
      -- CAMIÓN (siempre al final)
      ----------------------------------------------------------------
      Camiones.Subir(Id);

   end Vaca;


   -----------------------------------------------------------------------
   -- CREAR LAS VACAS
   -----------------------------------------------------------------------
   type Ptr_Vaca is access Vaca;
   Arr : array (1 .. Cantidad_Vacas) of Ptr_Vaca;

begin
   Azar_Entero.Reset(Generador_Global);

   for I in Arr'Range loop
      Arr(I) := new Vaca(I);
   end loop;

   -- Esperar todas
   loop
      exit when Camiones.Total = Cantidad_Vacas;
      delay 0.05;
   end loop;

   Put_Line("Simulacion finalizada: total vacas = " & Integer'Image(Camiones.Total));

end Tambo;
