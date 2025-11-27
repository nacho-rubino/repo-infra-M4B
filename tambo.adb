with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Tambo is

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


   protected Log is
      procedure P (S : String);
   end Log;

   protected body Log is
      procedure P (S : String) is
      begin
         Put_Line(S);
      end;
   end Log;

   protected Pasillo is
      entry Entrar (Id : Integer);
      entry Salir  (Id : Integer);
   private
      Ocupado : Boolean := False;
   end Pasillo;

   protected body Pasillo is

      entry Entrar (Id : Integer)
        when not Ocupado
      is
      begin
         Ocupado := True;
         Log.P("La vaca" & Integer'Image(Id) & " esta entrando al pasillo");
      end Entrar;

      entry Salir (Id : Integer)
        when Ocupado
      is
      begin
         Ocupado := False;
         Log.P("La vaca" & Integer'Image(Id) & " esta saliendo del pasillo");
      end Salir;

   end Pasillo;


   protected Ordenie is
      entry Esperar_Lugar;         -- Bloquea hasta que haya lugar
      entry Entrar (Id : Integer); -- Entra al área
      entry Salir  (Id : Integer); -- Sale del área
   private
      Ocupacion : Integer := 0;
   end Ordenie;

   protected body Ordenie is

      entry Esperar_Lugar
        when Ocupacion < Capacidad_Ordenie
      is
      begin
         null;  -- solo espera
      end Esperar_Lugar;

      entry Entrar (Id : Integer)
        when Ocupacion < Capacidad_Ordenie
      is
      begin
         Ocupacion := Ocupacion + 1;
         Log.P("La vaca" & Integer'Image(Id) & " esta entrando al area de ordenie");
      end Entrar;

      entry Salir (Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Log.P("La vaca" & Integer'Image(Id) & " esta saliendo del area de ordenie");
      end Salir;

   end Ordenie;


   protected Vacunacion is
      entry Esperar_Lugar;
      entry Entrar (Id : Integer);
      entry Salir  (Id : Integer);
   private
      Ocupacion : Integer := 0;
   end Vacunacion;

   protected body Vacunacion is

      entry Esperar_Lugar
        when Ocupacion < Capacidad_Vacunas
      is
      begin
         null;
      end Esperar_Lugar;

      entry Entrar (Id : Integer)
        when Ocupacion < Capacidad_Vacunas
      is
      begin
         Ocupacion := Ocupacion + 1;
         Log.P("La vaca" & Integer'Image(Id) & " esta entrando al area de vacunacion");
      end Entrar;

      entry Salir (Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Log.P("La vaca" & Integer'Image(Id) & " esta saliendo del area de vacunacion");
      end Salir;

   end Vacunacion;

   protected Camiones is
      entry Subir (Id : Integer);
      function Total return Integer;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is

      entry Subir (Id : Integer)
        when (C1 < Capacidad_Camion) or else (C2 < Capacidad_Camion)
      is
      begin
         if C1 < Capacidad_Camion then
            C1 := C1 + 1;
            Log.P("La vaca" & Integer'Image(Id) & " esta subiendo al Camion 1");
         else
            C2 := C2 + 1;
            Log.P("La vaca" & Integer'Image(Id) & " esta subiendo al Camion 2");
         end if;
      end Subir;

      function Total return Integer is
      begin
         return C1 + C2;
      end Total;

   end Camiones;


   task type Vaca (Id : Integer);

   task body Vaca is
      R : Rango_Azar := Azar_Entero.Random(Generador_Global);
      Primero_Vacuna : Boolean := (R mod 2 = 0);
   begin
      if Primero_Vacuna then

         Vacunacion.Esperar_Lugar;
         Pasillo.Entrar(Id);
         Pasillo.Salir(Id);
         Vacunacion.Entrar(Id);

         delay Tiempo_Aleatorio(2.0);

         Vacunacion.Salir(Id);

         Ordenie.Esperar_Lugar;
         Pasillo.Entrar(Id);
         Pasillo.Salir(Id);
         Ordenie.Entrar(Id);

         delay Tiempo_Aleatorio(3.0);

         Ordenie.Salir(Id);

      else

         Ordenie.Esperar_Lugar;
         Pasillo.Entrar(Id);
         Pasillo.Salir(Id);
         Ordenie.Entrar(Id);

         delay Tiempo_Aleatorio(3.0);

         Ordenie.Salir(Id);

         Vacunacion.Esperar_Lugar;
         Pasillo.Entrar(Id);
         Pasillo.Salir(Id);
         Vacunacion.Entrar(Id);

         delay Tiempo_Aleatorio(2.0);

         Vacunacion.Salir(Id);

      end if;

      Camiones.Subir(Id);

   end Vaca;

   type Ptr_Vaca is access Vaca;
   Vacas : array (1 .. Cantidad_Vacas) of Ptr_Vaca;

begin
   Azar_Entero.Reset(Generador_Global);

   for I in Vacas'Range loop
      Vacas(I) := new Vaca(I);
   end loop;

   loop
      exit when Camiones.Total = Cantidad_Vacas;
      delay 0.05;
   end loop;
   
end Tambo;
