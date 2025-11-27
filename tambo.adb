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
   -- ÁREA DE ORDEÑE
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
         Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                   " está entrando al área de ordeñe");
      end Entrar;

      entry Salir (Vaca_Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                   " está saliendo del área de ordeñe");
      end Salir;

   end Ordenie;


   -----------------------------------------------------------------------
   -- ÁREA DE VACUNACIÓN
   -----------------------------------------------------------------------
   protected Vacunacion is
      entry Entrar_Vacuna (Vaca_Id : Integer);
      entry Salir_Vacuna  (Vaca_Id : Integer);
   private
      Ocupacion : Integer := 0;
   end Vacunacion;

   protected body Vacunacion is

      entry Entrar_Vacuna (Vaca_Id : Integer)
        when Ocupacion < Capacidad_Vacunas
      is
      begin
         Ocupacion := Ocupacion + 1;
         Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                   " está entrando al área de vacunación");
      end Entrar_Vacuna;

      entry Salir_Vacuna (Vaca_Id : Integer)
        when Ocupacion > 0
      is
      begin
         Ocupacion := Ocupacion - 1;
         Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                   " está saliendo del área de vacunación");
      end Salir_Vacuna;

   end Vacunacion;


   -----------------------------------------------------------------------
   -- CAMIONES (PRODUCTOR–CONSUMIDOR)
   -----------------------------------------------------------------------
   protected Camiones is
      entry Subir (Vaca_Id : Integer);
   private
      Camion1 : Integer := 0;
      Camion2 : Integer := 0;
   end Camiones;

   protected body Camiones is
      entry Subir (Vaca_Id : Integer)
        when (Camion1 < Capacidad_Camion) or else (Camion2 < Capacidad_Camion)
      is
      begin
         if Camion1 < Capacidad_Camion then
            Camion1 := Camion1 + 1;
            Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                      " está subiendo al Camión 1");
         else
            Camion2 := Camion2 + 1;
            Put_Line ("La vaca" & Integer'Image(Vaca_Id) &
                      " está subiendo al Camión 2");
         end if;
      end Subir;
   end Camiones;


   -----------------------------------------------------------------------
   -- TAREA VACA
   -----------------------------------------------------------------------
   task type Vaca (Identificador : Integer);

   task body Vaca is
      Valor_Azar   : Rango_Azar := Azar_Entero.Random(Generador_Global);
      Primero_Vacuna : Boolean := (Valor_Azar mod 2 = 0);
   begin
      if Primero_Vacuna then
         -- VACUNACIÓN PRIMERO
         Vacunacion.Entrar_Vacuna(Identificador);
         delay Tiempo_Aleatorio(2.0);
         Vacunacion.Salir_Vacuna(Identificador);

         Ordenie.Entrar(Identificador);
         delay Tiempo_Aleatorio(3.0);
         Ordenie.Salir(Identificador);

      else
         -- ORDEÑE PRIMERO
         Ordenie.Entrar(Identificador);
         delay Tiempo_Aleatorio(3.0);
         Ordenie.Salir(Identificador);

         Vacunacion.Entrar_Vacuna(Identificador);
         delay Tiempo_Aleatorio(2.0);
         Vacunacion.Salir_Vacuna(Identificador);
      end if;

      Camiones.Subir(Identificador);
   end Vaca;


   -----------------------------------------------------------------------
   -- CREACIÓN DE VACAS
   -----------------------------------------------------------------------
   type Acceso_Vaca is access Vaca;
   Vacas : array (1 .. Cantidad_Vacas) of Acceso_Vaca;

begin
   Azar_Entero.Reset(Generador_Global);

   for I in Vacas'Range loop
      Vacas(I) := new Vaca(I);
   end loop;

end Tambo;
