with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Real_Time; use Ada.Real_Time;

procedure Tambo is

   package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);
   Gen : Rand_Int.Generator;

   function Random_Delay (Max : Integer) return Duration is
   begin
      return Duration(Rand_Int.Random (Gen) mod Max + 1);
   end Random_Delay;

   protected Ordeñe is
      entry Entrar;
      procedure Salir;
   private
      Capacidad : Integer := 15;
   end Ordeñe;

   protected body Ordeñe is
      entry Entrar when Capacidad > 0 is
      begin
         Capacidad := Capacidad - 1;
      end Entrar;

      procedure Salir is
      begin
         Capacidad := Capacidad + 1;
      end Salir;
   end Ordeñe;

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

   protected Camiones is
      entry Subir (Vaca : Integer);
      function Llenos return Boolean;
   private
      Cam1 : Integer := 0;
      Cam2 : Integer := 0;
   end Camiones;

   protected body Camiones is
      entry Subir (Vaca : Integer) when Cam1 < 50 or Cam2 < 50 is
      begin
         if Cam1 < 50 then
            Cam1 := Cam1 + 1;
            Put_Line ("La vaca " & Integer'Image(Vaca) & " está entrando al Camión 1");
         else
            Cam2 := Cam2 + 1;
            Put_Line ("La vaca " & Integer'Image(Vaca) & " está entrando al Camión 2");
         end if;
      end Subir;

      function Llenos return Boolean is
      begin
         return Cam1 = 50 and Cam2 = 50;
      end Llenos;
   end Camiones;

   task type Vaca (ID : Integer);

   task body Vaca is
   begin

      Put_Line ("La vaca" & Integer'Image(ID) & " está entrando al área de ordeñe");
      Ordeñe.Entrar;
      delay Random_Delay(3);
      Put_Line ("La vaca" & Integer'Image(ID) & " está saliendo al área de ordeñe");
      Ordeñe.Salir;

      Put_Line ("La vaca" & Integer'Image(ID) & " está entrando al área de vacunación");
      Pasillo.Entrar;
      Mangas.Entrar;
      Pasillo.Salir;

      delay Random_Delay(2);

      Mangas.Salir;
      Put_Line ("La vaca" & Integer'Image(ID) & " está saliendo al área de vacunación");

      Camiones.Subir(ID);
   end Vaca;

   Vacas : array (1 .. 100) of Vaca;

begin
   Rand_Int.Reset(Gen);

   loop
      exit when Camiones.Llenos;
      delay 0.5;
   end loop;

   Put_Line ("Los dos camiones están llenos. Fin del proceso.");
end Tambo;