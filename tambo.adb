with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar;
use Ada.Calendar;
with Ada.Real_Time;
use Ada.Real_Time;

procedure Tambo is
   package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);
   Gen : Rand_Int.Generator;

   function Rand_Delay (Max : Integer) return Duration is
   begin
      return Duration (Float (Rand_Int.Random (Gen) mod Max + 1));
   end Rand_Delay;

   -- AREA DE ORDENE

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

   -- PASILLO

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

   -- MANGAS

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

   -- CAMIONES

   protected Camiones is
      entry Subir (V : Integer);
      function Llenos return Boolean;
   private
      C1 : Integer := 0;
      C2 : Integer := 0;
   end Camiones;

   protected body Camiones is
      entry Subir (V : Integer) when (C1 < 50 or C2 < 50) is
      begin
         if C1 < 50 then
            C1 := C1 + 1;
            Put_Line ("Camion 1: sube vaca" & Integer'Image(V));
         else
            C2 := C2 + 1;
            Put_Line ("Camion 2: sube vaca" & Integer'Image(V));
         end if;
      end Subir;

      function Llenos return Boolean is
      begin
         return C1 = 50 and C2 = 50;
      end Llenos;
   end Camiones;

   -- TASK VACA

   task type Vaca (ID : Integer);

   task body Vaca is
      Orden : Integer := Rand_Int.Random (Gen) mod 2;
   begin
      if Orden = 0 then
         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Ordene");
         Ordene.Entrar;
         delay Rand_Delay (3);
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Ordene");
         Ordene.Salir;

         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Vacunacion");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;

         delay Rand_Delay (2);

         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Vacunacion");

      else
         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Vacunacion");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;

         delay Rand_Delay (2);

         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Vacunacion");

         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Ordene");
         Ordene.Entrar;
         delay Rand_Delay (3);
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Ordene");
         Ordene.Salir;
      end if;

      Camiones.Subir (ID);

   exception
      when others =>
         Put_Line ("ERROR en vaca" & Integer'Image(ID));
   end Vaca;

   -- CREACION DE 100 VACAS

   type Vaca_Access is access Vaca;
   Vacas : array (1 .. 100) of Vaca_Access;

begin
   Rand_Int.Reset (Gen);

   for I in 1 .. 100 loop
      Vacas(I) := new Vaca (ID => I);
   end loop;

   loop
      exit when Camiones.Llenos;
      delay 0.3;
   end loop;

   Put_Line ("=======================================");
   Put_Line ("LOS DOS CAMIONES ESTAN LLENOS. FIN.");
   Put_Line ("=======================================");
end Tambo;
