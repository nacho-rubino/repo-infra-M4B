with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Real_Time;              use Ada.Real_Time;

procedure Tambo is

   package Rand_Int is new Ada.Numerics.Discrete_Random (Integer);

   function Rand_Delay (Max : Integer; Local_Gen : Rand_Int.Generator)
     return Duration
   is
   begin
      return Duration (Float (Rand_Int.Random (Local_Gen) mod Max + 1));
   end Rand_Delay;

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
            Put_Line ("Camión 1: sube vaca" & Integer'Image(V));
            Ada.Text_IO.Flush;------------------------------------------------------------------------
         else
            C2 := C2 + 1;
            Put_Line ("Camión 2: sube vaca" & Integer'Image(V));
            Ada.Text_IO.Flush;
         end if;
      end Subir;

      function Llenos return Boolean is
      begin
         return C1 = 50 and C2 = 50;
      end Llenos;
   end Camiones;

   task type Vaca (ID : Integer);

   task body Vaca is
      Local_Gen : Rand_Int.Generator;
      Orden     : Integer;
   begin
      Rand_Int.Reset(Local_Gen, ID * 17 + 54321);

      Orden := Rand_Int.Random(Local_Gen) mod 2;

      if Orden = 0 then
         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Ordene");
         Ordene.Entrar;
         delay Rand_Delay(3, Local_Gen);
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Ordene");
         Ordene.Salir;

         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Vacunación");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;

         delay Rand_Delay(2, Local_Gen);

         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;

         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Vacunación");

      else
         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Vacunación");
         Pasillo.Entrar;
         Mangas.Entrar;
         Pasillo.Salir;

         delay Rand_Delay(2, Local_Gen);

         Pasillo.Entrar;
         Mangas.Salir;
         Pasillo.Salir;

         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Vacunación");

         Put_Line ("Vaca" & Integer'Image(ID) & " entra a Ordene");
         Ordene.Entrar;
         delay Rand_Delay(3, Local_Gen);
         Put_Line ("Vaca" & Integer'Image(ID) & " sale de Ordene");
         Ordene.Salir;
      end if;

      Camiones.Subir(ID);
   end Vaca;

   type Vaca_Access is access Vaca;
   Vacas : array (1 .. 100) of Vaca_Access;

begin
   Put_Line ("=== INICIO DEL TAMBO ===");

   for I in 1 .. 100 loop
      Vacas(I) := new Vaca(ID => I);
   end loop;

   loop
      exit when Camiones.Llenos;
      delay 0.2;
   end loop;

   Put_Line ("=======================================");
   Put_Line ("LOS DOS CAMIONES ESTÁN LLENOS. FIN.");
   Put_Line ("=======================================");

end Tambo;