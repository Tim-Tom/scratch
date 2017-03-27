with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Sequential_IO;

procedure RandomArt is
   package IO renames Ada.Text_IO;
   package I_IO renames Ada.Integer_Text_IO;
   type Byte is mod 256;
   for Byte'Size use 8;
   package Byte_IO is new Ada.Sequential_IO(Byte);
   Scale : constant := 150;
   Size : constant := 2*Scale + 1;
   procedure GreyScale is
      file : Byte_IO.File_Type;
      filename : constant String := "test.pgm";
      header : constant String := "P5 301 301 255";
   begin
      Byte_IO.Create(File => file, Name => filename);
      for i in header'Range loop
         Byte_IO.Write(file, Byte(Character'Pos(header(i))));
      end loop;
      Byte_IO.Write(file, 10); -- newline
      for xi in -Scale .. Scale loop
         for yi in -Scale .. Scale loop
            declare
               x : constant Float := Float(xi) / Float(Scale);
               y : constant Float := Float(xi) / Float(Scale);
               intensity : constant Byte := Byte(127.5 + 127.5*(x + y)/2.0);
            begin
               Byte_IO.Write(file, intensity);
            end;
         end loop;
      end loop;
   end GreyScale;
begin
   GreyScale;
end RandomArt;
