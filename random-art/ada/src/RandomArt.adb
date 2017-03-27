with Ada.Sequential_IO;
with Expressions; use Expressions;

procedure RandomArt is
   type Byte is mod 256;
   for Byte'Size use 8;
   package Byte_IO is new Ada.Sequential_IO(Byte);
   Scale : constant := 150;
   Size : constant := 2*Scale + 1;
   procedure GreyScale(node : not null Expression_Node_Access) is
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
               x : constant Expression_Value := Float(xi) / Float(Scale);
               y : constant Expression_Value := Float(yi) / Float(Scale);
               intensity : constant Byte := Byte(127.5 + 127.5*Evaluate_Expression(node, x, y));
            begin
               Byte_IO.Write(file, intensity);
            end;
         end loop;
      end loop;
   end GreyScale;
   gen : Rand.Generator;
   root : Expression_Node_Access;
begin
   Rand.reset(gen);
   root := Build_Expression_tree(10, gen);
   GreyScale(root);
end RandomArt;
