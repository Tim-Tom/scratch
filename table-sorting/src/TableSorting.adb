with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure TableSorting is
   package IO renames Ada.Text_IO;
   package I_IO renames Ada.Integer_Text_IO;

   c : Array(1 .. 5) of Character;
   a : Array(1 .. 5) of Integer;
   output_width : constant := 1;
   function Next_Permutation return Boolean is
      i : Integer := a'Last;
      j : Integer := a'Last;
      temp : Integer;
   begin
      while i > a'First and then a(i - 1) >= a(i) loop
         i := i - 1;
      end loop;
      if i = a'First then
         return False;
      end if;
      c(i - 1) := 'v';
      while a(j) <= a(i-1) loop
         j := j - 1;
      end loop;
      c(j) := 'v';
      temp := a(i - 1);
      a(i - 1) := a(j);
      a(j) := temp;
      j := a'Last;
      while i < j loop
         temp := a(i);
         a(i) := a(j);
         a(j) := temp;
         i := i + 1;
         j := j - 1;
      end loop;
      return True;
   end Next_Permutation;
   procedure Print_Array is
   begin
      for i in c'Range loop
         IO.Put(c(i));
         if i = c'Last then
            IO.New_Line;
         else
            IO.Put(' ');
         end if;
         c(i) := ' ';
      end loop;
      for i in a'Range loop
         I_IO.put(a(i), Width => output_width);
         if i = a'Last then
            IO.New_Line;
         else
            IO.Put(' ');
         end if;
      end loop;
   end Print_Array;
begin
   for i in a'Range loop
      a(i) := i;
      c(i) := ' ';
   end loop;
   loop
      Print_Array;
      exit when not Next_Permutation;
   end loop;
end TableSorting;
