with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure MagicPuzzle is
   package IO renames Ada.Text_IO;
   package I_IO renames Ada.Integer_Text_IO;
   goal : constant := 34;
   width : constant := 4;
   wm : constant := width - 1;
   wp : constant := width + 1;
   size : constant := width * width;
   type Extended_Choice_Index is new Integer range -1 .. size -1;
   invalid : constant := Extended_Choice_Index'First;
   subtype Choice_Index is Extended_Choice_Index range invalid + 1 .. Extended_Choice_Index'Last;
   type Value_Index is new Choice_Index;
   -- choices : constant Array (Choice_Index) of Integer := (3,4,5,6,7,8,9,10,11);
   choices : constant Array (Choice_Index) of Integer := (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
   picked : Array (Choice_Index) of Boolean := (others => false);
   a : Array (Value_Index) of Integer;

   function Choice_Search(c : Integer) return Extended_Choice_Index is
   begin
      for i in choices'range loop
         if choices(i) = c then
            return i;
         end if;
      end loop;
      return invalid;
   end Choice_Search;

   function Reverse_Cross_Valid return Boolean is
      i, sum : Integer;
   begin
      i := wm;
      sum := 0;
      loop
         sum := sum + a(Value_Index(i));
         exit when i = width * wm;
         i := i + wm;
      end loop;
      return sum = goal;
   end Reverse_Cross_Valid;

   function Bottom_Right_Valid return Boolean is
      i, sum : Integer;
   begin
      i := size - width;
      sum := 0;
      loop
         sum := sum + a(Value_Index(i));
         i := i + 1;
         exit when i >= size;
      end loop;
      if sum /= goal then
         return False;
      end if;
      i := 0;
      sum := 0;
      loop
         sum := sum + a(Value_Index(i));
         i := i + wp;
         exit when i >= size;
      end loop;
      return sum = goal;
   end Bottom_Right_Valid;

   procedure Pick_Next(i : Value_Index);

   procedure Pick_Internal(i : Value_Index) is
   begin
      for c in Choices'Range loop
         a(i) := choices(c);
         if not picked(c) and then (i /= size - (2*width) + 1 or else Reverse_Cross_Valid) then
            picked(c) := True;
            Pick_Next(i);
            picked(c) := False;
         end if;
      end loop;
   end Pick_Internal;

   procedure Pick_Right(i : Value_Index) is
      n : Integer;
      v : Value_Index;
      c : Extended_Choice_Index;
   begin
      n := 0;
      v := i - i mod width;
      loop
         n := n + a(v);
         v := v + 1;
         exit when v = i;
      end loop;
      n := goal - n;
      c := Choice_Search(n);
      a(i) := n;
      if c /= invalid and then not picked(c) and then (i /= size - 1 or else Bottom_Right_Valid) then
         picked(c) := True;
         Pick_Next(i);
         picked(c) := False;
      end if;
   end Pick_Right;

   procedure Pick_Bottom(i : Value_Index) is
      n : Integer;
      v : Value_Index;
      c : Extended_Choice_Index;
   begin
      n := 0;
      v := i mod width;
      loop
         n := n + a(v);
         v := v + width;
         exit when v = i;
      end loop;
      n := goal - n;
      c := Choice_Search(n);
      a(i) := n;
      if c /= invalid and then not picked(c) and then (i /= size - 1 or else Bottom_Right_Valid) then
         picked(c) := True;
         Pick_Next(i);
         picked(c) := False;
      end if;
   end Pick_Bottom;
   
   Solution_Count : Natural := 0;
   procedure Solution is
      c : Integer := 1;
   begin
      Solution_Count := Solution_Count + 1;
      IO.Put_Line("--- Solution" & Natural'Image(Solution_Count) & " ---");
      for i in Value_Index'Range loop
         I_IO.Put(a(i), Width => 2);
         if c = width then
            IO.New_Line;
            c := 1;
         else
            IO.Put(' ');
            c := c + 1;
         end if;
      end loop;
   end Solution;

   procedure Pick_Next(i : Value_Index) is
      next : Value_Index;
   begin
      if i = size - 1 then
         solution;
         return;
      end if;
      if i >= width * wm then
         next := i - wm;
      elsif i >= width * (width - 2) then
         Pick_Bottom(i + width);
         return;
      else
         next := i + 1;
      end if;
      if next mod width = wm then
         Pick_Right(next);
      else
         Pick_Internal(next);
      end if;
   end Pick_Next;
begin
   Pick_Internal(0);
end MagicPuzzle;
