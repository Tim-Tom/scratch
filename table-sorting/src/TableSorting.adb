with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure TableSorting is
   package IO renames Ada.Text_IO;
   package I_IO renames Ada.Integer_Text_IO;
   
   Num_Elements : constant := 5;
   type Index is new Integer range 1 .. Num_Elements;

   a : Array(Index'Range) of Integer;
   output_width : constant := 1;
   -- standard next Lexicographic order permutation function from knuth modified slightly
   -- to ensure all indexes stay in bounds since I'm in Ada and it checks those things.
   function Next_Permutation return Boolean is
      i : Index := Index'Last;
      j : Index := Index'Last;
      temp : Integer;
   begin
      -- Find left pivot (first item that is less than a successor)
      while i > Index'First and then a(i - 1) >= a(i) loop
         i := i - 1;
      end loop;
      -- Dropped off the left side, no more pivots.
      if i = Index'First then
         return False;
      end if;
      -- Previously i was the head of the ascending suffix, change it to be the pivot.
      i := i - 1;
      -- Find the rightmost element that exceeds our pivot
      while a(j) <= a(i) loop
         j := j - 1;
      end loop;
      -- Swap the pivot and that element
      temp := a(i);
      a(i) := a(j);
      a(j) := temp;
      -- Reverse the suffix
      i := i + 1;
      j := Index'Last;
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
      for i in Index'Range loop
         I_IO.put(a(i), Width => output_width);
         if i = Index'Last then
            IO.New_Line;
         else
            IO.Put(' ');
         end if;
      end loop;
   end Print_Array;
begin
   for i in Index'Range loop
      a(i) := Integer(i);
   end loop;
   loop
      Print_Array;
      exit when not Next_Permutation;
   end loop;
end TableSorting;
