with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure TableSorting is
   package IO renames Ada.Text_IO;
   package I_IO renames Ada.Integer_Text_IO;

   Num_Elements : constant := 14;
   type Extended_Index is new Integer range 1 .. Num_Elements + 1;
   subtype Index is Extended_Index range 1 .. Num_Elements;

   a : Array(Index'Range) of Integer;
   output_width : constant := 3;
   
   count : Natural := 0;

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

   -- Standard next Lexicographic order permutation function from Knuth modified slightly
   -- to ensure all indexes stay in bounds since I'm in Ada and it checks those things.
   procedure Permute is
      i : Index;
      j : Index;
      temp : Integer;
   begin
      -- Output
      count := count + 1;
      loop
         i := Index'Last;
         j := Index'Last;
         -- Find left pivot (first item that is less than a successor)
         while i > Index'First and then a(i - 1) >= a(i) loop
            i := i - 1;
         end loop;
         -- Dropped off the left side, no more pivots.
         exit when i = Index'First;
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
         -- Output
         count := count + 1;
      end loop;
   end Permute;

   procedure Initialize(start : Extended_Index; size : Index) is
      next : Extended_Index := start;
   begin
      if size = 1 then
         for i in start .. Index'Last loop
            a(i) := 100 + Integer(start);
         end loop;
         count := 0;
         Permute;
         for i in Index'First .. Index'Last / 2 loop
            declare
               j : constant Index := Index'Last - i + 1;
               temp : constant Integer := a(i);
            begin
               a(i) := a(j);
               a(j) := temp;
            end;
         end loop;
         IO.Put(Natural'Image(count) & " iterations to solve: ");
         Print_Array;
      else
         Initialize(next, size - 1);
         while next + size <= Extended_Index'Last loop
            for i in 0 .. size - 1 loop
               a(next + i) := Integer(next);
            end loop;
            next := next + size;
            Initialize(next, size - 1);
         end loop;
      end if;
   end Initialize;
   
begin
   Initialize(Index'First, 4);
   --  for i in Index'Range loop
   --     a(i) := Integer(i);
   --  end loop;
   --  loop
   --     Print_Array;
   --     exit when not Next_Permutation;
   --  end loop;
end TableSorting;
