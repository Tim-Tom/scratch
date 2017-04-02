with Ada.Text_IO;
procedure RandomBet is
   package IO renames Ada.Text_IO;
   Max_Starting_Bet : constant := 255;
   Max_Bet_Pool : constant := Max_Starting_Bet * 3;
   type Offset_Array is Array (1 .. Max_Starting_Bet, 1 .. (Max_Bet_Pool - 1) / 2) of Natural;
   type Memory_Record is record
      Predecessor : Integer;
      Next_Record : Integer;
      P1,P2,P3 : Natural;
   end record;
   
   type Memory_Array is Array (Natural range <>) of Memory_Record;
   type Memory_Array_Access is access Memory_Array;
   Offsets : Offset_Array;
   function Init_Offsets return Natural is
      sum : Natural := 0;
   begin
      for x in Offsets'Range(1) loop
         for y in Offsets'Range(2) loop
            if y < x or 2*y + x > Max_Bet_Pool then
               Offsets(x,y) := 0;
            else
               Offsets(x,y) := sum;
               sum := sum + (Max_Bet_Pool - x - 2*y + 1);
            end if;
         end loop;
      end loop;
      return sum;
   end Init_Offsets;
   function Get_Offset(p1, p2, p3 : Natural) return Natural is
      base : constant Natural := Offsets(p1, p2);
   begin
      return base + p3 - p2;
   end Get_Offset;
   Memory_Size : constant Natural := Init_Offsets;
   Memory : constant Memory_Array_Access := new Memory_Array(0 .. Memory_Size);
   Next_Record : Integer := -1;
   Last_Record : Integer := -1;
   procedure Evaluate_Candidate(predecessor, p1, p2, p3: Natural) is
      offset : Natural;
   begin
      -- IO.Put_Line(Integer'Image(predecessor) & Integer'Image(p1) & Integer'Image(p2) & Integer'Image(p3));
      if p1 = p2 or else p2 = p3 then
         return;
      end if;
      offset := Get_Offset(p1, p2, p3);
      if Memory(offset).Predecessor /= -1 then
         return;
      end if;
      Memory(offset).Predecessor := predecessor;
      Memory(Last_Record).Next_Record := offset;
      Last_Record := offset;
   end Evaluate_Candidate;
   procedure Evaluate_Candidate_Sort(predecessor, p1, p2, p3: Natural) is
   begin
      if p1 <= p2 then
         if p2 <= p3 then
            Evaluate_Candidate(predecessor, p1, p2, p3);
         elsif p1 <= p3 then
            Evaluate_Candidate(predecessor, p1, p3, p2);
         else
            Evaluate_Candidate(predecessor, p3, p1, p2);
         end if;
      elsif p2 <= p3 then
         if p1 <= p3 then
            Evaluate_Candidate(predecessor, p2, p1, p3);
         else
            Evaluate_Candidate(predecessor, p2, p3, p1);
         end if;
      else
         Evaluate_Candidate(predecessor, p3, p2, p1);
      end if;
   end Evaluate_Candidate_Sort;
   procedure Print_Solution(offset : Natural) is
      nr : Integer := offset;
   begin
      while nr /= -1 loop
         IO.Put_Line(Integer'Image(memory(nr).P1) & Integer'Image(memory(nr).P2) & Integer'Image(memory(nr).P3));
         nr := memory(nr).Predecessor;
      end loop;
   end Print_Solution;
   Processed : Natural := 0;
begin
   for x in 1 .. Max_Starting_Bet loop
      for y in x .. (Max_Bet_Pool - x) / 2 loop
         for z in y .. (Max_Bet_Pool - x - y) loop
            declare
               offset : Natural;
            begin
               offset := Get_Offset(x, y, z);
               Memory(offset).P1 := x;
               Memory(offset).P2 := y;
               Memory(offset).P3 := z;
               Memory(offset).Predecessor := -1;
               Memory(offset).Next_Record := -1;
            end;
         end loop;
      end loop;
   end loop;
   for x in 1 .. Max_Starting_Bet loop
      declare
         offset : Integer;
      begin
         if x mod 2 = 0 then
            offset := Get_Offset(x,x,x);
            Memory(offset).Next_Record := Next_Record;
            Next_Record := offset;
         end if;
         for y in x + 1 .. Max_Bet_Pool - 2*x loop
            if x mod 2 = 0 or else y mod 2 = 0 then
               offset := Get_Offset(x,x,y);
               Memory(offset).Next_Record := Next_Record;
               Next_Record := offset;
            end if;
         end loop;
         for y in x + 1 .. (Max_Bet_Pool - x)/2 loop
            if x mod 2 = 0 or else y mod 2 = 0 then
               offset := Get_Offset(x,y,y);
               Memory(offset).Next_Record := Next_Record;
               Next_Record := offset;
            end if;
         end loop;
      end;
   end loop;
   -- 1,1,2 is always the first record encountered.
   Last_Record := Get_Offset(1,1,2);
   while Next_Record /= -1 loop
      processed := processed + 1;
      declare
         mr : Memory_Record renames Memory(Next_Record);
         half : Natural;
      begin
         if mr.P1 mod 2 = 0 then
            half := mr.P1 / 2;
            Evaluate_Candidate_Sort(Next_Record, half, mr.P2 + half, mr.P3);
            Evaluate_Candidate_Sort(Next_Record, half, mr.P2, mr.P3 + half);
         end if;
         if mr.P2 mod 2 = 0 then
            half := mr.P2 / 2;
            Evaluate_Candidate_Sort(Next_Record, half, mr.P1 + half, mr.P3);
            Evaluate_Candidate_Sort(Next_Record, half, mr.P1, mr.P3 + half);
         end if;
         if mr.P3 mod 2 = 0 then
            half := mr.P3 / 2;
            Evaluate_Candidate_Sort(Next_Record, half, mr.P1 + half, mr.P2);
            Evaluate_Candidate_Sort(Next_Record, half, mr.P1, mr.P2 + half);
         end if;
         Next_Record := mr.Next_Record;
      end;
   end loop;
   IO.Put_Line("Processed " & Integer'Image(processed) & " records");
   Print_Solution(Get_Offset(175, 199, 223));
end RandomBet;
