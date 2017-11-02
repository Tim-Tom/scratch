with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

package body Decipherer is
   package Character_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Encrypted_Char);
   package IO renames Ada.Text_IO;

   type Mapping_Candidates is Array(Positive range 1 .. 26) of Character;

   type Char_Possibilities is record
      c : Encrypted_Char;
      num_possible : Natural;
      possibilities : Mapping_Candidates;
   end record;

   type Word_Possibilities is record
      is_using_root : Boolean;
      words : Word_List.Word_Vector;
   end record;

   type Char_Possibilities_Array is Array(Positive range <>) of Char_Possibilities;
   type Word_Possibilities_Array is Array(Positive range <>) of Word_Possibilities;
   type Word_Inclusion is Array(Positive range <>, Positive range <>) of Natural;

   type Letter_Toggle is Array(Positive range <>) of Boolean;
   type Word_Toggle is Array(Positive range <>) of Boolean;

   package Word_Vectors renames Word_List.Word_Vectors;
   function "="(a, b: Word_Vectors.Cursor) return Boolean renames Word_Vectors."=";
   procedure Free_Word_Vector is new Ada.Unchecked_Deallocation(Object => Word_Vectors.Vector, Name => Word_List.Word_Vector);

   type Decipher_State (num_words : Positive; num_letters : Positive) is record
      candidates : Candidate_Set(1 .. num_words);
      characters : Char_Possibilities_Array(1 .. num_letters);
      words : Word_Possibilities_Array(1 .. num_words);
      inclusion : Word_Inclusion(1 .. num_letters, 1 .. num_words);
   end record;
   
   function Image(ew: Encrypted_Word) return Word_List.Word is
      w : Word_List.Word;
   begin
      for i in ew'Range loop
         w(i) := Character(ew(i));
      end loop;
      return w;
   end Image;

   function Get_Character_Index(state : Decipher_State; c : Encrypted_Char) return Positive is
   begin
      for ci in state.characters'Range loop
         if c = state.characters(ci).c then
            return ci;
         end if;
      end loop;
      raise Constraint_Error;
   end Get_Character_Index;

   function Is_Word_Valid(state : Decipher_State; candidate : Encrypted_Word; word : Word_List.Word) return Boolean is
   begin
      for i in candidate'Range loop
         exit when candidate(i) = ' ';
         declare
            ci : constant Positive := Get_Character_Index(state, candidate(i));
            c : constant Character := word(i);
            cp : Char_Possibilities renames state.characters(ci);
            found : Boolean := False;
         begin
            if cp.num_possible = 1 then
               found := cp.possibilities(1) = c;
            elsif cp.num_possible = 26 then
               found := True;
            else
               for j in 1 .. cp.num_possible loop
                  if cp.possibilities(j) = c then
                     found := True;
                     exit;
                  end if;
               end loop;
            end if;
            if not found then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Is_Word_Valid;

   procedure Filter_Word_List(state : Decipher_State; candidate : Encrypted_Word; initial : Word_List.Word_Vector; final : Word_List.Word_Vector) is
      cur : Word_Vectors.Cursor := initial.First;
   begin
      while cur /= Word_Vectors.No_Element loop
         if Is_Word_Valid(state, candidate, Word_Vectors.Element(cur)) then
            final.Append(Word_Vectors.Element(cur));
         end if;
         cur := Word_Vectors.Next(cur);
      end loop;
   end Filter_Word_List;
   
   procedure Put_Possible(cp: Char_Possibilities) is
   begin
      for i in 1 .. cp.Num_Possible loop
         IO.Put(cp.possibilities(i));
      end loop;
      IO.New_Line;
   end Put_Possible;

   procedure Make_Unique(state : in out Decipher_State; ci : Positive; success : out Boolean; changed : in out Letter_Toggle) is
      letter : constant Character := state.characters(ci).possibilities(1);
   begin
      success := True;
      IO.Put_Line("Determined that " & Encrypted_Char'Image(state.characters(ci).c) & " has to be a " & Character'Image(letter));
      for i in state.characters'Range loop
         if i /= ci then
            declare
               cp : Char_Possibilities renames state.characters(i);
            begin
               for j in 1 .. cp.num_possible loop
                  if cp.possibilities(j) = letter then
                     if cp.num_possible = 1 then
                        success := False;
                        return;
                     else
                        IO.Put("Before: "); Put_Possible(cp);
                        cp.possibilities(j .. cp.num_possible - 1) := cp.possibilities(j + 1 .. cp.num_possible);
                        cp.num_possible := cp.num_possible - 1;
                        IO.Put("After: "); Put_Possible(cp);
                        changed(i) := True;
                        if cp.num_possible = 1 then
                           IO.Put_Line("Make_Unique from Make_Unique");
                           Make_Unique(state, i, success, changed);
                           if not success then
                              return;
                           end if;
                        end if;
                        exit;
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Make_Unique;

   procedure Constrain_Letters(state : in out Decipher_State;  wi : Positive; success : out Boolean; changed : in out Letter_Toggle) is
      ci : Positive;
      cur : Word_Vectors.Cursor := state.words(wi).words.all.First;
      word : Word_List.Word;
      seen : Array(Positive range 1 .. state.num_letters, Character range 'a' .. 'z') of Boolean := (others => (others => False));
      used : Array(Positive range 1 .. state.num_letters) of Boolean := (others => False);
   begin
      success := True;
      while cur /= Word_Vectors.No_Element loop
         word := Word_Vectors.Element(cur);
         for i in word'Range loop
            exit when word(i) = ' ';
            ci := Get_Character_Index(state, state.candidates(wi)(i));
            seen(ci, word(i)) := True;
            used(ci) := True;
         end loop;
         cur := Word_Vectors.Next(cur);
      end loop;
      for i in used'range loop
         if used(i) then
            IO.Put("Seen: ");
            for c in Character range 'a' .. 'z' loop
               if (seen(i, c)) then
                  IO.Put(c);
               end if;
            end loop;
            IO.New_Line;
            declare
               cp : Char_Possibilities renames state.characters(i);
               shrunk : Boolean := False;
               write_head : Natural := 0;
            begin
               IO.Put("Before: "); Put_Possible(cp);
               for read_head in 1 .. cp.Num_Possible loop
                  if seen(i, cp.possibilities(read_head)) then
                     write_head := write_head + 1;
                     if write_head /= read_head then
                        cp.possibilities(write_head) := cp.possibilities(read_head);
                     end if;
                  else
                     shrunk := True;
                  end if;
               end loop;
               cp.Num_Possible := write_head;
               IO.Put("After: "); Put_Possible(cp);
               if Shrunk then
                  changed(i) := True;
                  if cp.Num_Possible = 0 then
                     success := False;
                     return;
                  elsif cp.Num_Possible = 1 then
                     IO.Put_Line("Make_Unique from Constrain_Letters");
                     Make_Unique(state, i, success, changed);
                     if not success then
                        return;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Constrain_Letters;

   procedure Check_Constraints(state : in out Decipher_State; changed : Letter_Toggle; success : out Boolean) is
      words : Word_Toggle(1 .. state.num_words) := (others => False);
      follow_up : Letter_Toggle(1 .. state.num_letters) := (others => False);
      any_changed : Boolean := False;
   begin
      success := True;
      for i in 1 .. state.num_letters loop
         if changed(i) then
            any_changed := True;
            for j in 1 .. state.num_words loop
               exit when state.inclusion(i, j) = 0;
               words(state.inclusion(i, j)) := True;
            end loop;
         end if;
      end loop;
      if not any_changed then
         return;
      end if;
      for i in 1 .. state.num_words loop
         if words(i) then
            declare
               new_words : Word_List.Word_Vector := new Word_Vectors.Vector;
            begin
               Filter_Word_List(state, state.candidates(i), state.words(i).words, new_words);
               if Natural(new_words.Length) = 0 then
                  Free_Word_Vector(new_words);
                  success := False;
                  return;
               elsif Natural(new_words.Length) = Natural(state.words(i).words.Length) then
                  IO.Put_Line("Word set for " & Positive'Image(i) & "(" & Image(state.candidates(i)) & ") did not shrink from " & Ada.Containers.Count_Type'Image(state.words(i).words.all.Length));
                  Free_Word_Vector(new_Words);
               else
                  IO.Put_Line("Restricting word set for " & Positive'Image(i) & "(" & Image(state.candidates(i)) & ") from " & Ada.Containers.Count_Type'Image(state.words(i).words.all.Length) & " to " & Ada.Containers.Count_Type'Image(new_words.all.Length));
                  if state.words(i).is_using_root then
                     state.words(i).is_using_root := False;
                  else
                     Free_Word_Vector(state.words(i).words);
                  end if;
                  state.words(i).words := new_words;
                  Constrain_Letters(state, i, success, follow_up);
                  if not success then
                     return;
                  end if;
               end if;
            end;
         end if;
      end loop;
      Check_Constraints(state, follow_up, success);
   end Check_Constraints;
   
   procedure Guess_Letter(state : in out Decipher_State; ci : Positive) is
   begin
      if ci > state.num_letters then
         IO.Put_Line("Found Solution");
         for ci in 1 .. state.num_letters loop
            IO.Put(Character(state.characters(ci).c));
            if state.characters(ci).num_possible /= 1 then
               IO.Put_Line(": Invalid State");
               return;
            end if;
         end loop;
         IO.New_Line;
         for ci in 1 .. state.num_letters loop
            IO.Put(state.characters(ci).possibilities(1));
         end loop;
         IO.New_Line;
         for wi in 1 .. state.num_words loop
            IO.Put(Image(state.candidates(wi)) & ": ");
            if Natural(state.words(wi).words.all.Length) = 1 then
               IO.Put_Line(state.words(wi).words.all(1));
            else
               IO.Put_Line("*Invalid: " & Ada.Containers.Count_Type'Image(state.words(wi).words.all.Length) & " Words");
            end if;
         end loop;
         IO.Put_Line("--------------------------");
      elsif state.characters(ci).num_possible = 1 then
         -- Nothing to do, pass it up the line
         Guess_Letter(state, ci + 1);
      else
         declare
            success : Boolean;
            changed : Letter_Toggle(1 .. state.num_letters);
            characters : constant Char_Possibilities_Array := state.characters;
            words : constant Word_Possibilities_Array := state.words;
            cp : Char_Possibilities renames characters(ci);
         begin
            for mi in 1 .. cp.num_possible loop
               changed := (others => False);
               changed(ci) := True;
               state.characters(ci).possibilities(1) := characters(ci).possibilities(mi);
               IO.Put_Line("Guessing " & Character'Image(state.characters(ci).possibilities(mi)) & " for " & Encrypted_Char'Image(state.characters(ci).c));
               state.characters(ci).num_possible := 1;
               for wi in 1 .. state.num_words loop
                  state.words(wi).is_using_root := True;
               end loop;
               IO.Put_Line("Make_Unique from Guess_Letter");
               Make_Unique(state, ci, success, changed);
               if success then
                  Check_Constraints(state, changed, success);
                  if success then
                     Guess_Letter(state, ci + 1);
                  end if;
               end if;
               IO.Put_Line("Restore Letter guess for " & Positive'Image(ci));
               state.characters := characters;
               state.words := words;
            end loop;
         end;
      end if;
   end Guess_Letter;

   function Decipher(candidates: Candidate_Set; words: Word_List.Word_List) return Result_Set is
      letters : Character_Sets.Set;
   begin
      for ci in candidates'Range loop
         for i in candidates(ci)'Range loop
            exit when candidates(ci)(i) = ' ';
            letters.Include(candidates(ci)(i));
         end loop;
      end loop;
      declare
         num_words : constant Positive := Positive(candidates'Length);
         num_letters : constant Positive := Positive(letters.Length);
         result : Result_Set(1 .. num_letters);
         state : Decipher_State(num_words, num_letters);
         cur : Character_Sets.Cursor := letters.First;
      begin
         state.candidates := candidates;
         state.inclusion := (others => (others => 0));
         for i in 1 .. num_letters loop
            state.characters(i).c := Character_Sets.Element(cur);
            state.characters(i).num_possible := 26;
            for l in Character range 'a' .. 'z' loop
               state.characters(i).possibilities(Character'Pos(l) - Character'Pos('a') + 1) := l;
            end loop;
            cur := Character_Sets.Next(cur);
         end loop;
         for i in 1 .. num_words loop
            for l in candidates(Candidates'First + i - 1)'Range loop
               declare
                  c : constant Encrypted_Char := candidates(Candidates'First + i - 1)(l);
                  ci : Positive;
               begin
                  exit when c = ' ';
                  ci := Get_Character_Index(state, c);
                  for mi in 1 .. num_words loop
                     if state.inclusion(ci, mi) = 0 then
                        state.inclusion(ci, mi) := i;
                     elsif state.inclusion(ci, mi) = i then
                        exit;
                     end if;
                  end loop;
               end;
            end loop;
            state.words(i).is_using_root := True;
            state.words(i).words := words.Element(Word_List.Make_Pattern(Image(candidates(i))));
         end loop;
         Guess_Letter(state, 1);
         return result;
      end;
   end Decipher;

end Decipherer;
