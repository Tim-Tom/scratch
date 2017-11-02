with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;

with Word_List;
with Decipherer;

procedure Single_Substitution is
   package IO renames Ada.Text_IO;
   
   function "="(a, b: Decipherer.Encrypted_Word) return Boolean renames Decipherer."=";
   function "<"(a, b: Decipherer.Encrypted_Word) return Boolean renames Decipherer."<";
   package Word_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Decipherer.Encrypted_Word);
   -- function "="(a, b: Word_Sets.Cursor) return Boolean renames Word_Sets."=";
   
   line : String (1 .. 512);
   last : Natural;
   word : Decipherer.Encrypted_Word := (others => Decipherer.Encrypted_Char'(' '));
   all_words : Word_Sets.Set;
   patterns : Word_List.Pattern_Set;
begin
   while not IO.End_Of_File loop
      IO.Get_Line(line, last);
      if last = line'Last then
         raise Constraint_Error;
      end if;
      declare
         first : Integer := line'First;
      begin
         for i in line'First .. Last loop
            if not Ada.Characters.Handling.Is_Letter(line(i)) then
               if i > first then
                  for wi in 0 .. i - first - 1 loop
                     word(wi + 1) := Decipherer.Encrypted_Char(Ada.Characters.Handling.To_Lower(line(first + wi)));
                  end loop;
                  all_words.Include(word);
                  patterns.Include(Word_List.Make_Pattern(Decipherer.Image(word)));
                  word(1 .. i - first) := (others => ' ');
               end if;
               first := i + 1;
            end if;
         end loop;
      end;
   end loop;
   declare
      list : constant Word_List.Word_List := Word_List.Build_Word_List("../words.txt", patterns);
      words : Decipherer.Candidate_Set(1 .. Positive(all_words.Length));
      c : Word_Sets.Cursor := all_words.First;
   begin
      for i in words'Range loop
         words(i) := Word_Sets.Element(c);
         c := Word_Sets.Next(c);
      end loop;
      declare
         mapping : Decipherer.Result_Set := Decipherer.Decipher(words, list);
      begin
         null;
      end;
   end;
end;
