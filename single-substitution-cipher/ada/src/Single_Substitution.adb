with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Command_Line;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;

with Word_List;
with Decipherer;

procedure Single_Substitution is
   package IO renames Ada.Text_IO;
   package S_IO renames Ada.Text_IO.Text_Streams;
   package CH renames Ada.Characters.Handling;
   
   function "="(a, b: Decipherer.Encrypted_Word) return Boolean renames Decipherer."=";
   function "<"(a, b: Decipherer.Encrypted_Word) return Boolean renames Decipherer."<";
   package Word_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Decipherer.Encrypted_Word);
   
   line : String (1 .. 512);
   last : Natural;
   word : Decipherer.Encrypted_Word := (others => Decipherer.Encrypted_Char'(' '));
   all_words : Word_Sets.Set;
   patterns : Word_List.Pattern_Set;
   procedure Get_Words(file : IO.File_Type) is
   begin
      while not IO.End_Of_File(file) loop
         IO.Get_Line(file, line, last);
         if last = line'Last then
            raise Constraint_Error;
         end if;
         declare
            first : Integer := line'First;
         begin
            for i in line'First .. Last loop
               if not CH.Is_Letter(line(i)) then
                  if i > first then
                     for wi in 0 .. i - first - 1 loop
                        word(wi + 1) := Decipherer.Encrypted_Char(CH.To_Lower(line(first + wi)));
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
   end Get_Words;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      IO.Put_Line(IO.Standard_Error, "Two Arguments Required");
      Ada.Command_Line.Set_Exit_Status(1);
      return;
   end if;
   declare
      word_filename : constant String := Ada.Command_Line.Argument(1);
      content_filename : constant String := Ada.Command_Line.Argument(2);
      content : IO.File_Type;
   begin
      IO.Open(content, IO.In_File, content_filename);
      Get_Words(content);
      IO.Close(content);
      declare
         list : constant Word_List.Word_List := Word_List.Build_Word_List(word_filename, patterns);
         words : Decipherer.Candidate_Set(1 .. Positive(all_words.Length));
         c : Word_Sets.Cursor := all_words.First;
      begin
      for i in words'Range loop
         words(i) := Word_Sets.Element(c);
         c := Word_Sets.Next(c);
      end loop;
         declare
            mappings : constant Decipherer.Result_Vector := Decipherer.Decipher(words, list);
            length : constant Natural := Natural(mappings.Length);
            mapping : Decipherer.Result_Set;
            c : Character;
            ec : Decipherer.Encrypted_Char;
            content_stream : S_IO.Stream_Access;
         begin
            if length = 0 then
               IO.Put_Line("No solutions found");
            end if;
            for i in 1 .. length loop
               IO.Put_Line("===== Solution" & Integer'Image(i) & " of" & Integer'Image(length) & " =====");
               mapping := mappings.Element(i);
               IO.Open(content, IO.In_File, content_filename);
               content_stream := S_IO.Stream(content);
               while not IO.End_Of_File(content) loop
                  Character'Read(content_stream, c);
                  -- IO.Get(content, c);
                  if CH.Is_Letter(c) then
                     ec := Decipherer.Encrypted_Char(CH.To_Lower(c));
                     if CH.Is_Upper(c) and mapping(ec) /= '.' then
                        c := Character'Val(Character'Pos(mapping(ec)) - Character'Pos('a') + Character'Pos('A'));
                     else
                        c := mapping(ec);
                     end if;
                  end if;
                  IO.Put(IO.Standard_Output, c);
               end loop;
               IO.Close(content);
            end loop;
         end;
      end;
   end;
end;
