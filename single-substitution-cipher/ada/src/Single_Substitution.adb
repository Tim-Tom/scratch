with Ada.Text_IO;
with Word_List;
with Ada.Containers; use Ada.Containers;
with Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;

procedure Single_Substitution is
   package IO renames Ada.Text_IO;

   function "<"(a, b: Word_List.Pattern) return Boolean renames Word_List."<";
   function "="(a, b: Word_List.Pattern) return Boolean renames Word_List."=";
   
   package Word_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Word_List.Word);
   function "="(a, b: Word_Sets.Cursor) return Boolean renames Word_Sets."=";

   line : String (1 .. 512);
   last : Natural;
   word : Word_List.Word := (others => ' ');
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
                     word(wi + 1) := Ada.Characters.Handling.To_Lower(line(first + wi));
                  end loop;
                  Word_Sets.Include(all_words, word);
                  Word_List.Pattern_Sets.Include(patterns, Word_List.Make_Pattern(word));
                  word(1 .. i - first) := (others => ' ');
               end if;
               first := i + 1;
            end if;
         end loop;
      end;
   end loop;
   declare
      list : constant Word_List.Word_List := Word_List.Build_Word_List("../words.txt", patterns);
      words : 
   begin
      
   end;
end;
