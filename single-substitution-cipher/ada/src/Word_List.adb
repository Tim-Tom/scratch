with Ada.Text_IO;

package body Word_List is
   package IO renames Ada.Text_IO;
   
   use Word_Lists;
   use Pattern_Sets;

   function "<"(a, b: pattern) return Boolean is
   begin
      return String(a) < String(b);
   end "<";

   function "="(a, b: pattern) return Boolean is
   begin
      return String(a) = String(b);
   end "=";

   function Make_Pattern(w : Word) return Pattern is
      p : Pattern := (others => ' ');
      seen : Array(1 .. 26) of Character;
      seen_max : Natural := 0;
      function Map_Letter(c : Character) return Character is
      begin
         for i in 1 .. seen_max loop
            if seen(i) = c then
               return Character'Val(Character'Pos('a') + i);
            end if;
         end loop;
         seen_max := seen_max + 1;
         seen(seen_max) := c;
         return Character'Val(Character'Pos('a') + seen_max);
      end Map_Letter;
   begin
      for i in w'Range loop
         exit when w(i) = ' ';
         p(i) := Map_Letter(w(i));
      end loop;
      return p;
   end Make_Pattern;

   function Build_Word_List(filename : String; patterns : Pattern_Set) return Word_List is
      list : Word_List;
      input : IO.File_Type;
      w : Word;
      p : Pattern;
      last : Natural;
      c : Word_Lists.Cursor;
      inserted : Boolean;
   begin
      IO.Open(input, IO.In_File, filename);
      while not IO.End_Of_File(input) loop
         IO.Get_Line(input, w, last);
         if last = Word'Last then
            IO.Put_Line(w);
            raise Constraint_Error;
         end if;
         w(last + 1 .. Word'Last) := (others => ' ');
         p := Make_Pattern(w);

         if Pattern_Sets.Find(patterns, p) /= Pattern_Sets.No_Element then
            c := Word_Lists.Find(list, p);
            if c = Word_Lists.No_Element then
               Word_Lists.Insert(list, p, new Word_Vectors.Vector, c, inserted);
            end if;
            Word_Vectors.Append(Word_Lists.Element(c).all, w);
         end if;
      end loop;
      return list;
   end Build_Word_List;
end Word_List;
