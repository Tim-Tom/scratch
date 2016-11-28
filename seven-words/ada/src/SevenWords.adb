with Ada.Command_Line;
with Trie;

procedure SevenWords is
   words : constant Trie.Trie := Trie.Make_Trie("../american-english-filtered");
   argument_length : Natural := 0;
   fragment_ends : Trie.Fragment_Endpoint_Array(1 .. Ada.Command_Line.Argument_Count);
begin
   if Ada.Command_Line.Argument_Count = 0 then
      raise Constraint_Error;
   end if;
   for index in 1 .. Ada.Command_Line.Argument_Count loop
      fragment_ends(index).first := argument_length + 1;
      argument_length := argument_length + Ada.Command_Line.Argument(index)'Length;
      fragment_ends(index).last := argument_length;
   end loop;
   declare
      argument_string : String(1 .. argument_length);
   begin
      for index in 1 .. Ada.Command_Line.Argument_Count loop
         argument_string(fragment_ends(index).first .. fragment_ends(index).last) := Ada.Command_Line.Argument(index);
      end loop;
      Trie.find_words(words, argument_string, fragment_ends);
   end;
end SevenWords;
