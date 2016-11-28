with Ada.Text_IO;
package body Trie is
   package IO renames Ada.Text_IO;

   procedure Add_Word(node : in out Trie_Node; word, partial : String; node_count : in out Natural) is
      first : Character;
   begin
      if partial'Length = 0 then
         node.is_terminal := true;
         node.word := BoundedString.To_Bounded_String(word);
      else
         first := partial(partial'First);
         if node.children(first) = null then
            node_count := node_count + 1;
            node.children(first) := new Trie_Node;
         end if;
         Add_Word(node.children(first).all, word, partial(partial'First + 1 .. partial'Last), node_count);
      end if;
   end Add_Word;

   function Find_Partial(node : in Trie_Node_Access; word : String) return Trie_Node_Access is
   begin
      if node = null or else word'Length = 0 then
         return node;
      else
         return Find_Partial(node.all.children(word(word'First)), word(word'First + 1 .. word'Last));
      end if;
   end Find_Partial;

   function Make_Trie(filename : String) return Trie is
      input : IO.File_Type;
      result : Trie;
      word : String (1 .. 32);
      last : Natural;
   begin
      IO.Open(input, IO.In_File, filename);
      result.node_count := 0;
      result.root := new Trie_Node;
      while not IO.End_Of_File(input) loop
         IO.Get_Line(input, word, last);
         if last = word'Last then
            raise Constraint_Error;
         elsif last > 3 and last < BoundedString.Max_Length then
            Add_Word(result.root.all, word(1 .. last), word(1 .. last), result.node_count);
         end if;
      end loop;
      return result;
   end Make_Trie;

   procedure Find_Words(words : Trie; fragments: String; endpoints: Fragment_Endpoint_Array) is
      taken : Array (fragments'Range) of Boolean := (others => False);
      procedure Find_Words_Helper(node : Trie_Node_Access ) is
         new_node : Trie_Node_Access;
      begin
         for index in endpoints'Range loop
            if not taken(index) then
               new_node := Find_Partial(node, fragments(endpoints(index).first .. endpoints(index).last));
               if new_node /= null then
                  taken(index) := True;
                  if new_node.all.is_terminal then
                     IO.Put_Line(BoundedString.To_String(new_node.all.word));
                  end if;
                  Find_Words_Helper(new_node);
                  taken(index) := False;
               end if;
            end if;
         end loop;
      end Find_Words_Helper;
   begin
      IO.Put_Line(fragments);
      Find_Words_Helper(words.root);
   end Find_Words;
end Trie;
