with Ada.Strings.Bounded;

package Trie is
   package BoundedString is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 16);
   type Trie is private;
   type Fragment_Endpoints is record
      first, last : Positive;
   end record;
   type Fragment_Endpoint_Array is Array(Positive range <>) of Fragment_Endpoints;
   function Make_Trie(filename : String) return Trie;
   procedure Find_Words(words : Trie; fragments: String; endpoints: Fragment_Endpoint_Array);
private
   
   type Trie_Node;
   type Trie_Node_Access is access Trie_Node;
   type Trie_Descendant is Array (Character range 'a' .. 'z') of Trie_Node_Access;
   type Trie_Node is record
      is_terminal : Boolean;
      word : BoundedString.Bounded_String;
      children : Trie_Descendant;
   end record;
   
   type Trie is record
      root : Trie_Node_Access;
      node_count : Natural;
   end record;
end Trie;
