with Word_List;
with Ada.Containers.Vectors;

package Decipherer is

   type Encrypted_Char is new Character;
   type Encrypted_Word is Array(Positive range 1 .. Word_List.Word'Last) of Encrypted_Char;
   type Candidate_Set is Array(Positive range <>) of Encrypted_Word;

   -- A mapping from the encrypted character to its partner. If the character is not in
   -- the input set, it will map to a period.
   type Result_Set is Array(Encrypted_Char range 'a' .. 'z') of Character;

   package Result_Vectors is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Result_Set);
   subtype Result_Vector is Result_Vectors.Vector;

   function Image(ew: Encrypted_Word) return Word_List.Word;
   function Decipher(candidates: Candidate_Set; words: Word_List.Word_List) return Result_Vector;

end Decipherer;
