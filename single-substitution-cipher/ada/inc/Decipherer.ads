with Word_List;

package Decipherer is
   
   type Encrypted_Char is new Character;
   type Encrypted_Word is Array(Positive range 1 .. Word_List.Word'Last) of Encrypted_Char;
   type Candidate_Set is Array(Positive range <>) of Encrypted_Word;

   type Result_Element is record
      from: Encrypted_Char;
      to : Character;
   end record;

   type Result_Set is Array(Positive range<>) of Result_Element;

   function Image(ew: Encrypted_Word) return Word_List.Word;
   function Decipher(candidates: Candidate_Set; words: Word_List.Word_List) return Result_Set;

end Decipherer;
