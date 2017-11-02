with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;

package Word_List is
   subtype Word is String(1 .. 24);
   type Pattern is new String(1 .. 24);
   
   function "<"(a, b : Pattern) return Boolean;
   function "="(a, b : Pattern) return Boolean;

   package Word_Vectors is new Ada.Containers.Vectors(Element_Type => Word, Index_Type => Positive);

   type Word_Vector is access Word_Vectors.Vector;
   
   package Word_Lists is new Ada.Containers.Ordered_Maps(Element_Type => Word_Vector, Key_Type => Pattern);
   package Pattern_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Word_List.Pattern);

   subtype Word_List is Word_Lists.Map;
   subtype Pattern_Set is Pattern_Sets.Set;

   function Make_Pattern(w : Word) return Pattern;
   function Build_Word_List(filename : String; patterns : Pattern_Set) return Word_List;


end Word_List;
