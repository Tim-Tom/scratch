with Ada.Numerics.Float_Random;

package Expressions is
   package Rand renames Ada.Numerics.Float_Random;
   type Expression_Type is (Terminal, Single, Double);
   subtype Expression_Value is Float range -1.0 .. 1.0;
   type Expression_Terminal_Function is access function (x, y : Expression_Value) return Expression_Value;
   type Expression_Single_Function is access function(e : Expression_Value) return Expression_Value;
   type Expression_Double_Function is access function(e1, e2 : Expression_Value) return Expression_Value;
   type Expression_Node(arguments : Expression_Type) is private;
   type Expression_Node_Access is access all Expression_Node;
   function Build_Expression_Tree(depth : Positive; gen : Rand.Generator) return Expression_Node_Access;
   function Evaluate_Expression(node : not null Expression_Node_Access; x,y : Expression_Value) return Expression_Value;
private
   type Expression_Node(arguments : Expression_Type) is record
      case arguments is
         when Terminal =>
            expr_t : Expression_Terminal_Function;
         when Single =>
            e : Expression_Node_Access;
            expr_s : Expression_Single_Function;
         when Double =>
            e1, e2 : Expression_Node_Access;
            expr_d : Expression_Double_Function;
      end case;
   end record;
end Expressions;
