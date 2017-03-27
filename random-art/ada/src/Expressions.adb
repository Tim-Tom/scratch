with Ada.Numerics.Elementary_Functions;

package body Expressions is
   package Math renames Ada.Numerics.Elementary_Functions;
   -- Terminals
   pragma Warnings(Off, "formal parameter ""x"" is not referenced");
   pragma Warnings(Off, "formal parameter ""y"" is not referenced");
   function expr_x(x,y: Expression_Value) return Expression_Value is
   begin
      return x;
   end expr_x;

   function expr_y(x,y: Expression_Value) return Expression_Value is
   begin
      return y;
   end expr_y;

   --  function expr_negative_one(x,y: Expression_Value) return Expression_Value is
   --  begin
   --     return -1.0;
   --  end expr_negative_one;

   --  function expr_zero(x,y: Expression_Value) return Expression_Value is
   --  begin
   --     return 0.0;
   --  end expr_zero;

   --  function expr_one(x,y: Expression_Value) return Expression_Value is
   --  begin
   --     return 1.0;
   --  end expr_one;

   pragma Warnings(On, "formal parameter ""x"" is not referenced");
   pragma Warnings(On, "formal parameter ""y"" is not referenced");

   -- Single
   function expr_sin(e: Expression_Value) return Expression_Value is
   begin
      return Math.Sin(Ada.Numerics.pi * e);
   end expr_sin;

   function expr_cos(e: Expression_Value) return Expression_Value is
   begin
      return Math.Cos(Ada.Numerics.pi * e);
   end expr_cos;

   function expr_negate(e: Expression_Value) return Expression_Value is
   begin
      return -e;
   end expr_negate;

   function expr_sqrt(e: Expression_Value) return Expression_Value is
   begin
      if e < 0.0 then
         return -Math.Sqrt(-e);
      else
         return Math.Sqrt(e);
      end if;
   end expr_sqrt;

   function expr_clamp(e: Expression_Value) return Expression_Value is
   begin
      if e < 0.0 then
         return -1.0;
      elsif e > 0.0 then
         return 1.0;
      else
         return 0.0;
      end if;
   end expr_clamp;

   function expr_round(e: Expression_Value) return Expression_Value is
   begin
      if e < 1.0/3.0 then
         return -1.0;
      elsif e > 1.0/3.0 then
         return 1.0;
      else
         return 0.0;
      end if;
   end expr_round;

   function expr_arith_mean(e1, e2: Expression_Value) return Expression_Value is
   begin
      return (e1 + e2)/2.0;
   end expr_arith_mean;

   function expr_geo_mean(e1, e2: Expression_Value) return Expression_Value is
   begin
      return expr_sqrt(e1 * e2);
   end expr_geo_mean;

   function expr_mult(e1, e2: Expression_Value) return Expression_Value is
   begin
      return e1 * e2;
   end expr_mult;

   function expr_max(e1, e2: Expression_Value) return Expression_Value is
   begin
      if e1 > e2 then
         return e1;
      else
         return e2;
      end if;
   end expr_max;

   function expr_min(e1, e2: Expression_Value) return Expression_Value is
   begin
      if e1 > e2 then
         return e2;
      else
         return e1;
      end if;
   end expr_min;

   Terminals : constant Array(1 .. 2) of Expression_Terminal_Function := (expr_x'access, expr_y'access);
   -- Singles : constant Array(1 .. 6) of Expression_Single_Function := (expr_sin'access, expr_cos'access, expr_negate'access, expr_sqrt'access, expr_clamp'access, expr_round'access);
   Singles : constant Array(1 .. 4) of Expression_Single_Function := (expr_sin'access, expr_cos'access, expr_negate'access, expr_sqrt'access);
   Doubles : constant Array(1 .. 5) of Expression_Double_Function := (expr_arith_mean'access, expr_geo_mean'access, expr_mult'access, expr_max'access, expr_min'access);
   
   function Build_Expression_Tree(depth : Positive; gen : Rand.Generator) return Expression_Node_Access is
      node : Expression_Node_Access;
      choice_f : constant Rand.Uniformly_Distributed := Rand.Random(gen);
      index : Integer;
   begin
      if choice_f = 1.0 then
         return Build_Expression_Tree(depth, gen);
      end if;
      if depth = 1 then
         index := Terminals'First + Integer(Float'Floor(choice_f*Float(Terminals'Length)));
         node := new Expression_Node(arguments => Terminal);
         node.expr_t := Terminals(index);
      else
         index := Integer(Float'Floor(choice_f * Float(Singles'Length + Doubles'Length)));
         if index < Singles'Length then
            index := index + Singles'First;
            node := new Expression_Node(arguments => Single);
            node.expr_s := Singles(index);
            node.e := Build_Expression_Tree(depth - 1, gen);
         else
            index := index - Singles'Length + Doubles'First;
            node := new Expression_Node(arguments => Double);
            node.expr_d := Doubles(index);
            node.e1 := Build_Expression_Tree(depth - 1, gen);
            node.e2 := Build_Expression_Tree(depth - 1, gen);
         end if;
      end if;
      return node;
   end Build_Expression_Tree;
   
   function Evaluate_Expression(node : not null Expression_Node_Access; x,y : Expression_Value) return Expression_Value is
   begin
      case node.arguments is
         when Terminal =>
           return node.expr_t(x,y);
         when Single =>
            return node.expr_s(Evaluate_Expression(node.e, x, y));
         when Double =>
            return node.expr_d(Evaluate_Expression(node.e1, x, y), Evaluate_Expression(node.e2, x, y));
      end case;
   end Evaluate_Expression;
end Expressions;
