with Ada.Text_IO;
with Ada.Command_Line;

procedure Symbolica is

   NumColors  : constant := 4;
   NumSymbols : constant := 3;
   Length     : constant := 5;
   Width      : constant := 5;

   package IO renames Ada.Text_IO;

   type Color_T  is new Natural range 0 .. NumColors - 1;
   type Symbol_T is new Natural range 0 .. NumSymbols - 1;
   type Column_T is new Natural range 1 .. Length;
   type Row_T    is new Natural range 1 .. Width;

   type Tile_T is record
      Color  : Color_T;
      Symbol : Symbol_T;
   end record;

   type Board_T is Array(Row_T, Column_T)   of Tile_T;
   type Count_T is Array(Color_T, Symbol_T) of Natural;

   colors  : constant Array(Color_T)  of Character := ('R', 'B', 'G', 'Y');
   symbols : constant Array(Symbol_T) of Character := ('a', 'b', 'c');

   original_board, best_board, board : Board_T;

   best_distance   : Natural := Length * Width;
   total_solutions : Natural := 0;
   iterations      : Natural := 0;
   tile_count      : Count_T := (others => (others => 0));

   function TileImage(tile : Tile_T) return String is
   begin
      return (colors(tile.Color), symbols(tile.Symbol));
   end TileImage;
   procedure PrintBoard(b : Board_T) is
   begin
      for r in Row_T loop
         for c in Column_T loop
            IO.Put(TileImage(b(r,c)) & " ");
         end loop;
         IO.New_Line;
      end loop;
   end PrintBoard;

   procedure ReadBoard(filename: in String; b : out Board_T) is

      Not_In : exception;

      function GetColor(c: Character) return Color_T is
      begin
         for color in Color_T'Range loop
            if colors(color) = c then
               return color;
            end if;
         end loop;
         raise Not_In;
      end GetColor;

      function GetSymbol(s: Character) return Symbol_T is
      begin
         for symbol in Symbol_T'Range loop
            if symbols(symbol) = s then
               return symbol;
            end if;
         end loop;
         raise Not_In;
      end GetSymbol;

      f   : IO.File_Type;
      c,s : Character;
      eol : Boolean;
   begin
      IO.Open(f, IO.In_File, filename);
      for row in Row_T'Range loop
         for col in Column_T'Range loop
            IO.Get(f, c);
            IO.Get(f, s);
            b(row, col) := (GetColor(c), GetSymbol(s));
            loop
               IO.Look_Ahead(f,c,eol);
               exit when eol or else c /= ' ';
               IO.Get(f, c);
            end loop;
         end loop;
      end loop;
      IO.Put_Line("Read in original Board");
      PrintBoard(b);
   end ReadBoard;

   procedure Validate_Counts is
      counts : Array(Color_T, Symbol_T) of Integer := (others => (others => 0));
      count_incorrect : exception;
   begin
      for row in Row_T'range loop
         for col in Column_T'range loop
            declare
               o : Tile_T renames original_board(row, col);
               b : Tile_T renames board(row, col);
            begin
               counts(o.Color, o.Symbol) := counts(o.Color, o.Symbol) + 1;
               counts(b.Color, b.Symbol) := counts(b.Color, b.Symbol) - 1;
            end;
         end loop;
      end loop;
      for c in Color_T'range loop
         for s in Symbol_T'range loop
            if counts(c, s) /= 0 then
               IO.Put_Line("New Board has incorrect count of " & (colors(c), symbols(s)) & ": " & Integer'Image(counts(c, s)));
               PrintBoard(board);
               raise count_incorrect;
            end if;
         end loop;
      end loop;
   end Validate_Counts;

   procedure Validate_Integrity is
      board_integrity : exception;
      function Pair_Is_Valid(a, b : Tile_T) return Boolean is
      begin
         return (a.Color = b.Color and then a.Symbol /= b.Symbol) or else (a.Color /= b.Color and then a.Symbol = b.Symbol);
      end Pair_Is_Valid;
   begin
      for row in Row_T'range loop
         for col in Column_T'range loop
            declare
               b : Tile_T renames board(row, col);
            begin
               if (row > Row_T'First    and then not Pair_Is_Valid(b, board(row-1, col))) or else
                  (row < Row_T'Last     and then not Pair_Is_Valid(b, board(row+1, col))) or else
                  (col > Column_T'First and then not Pair_Is_Valid(b, board(row, col-1))) or else
                  (col < Column_T'Last  and then not Pair_Is_Valid(b, board(row, col+1))) then
                  IO.Put_Line("New Board has incorrect incorrectly placed " & TileImage(b) & " at " & Row_T'Image(row) & "," & Column_T'Image(col));
                  PrintBoard(board);
                  raise board_integrity;
               end if;
            end;
         end loop;
      end loop;
   end Validate_Integrity;
   
   Perform_Additional_Validation : constant Boolean := False;
   procedure CompareBoards is
      distance : Natural := 0;
      chain    : Natural := 0;
      waiting  : Array(Color_T, Symbol_T, Color_T, Symbol_T) of Natural := (others => (others => (others => (others => 0))));
   begin
      if Perform_Additional_Validation then
         Validate_Counts;
         Validate_Integrity;
      end if;
      total_solutions := total_solutions + 1;
      for row in Row_T'Range loop
         for col in Column_T'Range loop
            declare
               o  : Tile_T renames original_board(row, col);
               b  : Tile_T renames board(row, col);
               w  : natural renames waiting(o.Color, o.Symbol, b.Color, b.Symbol);
               ow : natural renames waiting(b.Color, b.Symbol, o.Color, o.Symbol);
            begin
               if o.Color /= b.Color or else o.Symbol /= b.Symbol then
                  if ow > 0 then
                     ow := ow - 1;
                     chain := chain - 1;
                     distance := distance + 1;
                  else
                     chain := chain + 1;
                     w := w + 1;
                  end if;
               end if;
            end;
         end loop;
      end loop;
      if chain > 0 then
         distance := distance + chain - 1;
      end if;
      if distance < best_distance then
         best_board := board;
         best_distance := distance;
      end if;
   end CompareBoards;

   procedure Solve(row : Row_T; col : Column_T);
   procedure Pick(row : Row_T; col : Column_T; c : Color_T; s : Symbol_T) is
     tc : Natural renames tile_count(c, s);
   begin
      if tc = 0 then
         return;
      end if;
      board(row, col) := (c, s);
      if col = Column_T'Last then
         if row = Row_T'Last then
            CompareBoards;
         else
            tc := tc - 1;
            Solve(row + 1, 1);
            tc := tc + 1;
         end if;
      else
         tc := tc - 1;
         Solve(row, col + 1);
         tc := tc + 1;
      end if;
   end Pick;
   pragma inline(Pick);

   procedure Solve(row : Row_T; col : Column_T) is
   begin
      iterations := iterations + 1;
      if row = 1 then
         declare
            left : Tile_T renames board(row, col - 1);
         begin
            for color in Color_T'Range loop
               if color /= left.Color then
                  Pick(row, col, color, left.Symbol);
               end if;
            end loop;
            for symbol in Symbol_T'Range loop
               if symbol /= left.Symbol then
                  Pick(row, col, left.Color, symbol);
               end if;
            end loop;
         end;
      elsif col = 1 then
         declare
            above : Tile_T renames board(row - 1, col);
         begin
            for color in Color_T'Range loop
               if color /= above.Color then
                  Pick(row, col, color, above.Symbol);
               end if;
            end loop;
            for symbol in Symbol_T'Range loop
               if symbol /= above.Symbol then
                  Pick(row, col, above.Color, symbol);
               end if;
            end loop;
         end;
      else
         declare
            left  : Tile_T renames board(row, col -1);
            above : Tile_T renames board(row - 1, col);
         begin
            if left.Color = above.Color then
               for symbol in Symbol_T'Range loop
                  if symbol /= left.Symbol and then symbol /= above.Symbol then
                     Pick(row, col, left.Color, symbol);
                  end if;
               end loop;
            elsif left.Symbol = above.Symbol then
               for color in Color_T'Range loop
                  if color /= left.Color and then color /= above.Color then
                     Pick(row, col, color, left.Symbol);
                  end if;
               end loop;
            else
               Pick(row, col, left.Color, above.Symbol);
               Pick(row, col, above.Color, left.Symbol);
            end if;
         end;
      end if;
   end Solve;

begin
   ReadBoard(Ada.Command_Line.Argument(1), original_board);
   for row in Row_T'Range loop
      for col in Column_T'Range loop
         declare
            tile : constant Tile_T := original_board(row, col);
         begin
            tile_count(tile.Color, tile.Symbol) := tile_count(tile.Color, tile.Symbol) + 1;
         end;
      end loop;
   end loop;
   for color in Color_T'Range loop
      for symbol in Symbol_T'Range loop
         Pick(1, 1, color, symbol);
      end loop;
   end loop;
   IO.Put_Line("Total Iterations" & Natural'Image(iterations));
   IO.Put_Line("Total Solutions found:" & Natural'Image(total_solutions));
   IO.Put_Line("Total Swaps: " & Natural'Image(best_distance));
   PrintBoard(best_board);
end Symbolica;
