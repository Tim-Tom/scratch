package main

import (
	"log"
	"os"
	"fmt"
	"math"
)

// Terminal

func expr_x(x float64, y float64) float64 {
	return x
}

func expr_y(x float64, y float64) float64 {
	return y
}

// Single

func expr_sin(e float64) float64 {
	return math.Sin(math.Pi * e)
}

func expr_cos(e float64) float64 {
	return math.Cos(math.Pi * e)
}

func expr_negate(e float64) float64 {
	return -e
}

func expr_sqrt(e float64) float64 {
	if e < 0.0 {
		return -math.Sqrt(-e)
	} else {
		return math.Sqrt(e)
	}
}

// Double

func expr_arith_mean(e1 float64, e2 float64) float64 {
	return (e1 + e2) / 2
}

func expr_geo_mean(e1 float64, e2 float64) float64 {
	return expr_sqrt(e1 * e2)
}

func expr_mult(e1 float64, e2 float64) float64 {
	return e1 * e2
}

func expr_max(e1 float64, e2 float64) float64 {
	if e1 > e2 {
		return e1
	} else {
		return e2
	}
}

func expr_min(e1 float64, e2 float64) float64 {
	if e1 > e2 {
		return e2
	} else {
		return e1
	}
}

type expr_terminal func(x float64, y float64) float64
type expr_single func(e float64) float64
type expr_double func(e1 float64, e2 float64) float64

type ExpressionEvaluator interface {
	evaluate(x float64, y float64) float64
}

type TerminalExpression struct {
	expr expr_terminal
}

type SingleExpression struct {
	expr expr_single
	e ExpressionEvaluator
}

type DoubleExpression struct {
	expr expr_double
	e1, e2 ExpressionEvaluator
}

func (expr TerminalExpression) evaluate(x float64, y float64) float64 {
	return expr.expr(x, y)
}

func (expr SingleExpression) evaluate(x float64, y float64) float64 {
	e := expr.e.evaluate(x, y)
	return expr.expr(e)
}

func (expr DoubleExpression) evaluate(x float64, y float64) float64 {
	e1 := expr.e1.evaluate(x, y)
	e2 := expr.e2.evaluate(x, y)
	return expr.expr(e1, e2)
}

func greyscale(e ExpressionEvaluator, scale int) {
	size := 2*scale+1
	f, err := os.OpenFile("test.pgm", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Fprintf(f, "P5 %d %d 255\n", size, size)
	buf := make([]byte, size)
	for xi := -scale; xi <= scale; xi++ {
		for yi := -scale; yi <= scale; yi++ {
			x := float64(xi)/float64(scale)
			y := float64(yi)/float64(scale)
			buf[yi + scale] = byte(127.5 + 127.5*e.evaluate(x,y));
		}
		f.Write(buf)
	}
	f.Close()
}

func build_expression(depth int) ExpressionEvaluator {
	if depth == 1 {
		return TerminalExpression { expr: expr_y }
	} else {
		e1 := build_expression(depth - 1)
		e2 := build_expression(depth - 1)
		return DoubleExpression { expr: expr_arith_mean, e1: e1, e2: e2 };
	}
}

func main() {
	e := build_expression(2)
	greyscale(e, 150)
}
