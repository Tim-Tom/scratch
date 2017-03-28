extern crate rand;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;
use std::f32;
use rand::Rng;

// Terminals
#[allow(unused_variables)]
fn expr_x(x:f32, y:f32) -> f32 {
    return x;
}

#[allow(unused_variables)]
fn expr_y(x:f32, y:f32) -> f32 {
    return y;
}

// Single

fn expr_sin(e: f32) -> f32 {
    return f32::sin(f32::consts::PI * e);
}

fn expr_cos(e: f32) -> f32 {
    return f32::cos(f32::consts::PI * e);
}

fn expr_negate(e: f32) -> f32 {
    return -e;
}

fn expr_sqrt(e: f32) -> f32 {
    if e < 0.0 {
        return -f32::sqrt(-e);
    } else {
        return f32::sqrt(e);
    }
}

// Double

fn expr_arith_mean(e1: f32, e2: f32) -> f32 {
    return (e1 + e2) / 2.0;
}

fn expr_geo_mean(e1: f32, e2: f32) -> f32 {
    return expr_sqrt(e1 * e2);
}

fn expr_mult(e1: f32, e2: f32) -> f32 {
    return e1*e2;
}

fn expr_max(e1: f32, e2: f32) -> f32 {
    if e1 > e2 {
        return e1;
    } else {
        return e2;
    }
}

fn expr_min(e1: f32, e2: f32) -> f32 {
    if e1 > e2 {
        return e2;
    } else {
        return e1;
    }
}

type ExprTerminal = fn(f32, f32) -> f32;
type ExprSingle = fn(f32) -> f32;
type ExprDouble = fn(f32, f32) -> f32;

enum Expression {
    Terminal {
        expr: ExprTerminal
    },
    Single {
        expr: ExprSingle,
        e : Box<Expression>
    },
    Double {
        expr: ExprDouble,
        e1 : Box<Expression>,
        e2 : Box<Expression>
    }
}

fn build_expression(depth: i32, rng: &mut rand::ThreadRng) -> Box<Expression> {
    let terminals: [ExprTerminal; 2] = [expr_x, expr_y];
    let singles: [ExprSingle; 4] = [expr_sin, expr_cos, expr_negate, expr_sqrt];
    let doubles: [ExprDouble; 5] = [expr_arith_mean, expr_geo_mean, expr_mult, expr_min, expr_max];
    if depth == 1 {
        let f = terminals[rng.gen_range(0, 2)];
        let e = Box::new(Expression::Terminal { expr: f });
        return e;
    } else {
        let i = rng.gen_range(0, 4 + 5);
        if i < 4 {
            let e1 = build_expression(depth - 1, rng);
            let e = Box::new(Expression::Single { expr: singles[i], e: e1 });
            return e;
        } else {
            let e1 = build_expression(depth - 1, rng);
            let e2 = build_expression(depth - 1, rng);
            let e = Box::new(Expression::Double { expr: doubles[i - 4], e1: e1, e2: e2 });
            return e;
        }
    }
}

fn eval_expression(expression: &Expression, x: f32, y: f32) -> f32 {
    return match *expression {
        Expression::Terminal { expr }  => expr(x,y),
        Expression::Single {expr, ref e} => expr(eval_expression((*e).as_ref(), x, y)),
        Expression::Double {expr, ref e1, ref e2} => expr(eval_expression((*e1).as_ref(), x, y), eval_expression((*e2).as_ref(), x, y))
    }
}

fn emit_greyscale(scale: i32, expr: &Expression) {
    let size: usize = 2*(scale as usize) + 1;
    let mut output = BufWriter::new(File::create("test.pgm").unwrap());
    // So apparently the size of arrays in rust have to be a compile time
    // constant at the moment. Hardcode the value because whatever.
    write!(output, "P5 {} {} 255\n", size, size).unwrap();
    for xi in -scale .. (scale + 1) {
        for yi in -scale .. (scale + 1) {
            let x = xi as f32 / scale as f32;
            let y = yi as f32 / scale as f32;
            let intensity = (127.5 + 127.5*eval_expression(&expr, x, y)/2.0) as u8;
            let buf: [u8; 1] = [intensity];
            output.write(&buf).unwrap();
        }
    }
}

fn main() {
    let mut rng : rand::ThreadRng = rand::thread_rng();
    let expr = build_expression(15, &mut rng);
    emit_greyscale(150, expr.as_ref());
}
