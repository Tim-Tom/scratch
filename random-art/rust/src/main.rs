use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;
use std::f32;

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

fn build_expression(depth: i32) -> Box<Expression> {
    if depth == 1 {
        let e = Box::new(Expression::Terminal { expr: expr_x });
        return e;
    } else {
        let e = Box::new(Expression::Terminal { expr: expr_x });
        return e;
    }
}

fn emit_greyscale(scale: i32) {
    let size: usize = 2*(scale as usize) + 1;
    let mut output = BufWriter::new(File::create("test.pgm").unwrap());
    // So apparently the size of arrays in rust have to be a compile time
    // constant at the moment. Hardcode the value because whatever.
    write!(output, "P5 {} {} 255\n", size, size).unwrap();
    for xi in -scale .. (scale + 1) {
        for yi in -scale .. (scale + 1) {
            let x = xi as f32 / scale as f32;
            let y = yi as f32 / scale as f32;
            let intensity = (127.5 + 127.5*(x + y)/2.0) as u8;
            let buf: [u8; 1] = [intensity];
            output.write(&buf).unwrap();
        }
    }
}

fn main() {
    emit_greyscale(150);
}
