use std::io::prelude::*;
use std::fs::File;
use std::io::BufWriter;

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
