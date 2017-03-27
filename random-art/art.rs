use std::fs::File;
use std::io::prelude::*;

fn emit_greyscale(scale: i32) {
    let size: usize = 2*(scale as usize) + 1;
    let mut output = File::create("test.pgm").unwrap();
    // So apparently the size of arrays in rust have to be a compile time
    // constant at the moment. Hardcode the value because whatever.
    let mut buf: [u8; 301] = [0; 301];
    write!(output, "P5 {} {} 255\n", size, size).unwrap();
    for xi in -scale .. (scale + 1) {
        for yi in -scale .. (scale + 1) {
            let x = xi as f32 / scale as f32;
            let y = yi as f32 / scale as f32;
            let intensity = (127.5 + 127.5*(x + y)/2.0) as u8;
            if xi == 0 {
                println!("Writing index {}", yi + scale);
            }
            buf[(yi + scale) as usize] = intensity;
        }
        output.write(&buf).unwrap();
    }
}
fn main() {
    emit_greyscale(150);
}
