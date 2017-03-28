package main

import (
	"log"
	"os"
	"fmt"
);

func greyscale(scale int) {
	size := 2*scale+1
	f, err := os.OpenFile("test.pgm", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Fprintf(f, "P5 %d %d 255\n", size, size);
	buf := make([]byte, size);
	for xi := -scale; xi <= scale; xi++ {
		for yi := -scale; yi <= scale; yi++ {
			x := float32(xi)/float32(scale)
			y := float32(yi)/float32(scale);
			buf[yi + scale] = byte(127.5 + 127.5*((x + y) / 2))
		}
		f.Write(buf);
	}
	f.Close()
}

func main() {
	greyscale(150);
}
