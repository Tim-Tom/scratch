Made some perl and c functions to match the routines mentioned in a d language blog post.

Original post at http://dlang.org/blog/2017/05/24/faster-command-line-tools-in-d/

The naive perl version is about 33% faster than the python version and using Text::CSV is
about the same speed. Almost all of that time is spend on IO, which isn't surprising as IO
on my virtual machine always seems super slow. (And about 4x slower than the pypy version,
pretty impressive python folks)

The c version is very fast which says the IO can be fast (though it's still about 3x
slower than I would expect).
