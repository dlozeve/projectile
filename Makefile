R7RS = -X r7rs -R r7rs

.PHONY: all clean

all: test tiler

bruijn.so: bruijn.scm
	csc $(R7RS) -sJ $^ -o $@

test: test.scm bruijn.so
	csc $(R7RS) $< -o $@

tiler: tiler.scm bruijn.so
	csc $< -o $@

clean:
	rm *.import.scm *.so test tiler
