DEBUGFLAGS = -g -C "opt-level=0"
RELEASEFLAGS = -C "opt-level=3"
LIBFLAGS = --crate-type=rlib
BIN_PREF = build/

all: mm examples
mm: build/libmm
examples: build/load_from_binary build/translate_masm

build/libmm: mm.rs
	mkdir -p build
	rustc $(DEBUGFLAGS) $(LIBFLAGS) -o $@.rlib $<

build/load_from_binary: build/libmm examples/load_from_binary.rs
	rustc $(DEBUGFLAGS) --extern mm=./build/libmm.rlib -o $@ examples/load_from_binary.rs

build/translate_masm: build/libmm examples/translate_masm.rs
	rustc $(DEBUGFLAGS) --extern mm=./build/libmm.rlib -o $@ examples/translate_masm.rs

clean:
	rm -f build/*
