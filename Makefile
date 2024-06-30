DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIB_FLAGS = --crate-type=rlib
THREADS = -Z threads=10
EDITION_FLAGS = --edition=2021

RUST_FLAGS = $(DEBUG_FLAGS) $(THREADS) $(EDITION_FLAGS)

BUILD_DIR = ./build

MMLIB_PATH = $(BUILD_DIR)/libmm.rlib

all: examples
examples: $(BUILD_DIR) $(BUILD_DIR)/libmm.rlib $(BUILD_DIR)/masm $(BUILD_DIR)/demasm

MASM = ./build/masm
DEMASM = ./build/demasm
MASM_DIR = ./masm

$(BUILD_DIR):
	mkdir -p $@

mm: $(BUILD_DIR)/libmm.rlib
$(BUILD_DIR)/libmm.rlib: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/nan.rs src/parser.rs src/comptime.rs
	rustc $(RUST_FLAGS) $(LIB_FLAGS) -o $@ $<

$(BUILD_DIR)/masm: mm masm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ masm.rs

$(BUILD_DIR)/demasm: mm demasm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ demasm.rs

clean:
	rm -f $(BUILD_DIR)/*
