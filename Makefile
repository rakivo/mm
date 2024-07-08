DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIB_FLAGS = --crate-type=rlib
THREADS = -Z threads=10
EDITION_FLAGS = --edition=2021

RUST_FLAGS = $(RELEASE_FLAGS) $(EDITION_FLAGS)

BUILD_DIR = ./build

MMLIB_PATH = $(BUILD_DIR)/libmm.rlib

all: $(BUILD_DIR) $(BUILD_DIR)/libmm.rlib $(BUILD_DIR)/masm $(BUILD_DIR)/demasm
examples: all $(BUILD_DIR)/masm_files

MASM = ./build/masm
DEMASM = ./build/demasm
MASM_DIR = ./masm

$(BUILD_DIR):
	mkdir -p $@

mm: $(BUILD_DIR)/libmm.rlib
$(BUILD_DIR)/libmm.rlib: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/nan.rs src/parser.rs src/lexer.rs
	rustc $(RUST_FLAGS) $(LIB_FLAGS) -o $@ $<

$(BUILD_DIR)/masm: $(BUILD_DIR)/libmm.rlib masm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ masm.rs

$(BUILD_DIR)/demasm: $(BUILD_DIR)/libmm.rlib demasm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ demasm.rs

$(BUILD_DIR)/masm_files: $(patsubst $(MASM_DIR)/%.masm,$(BUILD_DIR)/%.bin,$(wildcard $(MASM_DIR)/*.masm))

$(BUILD_DIR)/%.bin: $(MASM_DIR)/%.masm $(BUILD_DIR)/masm
	$(BUILD_DIR)/masm $< -o $@

clean:
	rm -f $(BUILD_DIR)/*
	rm -f *.bin
