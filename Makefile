DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIB_FLAGS = --crate-type=rlib
THREADS = -Z threads=10
EDITION_FLAGS = --edition=2021

RUST_FLAGS = $(DEBUG_FLAGS) $(THREADS) $(EDITION_FLAGS)

BUILD_DIR = ./build

MMLIB_PATH = $(BUILD_DIR)/libmm.rlib

all: examples
examples: $(BUILD_DIR) mm $(BUILD_DIR)/translate_masm $(BUILD_DIR)/gen_masm $(BUILD_DIR)/to_binary $(BUILD_DIR)/from_binary $(BUILD_DIR)/gen_from_bin $(BUILD_DIR)/masm_to_bin

TRANSLATE = ./build/translate_masm
GENERATE = ./build/gen_masm
FROM_BIN = ./build/from_binary
TO_BIN = ./build/to_binary
BINARIES = $(EXAMPLES:%.masm=./build/%)

MASM_DIR = ./masm
EXAMPLES = fib swap jumps
EXAMPLES_SRC = $(EXAMPLES:%=$(MASM_DIR)/%.masm)
BINARIES = $(EXAMPLES:%=$(BUILD_DIR)/%)

$(BUILD_DIR):
	mkdir -p $@

test_fib: test_fib_for_readme
test_fib_for_readme: mm examples
	$(TRANSLATE) $(MASM_DIR)/fib.masm

$(BUILD_DIR)/%: $(MASM_DIR)/%.masm
	$(TRANSLATE) $<

test_examples: mm examples $(BINARIES)
	$(foreach src,$(EXAMPLES_SRC),$(TRANSLATE) $(src);)

mm: $(BUILD_DIR)/libmm.rlib
$(BUILD_DIR)/libmm.rlib: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/regs.rs
	rustc $(RUST_FLAGS) $(LIB_FLAGS) -o $@ $<

$(BUILD_DIR)/from_binary: mm examples/from_binary.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/from_binary.rs

$(BUILD_DIR)/to_binary: mm examples/to_binary.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/to_binary.rs

$(BUILD_DIR)/translate_masm: mm examples/translate_masm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/translate_masm.rs

$(BUILD_DIR)/gen_masm: mm examples/gen_masm.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/gen_masm.rs

$(BUILD_DIR)/gen_from_bin: mm examples/gen_from_bin.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/gen_from_bin.rs

$(BUILD_DIR)/masm_to_bin: mm examples/masm_to_bin.rs
	rustc $(RUST_FLAGS) --extern mm=$(MMLIB_PATH) -o $@ examples/masm_to_bin.rs

clean:
	rm -f $(BUILD_DIR)/*
