DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIBFLAGS = --crate-type=rlib
BIN_PREF = build/
THREADS = -Z threads=10

BUILD_DIR = ./build

all: test_examples
examples: mm $(BUILD_DIR)/translate_masm $(BUILD_DIR)/gen_masm $(BUILD_DIR)/to_binary $(BUILD_DIR)/from_binary $(BUILD_DIR)/gen_from_bin

TRANSLATE = ./build/translate_masm
GENERATE = ./build/gen_masm
FROM_BIN = ./build/from_binary
TO_BIN = ./build/to_binary
BINARIES = $(EXAMPLES:%.masm=./build/%)

MASM_DIR = ./masm
EXAMPLES = fib swap jumps
EXAMPLES_SRC = $(EXAMPLES:%=$(MASM_DIR)/%.masm)
BINARIES = $(EXAMPLES:%=$(BUILD_DIR)/%)

test_fib: test_fib_for_readme
test_fib_for_readme: mm examples
	$(TRANSLATE) $(MASM_DIR)/fib.masm

$(BUILD_DIR)/%: $(MASM_DIR)/%.masm
	$(TRANSLATE) $<

test_examples: mm examples $(BINARIES)
	$(foreach src,$(EXAMPLES_SRC),$(TRANSLATE) $(src);)

mm: $(BUILD_DIR)/mm
$(BUILD_DIR)/mm: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/regs.rs
	cargo build --target-dir $(BUILD_DIR)

$(BUILD_DIR)/from_binary: mm examples/from_binary.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/from_binary.rs

$(BUILD_DIR)/to_binary: mm examples/to_binary.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/to_binary.rs

$(BUILD_DIR)/translate_masm: mm examples/translate_masm.rs
	rustc $(RELEASE_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/translate_masm.rs

$(BUILD_DIR)/gen_masm: mm examples/gen_masm.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/gen_masm.rs

$(BUILD_DIR)/gen_from_bin: mm examples/gen_from_bin.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/gen_from_bin.rs

clean:
	rm -f $(BUILD_DIR)/*
