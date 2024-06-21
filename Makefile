DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIBFLAGS = --crate-type=rlib
BIN_PREF = build/
THREADS = -Z threads=10

BUILD_DIR = ./build

all: test_examples
examples: $(BUILD_DIR)/load_from_binary $(BUILD_DIR)/translate_masm $(BUILD_DIR)/generate_masm

TRANSLATE = ./build/translate_masm
GENERATE = ./build/generate_masm
LOAD = ./build/load_from_binary
EXAMPLES = fib.masm swap.masm jumps.masm
BINARIES = $(EXAMPLES:%.masm=./build/%)

MASM_DIR = ./masm
EXAMPLES = fib swap jumps
BINARIES = $(EXAMPLES:%=$(BUILD_DIR)/%)

test_fib: test_fib_for_readme
test_fib_for_readme: mm examples
	$(TRANSLATE) $(MASM_DIR)/fib.masm $(BUILD_DIR)/fibm
	$(LOAD) $(BUILD_DIR)/fibm

test_generating: mm examples
	$(BUILD_DIR)/generate_masm generated.masm
	$(TRANSLATE) generated.masm $(BUILD_DIR)/generated
	$(LOAD) $(BUILD_DIR)/generated

test_examples: mm examples $(BINARIES) test_generating
	$(foreach bin,$(BINARIES),$(LOAD) $(bin);)

$(BUILD_DIR)/%: $(MASM_DIR)/%.masm
	$(TRANSLATE) $< $@

mm: $(BUILD_DIR)/mm
$(BUILD_DIR)/mm: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/regs.rs
	cargo build --target-dir $(BUILD_DIR)

$(BUILD_DIR)/load_from_binary: mm examples/load_from_binary.rs
	rustc $(RELEASE_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/load_from_binary.rs

$(BUILD_DIR)/translate_masm: mm examples/translate_masm.rs
	rustc $(RELEASE_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/translate_masm.rs

$(BUILD_DIR)/generate_masm: mm examples/generate_masm.rs
	rustc $(RELEASE_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/generate_masm.rs

clean:
	rm -f $(BUILD_DIR)/*
