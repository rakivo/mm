DEBUG_FLAGS = -g -C "opt-level=0"
RELEASE_FLAGS = -C "opt-level=3"
LIBFLAGS = --crate-type=rlib
BIN_PREF = build/
THREADS = -Z threads=10

BUILD_DIR = ./build

all: examples
examples: mm $(BUILD_DIR)/translate_masm $(BUILD_DIR)/generate_masm

TRANSLATE = ./build/translate_masm
GENERATE = ./build/generate_masm
LOAD = ./build/load_from_binary
EXAMPLES = fib.masm swap.masm jumps.masm
BINARIES = $(EXAMPLES:%.masm=./build/%)

test_jumps:
	$(TRANSLATE) $(MASM_DIR)/fib.masm
	$(LOAD) $(BUILD_DIR)/fibm

MASM_DIR = ./masm
EXAMPLES = fib swap jumps
BINARIES = $(EXAMPLES:%=$(BUILD_DIR)/%)

test_fib: test_fib_for_readme
test_fib_for_readme: mm examples
	$(TRANSLATE) $(MASM_DIR)/fib.masm

test_generating: mm examples
	$(BUILD_DIR)/generate_masm generated.masm
	$(TRANSLATE) generated.masm

$(BUILD_DIR)/%: $(MASM_DIR)/%.masm
	$(TRANSLATE) $< $@

mm: $(BUILD_DIR)/mm
$(BUILD_DIR)/mm: src/mm.rs src/flag.rs src/inst.rs src/trap.rs src/regs.rs
	cargo build --target-dir $(BUILD_DIR)

$(BUILD_DIR)/translate_masm: mm examples/translate_masm.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/translate_masm.rs
	$@ $(MASM_DIR)/jumps.masm

$(BUILD_DIR)/generate_masm: mm examples/generate_masm.rs
	rustc $(DEBUG_FLAGS) $(THREADS) --extern mm=$(BUILD_DIR)/debug/libmm.rlib -o $@ examples/generate_masm.rs
	$@ generated.masm

clean:
	rm -f $(BUILD_DIR)/*
