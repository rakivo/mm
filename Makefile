CC = clang++
SRC = src
BUILD = build
EXAMPLES = examples
CFLAGS = -Wall -Wextra -Werror -Wshadow -Wpedantic -Wswitch-enum -std=c++26

all: mm

examples: $(BUILD) $(BUILD)/save_program $(BUILD)/load_program

$(BUILD)/save_program: $(EXAMPLES)/save_program.cpp
	$(CC) $(CFLAGS) -o $@ $<

$(BUILD)/load_program: $(EXAMPLES)/load_program.cpp
	$(CC) $(CFLAGS) -o $@ $<

$(BUILD):
	@mkdir -p $@

mm: $(BUILD)/mm

$(BUILD)/mm: $(SRC)/main.cpp $(SRC)/mm.hpp $(SRC)/trap.hpp $(SRC)/inst.hpp $(SRC)/macros.hpp $(SRC)/types.hpp
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -rf build/*
