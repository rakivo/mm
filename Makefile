CC = clang++
SRC = src
BUILD = build
CFLAGS = -Wall -Wextra -Werror -Wshadow -Wpedantic -Wswitch-enum

all: $(BUILD) $(BUILD)/out

$(BUILD):
	@mkdir -p $@

$(BUILD)/out: $(SRC)/main.cpp $(SRC)/mm.hpp $(SRC)/trap.hpp $(SRC)/inst.hpp $(SRC)/macros.hpp $(SRC)/types.hpp
	$(CC) $(CFLAGS) -std=c++26 -o $@ $<
