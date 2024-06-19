CC = clang++
CFLAGS = -Wall -Wextra -Werror -Wshadow -Wpedantic -Wswitch-enum

SRC = src
BUILD = build

all: $(BUILD) $(BUILD)/out

$(BUILD):
	mkdir -p $t

$(BUILD)/out: $(SRC)/main.cpp $(SRC)/mm.hpp $(SRC)/trap.hpp $(SRC)/inst.hpp $(SRC)/macros.hpp $(SRC)/types.hpp
	$(CC) $(CFLAGS) -std=c++26 -o $t $d
