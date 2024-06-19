#ifndef _MM_HPP
#define _MM_HPP

#include <deque>
#include <vector>
#include <fstream>
#include <cstring>
#include <iostream>
#include <algorithm>

#include "inst.hpp"
#include "trap.hpp"
#include "types.hpp"
#include "macros.hpp"

class Mm
{
    bool halt_m = false;
    std::size_t ip_m = 0;

    std::deque<word_t> stack_m = {};
    const std::vector<Inst> program_m;

public:
    static constexpr std::size_t STACK_CAPACITY = 1024;

    constexpr Mm(const auto program) noexcept
        : program_m(program)
    {}

    Mm(const char* file_path) noexcept
        : program_m(load_program(file_path))
    {}

    void dump(void) const noexcept;

    const Trap run(void) noexcept;

    int save_program(const char *file_path) const noexcept;

    static std::vector<Inst> load_program(const char *file_path) noexcept
    {
        std::ifstream input_file(file_path, std::ios::binary);

        if (!input_file.is_open()) {
            std::cerr << "Failed to open file: " << file_path << std::endl;
            return {};
        }

        input_file.seekg(0, std::ios_base::end);
        const auto m = static_cast<std::size_t>(input_file.tellg());
        if (m < 0) {
            std::cerr << "Failed to read from file: " << file_path << std::endl;
            return {};
        }

        const auto program_size = m / sizeof(Inst);
        assert(m % sizeof(Inst) == 0);
        assert(program_size <= STACK_CAPACITY);

        input_file.seekg(0, std::ios_base::beg);

        std::vector<Inst> program(program_size);
        input_file.read(reinterpret_cast<char*>(program.data()),
                        static_cast<std::streamsize>(sizeof(Inst) * program_size));

        if (!input_file) {
            std::cerr << "Failed to read from file: " << file_path << std::endl;
            return {};
        }

        input_file.close();

        return program;
    }

    const Trap execute_instruction(const Inst& inst) noexcept;

private:
    const inline Trap make_trap(trap_t type) const noexcept
    {
        return Trap(type, program_m[ip_m]);
    }
};

#endif // _MM_HPP

#ifdef MM_IMPLEMENTATION

void Mm::dump(void) const noexcept
{
    std::cout << "stack: ";

    if (stack_m.empty()) std::cout << "[EMPTY]";
    else std::for_each(stack_m.cbegin(), stack_m.cend(), [](const auto& oper) {
        std::cout << oper << ' ';
    });

    std::cout << std::endl;
}

const Trap Mm::execute_instruction(const Inst& inst) noexcept
{
    if (ip_m >= program_m.size()) {
        halt_m = true;
        return trap_t::OK;
    }

    using enum inst_t;
    using enum trap_t;

    switch (inst.type) {
    case NOP: break;

    SOI(INC, if (stack_m.size()) {
        stack_m.back()++;
        break;
    }, STACK_UNDERFLOW);

    TOI(MUL, a * b);
    TOI(DIV, b / a);

    SOI(JMP, if (inst.operand < program_m.size()) {
        ip_m = static_cast<std::size_t>(inst.operand.value());
        break;
    }, ILLEGAL_INSTRUCTION_ACCESS);

    SOI(POP, if (stack_m.size()) {
        stack_m.pop_back();
        break;
    }, STACK_UNDERFLOW);

    SOI(PUSH, if (stack_m.size() < STACK_CAPACITY) {
        stack_m.emplace_back(inst.operand.value());
        break;
    }, STACK_OVERFLOW);
    }

    if (inst.type != JMP) ip_m++;

    return Trap(OK);
}

const Trap Mm::run(void) noexcept
{
    if (program_m.size() < 1)
        return trap_t::ILLEGAL_INSTRUCTION_ACCESS;

    while (!halt_m) {
        const auto& inst = program_m[ip_m];
        const auto result = execute_instruction(inst);

        if (result.type() != trap_t::OK) {
            std::cerr << result.what() << std::endl;
            return result;
        }

        dump();
    }

    return trap_t::OK;
}

int Mm::save_program(const char *file_path) const noexcept
{
    std::ofstream output_file(file_path);

    if (!output_file.is_open()) {
        std::cerr << "Failed to open file: " << file_path << std::endl;
        return 1;
    }

    output_file.write(reinterpret_cast<const char*>(program_m.data()),
                      sizeof(Inst) * program_m.size());

    if (!output_file) {
        std::cerr << "Error writing to file: " << file_path << std::endl;
        return 1;
    }

    output_file.close();

    return 0;
}

#endif // MM_IMPLEMENTATION
