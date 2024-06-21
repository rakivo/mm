#ifndef _MM_HPP
#define _MM_HPP

class Trap;
enum class trap_t;

struct Inst;
enum class inst_t;

#include "macros.hpp"

FORWARD_DECLARE_SSO(Inst);
FORWARD_DECLARE_SSO(trap_t);
FORWARD_DECLARE_SSO(inst_t);

#include <deque>
#include <vector>
#include <fstream>
#include <cstring>
#include <cassert>
#include <iostream>

#include "inst.hpp"
#include "trap.hpp"
#include "types.hpp"

class Mm
{
    bool halt_m = false;
    std::size_t ip_m = 0;

    std::deque<word_t> stack_m = {};

    const std::vector<Inst> program_m;

    static constexpr auto print_operand_m = [](const auto& oper)
    {
        std::cout << oper << ' ';
    };

public:
    static constexpr std::size_t STACK_CAPACITY = 1024;

    constexpr Mm(const auto program) noexcept
        : program_m(program)
    {}

    Mm(const char *file_path) noexcept
        : program_m(load_program(file_path))
    {}

    void dump(void) const noexcept;

    const Trap run(void) noexcept;

    int save_program(const char *file_path) const noexcept;
    static std::vector<Inst> load_program(const char *file_path) noexcept;

private:
    const Trap execute_instruction(const Inst& inst) noexcept;

    // Noi -> non-operand-instruction
    const Trap handle_noi(const inst_t type) noexcept;

    // Soi -> single-operand-instruction
    const Trap handle_soi(const instwop_t type, const Inst::operand_t& oper) noexcept;

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
    else std::for_each(stack_m.cbegin(), stack_m.cend(), print_operand_m);

    std::cout << std::endl;
}

const Trap Mm::handle_noi(const inst_t type) noexcept
{
    using enum inst_t;
    using enum trap_t;

    switch (type) {
    case NOP: break;

    case INC: if (stack_m.size()) {
        stack_m.back()++;
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    case DEC: if (stack_m.size()) {
        stack_m.back()--;
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    case ADD: if (stack_m.size() > 1) {
        const word_t a = stack_m.back(); stack_m.pop_back();
        const word_t b = stack_m.back(); stack_m.pop_back();
        stack_m.emplace_back(b + a);
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    case SUB: if (stack_m.size() > 1) {
        const word_t a = stack_m.back(); stack_m.pop_back();
        const word_t b = stack_m.back(); stack_m.pop_back();
        stack_m.emplace_back(b - a);
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    case MUL: if (stack_m.size() > 1) {
        const word_t a = stack_m.back(); stack_m.pop_back();
        const word_t b = stack_m.back(); stack_m.pop_back();
        stack_m.emplace_back(b * a);
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    case DIV: if (stack_m.size() > 1) {
       const word_t a = stack_m.back(); stack_m.pop_back();
       const word_t b = stack_m.back(); stack_m.pop_back();
       stack_m.emplace_back(b / a);
       break;
   } else return Trap(STACK_UNDERFLOW, Inst(type));

    case POP: if (stack_m.size()) {
        stack_m.pop_back();
        break;
    } else return Trap(STACK_UNDERFLOW, Inst(type));

    default:
        return Trap(ILLEGAL_INSTRUCTION_ACCESS, Inst(type));
    }

    ip_m++;

    return Trap(OK);
}

const Trap Mm::handle_soi(const instwop_t type, const Inst::operand_t& oper) noexcept
{
    using enum instwop_t;
    using enum trap_t;

    switch (type) {
    case JMP: if (oper < program_m.size()) {
        ip_m = static_cast<std::size_t>(oper.value());
        break;
    } else return Trap(ILLEGAL_INSTRUCTION_ACCESS, Inst(type, oper));

    case PUSH: if (stack_m.size() < STACK_CAPACITY) {
        stack_m.emplace_back(oper.value());
        break;
    } else return Trap(STACK_OVERFLOW, Inst(type, oper));

    default:
        return Trap(ILLEGAL_INSTRUCTION_ACCESS, Inst(type, oper));
    }

    if (type != JMP) ip_m++;
    return Trap(OK);
}

const Trap Mm::execute_instruction(const Inst& inst) noexcept
{
    if (ip_m >= program_m.size()) {
        halt_m = true;
        return trap_t::OK;
    }

    if (auto type = std::get_if<inst_t>(&inst.type))
        return handle_noi(*type);
    else if (auto typewop = std::get_if<instwop_t>(&inst.type))
        return handle_soi(*typewop, inst.operand);
    else assert(0 && "UNREACHABLE");
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

std::vector<Inst> Mm::load_program(const char *file_path) noexcept
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

TO_STRING(trap_t,
    MATCH(OK, return);
    MATCH(STACK_OVERFLOW, return);
    MATCH(STACK_UNDERFLOW, return)
    MATCH(ILLEGAL_INSTRUCTION_ACCESS, return));

STREAM(trap_t,
    MATCH(OK, os <<);
    MATCH(STACK_OVERFLOW, os <<);
    MATCH(STACK_UNDERFLOW, os <<);
    MATCH(ILLEGAL_INSTRUCTION_ACCESS, os <<));

STREAM(inst_t,
    MATCH(NOP, os <<);
    MATCH(INC, os <<);
    MATCH(DEC, os <<);
    MATCH(ADD, os <<);
    MATCH(SUB, os <<);
    MATCH(MUL, os <<);
    MATCH(DIV, os <<);
    MATCH(POP, os <<));

STREAM(instwop_t,
    MATCH(JMP, os <<);
    MATCH(PUSH, os <<));

TO_STRING(inst_t,
    MATCH(NOP, return);
    MATCH(INC, return);
    MATCH(DEC, return);
    MATCH(ADD, return);
    MATCH(SUB, return);
    MATCH(MUL, return);
    MATCH(DIV, return);
    MATCH(POP, return));

TO_STRING(instwop_t,
    MATCH(JMP, return);
    MATCH(PUSH, return));

static constexpr std::string to_string(const std::variant<inst_t, instwop_t>& var)
{
    return std::visit([](auto&& arg) -> std::string {
        return to_string(arg);
    }, var);
}

static constexpr std::string to_string(const Inst& inst)
{
    std::string msg = "Instruction: " + to_string(inst.type);

    if (inst.operand.has_value())
        msg += ". Operand: " + std::to_string(inst.operand.value());

    return msg;
}

static inline constexpr std::ostream& operator<<(std::ostream& os, const Inst& inst)
{
    os << to_string(inst);
    return os;
}

#endif // MM_IMPLEMENTATION
