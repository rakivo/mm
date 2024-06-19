#ifndef _INST_HPP
#define _INST_HPP

#include <cassert>
#include <optional>

#include "types.hpp"
#include "macros.hpp"

enum class inst_t
{
    NOP = 0,
    INC,
    MUL,
    DIV,
    JMP,
    POP,
    PUSH
};

struct Inst {
    using operand_t = std::optional<word_t>;

    const operand_t operand = std::nullopt;
    const inst_t type = inst_t::NOP;

    constexpr Inst(void) = default;

    constexpr Inst(const inst_t ty) noexcept
        : type(ty)
    {}

    constexpr Inst(const inst_t ty, const word_t oper) noexcept
        : operand(oper),
          type(ty)
    {}
};

TO_STRING(inst_t,
    MATCH(NOP, return);
    MATCH(INC, return);
    MATCH(MUL, return);
    MATCH(DIV, return);
    MATCH(JMP, return);
    MATCH(POP, return);
    MATCH(PUSH, return));

static constexpr std::string to_string(const Inst& inst)
{
    std::string msg = "Instruction: " + to_string(inst.type);

    if (inst.operand.has_value())
        msg += ". Operand: " + std::to_string(inst.operand.value());

    return msg;
}

STREAM(inst_t,
    MATCH(NOP, os <<);
    MATCH(INC, os <<);
    MATCH(MUL, os <<);
    MATCH(DIV, os <<);
    MATCH(JMP, os <<);
    MATCH(POP, os <<);
    MATCH(PUSH, os <<));

#endif // _INST_HPP
