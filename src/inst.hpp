#ifndef _INST_HPP
#define _INST_HPP

#include <variant>
#include <optional>

#include "types.hpp"

enum class instwop_t
{
    JMP,
    PUSH
};

enum class inst_t
{
    NOP = 0,
    INC,
    DEC,
    ADD,
    SUB,
    MUL,
    DIV,
    POP,
};

struct Inst
{
    using type_t = std::variant<inst_t, instwop_t>;
    using operand_t = std::optional<word_t>;

    const operand_t operand = std::nullopt;
    const type_t type = inst_t::NOP;

    constexpr Inst(void) = default;

    constexpr Inst(const inst_t ty) noexcept
        : type(ty)
    {}

    constexpr Inst(const operand_t oper) noexcept
        : operand(oper),
          type(instwop_t::PUSH)
    {}

    constexpr Inst(const instwop_t ty, const operand_t oper) noexcept
        : operand(oper),
          type(ty)
    {}
};

#endif // _INST_HPP
