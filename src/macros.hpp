#ifndef _MACROS_HPP
#define _MACROS_HPP

#include <string>
#include <iostream>

#define MATCH(var_, ...) case var_: __VA_ARGS__ "`"#var_"`"; break;

#define STREAM(type, ...)                                          \
    static constexpr std::ostream& operator<<(std::ostream& os, const type& var_) \
    {                                                              \
        using enum type;                                           \
        switch (var_) {                                            \
        __VA_ARGS__                                                \
        default: assert(0 && "UNREACHABLE");                       \
        }                                                          \
        return os;                                                 \
    }                                                              \

#define TO_STRING(type, ...)                                       \
    static constexpr std::string to_string(const type& var_)       \
    {                                                              \
        using enum type;                                           \
        switch (var_) {                                            \
        __VA_ARGS__                                                \
        default: assert(0 && "UNREACHABLE");                       \
        }                                                          \
    }                                                              \

#define TWO_OPERANDS_INSTRUCTION(case_, ...)                       \
    case case_: if (stack_m.size() > 1) {                          \
        const word_t a = stack_m.back(); stack_m.pop_back();       \
        const word_t b = stack_m.back(); stack_m.pop_back();       \
        stack_m.emplace_back(__VA_ARGS__);                         \
        break;                                                     \
    } else return Trap(STACK_UNDERFLOW, inst);                     \

#define SINGLE_OPERAND_INSTRUCTION(case_, body, else_type) case case_: body else return make_trap(else_type)

#define TOI(...) TWO_OPERANDS_INSTRUCTION(__VA_ARGS__)

#define SOI(...) SINGLE_OPERAND_INSTRUCTION(__VA_ARGS__)

#endif // _MACROS_HPP
