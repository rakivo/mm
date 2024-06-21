#ifndef _TRAP_HPP
#define _TRAP_HPP

#include <iostream>
#include <concepts>

#include "inst.hpp"

enum class trap_t
{
    OK = 0,
    STACK_OVERFLOW,
    STACK_UNDERFLOW,
    ILLEGAL_INSTRUCTION_ACCESS
};

class Trap
{
    const trap_t type_m = trap_t::OK;
    const Inst last_inst_m = Inst(inst_t::NOP);
    const std::string msg_m = {};

public:
    Trap(trap_t trap, const Inst& last_inst)
        : type_m(trap),
          last_inst_m(last_inst),
          msg_m("Cause: " + to_string(trap) + ". Last executed " + to_string(last_inst))
    {}

    Trap(trap_t trap) : type_m(trap) {}

    constexpr inline const trap_t& type(void) const noexcept
    {
        return type_m;
    }

    constexpr inline const char* what(void) const noexcept
    {
        return msg_m.c_str();
    }

    template <class T>
    requires requires (Trap* t) { *(T*)t; }
    constexpr operator T() const noexcept
    {
        return *(T*)this;
    }
};

#endif // _TRAP_HPP
