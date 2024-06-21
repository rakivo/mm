#ifndef _MACROS_HPP
#define _MACROS_HPP

#include <string>
#include <iostream>

#define MATCH(var_, ...) case var_: __VA_ARGS__ "`"#var_"`"; break;

#define STREAM(type, ...)                                                         \
    static constexpr std::ostream& operator<<(std::ostream& os, const type& var_) \
    {                                                                             \
        using enum type;                                                          \
        switch (var_) {                                                           \
        __VA_ARGS__                                                               \
        default: assert(0 && "UNREACHABLE");                                      \
        }                                                                         \
        return os;                                                                \
    }                                                                             \

#define TO_STRING(type, ...)                                                      \
    static constexpr std::string to_string(const type& var_)                      \
    {                                                                             \
        using enum type;                                                          \
        switch (var_) {                                                           \
        __VA_ARGS__                                                               \
        default: assert(0 && "UNREACHABLE");                                      \
        }                                                                         \
    }                                                                             \

// SSS -> String & Stream operations.
#define FORWARD_DECLARE_SSO(type)                                                 \
    static constexpr std::string to_string(const type&);                          \
    static constexpr std::ostream& operator<<(std::ostream&, const type&);        \

#endif // _MACROS_HPP
