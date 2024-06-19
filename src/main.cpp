#define MM_IMPLEMENTATION
#include "mm.hpp"

int main(void)
{
    Mm mm = "output.bin";
    const auto trap = mm.run();
    return trap;
}
