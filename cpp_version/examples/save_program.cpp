#define MM_IMPLEMENTATION
#include "../src/mm.hpp"

using enum inst_t;
using enum instwop_t;

const static auto program = {
    Inst(34),
    Inst(35),
    Inst(ADD),
    Inst(1000),
    Inst(580),
    Inst(SUB),
    Inst(ADD)
};

int main(const int argc, const char *const argv[])
{
    if (argc != 2) {
        std::cerr << "USAGE: " << argv[0] << " <output_file_path>" << std::endl;
        return 1;
    }

    Mm mm = program;
    return mm.save_program(argv[1]);
}
