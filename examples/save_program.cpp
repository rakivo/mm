#define MM_IMPLEMENTATION
#include "../src/mm.hpp"

using enum inst_t;

const static auto program = {
    Inst(PUSH, 68),
    Inst(INC),
    Inst(PUSH, 419),
    Inst(INC),
    Inst(POP),
    Inst(POP)
};

int main(const int argc, const char* const argv[])
{
    if (argc != 2) {
        std::cerr << "USAGE: " << argv[0] << " <output_file_path>" << std::endl;
        return 1;
    }

    Mm mm = program;
    return mm.save_program(argv[1]);
}
