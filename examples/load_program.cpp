#define MM_IMPLEMENTATION
#include "../src/mm.hpp"

int main(const int argc, const char* const argv[])
{
    if (argc != 2) {
        std::cerr << "USAGE: " << argv[0] << " <output_file_path>" << std::endl;
        return 1;
    }

    Mm mm = argv[1];
    const auto trap = mm.run();
    return trap;
}
