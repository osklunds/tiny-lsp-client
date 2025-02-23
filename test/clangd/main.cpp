
#include <iostream>
#include "other.hpp"

short other_function(int arg) {
    std::cout << arg << std::endl;
    return 1;
}

int main() {
    other_function(123);
    function_in_other_file();
}
