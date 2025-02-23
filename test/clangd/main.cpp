
#include <iostream>
#include "other.hpp"

void other_function(int arg) {
    std::cout << arg << std::endl;
}

int main() {
    other_function(123);
    function_in_other_file();
}
