
#include <iostream>

void other_function(int arg) {
    std::cout << "Howdy, other_function! " << arg << std::endl;
}

int main() {
    other_function(123);
    std::cout << "Howdy, pardner!" << std::endl;
}
