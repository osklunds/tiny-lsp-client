
#include <iostream>

#include "special++åäöあ本.hpp"

void function_in_other_file() {
    std::cout << "other" << std::endl;
    special();
}

void unicode_åäöåäöこんいちは() {

}

void unicode2() {
    unicode_åäöåäöこんいちは();
}
