
#include "vector.h"

void
delete_vector(Vector* ptr){
  delete ptr;
}

Vector*
new_vector(int size){
  return new std::vector<int>(size);
}
