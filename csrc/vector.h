
#include <vector>

extern "C" {
typedef std::vector<int> Vector;
void delete_vector(Vector*);
Vector* new_vector(int);
}
