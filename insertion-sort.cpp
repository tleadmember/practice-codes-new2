/* 
TQBH - 2023-04-10
Insertion sort algorithm
*/

#include <iostream>

// Function to print an array
void printarray(int array[], int length) {
  using std::cout;
  using std::endl;
  for (int index = 0; index < length; index++) {
    cout << array[index] << " ";
  }
  cout << endl;
}

int main() {
  using std::cout;
  using std::endl;
  // Assume we are given an array  A, unsorted
  int A[] = { 9, 6, 4, 3, 1, 2, 5, 7, 8 };

  // Find length of array A
  // int *lenx = *&A;
  // cout << "Pointer at the end of array A: " << lenx << endl;
  int len = *(&A + 1) - A;
  cout << "Length of array A: " << len << endl;

  // Print unsorted array A
  cout << "Unsorted array A:" << endl;
  printarray(A, len);
  
  // Perform insertion sort
  for (int i = 1; i < len; i++) {
    int key = A[i];

    int j = i-1;
    while (j >= 0 && A[j] > key) {
      A[j+1] = A[j];
      j = j-1;
    }
    A[j+1] = key;
  }

  // Print sorted array A
  cout << "Sorted array A:" << endl;
  printarray(A, len);
  
  return 0;
}
