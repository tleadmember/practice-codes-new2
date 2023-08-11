/*
TQBH - 2023-04-23
Matrix Multiply, Triply Nested
*/


#include <iostream>
//int ROWS = 4;
//int COLS = 4;

// Sample change to see in Magit

// Define function to print a matrix (2-D array)
void printarray(int array[][4], int row_count, int column_count) {
  using std::cout;
  using std::endl;
  for (int i = 0; i < row_count; i++) {
    for (int j = 0; j < column_count; j++) {
      cout << array[i][j] << "\t";
    }
    cout << endl;
  }
  cout << endl;
}


// Main function
int main() {
  using std::cout;
  using std::endl;
  // Suppose we are given two 4x4 matrices to multiply
  int A[4][4] = {
    {5, -2, 4, 1},
    {7, 6, -5, 0},
    {2, -3, -9, 8},
    {2, 7, 2, 9}
    };
  int B[4][4] = {
    {-6, 3, 7, 3},
    {2, 6, -3, 7},
    {2, -6, 8, 4},
    {-5, 7, -1, 0}
    };

  /* Result of A*B should be
    {
    {-31, -14, 72, 17}
    {-40, 87, -9, 43}
    {-76, 98, -57, -51}
    {-39, 99, 0, 63}
    }
  */

  // Create blank matrix C as the resulting matrix C = A*B
  // and assign zero values
  int C[4][4] = {0};
  
  // Print matrix A
  cout << "Matrix A=" << endl;
  printarray(A, 4, 4);
  // Print matrix B
  cout << "Matrix B=" << endl;
  printarray(B, 4, 4);

  // Perform matrix multiplication
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      for (int k = 0; k < 4; k++) {
        C[i][j] += A[i][k] * B[k][j];
      }
    }
  }
  
  // Print resulting matrix C
  cout << "Resulting matrix C = A*B =" << endl;
  printarray(C, 4, 4);
  
  return 0;
}
