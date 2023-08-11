/*
TQBH - 2023-05-04
Build Max Heap
*/

#include <iostream>

// Define function to print an array
void printarray(int array[], int length) {
  using std::cout;
  using std::endl;
  for (int index = 0; index < length; index++) {
    cout << array[index] << " ";
  }
  cout << endl;
}

// Define left() function to return index of left child of a node
int left(int i) {
  return 2*i + 1;
}

// Define right() function to return index of right child of a node
int right(int i) {
  return 2*i + 2;
}

// Define max_heapify() function
void max_heapify(int array[], int i, int len) {
  int l = left(i);
  int r = right(i);
  int largest = 0;

  if ((l < len) && (array[l] > array[i])) {
    largest = l;
  } else {
    largest = i;
  }

  if ((r < len) && (array[r] > array[largest])) {
    largest = r;
  }

  if (largest != i) {
    // If A[i] is not the largest, exchange A[i] with A[largest]
    int temp = array[i];
    array[i] = array[largest];
    array[largest] = temp;
    // Recursively run max_heapify()
    max_heapify(array, largest, len);
  }
}


// Main function
int main() {
  using std::cout;
  using std::endl;
  // Suppose we are given an unsorted array
  int A[] = { 2, 7, 4, 5, 3, 1, 9, 8, 6 };

  // Find length of array
  int len = *(&A + 1) - A;
  cout << "Length of array: " << len << endl;
  
  // Print unsorted array
  cout << "Unsorted array A:" << endl;
  printarray(A, len);

  // Go bottom up from A(floor(n/2)) to A(0), all internal nodes with
  // at least 1 child, and perform max_heapify() at each node
  for (int i = (len-1)/2; i >=0; i--) {
    max_heapify(A, i, len);
  }

  // Print sorted array
  cout << "Sorted array A:" << endl;
  printarray(A, len);
  
  return 0;
}
