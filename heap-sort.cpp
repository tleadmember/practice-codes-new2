/*
TQBH - 2023-05-05
Heap Sort
*/

#include <iostream>
#include <cstdlib>
#include <ctime>


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


// Define build_max_heap() function
void build_max_heap(int array[], int len) {
  // Go bottom up from A(floor(n/2)) to A(0), all internal nodes with
  // at least 1 child, and perform max_heapify() at each node
  for (int i = (len-1)/2; i >= 0; i--) {
    max_heapify(array, i, len);
  }
}


// Define heap_sort() function
void heap_sort(int array[], int len) {
  // First build a max heap data structure
  build_max_heap(array, len);
  // Then start bringing each iteration's root max towards the
  // end of the array
  for (int i = len-1; i >= 1; i--) {
    // Swap iteration's root max (first element) with iteration's end
    // of array, from true end to true second-to-first element
    // (second-to-first because with the first element it doesn't make
    // sense to swap it with itself)
    int temp = array[i];
    array[i] = array[0]; // now array[i] has the iteration's max
    array[0] = temp; // now array[0] is no longer the iteration's max
    // Reduce the heap size to be considered (leave the iteration's last
    // element in place after sorted)
    len = len - 1;
    // Call max_heapify() for the newly swapped-in first element
    // (which is now not the max, not correct for a max heap data
    // structure)
    max_heapify(array, 0, len);
  }
}


// Main function
int main() {
  using std::cout;
  using std::endl;
  // Suppose we are given an unsorted array
  //int A[] = { 2, 7, 4, 5, 3, 1, 9, 8, 6 };
  std::srand(std::time(nullptr));
  for (int i = 0; i < 10000; i++) {
    // Declare the array
    int A[20];
    // Find length of array
    int len = *(&A + 1) - A;
      //cout << "Length of array: " << len << endl;
    for (int i = 0; i < len; i++) {
      A[i] = std::rand() % 101; // number between 0 and 100
    }
      //cout << "Unsorted array A:" << endl;
      //printarray(A, len);
    // Sort the array by heap_sort()
    heap_sort(A, len);
    // Check that it is sorted
      //cout << "Sorted array A:" << endl;
      //printarray(A, len);
    for (int i = 0; i < len-1; i++) {
      if (A[i+1] < A[i]) {
	return -1;
      }
    }
  }

  cout << "No error." << endl;

  return 0;
}
