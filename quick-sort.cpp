/*
TQBH - 2023-05-12
Quick Sort
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


// Define partition() function
int partition(int array[], int start, int end) {
  int pivot = array[end];
  int i = start - 1; // initiate low side's highest index
  // Sort into low side and high side, leaving out pivot at the end
  for (int j = start; j <= end-1; j++) { //CAREFUL!!! j starts from
					//start, not from 0. And j
					//goes <= end-1, which is
					//different from < len-1
    if (array[j] <= pivot) {
      i = i + 1;
      // Switch array[j] with array[i], after i is incremented
      int temp = array[i];
      array[i] = array[j];
      array[j] = temp;
    }
  }
  // Switch pivot from the end to right after i (first of high side)
  int temp = array[i+1];
  array[i+1] = array[end];
  array[end] = temp;
  // Return the index of the pivot
  return i+1;
}


// Define quick_sort() function
void quick_sort(int array[], int start, int end) {
  if (start < end) {
   int pindex = partition(array, start, end);
   quick_sort(array, start, pindex-1);
   quick_sort(array, pindex+1, end);
  }
}


// Main function
int main() {
  using std::cout;
  using std::endl;
  // Suppose we are given random unsorted arrays
  std::srand(std::time(nullptr)); // seed the rand() function
  
  for (int i = 0; i < 10000; i++) {
    // Declare the array
    int A[20];
    
    int len = *(&A + 1) - A;

    for (int i = 0; i < len; i++) {
      A[i] = std::rand() % 101; // number between 0 and 100
    }

    // Sort the array by quick_sort()
    quick_sort(A, 0, len-1);
    
    // Check that it is sorted
    for (int i = 0; i < len-1; i++) {
      if (A[i+1] < A[i]) {
	return -1; // return error code -1 if not sorted correctly
      }
    }
  }

  cout << "No error." << endl; // if no errors and main() runs up to
			       // this point, print this message

  return 0;
}
