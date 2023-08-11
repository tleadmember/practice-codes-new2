/*
TQBH - 2023-05-19,20
Stack Implementation Using C++ Class
*/

#include <iostream>
#include <cstdlib>


/*
// Define function to print an array
void printarray(int array[], int length) {
  using std::cout;
  using std::endl;
  for (int index = 0; index < length; ++index) {
    cout << array[index] << " ";
  }
  cout << endl;
}
*/


// Define the default size of the stack (constant)
#define DEFAULT_SIZE 20


// Define stack using C++ class
class Stack {
  int* arr;  // if no access specifier = private access specifier
  int topIndex;
  int capacity;
  
public:
  Stack(int size);  // constructor, same name as class,
		    // called when an object is created
  ~Stack();  // destructor, called when an object goes out of scope

  // Define common methods for a stack
  int size();
  bool push(int);
  int pop();
  int top();
  bool isFull();
  bool isEmpty();
};


// Define constructor, outside of class body
Stack::Stack(int size = DEFAULT_SIZE) { // default argument
  arr = new int[size];
  topIndex = -1;
  capacity = size;
}


// Define destructor
Stack::~Stack() {
  delete[] arr;  // to avoid memory leak after new opeator, memory reserved
}


// Define size() method
int Stack::size() {
  return capacity;
}
 

// Define push() method to add a new element to the top
bool Stack::push(int x) {
  // Check if stack overflows
  if (isFull()) {
    std::cout << "Stack overflows." << std::endl;
    return false;
  } else {
    // Push
    std::cout << "Pushing " << x << std::endl;
    arr[++topIndex] = x;
    return true;
  }
}


// Define pop() method to remove an element from the top
int Stack::pop() {
  // Check if stack underflows
  if (isEmpty()) {
    std::cout << "Stack underflows." << std::endl;
    return -1;
  } else {
    // Pop
    std::cout << "Popping " << top() << std::endl;
    return arr[topIndex--];
  }
}


// Define isFull() method
bool Stack::isFull() {
  return (topIndex == (capacity-1));
}


// Define isEmpty() method
bool Stack::isEmpty() {
  return (topIndex == -1);
}


// Define top() method to return the top element
int Stack::top() {
  // Check if stack is empty
  if (isEmpty()) {
    std::cout << "ERROR" << std::endl;
    return -1;
  } else {
    // Otherwise, return top element
    return (arr[topIndex]);
  }
}



// Main function
int main() {
  using std::cout;
  using std::endl;

  int chosenSize = 5;
  Stack myStack1(chosenSize);
  Stack myStack2; // if size not specified, Stack
		  // constructor will use DEFAULT_SIZE

  cout << "Stack1 size is " << myStack1.size() << endl;
  cout << "Stack2 size is " << myStack2.size() << endl;

  for (int i = 0; i < 3; ++i) {
    myStack1.push(40+i);
  }

  for (int i = 0; i < 5; ++i) {
    myStack1.pop();
  }

  cout << "The current myStack1 top element is " << myStack1.top() << endl;

  
  return 0;
}
