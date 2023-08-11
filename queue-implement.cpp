/*
TQBH - 2023-05-21
Queue Implementation with C++
*/

#include <iostream>
#include <cstdlib>
#include <ctime>

#define DEFAULT_SIZE 11


// Define class of Queue
class Queue {
  int* arr;      // private access if no specifier
  int capacity;
  int headi;
  int taili;

public:             // public access
  Queue(int);       // constructor
  ~Queue();         // destructor
  int maxSize();
  bool isFull();
  bool isEmpty();
  int head();
  bool enqueue(int);
  int dequeue();
};


// Define constructor, called when an object is created
Queue::Queue(int size = DEFAULT_SIZE) {
  arr = new int[size];
  capacity = size;
  headi = 0;
  taili = 0;
}


// Define destructor, call when an object goes out of scope
Queue::~Queue() {
  delete[] arr;  // delete memory allocated for pointer int*
}


// Define method to return the size of the queue
int Queue::maxSize() {
  return capacity-1;
}


// Define method to tell if queue is full
bool Queue::isFull() {
  if ((headi == 0 && taili == capacity-1) || (taili == headi-1)) {
    return true;
  } else {
    return false;
  }
}


// Define method to tell if queue is empty
bool Queue::isEmpty() {
  if (taili == headi) {
    return true;
  } else {
    return false;
  }
}


// Define method to return the current head
int Queue::head() {
  if (isEmpty()) {
    std::cout << "Error. Empty queue." << std::endl;
    return -1;
  } else {
    return arr[headi];
  }
}


// Define method to insert an element to the queue
bool Queue::enqueue(int x) {
  if (isFull()) {
    std::cout << "Queue overflows." << std::endl;
    return false;
  } else {
    std::cout << "Enqueuing " << x << std::endl;
    arr[taili] = x;
    /*
    if (++taili > capacity-1) {
      taili = 0;
    }
    */
    taili = ++taili % capacity;
    return true;
  }	      
}


// Define method to remove an element from the queue
int Queue::dequeue() {
  if (isEmpty()) {
    std::cout << "Queue underflows." << std::endl;
    return -1;
  } else {
    std::cout << "Dequeuing " << head() << std::endl;
    int oldHead = arr[headi];
    /*
    if (++headi > capacity-1) {
      headi = 0;
    }
    */
    headi = ++headi % capacity;
    return oldHead;
  }
}



// Main function
int main() {
  using std::cout;
  using std::endl;

  // Seeding random numbers
  std::srand(std::time(nullptr));
  
  int chosen = 5;
  Queue q1(chosen+1); // Queue has at most n-1 elements
  //Queue q1;

  // Check the size allocated
  cout << "Max size of queue is " << q1.maxSize() << endl;

  // Check the current head
  cout << "The current head is " << q1.head() << endl;

  // Enqueue some random elements, values between 0 and 100
  for (int i = 0; i < 7; ++i) {
    q1.enqueue(std::rand() % 101);
  }
  
  // Check the current head
  cout << "The current head is " << q1.head() << endl;

  // Dequeue some elements (FIFO)
  for (int i = 0; i < 3; ++i) {
    q1.dequeue();
  }

  // Check the current head
  cout << "The current head is " << q1.head() << endl;

  // Enqueue some more random elements, values between 0 and 100
  for (int i = 0; i < 4; ++i) {
    q1.enqueue(std::rand() % 101);
  }

  // Dequeue some more elements, until queue underflows
  for (int i = 0; i < 6; ++i) {
    q1.dequeue();
  }
  
  return 0;
}

