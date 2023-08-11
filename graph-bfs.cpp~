/*
TQBH - 2023-05-28,29
Queue Implementation Using Doubly Linked List
*/

#include <iostream>


class Node {
public:
  int key;     // fields
  Node* prev;
  Node* next;

  Node(int); // constructor
  //~Node(); // destructor
};


Node::Node(int newKey = 0) { //constructor
  key = newKey;
  prev = nullptr;
  next = nullptr;
}



class Queue {
private:
  int count;
  Node* head;
  Node* tail;
public:
  Queue(); //constructor
  ~Queue(); //destructor

  int dequeue();       // other utility functions
  void enqueue(int);
  int front();
  int size();
  //bool isFull();
  //bool isEmpty();
  void print();
};


Queue::Queue() { // queue using linked list, no need to specify length
  count = 0;
  head = nullptr;
  tail = nullptr;
}


Queue::~Queue() {
  Node* tempNode1 = head, * tempNode2;
  while (tempNode1 != nullptr) {
    tempNode2 = tempNode1->next;
    delete tempNode1;
    tempNode1 = tempNode2;
  }
}


int Queue::dequeue() {
  Node* tempNode = head;

  --count;

  if (head == nullptr) {  // empty queue
    std::cout << "Error. Queue underflows.\n";
    return -1;
  } else {
    if (head == tail) {  // queue with one node
      head = nullptr;
      tail = nullptr;
    } else { // queue with more than one node
      tempNode->next->prev = nullptr;
      head = tempNode->next;
    }
  }

  int tempKey = tempNode->key;
  delete tempNode;
  return tempKey;
}


void Queue::enqueue(int newKey) {
  Node* newNode = new Node(newKey);

  ++count;

  if (head == nullptr) {  // empty queue
    head = newNode;
    tail = newNode;
  } else { // queue with at least one node
    newNode->prev = tail;
    tail->next = newNode;
    tail = newNode;    
  }
  
  return;
}


int Queue::front() {
  if (head == nullptr) {
    std::cout << "Error. Empty queue. No head.\n";
    return -1;
  } else {
    return head->key;
  }
}


int Queue::size() {
  return count;
}


void Queue::print() {
  Node* tempNode = head;
  while (tempNode != nullptr) {
    std::cout << tempNode->key << " -> ";
    tempNode = tempNode->next;
  }
  std::cout << "NULL\n";
  return;
}



int main() {
  Queue q1;

  std::cout << "Current queue size: " << q1.size() << std::endl;
  std::cout << "Current queue front: " << q1.front() << std::endl;

  q1.enqueue(5);
  q1.enqueue(10);
  q1.enqueue(15);

  std::cout << "Current queue size: " << q1.size() << std::endl;
  std::cout << "Current queue front: " << q1.front() << std::endl;
  std::cout << "Print current queue: ";
  q1.print();

  q1.dequeue(); // FIFO

  std::cout << "Current queue size: " << q1.size() << std::endl;
  std::cout << "Current queue front: " << q1.front() << std::endl;
  std::cout << "Print current queue: ";
  q1.print();

  q1.enqueue(20);

  std::cout << "Current queue size: " << q1.size() << std::endl;
  std::cout << "Current queue front: " << q1.front() << std::endl;
  std::cout << "Print current queue: ";
  q1.print();


  return 0;
}
