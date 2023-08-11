/*
TQBH - 2023-05-27
Stack Implementation Using Doubly Linked List
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



class Stack {
private:
  int count;
  Node* head;
  Node* tail;
public:
  Stack(); //constructor
  ~Stack(); //destructor

  int pop();      // other utility functions
  void push(int);
  int top();
  int size();
  //bool isFull();
  //bool isEmpty();
  void print();
};


Stack::Stack() { // stack using linked list, no need to specify length
  count = 0;
  head = nullptr;
  tail = nullptr;
}


Stack::~Stack() {
  Node* tempNode1 = head, * tempNode2;
  while (tempNode1 != nullptr) {
    tempNode2 = tempNode1->next;
    delete tempNode1;
    tempNode1 = tempNode2;
  }
}


int Stack::pop() {
  Node* tempNode = tail;

  --count;

  if (head == nullptr) {  // empty stack
    std::cout << "Error. Stack underflows.\n";
    return -1;
  } else {
    if (head == tail) {  // stack with one node
      head = nullptr;
      tail = nullptr;
    } else { // stack with more than one node
      tempNode->prev->next = nullptr;
      tail = tempNode->prev;
    }
  }

  int tempKey = tempNode->key;
  delete tempNode;
  return tempKey;
}


void Stack::push(int newKey) {
  Node* newNode = new Node(newKey);

  ++count;

  if (head == nullptr) {  // empty stack
    head = newNode;
    tail = newNode;
  } else { // stack with at least one node
    newNode->prev = tail;
    tail->next = newNode;
    tail = newNode;    
  }
  
  return;
}


int Stack::top() {
  if (head == nullptr) {
    std::cout << "Error. Empty stack. No top node.\n";
    return -1;
  } else {
    return tail->key;
  }
}


int Stack::size() {
  return count;
}


void Stack::print() {
  Node* tempNode = head;
  while (tempNode != nullptr) {
    std::cout << tempNode->key << " -> ";
    tempNode = tempNode->next;
  }
  std::cout << "NULL\n";
  return;
}



int main() {
  Stack stack1;

  std::cout << "Current stack size: " << stack1.size() << std::endl;
  std::cout << "Current stack top: " << stack1.top() << std::endl;

  stack1.push(5);
  stack1.push(10);
  stack1.push(15);

  std::cout << "Current stack size: " << stack1.size() << std::endl;
  std::cout << "Current stack top: " << stack1.top() << std::endl;
  std::cout << "Print current stack: " << std::endl;
  stack1.print();

  stack1.pop();

  std::cout << "Current stack size: " << stack1.size() << std::endl;
  std::cout << "Current stack top: " << stack1.top() << std::endl;
  std::cout << "Print current stack: " << std::endl;
  stack1.print();


  return 0;
}
