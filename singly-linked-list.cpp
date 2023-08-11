/* 
TQBH - 2023-05-22
Singly Linked List in C++
*/


#include <iostream>


// Define class Node
class Node {
public:
  int key; // data for each element or node in linked list
  Node* next; // pointer to the next element/node

  Node(int); // constructor
  //~Node(); // destructor
};


// Node constructor
Node::Node(int newKey = 0) { // default newKey is 0
  key = newKey;
  next = nullptr;
}


/*
// Define Node destructor
Node::~Node() {
  // no need because did not call "new" in class body
}
*/


// Define class LinkedList
class LinkedList {
private:
  Node* head, * tail; // declare pointers-to-node for list head and tail
public:
  LinkedList();  // constructor
  ~LinkedList(); // destructor

  void listPrepend(int); // method to add new element/node as new head
  void listAddAAfterB(int, int); // method to add new element/node after
                                 // another node with specified key
  void listPrint();
  void listDelete(int); // method to delete a node with specified key
};


// Define constructor of class LinkedList
LinkedList::LinkedList() {
  head = nullptr;  // when a new linked list is created, it is empty
  tail = nullptr;
}


// Define destructor of class LinkedList
LinkedList::~LinkedList() {
  Node* tempNode1 = head, * tempNode2 = nullptr;
  while (tempNode1 != nullptr) {
    tempNode2 = tempNode1->next;
    delete tempNode1;
    tempNode1 = tempNode2;
  }
}


// Definition
void LinkedList::listPrepend(int newKey) {
  // First, create new node
  Node* newNode = new Node(newKey); // different from: new Node[#]

  // Add to list
  newNode->next = head;
  head = newNode;
  if (head->next == nullptr) { // added to empty list
    tail = newNode;
  }

  std::cout << "Prepended node with key " << newKey << std::endl;
  
  return;
}


// Definition
void LinkedList::listAddAAfterB(int newKey, int prevKey) {
  // First, create new node
  Node* newNode = new Node(newKey);

  // Traverse the list to find the matching prevKey
  Node* tempNode = head;
  while ((tempNode != nullptr) && (tempNode->key != prevKey)) {
    tempNode = tempNode->next;
  }

  if (tempNode == nullptr) {
    std::cout << "Error, could not find key B\n";
    return;
  } else {
    newNode->next = tempNode->next;
    tempNode->next = newNode;
    if (newNode->next == nullptr) { // adding at the end
      tail = newNode;
    }
    
    std::cout << "Added node with key " << newKey;
    std::cout << " after node with key " << prevKey << std::endl;
  }

  return;
}


// Definition
void LinkedList::listPrint() {
  std::cout << "Current list: ";
  // Traverse the list and print each node key
  Node* tempNode = head;
  while (tempNode != nullptr) {
    std::cout << tempNode->key << " -> ";
    tempNode = tempNode->next;
  }
  std::cout << "NULL\n";
  return;
}


// Definition
void LinkedList::listDelete(int deleteKey) {
  Node* tempNode1 = head, * tempNode2 = nullptr;

  while ((tempNode1 != nullptr) && (tempNode1->key != deleteKey)) {
    tempNode2 = tempNode1;
    tempNode1 = tempNode1->next;
  }

  if (tempNode1 == nullptr) {
    std::cout << "Could not find a node with key " << deleteKey;
    std::cout << " to delete\n";
    return;
  } else {
    if (tempNode1 == head && tempNode1 == tail) {
      head = nullptr;
      tail = nullptr;
    } else if (tempNode1 == head) {
      head = tempNode1->next;
    } else if (tempNode1 == tail) {
      tempNode2->next = nullptr;
      tail = tempNode2;
    } else {
      tempNode2->next = tempNode1->next;
    }

    std::cout << "Deleted node with key " << deleteKey << std::endl;
    delete tempNode1; // deallocate memory
  }
  
  return;
}  



// Main function
int main() {
  LinkedList L1; // create

  L1.listPrepend(9);
  L1.listPrepend(8);

  L1.listPrint();

  L1.listAddAAfterB(10, 8);

  L1.listPrint();

  L1.listDelete(10);

  L1.listPrint();
 
  
  return 0;
}
