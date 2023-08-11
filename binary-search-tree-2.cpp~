/*
TQBH - 2023-06-05,06
Binary Search Tree using C++
*/

#include <iostream>


class Node {
public:
  int key;
  Node* left;
  Node* right;
  Node* p;

  Node(int); //constructor
  //~Node(); //destructor

  Node* treeMin() {
    if (left == nullptr) {
      return this;
    } else {
      return left->treeMin();
    }
  }
};


Node::Node(int newKey = 0) {
  key = newKey;
  left = nullptr;
  right = nullptr;
  p = nullptr;
}

/*
Node::~Node() {
  //std::cout << "Node destructor called.\n";
  //delete left;
  //delete right;
}
*/



class BST {
public:
  Node* root;
  
  BST();
  ~BST();

  void inorderTreeWalk(Node*);
  void destroyRecursive(Node*);
  Node* iterativeTreeSearch(Node*, int);
  Node* treeMin(Node*);
  Node* treeMax(Node*);
  Node* treeSuccessor(int);
  Node* treePredecessor(int);
  void treeInsert(int);
  void transplant(Node*, Node*);
  void treeDelete(int);
};


BST::BST() {
  root = nullptr;
}


BST::~BST() {
  //std::cout << "BST destructor called.\n";
  //delete root;

  destroyRecursive(root);
}


void BST::destroyRecursive(Node* node) {
  if (node) {
    destroyRecursive(node->left);
    destroyRecursive(node->right);
    delete node;
  }
}


void BST::inorderTreeWalk(Node* node) {
  if (node) {
    inorderTreeWalk(node->left);
    std::cout << node->key << "  ";
    inorderTreeWalk(node->right);
  }
}


Node* BST::iterativeTreeSearch(Node* subroot, int searchKey) {
  Node* temp = subroot;
  while (temp != nullptr && temp->key != searchKey) {
    if (temp->key > searchKey) {
      temp = temp->left;
    } else {
      temp = temp->right;
    }
  }
  return temp;
}


Node* BST::treeMin(Node* subroot) {
  Node* temp = subroot;
  if (temp == nullptr) { // empty subtree input
    return nullptr;
  }
  return temp->treeMin();
}


Node* BST::treeMax(Node* subroot) {
  Node* temp = subroot;
  if (temp == nullptr) { // empty subtree input
    return nullptr;
  }
  while (temp->right != nullptr) {
    temp = temp->right;
  }
  return temp;
}


Node* BST::treeSuccessor(int targetKey) {
  Node* node = iterativeTreeSearch(root, targetKey);
  if (node == nullptr) {
    return nullptr;
  }
  if (node->right != nullptr) { // if node has a right child
    return treeMin(node->right);
  } else { // if node has no right child
    Node* tempCh = node;
    Node* tempPa = node->p;
    while (tempPa != nullptr && tempPa->left != tempCh) {
      tempCh = tempPa;
      tempPa = tempPa->p;
    }
    return tempPa;
  }
}


Node* BST::treePredecessor(int targetKey) {
  Node* node = iterativeTreeSearch(root, targetKey);
  if (node == nullptr) {
    return nullptr;
  }
  if (node->left != nullptr) {
    return treeMax(node->left);
  } else {
    Node* tempCh = node;
    Node* tempPa = node->p;
    while (tempPa != nullptr && tempPa->right != tempCh) {
      tempCh = tempPa;
      tempPa = tempPa->p;
    }
    return tempPa;
  }
}


void BST::treeInsert(int newKey) { // insert at leaf level
  Node* newNode = new Node(newKey);
  Node* temp1 = root;
  Node* temp2 = nullptr;
  while (temp1 != nullptr) { // iterate down to leaf level
    temp2 = temp1;
    if (newNode->key < temp1->key) {
      temp1 = temp1->left;
    } else {
      temp1 = temp1->right;
    }
  }
  newNode->p = temp2;
  if (temp2 == nullptr) { // empty tree
    root = newNode;
  } else if (newNode->key < temp2->key) {
    temp2->left = newNode;
  } else {
    temp2->right = newNode;
  }

  std::cout << "Inserted node with key " << newKey << std::endl;
}


void BST::transplant(Node* u, Node* v) { //replace subtree rooted at u
					 //with subtree rooted at v
  if (u->p == nullptr) { // replacing the root
    root = v;
  } else if (u == u->p->left) {
    u->p->left = v;
  } else {
    u->p->right = v;
  }
  if (v != nullptr) { // if v is null, not do anything
    v->p = u->p;
  }
}


void BST::treeDelete(int deleteKey) {
  Node* z = iterativeTreeSearch(root, deleteKey);
  if (z == nullptr) {
    std::cout << "Cannot find node with key " << deleteKey << " to delete.\n";
    return;
  }

  if (z->left == nullptr) { // no left child
    transplant(z, z->right);
  } else if (z->right == nullptr) { // only left child, no right
    transplant(z, z->left);
  } else { // both left child and right child
    Node* y = treeMin(z->right); // find successor knowing z has a
				 // right child
    if (y != z->right) {
      transplant(y, y->right);
      y->right = z->right;
      y->right->p = y;
    }
    transplant(z, y);
    y->left = z->left;
    y->left->p = y;
    
  }

  std::cout << "Deleted node with key " << deleteKey << std::endl;
  delete z; // deallocate memory of node to delete
}



int main() {
  BST t1;

  Node* n1 = new Node(6);
  Node* n2 = new Node(5);
  Node* n3 = new Node(7);
  Node* n4 = new Node(2);
  Node* n5 = new Node(5);
  Node* n6 = new Node(8);
  
  // Example BST in Cormen, chap 12.1, page 313
  n1->left = n2;
  n1->right = n3;
  t1.root = n1;

  n2->p = n1;
  n2->left = n4;
  n2->right = n5;

  n3->p = n1;
  n3->right = n6;

  n4->p = n2;

  n5->p = n2;
  
  n6->p = n3;

  // Print BST in order
  t1.inorderTreeWalk(t1.root);
  std::cout << std::endl;

  // Search
  int targetKey1 = 9;
  Node* returnNode = t1.iterativeTreeSearch(t1.root, targetKey1);
  if (returnNode == nullptr) {
    std::cout << "Key " << targetKey1 << " not found.\n";
  } else {
    std::cout << "Search returns: " << returnNode->key << std::endl;
  }

  // Max
  std::cout << "Tree max: " << t1.treeMax(t1.root)->key << std::endl;

  // Min
  std::cout << "Tree min: " << t1.treeMin(t1.root)->key << std::endl;

  // Successor
  int targetKey2 = 2;
  std::cout << "Successor of " << targetKey2 << ": ";
  std::cout << t1.treeSuccessor(targetKey2)->key << std::endl;

  // Predecessor
  int targetKey3 = 8;
  std::cout << "Predecessor of " << targetKey3 << ": ";
  std::cout << t1.treePredecessor(targetKey3)->key << std::endl;

  // Tree insert
  t1.treeInsert(4);

  // Print BST in order, again
  t1.inorderTreeWalk(t1.root);
  std::cout << std::endl;

  // Check successor of 2 now
  std::cout << "Successor of " << targetKey2 << ": ";
  std::cout << t1.treeSuccessor(targetKey2)->key << std::endl;

  // Delete node
  t1.treeDelete(7);

  // Print BST in order, again
  t1.inorderTreeWalk(t1.root);
  std::cout << std::endl;

  // Predecessor
  int targetKey4 = 8;
  std::cout << "Predecessor of " << targetKey4 << ": ";
  std::cout << t1.treePredecessor(targetKey4)->key << std::endl;
  
  
  return 0;
}
