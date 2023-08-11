/*
TQBH - 2023-06-15,18,21,23,24
Graph and Breadth-First Search
*/

#include <iostream>


// A graph has a set of vertices and a set of edges


class Vertex {
public:
  int key;
  
  Vertex(int newKey) {
    key = newKey;
  }
  //~Vertex();
};


class Edge {
public:
  Vertex* v1, * v2;
  
  Edge(Vertex* va, Vertex* vb) {
    v1 = va;
    v2 = vb;
  }
  //~Edge();
};


class VertexList {
public:
  Vertex* data;
  VertexList* next;

  VertexList() {
    data = nullptr;
    next = nullptr;
  }
  
  ~VertexList() {
    //delete data;
  }
};


class EdgeList {
public:
  Edge* data;
  EdgeList* next;

  EdgeList() {
    data = nullptr;
    next = nullptr;
  }
  
  ~EdgeList() {
    //delete data;
  }
};


class Queue {
public:
  VertexList* head;
  VertexList* tail;

  Queue() {
    head = nullptr;
    tail = nullptr;
  }
  
  ~Queue() {
    while (head != nullptr) {
      VertexList* tempV = head->next;
      //delete head->data; // memory of Vertex* pointers will be
                           // deallocated in ~Graph()
      delete head;
      head = tempV;
    }
  }

  void enqueue(Vertex* v) { // add to end of queue
    VertexList* tempV = new VertexList;
    tempV->data = v;
    if (head == nullptr) { // empty queue
      head = tempV;
      tail = tempV;
    } else { // one or more existing elements
      tail->next = tempV;
      tail = tempV;
    }
  }

  Vertex* dequeue() {
    if (head == nullptr) { // empty queue
      std::cout << "Queue underflows.\n";
      return nullptr;
    } else if (head == tail) { // only one existing element
      VertexList* tempV = head;
      Vertex* tempv = head->data;
      head = nullptr;
      tail = nullptr;
      delete tempV;
      return tempv; // memory of Vertex* tempv will be deallocated in
		    // ~Graph()
    } else { // more than one existing element
      VertexList* tempV = head;
      Vertex* tempv = head->data;
      head = head->next;
      delete tempV;
      return tempv; // memory of Vertex* tempv will be deallocated in
		    // ~Graph()
    }
  }
};


class Set {
public:
  VertexList* VV;

  Set() {
    VV = nullptr;
  }

  ~Set() {
    while (VV != nullptr) {
      VertexList* tempV = VV->next;
      delete VV;
      VV = tempV;
    }
  }
  
  void addVertices(Vertex* v) {
    VertexList* tempV = new VertexList;
    tempV->data = v;
    tempV->next = VV;
    VV = tempV;
  }

  bool notContain(Vertex* v) {
    VertexList* tempV = VV;
    while (tempV != nullptr && tempV->data != v) {
      tempV = tempV->next;
    }
    if (tempV == nullptr) {
      return true;
    } else {
      return false;
    }
  }
};



class Graph {
public:
  VertexList* V;
  EdgeList* E;

  Graph();
  ~Graph();

  void addVertices(Vertex*);
  void addEdges(Vertex*, Vertex*);
  void graphPrint();
  Vertex* BFS(Vertex*, int);
};


Graph::Graph() {
  V = nullptr;
  E = nullptr;
}


Graph::~Graph() {
  while (V != nullptr) {
    VertexList* tempV = V->next;
    delete V->data; 
    delete V;
    V = tempV;
  }
  
  while (E != nullptr) {
    EdgeList* tempE = E->next;
    delete E->data;
    delete E;
    E = tempE;
  }
}


void Graph::addVertices(Vertex* newVertex) {
  VertexList* tempV = new VertexList;
  tempV->data = newVertex;
  tempV->next = V;
  V = tempV;
}


void Graph::addEdges(Vertex* va, Vertex* vb) {
  Edge* newEdge = new Edge(va, vb);
  EdgeList* tempE = new EdgeList;
  tempE->data = newEdge;
  tempE->next = E;
  E = tempE;
}


void Graph::graphPrint() {
  VertexList* tempV = V; // point to beginning of list
  while (tempV != nullptr) { 
    EdgeList* tempE = E; // point to beginning of list for each tempV
			 // iteration
    std::cout << "Vertex: " << tempV->data->key << " - ";
    std::cout << "Edges: ";
    while (tempE != nullptr) {
      if (tempE->data->v1 == tempV->data || tempE->data->v2 == tempV->data) {
	std::cout << tempE->data->v1->key << "-" << tempE->data->v2->key;
	std::cout << ", ";
      }
      tempE = tempE->next;
    }
    std::cout << std::endl;
    tempV = tempV->next; 
  }
  std::cout << std::endl;
}


Vertex* Graph::BFS(Vertex* x, int key) {
  // Create new frontier queue Q
  Queue Q;
  // Create new set V2 (all nodes ever added to frontier queue Q)
  Set V2;
  // Insert x into Q
  Q.enqueue(x);
  // Insert x into V2 (insert at beginning of list is fine)
  V2.addVertices(x);
  // While loop
  while (Q.head != nullptr) {
    Vertex* y = Q.dequeue();
    if (y->key == key) {
      return y;
    }
    // add all neighbors of y not already in V2 to Q and V2
    EdgeList* tempE = E;
    while (tempE != nullptr) {
      if (tempE->data->v1 == y && V2.notContain(tempE->data->v2)) {
	Q.enqueue(tempE->data->v2);
	V2.addVertices(tempE->data->v2);
      } else if (tempE->data->v2 == y && V2.notContain(tempE->data->v1)) {
	Q.enqueue(tempE->data->v1);
	V2.addVertices(tempE->data->v1);
      }
      tempE = tempE->next;
    }
  }
  // Otherwise, return that the key is not found
  std::cout << "Key " << key << " not found.\n";
  return nullptr;
}



int main() {
  // Create a graph like example in Figure 20.1a, p550, Cormen 4th ed
  Graph g; // for this practice, g is an undirected graph
  
  Vertex* vertex1 = new Vertex(1);
  g.addVertices(vertex1);
  Vertex* vertex2 = new Vertex(2);
  g.addVertices(vertex2);
  Vertex* vertex3 = new Vertex(3);
  g.addVertices(vertex3);
  Vertex* vertex4 = new Vertex(4);
  g.addVertices(vertex4);
  Vertex* vertex5 = new Vertex(5);
  g.addVertices(vertex5);
  Vertex* vertex6 = new Vertex(6);
  g.addVertices(vertex6);
  Vertex* vertex7 = new Vertex(7);
  g.addVertices(vertex7);
  
  g.addEdges(vertex1, vertex5); // undirected, order of vertices not matter
  g.addEdges(vertex1, vertex2);
  g.addEdges(vertex2, vertex5);
  g.addEdges(vertex2, vertex4);
  g.addEdges(vertex4, vertex5);
  g.addEdges(vertex3, vertex2);
  g.addEdges(vertex3, vertex4);
  g.addEdges(vertex6, vertex7); // vertices 6 and 7 form a connected component

  // Print
  g.graphPrint();

  // Breadth-First Search (BFS)
  int searchKey = 7;
  Vertex* searchResult = g.BFS(vertex1, searchKey);
  if (searchResult != nullptr) {
    std::cout << "BFS returns key " << searchResult->key << std::endl;
  }
  
  
  return 0;
}
