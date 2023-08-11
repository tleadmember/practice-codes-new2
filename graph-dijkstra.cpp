/*
TQBH - 2023-07-02
Graph and Dijkstra's Algorithm
*/

#include <iostream>
#include <set>

using namespace std;


#define INF 2147483640; // define infinity, near the max of int


// A graph has a set of vertices and a set of edges


class Vertex {
public:
  int key;
  Vertex* p; // predecessor pointer (in shortest-paths tree)
  int d;
  
  Vertex(int newKey) {
    key = newKey;
    p = nullptr;
    d = INF;
  }
  //~Vertex();
};


class Edge {
public:
  Vertex* v1, * v2;
  int weight;
  
  Edge(Vertex* va, Vertex* vb, int newWeight) {
    v1 = va;
    v2 = vb;
    weight = newWeight;
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



class myComparator {
public:
  int operator() (Vertex* v1, Vertex* v2) {
    return v1->d > v2->d;
  }
};




class Graph {
public:
  VertexList* V;
  EdgeList* E;

  Graph();
  ~Graph();

  void addVertices(Vertex*);
  void addEdges(Vertex*, Vertex*, int);
  void graphPrint();
  void pathPrint(Vertex*, Vertex*);
  void dijkstra(Vertex*);
  void initializeSS(Vertex*);
  void relax(Vertex*, Vertex*, Edge*);
  bool check(Vertex*, Vertex*, Edge*);
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


void Graph::addEdges(Vertex* va, Vertex* vb, int newWeight) {
  Edge* newEdge = new Edge(va, vb, newWeight);
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
      if (tempE->data->v1 == tempV->data) {
	std::cout << tempE->data->v1->key << "->" << tempE->data->v2->key;
	std::cout << ", ";
      }
      tempE = tempE->next;
    }
    std::cout << std::endl;
    tempV = tempV->next; 
  }
  std::cout << std::endl;
}


void Graph::pathPrint(Vertex* s, Vertex* v) { // source vertex s,
					      // target vertex v
  if (v == s) {
    std::cout << s->key << "  ";
  } else if (v->p == nullptr) {
    std::cout << "No path from " << s->key << " to " << v->key << std::endl;
    return; // end function in this case
  } else {
    pathPrint(s, v->p);
    std::cout << v->key << "  ";
  }	     
}


void Graph::dijkstra(Vertex* s) { // source vertex s
  // Initialize-single-source (distance, predecessor)
  initializeSS(s);
  // Create empty Q, min priority queue (maybe use STL min heap)
  std::set <Vertex*, myComparator> Q;
  // Create empty set S (DOES NOT REALLY SERVE A PURPOSE HERE)
  Set S;
  // Add all vertices to Q
  VertexList* tempV = V;
  while (tempV != nullptr) {
    Q.insert(tempV->data);
    tempV = tempV->next;
  }
  // While Q is not empty (length/size != 0)
  while (!Q.empty()) {
  //   u = vertex with d == Q.min-extract(), head of a min heap
    Vertex* u = *Q.begin(); // dereference the iterator to get the pointer
    Q.erase(Q.begin());
  //   add u to S
    S.addVertices(u);
  //   for each successor v of u
    EdgeList* tempE = E;
    while (tempE != nullptr) {
  //     relax(u,v)
      if (tempE->data->v1 == u) {
	if (check(u, tempE->data->v2, tempE->data)) {
	  Q.erase(tempE->data->v2); // erase pointer variable, not vertex
				    // memory
	  relax(u, tempE->data->v2, tempE->data);
	  
	  Q.insert(tempE->data->v2); // insert a new pointer to vertex
				     // (which is a new set element)
	}
      }
      tempE = tempE->next;
    }
  }
}


void Graph::initializeSS(Vertex* s) {
  VertexList* tempV = V;
  while (tempV != nullptr) {
    tempV->data->d = INF;
    tempV->data->p = nullptr;
    tempV = tempV->next;
  }
  s->d = 0;
}


bool Graph::check(Vertex* u, Vertex* v, Edge* e) {
  if (v->d > (u->d + e->weight)) {
    return true;
  }
  return false;
}


void Graph::relax(Vertex* u, Vertex* v, Edge* e) {
    v->d = u->d + e->weight;
    v->p = u;
}



int main() {
  // Create a graph like example in Figure 22.6, p621, Cormen 4th ed
  Graph g; // for this practice, g is an directed graph
  
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
  
  g.addEdges(vertex1, vertex2, 10); // directed edges, weighted
  g.addEdges(vertex1, vertex3, 5);
  g.addEdges(vertex2, vertex3, 2);
  g.addEdges(vertex2, vertex4, 1);
  g.addEdges(vertex3, vertex2, 3);
  g.addEdges(vertex3, vertex4, 9);
  g.addEdges(vertex3, vertex5, 2);
  g.addEdges(vertex4, vertex5, 4);
  g.addEdges(vertex5, vertex1, 7);
  g.addEdges(vertex5, vertex4, 6);
    
  // Print
  g.graphPrint();

  // Call Dijkstra's algorithm
  g.dijkstra(vertex1);
  g.pathPrint(vertex1, vertex5);
  
  return 0;
}
