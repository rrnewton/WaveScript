

/* 
 * This is the interface to the "WaveScope Query" (WSQ) runtime.  It
 * is meant to be used by a control module that manages streaming
 * queries in a distributed environment.
 */

// The type for unique IDs.
// This is used for operators, edges, subgraphs, and transactions.
typedef int id_t;

/// Initialization and shutdown.

// These bring up and bring down the runtime.
void WSQ_Init();
void WSQ_Shutdown();

//==============================================================================
// Runtime Query Graph Construction.
//==============================================================================

/// Transactions.

// These two calls bracket a transaction.
//void (*WSQ_BeginTransaction)(id_t id);
//void (*WSQ_EndTransaction)();

void WSQ_BeginTransaction(id_t id);
void WSQ_EndTransaction();

/// Subgraphs.

// Whereas these two bracket a subgraph.
// Begin/End Subgraph are executed *within* a transaction.
// The graph built up inbetween those calls forms a subgraph
// identified by 'id'.  This subgraph is added to the runtime as part
// of the current transaction.
void WSQ_BeginSubgraph(id_t id);
void WSQ_EndSubgraph();

// We can also remove already installed subgraphs as part of a transaction.
void WSQ_RemSubgraph(id_t id);

/// Stream Operators.

// Within a subgraph block, we add individual operators.
void WSQ_AddProject(id_t in, id_t out, char* expr);
void WSQ_AddFilter (id_t in, id_t out, char* expr);

// Sources will probably need a bunch more parameters when we understand how things work.
void WSQ_AddReutersSource(id_t id, char* schema_path);
void WSQ_AddPrinter(id_t id);

// void WSQ_AddWindowJoin(id_in1, id_in2, id_out, seconds, "left", "right", "left.FOO = right.FOO")


/// Inter-machine connections.

// While we don't need to do anything special to connect two operators
// running on the same machine (even within different subgraphs).  We
// do need to make explicit calls to setup outbound and inbound
// connections to other machines.  This interface doesn't commit to
// any particular implementation strategy.
void WSQ_ConnectRemoteOut (id_t out, char* host, int port);
void WSQ_ConnectRemoteIn  (id_t in,  char* host, int port);


//==============================================================================
// Schema Querying
//==============================================================================

// TODO: 

//==============================================================================
// Runtime Monitoring Interface
//==============================================================================

// TODO: current avg bandwidth
// TODO: current avg cpu

