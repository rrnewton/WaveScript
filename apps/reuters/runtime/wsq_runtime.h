

/* 
 * This is the interface to the "WaveScope Query" (WSQ) runtime.  It
 * is meant to be used by a control module that manages streaming
 * queries in a distributed environment.
 */

#ifdef __cplusplus 
extern "C" {
#endif 

// The type for unique IDs.
// This is used for operators, edges, subgraphs, and transactions.
typedef int wsid_t;

/// Initialization and shutdown.

// These bring up and bring down the runtime.
//   "outfile" can be "" (for stdout) or the name of an file to receive query output.
void WSQ_Init(const char* outfile);
void WSQ_Shutdown();

void WSQ_SetQueryName(const char* name);

enum WSQBackend {
 C_BACKEND = 1,        // Uses `wsc2` as the backend.
 SCHEME_BACKEND = 2,  // Uses `ws` as the backend.
 SCHEMEQUICK_BACKEND = 3  // Uses `ws-early` as the backend.
}; 

void WSQ_SetBackend(enum WSQBackend mode);

/// Change the pause state. 
// 
// When the stream engine is paused it will register new operators (and possibly compile
// them) but it will not process any tuples.
void WSQ_Pause();
// Like EndTransaction, a nonzero return from Unpause is a process ID.
int  WSQ_Unpause();

//==============================================================================
// Runtime Query Graph Construction.
//==============================================================================

/// Transactions.

// These two calls bracket a transaction.
//void (*WSQ_BeginTransaction)(wsid_t id);
//void (*WSQ_EndTransaction)();

void WSQ_BeginTransaction(wsid_t id);

// If EndTransaction returns a non-zero number it is a process ID for a subprocess that is
// running the current query.
int  WSQ_EndTransaction();


/// Subgraphs.

// Whereas these two bracket a subgraph.
// Begin/End Subgraph are executed *within* a transaction.
// The graph built up inbetween those calls forms a subgraph
// identified by 'id'.  This subgraph is added to the runtime as part
// of the current transaction.
void WSQ_BeginSubgraph(wsid_t id);
void WSQ_EndSubgraph();

// We can also remove already installed subgraphs as part of a transaction.
void WSQ_RemSubgraph(wsid_t id);

// This returns the schema for a given edge.
// This allocates a fresh string.  You are responsible for freeing it.
char* WSQ_EdgeType(wsid_t id);


//------------------------------------------------------------------------------
// Adding operators
//------------------------------------------------------------------------------

/// Stream Operators.

// Within a subgraph block, we add individual operators.
void WSQ_AddProject(wsid_t id, wsid_t in, wsid_t out, const char* expr);
void WSQ_AddFilter (wsid_t id, wsid_t in, wsid_t out, const char* expr);

//void WSQ_AddFilter (wsid_t in, wsid_t out, const char* expr);

// Sources will probably need a bunch more parameters when we understand how things work.
void WSQ_AddReutersSource(wsid_t id, wsid_t out, float freq, const char* schema_path);
void WSQ_AddPrinter(wsid_t id, const char* prefix, wsid_t in);

// void WSQ_AddWindowJoin(id_in1, id_in2, id_out, seconds, "left", "right", "left.FOO == right.FOO")

// void WSQ_AddWindowJoin(id_in1, id_in2, id_out, seconds, "FOO == FOO")
void WSQ_AddWindowJoin(wsid_t id, wsid_t id_in1, wsid_t id_in2, wsid_t id_out, float seconds, const char* recA, const char* recB, const char* expr);

/// Inter-machine connections.

// While we don't need to do anything special to connect two operators
// running on the same machine (even within different subgraphs).  We
// do need to make explicit calls to setup outbound and inbound
// connections to other machines.  This interface doesn't commit to
// any particular implementation strategy.
void WSQ_ConnectRemoteOut (wsid_t id, wsid_t out, const char* host, int port);
void WSQ_ConnectRemoteIn  (wsid_t id, wsid_t in,  const char* host, int port, const char* type_str);

//------------------------------------------------------------------------------
// Generic Interface
//------------------------------------------------------------------------------

// In place of the above per-operator calls, there is a single call
// which can add operators of any kind.

void WSQ_AddOp(wsid_t id, const char* optype, const char* inputs, 
	       const char* outputs, const char* args);


// See README.txt file for the syntax of supported operators (e.g.,
// what to put in the char* fields).


//==============================================================================
// Schema Querying
//==============================================================================

// TODO: 

//==============================================================================
// Runtime Monitoring Interface
//==============================================================================

// TODO: current avg bandwidth
// TODO: current avg cpu

#ifdef __cplusplus 
} // End extern C
#endif 
