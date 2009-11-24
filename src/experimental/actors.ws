

/*
 * [2009.03.07] 
 * Experimenting with the notion of impleminting erlang style actors
 * on top of WS.  I don't by any means think that asynchronous
 * dataflow with a fixed operator graph is the one true model that
 * everything should be layered on top of.  I just think it's fun to
 * map between these models, and WS is the one I implemented.
 *
 * My strategy would be to have a small number of WS boxes: one per
 * thread.  Each box is a server that executes actors.  Actors are
 * just pieces of state stored in a hash table.  To keep it simple,
 * combining an actor with a message produces an atomic computation.
 * Servers should be able to message one another to transfer actors as
 * well as exchange messages.  Messaging will require a routing table
 * maintained at each server.  Migration will involve (at some point)
 * modifying the routing tables.  
 *
 */
