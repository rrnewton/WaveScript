



/* 
 * This simply reports the node IDs of every node.
 *
 * .author Ryan Newton
 *
 */

//using TOS;
Node:ids = iterate _ in timer(1) { emit getID() }

main = Node:ids;
