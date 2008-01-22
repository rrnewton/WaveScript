
// [2007.11.01] Going ahead and factoring these out because of their utility in all demos.

//fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);
//fun assert(s,bool)   if not(bool) then wserror("Assert failed in '"++s++"' ");

assertchatter = GETENV("ASSERTSILENT") == ""

fun assert_prnt(str,pred) {
  assert(str,pred);
  if assertchatter then print("Assert passed: "++ str ++ "\n");
}
fun assert_eq_prnt(str,a,b) {
  assert_eq(str,a,b);
  if assertchatter then print("Assert passed: "++ str ++ "\n");
}
