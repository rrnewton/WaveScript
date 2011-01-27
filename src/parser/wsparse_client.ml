(* 
   Added this for reduced startup time vs. mzscheme. 

   It's only sometimes better.  Better for small files and worse for
   larger. 

    Wow, still worse for larger files even after I use block-reading
    rather than line -reading below.  I've tried all four different
    socket_types.  I don't know what accounts for the difference.  

    Of course, I'm talking realtime; the Caml process us using
    practically no CPU time.

*)

open Unix;;
open Printf;;

(* printf "Trying socket!\n";; *)

(* SOCK_STREAM  SOCK_RAW SOCK_SEQPACKET SOCK_DGRAM *)
let descr = socket PF_INET SOCK_STREAM 0;;

(* printf "Created socket!\n";; *)

connect descr (ADDR_INET (inet_addr_loopback, 60606));;

(* printf "Yay, socket bound!\n";; *)

let inp  = in_channel_of_descr descr;;
let outp = out_channel_of_descr descr;;

(* fprintf outp "\"~/wavescript/lib/stdlib.ws\"\n";; *)
output_string outp ("\"" ^ Sys.argv.(1) ^ "\"");;
flush outp;;

(* printf "Wrote request:\n";; flush Pervasives.stdout;; *)

(*
try while true do
    print_endline(input_line inp)
  done
with End_of_file -> ();;
*)

let buf = String.create 32768 in
let go = ref true in
while !go do
    let read = input inp buf 0 32768 in
    if read == 0 
    then go := false 
    else output Pervasives.stdout buf 0 read
done;;

flush outp;;
close descr;;

(*printf "Printed something to socket!!\n";;*)

