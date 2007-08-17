#! /bin/bash
#|
exec regiment i --script "$0"
|#

#|
|#

#;
(define rawdat 
  '((112 -5228.900146 -9371.299744 1090.900040 -10.800000 0.500000 1.600000 0.000000 0.000000 0.000000 1)
    (115 -3826.100159 -14488.400269 1119.099998 13.600000 0.000000 0.800000 0.000000 0.000000 0.000000 1)
    (104 1507.100010 -3799.100113 -98.000002 -9.500000 -1.900000 -1.800000 0.000000 0.000000 0.000000 1)
    (108 -3265.200043 -4839.799881 763.600016 5.000000 -2.500000 0.300000 0.000000 0.000000 0.000000 1)
    (100 0.000000 0.000000 0.000000 14.400000 -1.500000 -2.300000 0.000000 0.000000 0.000000 1)
    (109 972.099972 -7192.500305 176.400006 -160.699997 -0.100000 -1.400000 0.000000 0.000000 0.000000 1)
    (113 405.700016 -13919.900513 693.300009 -9.900000 0.300000 -1.300000 0.000000 0.000000 0.000000 1)
    (103 -4006.600189 -1251.299953 576.999998 16.500000 -2.300000 0.000000 0.000000 0.000000 0.000000 1)))

(printf "\nUpdating node locations...\n")
(flush-output-port)

(if (file-exists? "loc.txt")
    (printf "  \'loc.txt\' exists so I'm using it.  (Delete it if you want to refetch locations):\n")
    (begin 
      (printf "Logging into gateway and get locations from /dev/loc/ ...\n")
      (let ([code (system "ssh root@192.168.11.1 cat /dev/loc/dump_coords > loc.txt")])
	(unless (zero? code)
	  (printf "\nSSH COMMAND FAILED\n")
	  (exit -1))
	(printf "\nSSH succeeded, created loc.txt:\n\n")
	)))

(system "cat loc.txt")

(define (extract ls)
  (list (list-ref ls 0) (list-ref ls 1) (list-ref ls 2) (list-ref ls 4)))

(define rawdat 
  (filter (compose not null?)
    (map string->slist (file->lines "loc.txt"))))


(define file (open-output-file "nodelocs.ws" 'replace))

(for-eachi 
 (lambda (i row)
   (printf "ERK ~s\n" row)
   (fprintf file "node~s = (~a)\n" 
	   (add1 i)
	   (apply string-append
		  (insert-between ", "
		    (map number->string 
		      (extract row))))))
 (sort 
  (lambda (a b) (< (car a) (car b)))
  rawdat))

(close-output-port file)

(printf "\nCreated nodelocs.ws\n\n")
(system "cat nodelocs.ws")
