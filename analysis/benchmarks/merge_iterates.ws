
// This tests the effectiveness of merge-iterates.

// Run a loop to make 

fun loop(n) {
  if n == 0
  then iterate(w in audioFile("countup.raw", 2, 0)) 
    {
      //emit w[[w.start]];
      emit 0;
    } 
  else iterate (x in loop(n-1))
    {
      //emit x + x - x;
      emit x + 1;
    }
}

BASE <- loop(3);


// Elaborated:
/*

(static-elaborate-language
  '(program
     (iterate
       (lambda #0=(x_3)
         #1=(Int)
         (letrec ([VIRTQUEUE_4 #2=(VQueue Int) (virtqueue)])
           (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
       (iterate
         (lambda #0#
           #1#
           (letrec ([VIRTQUEUE_4 #2# (virtqueue)])
             (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
         (iterate
           (lambda #0#
             #1#
             (letrec ([VIRTQUEUE_4 #2# (virtqueue)])
               (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
           (iterate
             (lambda (w_5)
               ((Sigseg Int))
               (letrec ([VIRTQUEUE_6 (VQueue Int) (virtqueue)])
                 (begin
                   (emit VIRTQUEUE_6 (seg-get w_5 (start w_5)))
                   VIRTQUEUE_6)))
             (audioFile '"countup.raw" '2 '0)))))
     (Signal Int)))



////////////////////////////////////////////////////////////////////////////////


(static-elaborate-language
  '(program
     (iterate
       (lambda #0=(x_3)
         (Int)
         (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
           (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
       (iterate
         (lambda #0#
           (Int)
           (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
             (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
         (iterate
           (lambda #0#
             (Int)
             (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
               (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
           (iterate
             (lambda (w_5)
               ((Sigseg Int))
               (letrec ([VIRTQUEUE_6 (VQueue Int) (virtqueue)])
                 (begin
                   (emit VIRTQUEUE_6 (seg-get w_5 (start w_5)))
                   VIRTQUEUE_6)))
             (audioFile '"countup.raw" '2 '0)))))
     (Signal Int)))

////////////////////////////////////////////////////////////////////////////////



(verify-elaborated-language
  '(program
     (iterate
       (lambda #0=(x_3)
         (Int)
         (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
           (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
       (iterate
         (lambda #0#
           (Int)
           (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
             (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
         (iterate
           (lambda #0#
             (Int)
             (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
               (begin (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))
           (iterate
             (lambda (w_5)
               ((Sigseg Int))
               (letrec ([VIRTQUEUE_6 (VQueue Int) (virtqueue)])
                 (begin
                   (emit VIRTQUEUE_6 (seg-get w_5 (start w_5)))
                   VIRTQUEUE_6)))
             (audioFile '"countup.raw" '2 '0)))))
     (Signal Int)))


////////////////////////////////////////////////////////////////////////////////


(merge-iterates-language
  '(program
     (iterate
       (lambda (w_5)
         ((Sigseg Int))
         (letrec ([VIRTQUEUE_4 (VQueue Int) (virtqueue)])
	 (letrec ([f_3 (Int -> Int) (lambda (x_3)
	   (Int)
                                        (letrec ([f_2 (Int -> Int) (lambda (x_3)
                                                                     (Int)
                                                                     (letrec ([f_1 (Int -> Int) (lambda (x_3)
                                                                                                  (Int)
                                                                                                  (begin
                                                                                                    (emit VIRTQUEUE_4 (- (+ x_3 x_3) x_3))
                                                                                                    VIRTQUEUE_4))])
                                                                       (begin (app f_1 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))])
                                          (begin (app f_2 (- (+ x_3 x_3) x_3)) VIRTQUEUE_4)))])
             (begin (app f_3 (seg-get w_5 (start w_5))) VIRTQUEUE_6))))
       (audioFile '"countup.raw" '2 '0))
     (Signal Int)))




*/










/*







(remove-complex-opera*-language
  '(program
     (lazy-letrec
       ((tmpiterate_4
          (Signal Int)
          (iterate
            (lambda #0=(x_3)
              (Int)
              (lazy-letrec
                ()
                (lazy-letrec
                  ((VIRTQUEUE_4 (VQueue Int) (virtqueue)))
                  (begin (emit VIRTQUEUE_4 (+ x_3 '1)) VIRTQUEUE_4))))
            tmpiterate_3))
         (tmpiterate_3
           (Signal Int)
           (iterate
             (lambda #0#
               (Int)
               (lazy-letrec
                 ()
                 (lazy-letrec
                   ((VIRTQUEUE_4 (VQueue Int) (virtqueue)))
                   (begin (emit VIRTQUEUE_4 (+ x_3 '1)) VIRTQUEUE_4))))
             tmpiterate_2))
         (tmpiterate_2
           (Signal Int)
           (iterate
             (lambda (w_5)
               ((Sigseg Int))
               (lazy-letrec
                 ()
                 (lazy-letrec
                   ((VIRTQUEUE_6 (VQueue Int) (virtqueue)))
                   (begin (emit VIRTQUEUE_6 '0) VIRTQUEUE_6))))
             tmpaudioFile_1))
         (tmpaudioFile_1
           (Signal (Sigseg Int))
           (audioFile '"countup.raw" '2 '0)))
       (iterate
         (lambda #0#
           (Int)
           (lazy-letrec
             ()
             (lazy-letrec
               ((VIRTQUEUE_4 (VQueue Int) (virtqueue)))
               (begin (emit VIRTQUEUE_4 (+ x_3 '1)) VIRTQUEUE_4))))
         tmpiterate_4))
     (Signal Int)))








*/
