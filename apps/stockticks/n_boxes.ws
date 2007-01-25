
// This tests the effectiveness of merge-iterates.

// Run a loop to make 

fun loop(n) {
  if n == 0
  then iterate(w in audioFile("countup.raw", 2, 0)) 
    {
      emit w[[w.start]];
      //emit 0;
    } 
  else iterate (x in loop(n-1))
    {
      emit x + x - x;
      //emit x + 1;
    }
}

BASE <- loop(3);





// Elaborated:
/*

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
