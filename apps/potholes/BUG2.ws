

// BUG BUG BUG:



xw = iterate win in timer(3.0) { }
BASE <- iterate(_ in unionList([xw])) { emit 3 }


//BASE <- iterate(_ in unionList([timer(3.0)])) { emit 3 }


/*

(lift-generics-language
  '(program
     (letrec ([xw_1 (Stream 'bl) (iterate
                                   (lambda (win_3 ___VIRTQUEUE____2)
                                     (#() (VQueue 'bl))
                                     ___VIRTQUEUE____2)
                                   (timer '3.0))])
       (iterate
         (lambda (__5 ___VIRTQUEUE____4)
           (#(Int 'ay) (VQueue Int))
           (begin (emit ___VIRTQUEUE____4 '3) ___VIRTQUEUE____4))
         (unionN xw_1)))
     (union-types)
     (Stream Int)))


(retypecheck '(lift-generics-language
  '(program
     (letrec ([xw_1 (Stream 'bl) (iterate
                                   (lambda (win_3 ___VIRTQUEUE____2)
                                     (#() (VQueue 'bl))
                                     ___VIRTQUEUE____2)
                                   (timer '3.0))])
       (iterate
         (lambda (__5 ___VIRTQUEUE____4)
           (#(Int 'ay) (VQueue Int))
           (begin (emit ___VIRTQUEUE____4 '3) ___VIRTQUEUE____4))
         (unionN xw_1)))
     (union-types)
     (Stream Int))))

*/
