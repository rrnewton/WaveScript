(letrec ([s1 (audio 0 1024 0)]
	 [s2 (parmap (lambda (w) (fft (fft (to_array w)))) s1)]

#;         [s2 (parmap (lambda (w) (fft w))
		     (parmap (lambda (w) (fft w))
			     (parmap (lambda (w) (fft (to_array w)))
				     s1)))]

         [s3 (iterate
               (lambda (arr0 ___VIRTQUEUE___)
		 (begin
                     (letrec ([x 3])
                       (letrec ([arr (fft (fft arr0))])
                         (if (> (realpart (arr-get arr 100)) 224192.0)
                             (begin
                               (emit ___VIRTQUEUE___ 0.0)
                               (emit
                                 ___VIRTQUEUE___
                                 (imagpart (arr-get arr 100))))
                             (tuple))))
                     ___VIRTQUEUE___))
               s2)])
  s3)
