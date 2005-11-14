
(parameters 
  [simalpha-channel-model 'lossless]
  [simalpha-placement-type 'connected]
  [simalpha-failure-model  'none]
  ;[simalpha-sense-function sense-noisy-rising]
  [simalpha-sense-function sense-random-1to100]
  [simalpha-timeout 2000])


;; Main program
(rwhen-any (lambda (pr) (> (car (cdr pr)) 99))
	   (rmap (lambda (n) (cons (nodeid n) (cons (sense n) '())))
		 world))

