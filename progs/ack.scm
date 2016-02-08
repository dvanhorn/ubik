(define (ack m n)
  (if (= m 0) 
      (add1 n)
      (if (= n 0) 
	  (ack (sub1 m) 1)
	  (ack (sub1 m) (ack m (sub1 n))))))

(ack 3 7)
