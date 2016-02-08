(struct kons (kar kdr))
(struct mt ())
(define (map f ls)
  (match ls
    ((mt) (mt))
    ((kons a b) (kons (f a) (map f b)))))

(map add1 (kons 1 (kons 2 (kons 3 (mt)))))
