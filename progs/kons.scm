(struct kons (kar kdr))
(struct mt ())
(define (map f ls)
  (if (kons? ls)
      (kons (f (kons-kar ls))
            (map f (kons-kdr ls)))
      (mt)))

(map add1 (kons 1 (kons 2 (kons 3 (mt)))))
