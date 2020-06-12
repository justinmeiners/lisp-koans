


(defun vec-zero (n &key (element-type 'float))
  (make-array n 
              :element-type element-type
              :initial-element (coerce 0 element-type)))

(defun vec-basis (n i &key (element-type 'float))
  (let ((r (vec-zero n :element-type element-type)))
    (progn
      (setf (aref r i) (coerce 1 element-type))
      r)))

(defun vec-scale (a v)
    (map 'vector (lambda (x) (* a x)) v))

(defun vec-add (x y)
    (map 'vector #'+ x y))

(defun vec-mult (x y)
    (map 'vector #'* x y))

(defun vec-sub (x y)
    (map 'vector #'- x y))

(defun vec-dot (x y)
    (reduce #'+ (vec-mult x y)))

(defun vec-len (v)
    (sqrt (vec-dot v v)))

(defun vec-normalize (v)
  (vec-scale (/ 1 (vec-len v)) v))

(defun vec-lp-norm (v p)
  (expt 
    (reduce #'+ (map 'vector (lambda (a) (expt (abs a) p)) v))
    (/ 1 p)))

(defun vec-infinity-norm (v)
  (reduce (lambda (acc x)
            (if (> (abs x) acc)
              x
              acc)) v))

(print (vec-sub #(1 3 4) #(2 2 2)))

(print (vec-lp-norm #(1.0 1.0 1.0) 2.0))

(print (vec-infinity-norm #(1.0 1.0 1.0)))

(defconstant vec-i (vec-basis 3 0))
(defconstant vec-j (vec-basis 3 1))
(defconstant vec-k (vec-basis 3 2))

(print vec-k)




