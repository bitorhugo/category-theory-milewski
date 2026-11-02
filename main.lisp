;; 1

(defun f (a) ':b)

(defun g (b) ':c)

(defun id (a) a)

(defun compose (&rest fns)
  (let ((fst (car (last fns))))
    (lambda (&rest args)
      (reduce #'funcall (butlast fns)
	      :initial-value (apply fst args)
	      :from-end t))))

#+n(defun compose (&rest fns)
     (labels ((%compose (fns args)
		(if (null (cdr fns))
		    (apply (car fns) args)
		    (funcall (car fns)
			     (%compose (cdr fns) args)))))
       (lambda (&rest args)
	 (%compose fns args))))

(defun composition-respects-identity-p (f identity)
  (let* ((domain ':a)
	 (codomain (funcall f domain)))
    (and (eq (funcall (compose f identity) domain) ; left identity
	     codomain)
	 (eq (funcall (compose identity f) domain) ; right identity
	     codomain))))

;; Q: (1) Is the world-wide web a category in any sense?
;;    (2) Are links morphisms?
;; A: (1) No, because I'm assuming that not every website has a link
;;        that targets it's own domain (identity morphism). Also because
;;        a website A with a link to B, and a website B with a link to C
;;        does not imply that A has a link to C (composition).
;;    (2) Yes, in the sense that they point one website to another,
;;        giving a sense of direction from one object to another (A -> B).

;; Q: Is Facebook a category, with people as objects and friendships as
;;    morphisms?
;; A: No, for at least 3 reasons:
;;    first because you cannot be a friend of yourself;
;;    secondly, because friendships are modeled as an undirected graph,
;;    meaning they lack a sense of direction;
;;    lastly, just because I'm friends with Paul and Paul has a friend
;;    called Maggie, it doesn't mean that I'm friends with Maggie. She
;;    didn't accepted my request :(
;; Remark: Someone born after 2010, probably never used Facebook.
;;         This is comparable to asking someone born in the early 2000s
;;         about hi5 or MySpace.

;; Q: When is a directed graph a Category?
;; A: (1) When for every node, there exists an edge from and to itself;
;;    (2) In a Category, if I have a morphism f: A -> B, and another
;;        morphism g: B -> C, then I have h: A -> C; When modeling a
;;        Category as a DAG, one needs to decide whether h needs to be an
;;        edge of its own, or if having B as scaffold is enough.
;;        In my opinion, having A -> C present, gives an an edge (eheh)
;;        in readability.


;; 2

product of itself, with its antecedent

(defun product (lst)
  (reduce #'* lst :initial-value 1))

(defun between (start end)
  (labels ((recur (start n)
	     (if (<= n start)
		 (list start)
		 (cons n (recur start (1- n))))))
    (reverse (recur start end))))

(defun factorial (a)
  (product (between 1 a)))

(defun absurd (void)
  (declare (ftype (function ((satisfies null)) t)
		  absurd)
	   (ignore void))
  (error "Cannot invoke.")
  't)

;; this is a pair whose 2nd value is a pair
(a (b (c (d nil))))

(defun unit (a) (declare (ignore a)))

(defun memoize (f)
  (let ((memo (make-hash-table :test 'equalp)))
    (lambda (&rest args)
      (or (gethash (cons f args) memo)
	  (setf (gethash (cons f args) memo)
		(apply f args))))))

(defun test-memo ()
  (let* ((memo-fact (memoize #'factorial))
	 (memo-random (memoize #'random))
	 (memo-random-seed (memoize #'random)))
    ;; factorial
    (time (funcall memo-fact 10000))
    (time (funcall memo-fact 10000))
    ;; random
    (format t "Random Test:~%Call-1: res=~a~%Call-2: res=~a~%"
	    (funcall memo-random 42)
	    (funcall memo-random 42))
    ;; random with seed
    (format t "Random with Seed Test:~%Call-1: res=~a~%Call-2: res=~a~%"
	    (funcall memo-random-seed 42 (seed-random-state t))
	    (funcall memo-random-seed 42 (seed-random-state t)))))


;; Q: Try to memoize a function from your standard library that you
;;    normally use to produce random numbers. Does it work?
;; A: No because once we memoize expects pure functions, meaning we cannot
;;    memoize function with side effects since we no longer evaluate their
;;    body.
;;
;; Q: Does memoizing random with a seed work as expected?
;; A: Yes
;;
;; Q: Which of these functions are pure? (fact, getchar, "id")
;; A: Only getchar is impure
;;
;; Q: How many different functions are there from Bool to Bool? Can
;;    you implement them all?
;; A: There are 2 x 2 counts of functions from Bool to Bool.
;;    Yes, since Bool is a finite set.
;; Remarks: In Haskell, every type is extend with one additional value,
;;          called __bottom__ (_|_). Taking this in consideration,
;;          we have 3 x 3 counts of functions from Bool to Bool.
