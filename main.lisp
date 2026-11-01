;;

(defun f (a) ':b)

(defun g (b) ':c)

(defun identity (a) a)

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
