#lang racket/base
;
; AVL - Binary Search Tree
;

(require racket/generator
         racket/contract
         racket/stream
         racket/match)

(provide avl?)

(provide
  (contract-out
    (make-avl (-> (-> any/c any/c boolean?) avl?))
    (avl-insert (-> avl? any/c avl?))
    (avl-insert! (-> avl? any/c void?))
    (avl-remove (-> avl? any/c avl?))
    (avl-remove! (-> avl? any/c void?))
    (avl-min (-> avl? any/c))
    (avl-max (-> avl? any/c))
    (avl-pop-min (-> avl? (values any/c avl?)))
    (avl-pop-min! (-> avl? any/c))
    (avl-pop-max (-> avl? (values any/c avl?)))
    (avl-pop-max! (-> avl? any/c))
    (avl-empty? (-> avl? boolean?))
    (avl-contains? (-> avl? any/c boolean?))
    (in-avl (-> avl? sequence?))
    (in-avl/reverse (-> avl? sequence?))))


;; Wrapper to hide AVL tree nodes from the user.
;; Possibly mutable, unlike the individual nodes.
(struct avl
  ((<=?)
   (root #:mutable)))

;; An immutable tree node.
(struct node
  (left right value height))


;; Create an empty tree with specified comparator.
(define (make-avl <=?)
  (avl <=? #f))


;; Determine whether is the AVL tree empty or not.
(define (avl-empty? tree)
  (not (avl-root tree)))


;; Create new tree including given value.
(define (avl-insert tree value)
  (match tree
    ((avl <=? root)
     (avl <=? (insert <=? root value)))))


;; Modify an existing tree to include given value.
(define (avl-insert! tree value)
  (match tree
    ((avl <=? root)
     (set-avl-root! tree (insert <=? root value)))))


;; Perform the non-modifying insertion of a value into the tree.
(define (insert <=? parent new-value)
  (match parent
    ((node left right value height)
     (rebalance
       (cond
         ((<=? new-value value)
          (make-node (insert <=? left new-value) right value))

         (else
          (make-node left (insert <=? right new-value) value)))))

    (else
     (make-node #f #f new-value))))


;; Rebalance tree node if required.
(define (rebalance parent)
  (match parent
    ((node left right value _)
     (cond
       ((= (balance parent) 2)
        (if (= (balance left) -1)
            (let ((left (rotate-left left)))
              (rotate-right (make-node left right value)))
            (rotate-right parent)))

       ((= (balance parent) -2)
        (if (= (balance right) 1)
            (let ((right (rotate-right right)))
              (rotate-left (make-node left right value)))
            (rotate-left parent)))

       (else parent)))))


;; Create right-rotated version of the node.
(define (rotate-right parent)
  (match parent
    ((node left right value _)
     (match left
       ((node l-left l-right l-value _)
        (let ((new-right (make-node l-right right value)))
          (make-node l-left new-right l-value)))))))


;; Create left-rotated version of the node.
(define (rotate-left parent)
  (match parent
    ((node left right value _)
     (match right
       ((node r-left r-right r-value _)
        (let ((new-left (make-node left r-left value)))
          (make-node new-left r-right r-value)))))))


;; Create new node, automatically computing height using the
;; higher of left and right children.
(define (make-node left right value)
  (node left right value (add1 (max (height right) (height left)))))


;; Return height of node or 0 for #f.
(define (height maybe-node)
  (if maybe-node (node-height maybe-node) 0))


;; Return balance for node or 0 for #f.
(define (balance maybe-node)
  (match maybe-node
    ((node left right _ _)
     (- (height left)
        (height right)))

    (else 0)))


;; Return minimal (leftmost) value in the tree.
(define (avl-min tree)
  (match tree
    ((avl _ #f)
     (error 'avl-min "empty tree"))

    ((avl _ root)
     (leftmost root))))


;; Return maximal (rightmost) value in the tree.
(define (avl-max tree)
  (match tree
    ((avl _ #f)
     (error 'avl-min "empty tree"))

    ((avl _ root)
     (rightmost root))))


;; Recursively reach leftmost value in the tree of nodes.
(define (leftmost parent)
  (match parent
    ((node #f _ value _)
     (values value))

    ((node left _ _ _)
     (leftmost left))))


;; Recursively reach rightmost value in the tree of nodes.
(define (rightmost parent)
  (match parent
    ((node _ #f value _)
     (values value))

    ((node _ right _ _)
     (rightmost right))))


;; Return tree's minimal item and a new tree without it.
(define (avl-pop-min tree)
  (match tree
    ((avl _ #f)
     (error 'avl-pop-min "empty tree"))

    ((avl <=? root)
     (let-values (((value new-root) (pop-min root)))
       (values value (avl <=? new-root))))))


;; Remove tree's minimal item and return it.
(define (avl-pop-min! tree)
  (match tree
    ((avl _ #f)
     (error 'avl-pop-min! "empty tree"))

    ((avl _ root)
     (let-values (((value new-root) (pop-min root)))
       (set-avl-root! tree new-root)
       (values value)))))


;; Recursively rebuild nodes without the leftmost node,
;; returning it's value and a new tree of nodes.
(define (pop-min parent)
  (match parent
    ((node #f right value _)
     (values value right))

    ((node left right value _)
     (let-values (((result left) (pop-min left)))
       (values result (rebalance (make-node left right value)))))))


;; Return tree's maximal item and a new tree without it.
(define (avl-pop-max tree)
  (match tree
    ((avl _ #f)
     (error 'avl-pop-max "empty tree"))

    ((avl <=? root)
     (let-values (((value new-root) (pop-max root)))
       (values value (avl <=? new-root))))))


;; Remove tree's maximal item and return it.
(define (avl-pop-max! tree)
  (match tree
    ((avl _ #f)
     (error 'avl-pop-max! "empty tree"))

    ((avl _ root)
     (let-values (((value new-root) (pop-max root)))
       (set-avl-root! tree new-root)
       (values value)))))


;; Recursively rebuild nodes without the rightmost node,
;; returning it's value and a new tree of nodes.
(define (pop-max parent)
  (match parent
    ((node left #f value _)
     (values value left))

    ((node left right value _)
     (let-values (((result right) (pop-max right)))
       (values result (rebalance (make-node left right value)))))))


;; Return new AVL tree without specified value.
(define (avl-remove tree value)
  (match tree
    ((avl <=? root)
     (with-handlers ((boolean? (λ _ tree)))
       (let ((new-root (remove-value <=? root value)))
         (avl <=? new-root))))))


;; Remove specified value from the AVL tree.
(define (avl-remove! tree value)
  (match tree
    ((avl <=? root)
     (with-handlers ((boolean? void))
       (let ((new-root (remove-value <=? root value)))
         (set-avl-root! tree new-root))))))


;; Return node tree without specified target.
;; If the value is not present within the tree, raise #f.
(define (remove-value <=? parent victim)
  (match parent
    ((node left right value _)
     (cond
       ((equal? victim value)
        (cond
          ((and left right)
           (let-values (((value right) (pop-min right)))
             (rebalance (make-node left right value))))

          (else
           (or left right))))

       ((<=? victim value)
        (let ((left (remove-value <=? left victim)))
          (rebalance (make-node left right value))))

       ((<=? value victim)
        (let ((right (remove-value <=? right victim)))
          (rebalance (make-node left right value))))))

    (else (raise #f))))


;; Determine whether the tree contains specified value.
(define (avl-contains? tree value)
  (match tree
    ((avl <=? root)
     (contains? <=? root value))))


;; Return boolean value indicating presence of the needle in node tree.
(define (contains? <=? parent needle)
  (match parent
    ((node left right value _)
     (cond
       ((equal? value needle)
        (values #t))

       ((<=? value needle)
        (contains? <=? left needle))

       (else
        (contains? <=? right needle))))

    (else #f)))


;; Create ordered value sequence.
(define (in-avl tree)
  (in-generator
    (let iterate ((parent (avl-root tree)))
      (match parent
        ((node left right value _)
         (iterate left)
         (yield value)
         (iterate right))

        (else #t)))))


;; Create reverse ordered value sequence.
(define (in-avl/reverse tree)
  (in-generator
    (let iterate ((parent (avl-root tree)))
      (match parent
        ((node left right value _)
         (iterate right)
         (yield value)
         (iterate left))

        (else #t)))))


; vim:set ts=2 sw=2 et:
