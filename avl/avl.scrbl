#lang scribble/manual

@require[scribble/eval]

@require[(for-label racket)
         (for-label "main.rkt")]

@define[avl-eval (make-base-eval)]
@interaction-eval[#:eval avl-eval (require "main.rkt")]

@title{AVL Trees}
@author+email["Jan Dvořák" "mordae@anilinux.org"]

@defmodule[avl]

A self-balancing binary search tree variant.

All mutations of the AVL tree create new nodes instead of modifying the
data in place.  The imperative variants change the root node in place
for convenience.  Mutating the tree is not thread-safe.

These trees be used for as priority queues with possibility to remove
elements from the middle.

@section{Creating Trees}

@deftogether[(@defproc[(make-avl (<=? procedure?)) avl?]
              @defproc[(make-avleq (<=? procedure?)) avl?]
              @defproc[(make-avleqv (<=? procedure?)) avl?])]{
  Create new tree using specified comparator function.

  Like hash tables, every AVL tree variant uses a different equality
  predicate.  @racket[make-avl] uses @racket[equal?], @racket[make-avleq]
  uses @racket[eq?] and @racket[make-avleqv] uses @racket[eqv?].

  Tree with @racket[number?] elements would use @racket[<=] as the comparator,
  tree with @racket[string?] elements would use @racket[string<=?] and so on.

  @examples[#:eval avl-eval
    (define tree (make-avleqv <=))
    (avl-add! tree 42)
  ]
}

@defproc[(make-custom-avl (<=? procedure?) (=? procedure?)) avl?]{
  Create new tree using both specified comparator function and
  equality predicate.

  @examples[#:eval avl-eval
    (define custom-avl
      (make-custom-avl (λ (x y) (<= (car x) (car y)))
                       (λ (x y) (equal? (car x) (car y)))))
    (avl-add! custom-avl (cons 1 'hello))
    (avl-add! custom-avl (cons 2 'ciao))
    (avl-add! custom-avl (cons 1 'bye))
    (avl->list custom-avl)
  ]
}

@defproc[(avl-copy (tree avl?)) avl?]{
  Copy the tree container, effectively creating a standalone tree that is
  decoupled from the original.

  @examples[#:eval avl-eval
    (define copy (avl-copy tree))
    (avl-remove! copy 42)
  ]
}


@section{Predicates}

@defproc[(avl? (v any/c)) boolean?]{
  Predicate identifying the AVL tree.

  @examples[#:eval avl-eval
    (avl? tree)
    (avl? copy)
    (avl? 'something-else)
  ]
}

@deftogether[(@defproc[(avl-equal? (v any/c)) boolean?]
              @defproc[(avl-eqv? (v any/c)) boolean?]
              @defproc[(avl-eq? (v any/c)) boolean?])]{
  Predicates for trees created using respective constructors above.

  @examples[#:eval avl-eval
    (avl-equal? tree)
    (avl-eqv? tree)
    (avl-eq? tree)
  ]
}

@defproc[(avl-empty? (tree avl?)) boolean?]{
  Determine whether the tree contains no values.

  @examples[#:eval avl-eval
    (avl-empty? tree)
    (avl-empty? copy)
  ]
}

@defproc[(avl-contains? (tree avl?) (value any/c)) boolean?]{
  Determine whether the tree contains specified value at least once.

  @examples[#:eval avl-eval
    (avl-contains? tree 42)
    (avl-contains? copy 42)
  ]
}


@section{Manipulating Values}

@defproc[(avl-add (tree avl?) (value any/c)) avl?]{
  Create new tree containing specified @racket[value].

  @examples[#:eval avl-eval
    (let ((new-tree (avl-add tree 13)))
      (avl-contains? new-tree 13))
    (avl-contains? tree 13)
  ]
}

@defproc[(avl-add! (tree avl?) (value any/c)) void?]{
  Like @racket[avl-add], but the container is modified in place.

  @examples[#:eval avl-eval
    (avl-add! tree 13)
    (avl-contains? tree 13)
  ]
}

@defproc[(avl-remove (tree avl?) (value any/c)) avl?]{
  Create new tree without the first instance of the @racket[value].

  @examples[#:eval avl-eval
    (let ((new-tree (avl-remove tree 13)))
      (avl-contains? new-tree 13))
    (avl-contains? tree 13)
  ]
}

@defproc[(avl-remove! (tree avl?) (value any/c)) void?]{
  Like @racket[avl-remove], but the container is modified in place.

  @examples[#:eval avl-eval
    (avl-remove! tree 13)
    (avl-contains? tree 13)
  ]
}


@section{Queue Usage}

@defproc[(avl-min (tree avl?)) any/c]{
  Find smallest (leftmost) value in the tree.

  @examples[#:eval avl-eval
    (avl-add! tree 21)
    (avl-min tree)
  ]
}

@defproc[(avl-max (tree avl?)) any/c]{
  Find largest (rightmost) value in the tree.

  @examples[#:eval avl-eval
    (avl-add! tree 101)
    (avl-max tree)
  ]
}

@defproc[(avl-pop-min (tree avl?)) (values any/c avl?)]{
  Find and remove smallest (leftmost) value from the tree.
  Returns both the removed value and new tree container.

  @examples[#:eval avl-eval
    (avl-pop-min tree)
    (avl-min tree)
  ]
}

@defproc[(avl-pop-min! (tree avl?)) any/c]{
  Like @racket[avl-pop-min], but returns only the value and
  modifies the container in place.

  @examples[#:eval avl-eval
    (avl-pop-min! tree)
    (avl-min tree)
  ]
}

@defproc[(avl-pop-max (tree avl?)) (values any/c avl?)]{
  Find and remove largest (rightmost) value from the tree.
  Returns both the removed value and new tree container.

  @examples[#:eval avl-eval
    (avl-pop-max tree)
    (avl-max tree)
  ]
}

@defproc[(avl-pop-max! (tree avl?)) any/c]{
  Like @racket[avl-pop-max], but returns only the value and
  modifies the container in place.

  @examples[#:eval avl-eval
    (avl-pop-max! tree)
    (avl-max tree)
  ]
}


@section{Iterating Over Values}

@defproc[(in-avl (tree avl?)) sequence?]{
  Iterate over the tree values in ascending order.

  @examples[#:eval avl-eval
    (for/list ((value (in-avl tree)))
      (* value 10))
  ]
}

@defproc[(in-avl/reverse (tree avl?)) sequence?]{
  Iterate over the tree values in descending order.

  @examples[#:eval avl-eval
    (for/list ((value (in-avl/reverse tree)))
      (/ value 10))
  ]
}

@defproc[(avl->list (tree avl?)) list?]{
  Convert the tree to an ordered list.

  @examples[#:eval avl-eval
    (avl->list tree)
    (avl->list copy)
  ]
}


@; vim:set ft=scribble sw=2 ts=2 et:
