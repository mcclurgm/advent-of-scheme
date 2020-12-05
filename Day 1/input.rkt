(define test-data (list 1721 979 366 299 675 1456))
(define test-3 (list 979 4321 366 1721 979 366 299 675 1456))

(define add-valid
  (lambda (x y)
    (= (+ x y) 2020)))

(define add-val-2
  (lambda (val lst)
    (if (null? lst)
        #f ; If lst is null, there's nothing in this round
        ; otherwise, try with (car lst)
        (let ((head (car lst)))
          (if (= (+ val head) 2020)
              (* val head)
              (add-val-2 val (cdr lst))))))) ; if it doesn't work: try with (cdr lst)

(define add-val-3-inner
  (lambda (a b lst)
    (if (null? lst)
        #f ; If lst is null, there's nothing with a and b
        ; otherwise, try with a, b, (car lst)
        (let ((head (car lst)))
          (if (= (+ a b head) 2020)
              (* a b head)
              (add-val-3-inner a b (cdr lst))))))) ; if it doesn't work: try with (cdr lst)
(define add-val-3-middle
  (lambda (a lst)
    (if (null? lst)
        #f ; If lst is null, there's nothing using a
        ; otherwise, try with a and b=(car lst)
        (let ((head (car lst)) (rest (cdr lst)))
          (let ((result (add-val-3-inner a head rest)))
            (if (eq? result #f)
                (add-val-3-middle a rest) ; If result is false, it wasn't found.
                                          ; Try again with a and b=(cadr lst)
                result)))))) ; Otherwise, it was found! Return result.
        
; Given a list
(define add-list-2
  (lambda (lst)
    (if (null? lst)
        #f ; If lst is null, there's no matching pair in this list anywhere.
        ; otherwise, try with (car lst)
        (let ((val (car lst)) (rest (cdr lst)))
          (let ((result (add-val-2 val rest)))
            (if (eq? result #f)
                (add-list-2 rest) ; If result is false, it wasn't found.
                                  ; Try again with a=(cadr lst).
                result)))))) ; Otherwise, it was found! Return result.
(define test
  (lambda (mess x)
    (displayln mess)
    x))
; Given a list
(define add-list-3
  (lambda (lst)
    (if (null? lst)
        #f ; If lst is null, there's no matching pair in this list anywhere.
        ; otherwise, try with (car lst)
        (let ((a (car lst)) (rest (cdr lst)))
          (let ((result (add-val-3-middle a rest)))
            (if (eq? result #f)
                (add-list-3 rest) ; If result is false, it wasn't found.
                                  ; Try again with a=(cadr lst).
                result)))))) ; Otherwise, it was found! Return result.

;; BOILERPLATE
(define list-from-filename
  (lambda (name)
    (call-with-input-file name list-from-file)))
(define list-from-file
    (lambda (input)
      (let ((val (read input)))
        (if (eof-object? val)
            '()
            (cons val (list-from-file input))))))