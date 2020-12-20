#lang racket
;; Requires Racket (for the range and curry functions).
;; I could fix this, but I don't feel like it right now.

;; Tells whether the character in the map is a "tree".
;; Given a line (the string that defines the line),
;; and the index in that string to check.
;; If it is a "tree", returns 1, otherwise 0.
;; For example, (is-tree ".#" 1) -> 1
(define is-tree
  (lambda (line index)
    (let* ((index-wrapped (modulo index (string-length line)))
           (ch (string-ref line index-wrapped)))
        (if (char=? ch #\#)
            1
            0))))

;; Gets the number of trees you'd encounter in the map, for a 
;; hard-coded slope of down 1, over 3.
(define number-of-trees-1
  (lambda (map-lines)
    (let* ((indices (map (curry * 3)
                         (range 0 (length map-lines))))
           (trees (map is-tree map-lines indices)))
      (apply + trees))))

;; For part 2: since vertical component of slope varies, I need to filter down
;; the list to be only the lines I will actually use
;; For part 2: since vertical component of slope varies, I need to filter down
;; the list to be only the lines I will actually use
(define every-nth-tail
  (lambda (lst use? index result)
    (if (null? lst)
        result
        (let ((next-index (+ index 1))
              (next-lst (cdr lst)))
          (if (use? index)
              (every-nth-tail next-lst
                              use?
                              next-index
                              (cons (car lst) result))
              (every-nth-tail next-lst
                              use?
                              next-index
                              result))))))
;; Gets every nth element of lst, starting with start.
(define every-nth
  (lambda (lst n start)
    (let* ((use? (lambda (index)
                   (and (zero? (modulo (- index start) n))
                        (>= index start)))))
      (every-nth-tail lst use? 0 '()))))
      
;; Gets the number of trees you'd encounter in the map.
;; This uses a user-defined slope of down v, over h.
(define number-of-trees-2
  (lambda (lines v h)
    (let* ((filtered-lines (every-nth lines v 0)) ; Every line the vertical component of the slope will hit
           (indices (map (curry * h)
                     (range 0 (length filtered-lines))))
           (trees (map is-tree filtered-lines indices)))
      (apply + trees))))

(define multiply-slopes
  (lambda (slopes lines)
    (let* ((trees-for-slope (curry number-of-trees-2 lines))
           (trees-in-map (curry apply trees-for-slope)))
      (apply * (map trees-in-map slopes)))))

;; basic file I/O
(define lines-from-filename
  (lambda (name)
    (call-with-input-file name lines-from-file)))
(define lines-from-file
    (lambda (input)
      (let ((val (read-line input)))
        (if (eof-object? val)
            '()
            (cons val (lines-from-file input))))))

; Run part 1
(number-of-trees-1 (lines-from-filename "input.txt"))
(number-of-trees-2 (lines-from-filename "input.txt") 2 1) ; -> 2
(define slopes '((1 1) (1 3) (1 5) (1 7) (2 1)))
(multiply-slopes slopes (lines-from-filename "input.txt"))
