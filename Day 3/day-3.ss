;; Requires Pretty Big dialect (for the range function only).

(define test-line "..##.......") ;; length 11
(define test-lines '("..##......."
                     "#...#...#.."
                     ".#....#..#."
                     "..#.#...#.#"
                     ".#...##..#."
                     "..#.##....."
                     ".#.#.#....#"
                     ".#........#"
                     "#.##...#..."
                     "#...##....#"
                     ".#..#...#.#"))

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

; (list (is-tree test-line 0) ;; 0
;       (is-tree test-line 2) ;; 1
;       (is-tree test-line 11) ;; 0
;       (is-tree test-line 13)) ;; 1

(define indices 
  (map (curry * 3)
       (range 0 (length test-lines))))
indices
(define number-of-trees
  (lambda (map-lines)
    (let* ((indices (map (curry * 3)
                         (range 0 (length map-lines))))
           (trees (map is-tree map-lines indices)))
      (apply + trees))))

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
(number-of-trees (lines-from-filename "input.txt"))
