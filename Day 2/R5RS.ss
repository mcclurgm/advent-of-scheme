;; This uses regular expressions, which are not a part of RnRS Scheme. It's
;; tested in DrRacket's "Pretty Big" PLT language, which has a much bigger
;; library. However, the syntax should be compatible with Alex Shinn's
;; "IrRegular Expressions" library, which is compatible with R5RS and above.

;; Gets the number of times ch appears in lst.
;; Tail recursive, for speed!
(define accumulate-char-tail
  (lambda (ch lst tot)
    (if (null? lst)
        tot
        (let* ((head (if (char=? (car lst) ch) 1 0))
               (cur (+ head tot)))
          (accumulate-char-tail ch (cdr lst) cur)))))

;; A wrapper around accumulate-char-tail so I don't have to initialize tot
;; every time
(define accumulate-char
  (lambda (ch lst)
    (accumulate-char-tail ch lst 0)))

;; Given a password spec string "(\d+)-(\d+) (\w): (\w+)", tells whether it
;; matches the spec.
(define password-valid-1?
  (lambda (password-str)
    (let* ((re (pregexp "(\\d+)-(\\d+) (\\w): (\\w+)"))
           (match (cdr (regexp-match re password-str)))
           (min-n (string->number (car match)))
           (max-n (string->number (cadr match)))
           (ch (string-ref (caddr match) 0))
           (str-list (string->list (cadddr match)))
           (n (accumulate-char ch str-list)))
      (and (>= n min-n) (<= n max-n)))))

;; A quick-and-dirty xor. It only works for 2 bools, so don't pass 
;; anything else!
(define xor
  (lambda (a b)
    (not (eq? a b))))
;; Like password-valid-1?, but for the part 2 spec, where the numbers are 
;; indices.
(define password-valid-2?
  (lambda (password-str)
    (let* ((re (pregexp "(\\d+)-(\\d+) (\\w): (\\w+)"))
           (match (cdr (regexp-match re password-str)))
           (a (string->number (car match)))
           (b (string->number (cadr match)))
           (ch (string-ref (caddr match) 0))
           (str (cadddr match))
           (ch-a (string-ref str (- a 1)))
           (ch-b (string-ref str (- b 1))))
        (xor (char=? ch-a ch) (char=? ch-b ch)))))

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
(length (filter (lambda (v) v)
                (map password-valid-1? (lines-from-filename "input.txt"))))
(length (filter (lambda (v) v)
                (map password-valid-2? (lines-from-filename "input.txt"))))
