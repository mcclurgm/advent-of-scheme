(define check-number-field
  (lambda (data min max len)
    (let ((num (string->number data)))
      (and num ; If it's not a number, this will be #f and fail
           (= (string-length data) len)
           (and (>= num min)
                (<= num max))))))

(define check-year-field
  (lambda (data first-year last-year)
    (check-number-field data first-year last-year 4)))

(define check-byr
  (lambda (data)
    (check-year-field data 1920 2002))) ; Otherwise it's the right format, check value

(define byr-test-strings '("2014" "1920" "2002" "1918" "2000" "02000" "ah" "2e03"))
(equal? (map check-byr byr-test-strings)
        '(#f #t #t #f #t #f #f #t))

(define check-iyr
  (lambda (data)
    (check-year-field data 2010 2020)))

(define iyr-test-strings '("2010" "2020" "2015" "2000" "2030" "0200" "ah" "1e4"))
(equal? (map check-iyr iyr-test-strings)
        '(#t #t #t #f #f #f #f #f))

(define check-eyr
  (lambda (data)
    (check-year-field data 2020 2030)))

(define eyr-test-strings '("2020" "2030" "2025" "2000" "2040" "0200" "ah" "1e4"))
(equal? (map check-eyr eyr-test-strings)
        '(#t #t #t #f #f #f #f #f))

(define check-hgt
  (lambda (data)
    (let* ((unit-index (- (string-length data) 2))
           (number (substring data 0 unit-index))
           (unit (substring data unit-index (string-length data))))
      (cond ((string=? unit "cm")
             (check-number-field number 150 193 3))
            ((string=? unit "in")
             (check-number-field number 59 76 2))
            (else #f)))))

(define hgt-test-strings '("60in" "190cm" "190in" "190"))
(equal? (map check-hgt hgt-test-strings)
        '(#t #t #f #f))

;; Checks whether the given character counts as a hex number.
;; ie, is it in the range 0-9 or a-f (lowercase)?
(define hex-number?
  (lambda (ch)
    (or (and (char>=? ch #\0) (char<=? ch #\9))
        (and (char>=? ch #\a) (char<=? ch #\f)))))
; Alternate form:
; (define hex-number?-old
;     (lambda (ch))
;     (member ch '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\a #\b #\c #\d #\e #\f)))

;; Returns whether all elements of the list are true.
;; The empty case is considered to be true.
;; Credit for accounting for non-list structures from Stack Overflow.
(define all
  (lambda (lst)
    (cond ((null? lst) #t)
          ((not (pair? lst)) (error 'all "Expected list; got something else"))
          ((not (car lst)) #f) ; Short circuit if we encounter a false
          (else (all (cdr lst)))))) ; Otherwise, keep trying down the line

;; Checks for a 6 digit lower-case hex number. Essentially /#[0-9a-f]{6}/
(define check-hcl
  (lambda (data)
    (and (char=? (string-ref data 0) #\#)
         (= (string-length data) 7)
         (all (map hex-number? (cdr (string->list data)))))))

(define hcl-test-strings '("#123abc" "#123abz" "123abc" "#123ab"))
(equal? (map check-hcl hcl-test-strings)
        '(#t #f #f #f))

(define check-ecl
  (lambda (data)
    (member data '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

(define ecl-test-strings '("brn" "jfd" "fdjsakl" "oth-er things"))
(equal? (map check-ecl ecl-test-strings)
        '(#t #f #f #f))

(define char-is-number?
  (lambda (ch)
    (and (char>=? ch #\0) (char<=? ch #\9))))
(define check-pid
  (lambda (data)
    (and (all (map char-is-number? (string->list data)))
         (= (string-length data) 9))))

(define pid-test-strings '("000000001" "0123456789" "abcdefghi" "0123456e7"))
(equal? (map check-pid pid-test-strings)
        '(#t #f #f #f))