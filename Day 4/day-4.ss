;; This uses some bits of Racket's library that aren't part of any standard
;; Scheme. Particularly, I use string-split, because it's just so much easier
;; than writing string functions like that by hand.

(define test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

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

;; General utilities
(define sum
  (lambda (lst)
    (apply + lst)))

(define count-true
  (lambda (lst)
    (let ((convert-bool
            (lambda (x) (if x 1 0))))
      (sum (map convert-bool lst)))))

(define canonize-data
  (lambda (data-lines)
    (let* ((canonize-entries-accum
             (lambda (line entries)
               (if (= (string-length line) 0)
                   (cons "" entries) ; Create a new entry
                   (cons (string-append (car entries) " " line)
                         (cdr entries))))) ; Append to the current entry
           (canonize-entries
             (lambda (lines)
               (foldr canonize-entries-accum (list "") lines)))
           (split-map
             (lambda (strings delimiter)
               (map (lambda (s) (string-split s delimiter))
                    strings)))
           (split-entry
             (lambda (entry)
                (split-map entry " ")))
           (split-field
             (lambda (field)
               (split-map field ":"))))
      (map split-field
        (split-entry
          (canonize-entries
            data-lines))))))
; This should be equivalent to
; (-> data-lines
;     canonize-entries
;     split-entry
;     (map split-field))
; if I had that macro. (That's much more readable.)

;; The fields that must be in each entry for it to be valid. (Since
; we don't care about cid, I'm ignoring it for this purpose)
(define check-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define accumulate-field?
  (lambda (field)
    (let ((name (car field)))
      (member name check-fields))))

(define accumulate-fields
  (lambda (fields)
    (count-true (map accumulate-field? fields))))

(define valid-entry?
  (lambda (entry)
    (>= (accumulate-fields entry) 7)))

(define count-valid-entries
  (lambda (entries)
    (count-true (map valid-entry? entries))))

(count-valid-entries (canonize-data (lines-from-filename "input.txt")))

;; Part 2

;; Functions to check the data format of each
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

(define check-iyr
  (lambda (data)
    (check-year-field data 2010 2020)))

(define check-eyr
  (lambda (data)
    (check-year-field data 2020 2030)))

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

(define accumulate-field?-2
  (lambda (field)
    (let ((name (car field))
          (data (cadr field)))
      (cond ((string=? name "byr") (check-byr data))
            ((string=? name "iyr") (check-iyr data))
            ((string=? name "eyr") (check-eyr data))
            ((string=? name "hgt") (check-hgt data))
            ((string=? name "hcl") (check-hcl data))
            ((string=? name "ecl") (check-ecl data))
            ((string=? name "pid") (check-pid data))
            (else #f)))))

(define accumulate-fields-2
  (lambda (fields)
    (count-true (map accumulate-field?-2 fields))))

(define valid-entry?-2
  (lambda (entry)
    (>= (accumulate-fields-2 entry) 7)))

(define count-valid-entries-2
  (lambda (entries)
    (count-true (map valid-entry?-2 entries))))

(count-valid-entries-2 (canonize-data (lines-from-filename "input.txt")))