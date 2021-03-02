;; This uses regular expressions, which are not a part of RnRS Scheme. It's
;; tested in DrRacket's "Pretty Big" PLT language, which has a much bigger
;; library. However, the syntax should be compatible with Alex Shinn's
;; "IrRegular Expressions" library, which is compatible with R5RS and above.

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

(define test-lines (lines-from-filename "test-input.txt"))
(pretty-print test-lines)

; (define canonize-entries-accum
;   (lambda (line entries)
;     (if (= (string-length line) 0)
;         (cons "" entries) ; Create a new entry
;         (cons (string-append (car entries) " " line)
;               (cdr entries))))) ; Append to the current entry

; (define canonize-entries 
;   (lambda (lines)
;     (foldr canonize-entries-accum (list "") lines)))
; (define entries (canonize-entries test-lines))
; entries

; (define split-map
;   (lambda (strings delimiter)
;     (map (lambda (s) (string-split s delimiter))
;          strings)))
; 
; (define test (split-map entries " "))
; (pretty-print test)

; (caar test)
; (string-split (caar test) ":")
; (map (lambda (strings) (split-map strings ":")) test)
; ; (split-map test ":")
; (define split-field
;   (lambda (field)
;     (split-map field ":")))
; (map split-field test)

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

(define parsed-entries (canonize-data test-lines))

(define test-entry (cadr parsed-entries))
(pretty-print test-entry)

; The fields that must be in each entry for it to be valid. (Since
; we don't care about cid, I'm ignoring it for this purpose)
(define check-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define accumulate-field?
  (lambda (field)
    (let ((name (car field)))
      (member name check-fields))))

(define accumulate-fields
  (lambda (fields)
    (letrec ((helper
              (lambda (fields accum)
                (if (null? fields) accum
                    (let ((iter (if (accumulate-field? (car fields))
                                    1
                                    0)))
                      (helper (cdr fields) (+ accum iter)))))))
      (helper fields 0))))
(define test2 (cdddr test-entry))
(displayln test2)
(displayln (accumulate-fields test2))
(displayln (map accumulate-fields parsed-entries))


(define valid-entry?
  (lambda (entry)
    (>= (accumulate-fields entry) 7)))
(valid-entry? test-entry)
(map valid-entry? parsed-entries)

(define sum
  (lambda (lst)
    (apply + lst)))

(define count-true
  (lambda (lst)
    (let ((convert-bool
            (lambda (x) (if x 1 0))))
      (sum (map convert-bool lst)))))

(count-true (map valid-entry? parsed-entries))

(define count-valid-entries
  (lambda (entries)
    (count-true (map valid-entry? entries))))

(map valid-entry? (canonize-data (lines-from-filename "input.txt")))

(pretty-print (canonize-data (lines-from-filename "input.txt")))

(count-valid-entries (canonize-data (lines-from-filename "input.txt")))
