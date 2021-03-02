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

(define valid-entry?
  (lambda (entry)
    (>= (accumulate-fields entry) 7)))

(define sum
  (lambda (lst)
    (apply + lst)))

(define count-true
  (lambda (lst)
    (let ((convert-bool
            (lambda (x) (if x 1 0))))
      (sum (map convert-bool lst)))))

(define count-valid-entries
  (lambda (entries)
    (count-true (map valid-entry? entries))))

(count-valid-entries (canonize-data (lines-from-filename "input.txt")))
