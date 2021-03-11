# Day 4

## Part 1
Given a set of data, which are a collection of entries with fields grouped by line breaks. There's a certain set of data that must be included in every group: `byr`, `iyr`, `eyr`, `hgt`, `hcl`, `ecl`, `pid`, `cid`.

These fields can appear in any order, separated by either spaces or newlines. The entries are distinguished by a single empty line.

My goal is to find the number of valid entries. These entries have all 8 fields, or all 8 except the `cid` field. (Actually, I'm supposed to ignore the `cid` field entirely. I don't think that makes any difference though.)

One thing for sure: if an entry has only 6 fields, it is guaranteed to be wrong.

I have some questions about the input before I go too far on this.

1. Can there be wrong fields? (ie fields that aren't in the spec.)
    - According to a regex search, this doesn't happen.
2. Can there be duplicate fields?
    - Doesn't seem like it.
3. Do all fields have data in the same format? (`field:datum`, with no spaces)
    - Looks like it.

Now to think about how to implement this.

It feels like I should create a function that takes an entry and determines whether it's valid or not. I can then use a function to count the number of entries that pass this function.

This is really a job for `for/add`, but if I want to stay in standard R5RS I need to do this myself. It'll just be a matter of summing a list, which I've done plenty of times before. (One of these days, I should really just implement `for/add` for myself.)

Random thought: programming style wise, it would probably be better to return `#t` or `#f` from the function, and then count instances of `#t`. Make a function like `count-true`. It would be pretty trivial to implement using recursion, or `map`ing a function to translate bool to numbers (which would be slower, and probably more opaque, so not ideal).

Anyway. Before I get there, I need to parse the input into a list of entries. This is where I keep stumbling in Scheme land is dealing with inputs, which I would otherwise find fairly easy in imperative languages like Python.

The first step is to divide the input by empty lines. When I reach an empty line, I can end the current entry and move on. This sounds awfully side-effect-y, so I don't know quite how to make it functional. Here's one thought: make it recursive. (What a surprise!)

While I can read lines from a file relatively easily, getting a list of lines from an input string is surprisingly hard. Wow, I wish Scheme weren't so minimalist sometimes!

I wonder if this would work with `fold`. A function would take the existing list of entries and a new line. If the line isn't empty, it would append the contents of the line to the current entry. (I could probably do this using the `string-append` function.) Otherwise, if the new line is empty, it would append a new `'()` to the list of entries.

My first attempt is:
```scheme
(define accumulate-entries
  (lambda (entries line)
    (if (= (string-length line) 0)
        (cons "" entries) ; Create a new entry
        (cons (string-append (car entries) line)
              (cdr entries))))) ; Append to the current entry

(foldl accumulate-entries (list "") test-lines)
```

This doesn't work: I've forgotten the format that a `fold` function needs to be in. After fooling around a while (I only go this by inference from the examples, the documentation is surprisingly unclear on the subject) I realize that I need to swap `entries` and `line`: the current datum is the first argument, and the working result is the second (or last, I suppose, since the `fold`s can take many lists). So now I have:

```scheme
(define accumulate-entries
  (lambda (line entries)
    (if (= (string-length line) 0)
        (cons "" entries) ; Create a new entry
        (cons (string-append (car entries) line)
              (cdr entries))))) ; Append to the current entry

(foldl accumulate-entries (list "") test-lines)
```
This produces:
```scheme
'("hcl:#cfa07d eyr:2025 pid:166559648iyr:2011 ecl:brn hgt:59in"
  "hcl:#ae17e1 iyr:2013eyr:2024ecl:brn pid:760753108 byr:1931hgt:179cm"
  "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884hcl:#cfa07d byr:1929"
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffdbyr:1937 iyr:2017 cid:147 hgt:183cm")
```

I forgot that `string-append` doesn't add any whitespace. Oops. So the `string-append` statement becomes `string-append (car entries) " " line)`. This adds an extra space at the beginning of each entry, but at least each entry is a single line separated by spaces, in a list. It also reverses the order, which could probably be fixed by `foldr`, but since I'm only looking for count I don't care. I then bind this result to `entry-strings` and continue on.

Next up, I need to get each of these strings in a readable format. This seems like a perfect use for `map`, since each entry is a single string and I just need to parse it. So on to my next function! (Side note: this seems like a great use to try out the Clojure-style `->>` macro. We'll see if that stays the case!)

So the first matter here is to decide what format to make each entry. I could theoretically just pick out the names of each field, but I'm a little scared that I'll need the values in part B, which would require a refactor. If I'm going with the `->>` method, then it's really only another quick function to get the name of each field. So let's say make each entry a cons pair: the car is the name, and the cdr is the value. (My first improper list! How exciting.)

Alright, so now I want to divide this string into a set of pairs. They're separated by spaces now. So step 1 in this task is to separate the space-delimited string I'm given into a list of substrings. In Python, this would just be `split`. I could use the `string-split` function in Racket. (If I want to really work on learning to Scheme, I should try to implement some sort of 1-character-delimited split function myself. But I'll start with the basics.)

So I'm working with `string-split`, which makes this much easier. I defined a function to map `string-split` to a list of strings, so I have now split up each entry into its constituent fields. The same function should roughly apply to splitting each field into a name and value, although it'll have to `map` one more level. This works:

```scheme
(define split-map
  (lambda (strings delimiter)
    (map (lambda (s) (string-split s delimiter))
         strings)))

(define test (split-map entries " "))
(map (lambda (strings) (split-map strings ":")) test)
```

Now I have a super-nested data structure, where the top level is entries, which are lists of fields, which are (2-element) lists containing the field name and data. I would like to make a function that takes the input string and splits it into this data structure. I think this sounds like a good time to try out the `->` macro. I don't have one immediately available, though, so I'll try the "raw" version (not Lispy though it may be to skip out on a macro when I could use it). I came up with this kind-of monster function:
```scheme
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
               (foldl canonize-entries-accum (list "") lines)))
           (split-map 
             (lambda (strings delimiter)
               (map (lambda (s) (string-split s delimiter))
                    strings)))
           (split-field
             (lambda (field)
               (split-map field ":"))))
      (map split-field
          (split-map (canonize-entries data-lines) " ")))))
```

One very quick bit of readability work I can do is to make the "threading" more explicit:
```scheme
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
               (foldl canonize-entries-accum (list "") lines)))
           (split-map 
             (lambda (strings delimiter)
               (map (lambda (s) (string-split s delimiter))
                    strings)))
           (split-entry
             (lambda (entry)
                (split-map entries " ")))
           (split-fields
             (lambda (field)
               (split-map field ":"))))
      (map split-field
        (split-entry 
          (canonize-entries
            data-lines))))))
(canonize-data test-lines)
```

Alright! Now I have my data in a data structure. What I need to do now is decide whether each entry is valid. This should be a fairly simple `map` function, where given an entry, I decide whether it's valid or not. I can `map` this to the list of entries, and accumulate how many work.

First things first on this section. I need to figure out what exactly it means for an entry to be valid. Here are two simple cases: if it has 8 entries, it's valid, or if it has 6 or fewer, it's invalid. Otherwise, the only question is when it has 7. If it's only missing the `cid` field, then it's valid too. It's not very easy to determine missing-ness, though, so I think I might as well accumulate positives. I'm not super sure how to go about this, though (functional or otherwise). 

There's something about this that feels like an "or" would be appropriate. I can "or" whether the `cid` is there, and then afterwards check that they're all there. I guess this is the same as just assuming that it is there. If it's the only one missing, then I'll have 8 with that assumption and it'll work. If it's not missing, then of course I'll have 8 too. If it's not the only one that's missing, then I'll have 7 or fewer. This is equivalent to skipping `cid` altogether and looking for 7 fields.

Now the question is how to accumulate this. I feel like there's something oddly challenging about this process in Lisp, and it just hasn't clicked yet. (Maybe it's because I just want the `for` macros from Racket.) I could make a custom recursion scheme for this, and add 1 to the accumulating variable as I go down if it's one of the non-`cid` fields, and doing nothing if it is `cid`.

Next up: define a function to tell if the field is correct and should be accumulated. A crazy idea (kinda proud of how I'm starting to think outside of the usual imperative style) is to do some crazy `map`ing and an `or`. And it just so happens that, looking at the docs, Scheme expects this to be a thing! It's `ormap`.

I really just want a way of `map`ing a function with one argument given. It happens _so much_ in my code. It's sort of like how you can provide 2 lists to `map` and it'll apply the function to all the elements `n` at once (so `(map cons '(1 2) '(3 4)) -> '((1 . 3) (2 . 4))'`). Except instead of applying element `n`, I just want one value to be applied every time. My use case right now is that I want to see if one value is equal to any in a list. This is sort of a job for `findf` in the Racket library, which does about what I need because of Scheme's liberal truth concept. But it's not in standard Scheme, so in the spirit of trying to stick to basics, I'll not use it. I could use `member`, which returns the entire tail of the list, but it would be a bit awkward.

Here's my first pass at all of this:
```scheme
(define accumulate-field?
  (lambda (field)
    (let ((name (car field)))
      (not (null? (member name check-fields))))))

(define accumulate-fields
  (lambda (fields)
    (letrec ((helper
               (lambda (fields accum)
                 (let ((accumulate? (accumulate-field? (car fields))))
                   (helper (cdr fields) (+ accum accumulate?))))))
      (helper fields 0))))
```

There are some glaring issues with that. First off, I forgot to do the whole thing that I make `accumulate?` a bound variable for: convert the boolean to an integer. Second, it's missing a base case. (😬️) First issue was easy to fix, using an if statement. Then, fixing the base case is also pretty easy, since this recursion scheme follows the usual accumulated structure I've grown pretty used to. The new `helper` looks like this now:
```scheme
(lambda (fields accum)
  (if (null? fields) accum
        (let ((iter (if (accumulate-field? (car fields))
                        1
                        0)))
          (helper (cdr fields) (+ accum iter)))))
```
I don't like how nested it is, especially when it's being bound in a `let` in a `lambda` in a `define`. But oh well? I really want to find ways of making my code less nested. It works on my test entry, so on to the next part.

The goal with `accumulate-fields` is that I can `map` it to each entry and get the number of fields. I can wrap it into an `if` before `map`ing to check the number and determine whether it's valid. So let's make a super-quick `lambda` for that:
```scheme
(define valid-entry?
  (lambda (entry)
    (>= (accumulate-fields entry) 7)))
```

I (finally) just realized an issue I have with my `canonize-entries`. It won't break anything, but it is a bit inconvenient: it reverses the list. The good news is that since I'm using `fold`, I just switched `foldl` to `foldr`. Ah, Lisp! I guess this is the first case of writing something that I can easily fix, instead of having to redo the recursion scheme from scratch…

Anyway, back on topic. This doesn't work one bit! It's saying that a lot of things are valid when they shouldn't be. I guess the first thing I should do is double-check `accumulate-fields` with more than just 1 entry. So I'm now testing with `(map accumulate-fields parsed-entries)`. It seems to be accumulating 1 more element than is actually there. I'll do the most basic test I can think of by double checking its base case: `(accumulate-fields '())`. This properly returns 0, so I guess that's fine. The issue is somewhere higher up. After some more digging around, it looks like the issue is that it's accumulating `cid` fields, and I can't figure out why. I guess I'll check `accumulate-field?` then. Sure enough, `(accumulate-field? '("cid" 350)) -> #t`. The culprit is that I messed up how `member` works: it returns `#f` on failure. Since I was checking `(not (null? (member ...)))`, that was always returning true. But now that I think about it, that whole function can become a lot more readable because of Scheme's super-liberal truth definition: anything other than `#f` (in my case, meaning the tail of the list returned on success) will be true. The only thing is that I'd rather not pass the tail out of the whole function, so I guess I could `(not (not ...))` it. Or I could just trust that the returned value is still true, and stick just return the result of `member`. I guess I'll go with that for now? The good news is that it seems to work, and my entry checker is fine.

Now all I do is define a little syntactic sugar: 
```scheme
(define sum
  (lambda (lst)
    (apply + lst)))
(sum (map valid-entry? parsed-entries))
```

Oh right. I forgot that `valid-entry?` returns a boolean. So I guess I have to convert that. Easy enough (right?): 
```scheme
(define count-true
  (lambda (lst)
    (let ((convert-bool
            (lambda (x) (if x 1 0))))
      (sum (map convert-bool lst)))))
(count-true (map valid-entry? parsed-entries))
```
This would be a super nice case for my anonymous function syntax! Wouldn't `(map (& (if #1 1 0)) lst)` as the whole body of the function be nicer? I really need to get that finished one of these days… The good news is that at any rate, it works!

Now all that's left is to try it with the full input file. First off, I should wrap all of this into a function that I can simply run on the input.

```scheme
(define count-valid-entries
  (lambda (entries)
    (count-true (map valid-entry? parsed-entries))))
(count-valid-entries (canonize-data (lines-from-filename "input.txt")))
```
But it doesn't work! (Again!) It returns 2, which I can count is wrong within the first 5 entries of probably hundreds. This is proving to be excessively hard. Back at it again…

It seems like I'm only getting 4 entries out of `canonize-data`. Why could that be?? But boy, is my implementation of `canonize-entries` impossible to read.

First up, there's something weird about how it seems to make no difference whether I use `foldl` or `foldr` in the internal implementation of `canonize-entries`. It seems to just follow what I use in my top-level definition of `canonize-entries`. But then, if I delete that, then it still works?

Ok, so it turns out that I had a whole bunch of dangling references to variables that I used in my testing that stuck around inside of my lambda definitions. A couple times in `canonize-entries` and one in `count-valid-entries`. So fix those, and I get 208 valid entries. It's correct! 🎉️

Yikes. That was a bit of a time. I have a lot to learn about how to write these sorts of things. I learned this round that my general method of "trial-and-error this function, then wrap it up in something else" is super prone to errors, since I can do things like forget to change the name of a testing variable all the time. I should probably adapt that method to define the final function from the start, then calling that function, instead of relying on the REPL. Another thing that I found out when looking at `accumulate-fields` is that I should be able to decently easily swap out my accumulator recursion scheme for a combination of `map` and `apply`. In the typical case where I accumulate by `+`, it would be super easy to generalize. But I can also define it as a higher-order function. Or is it possible that this is what `fold` is for? I feel like I haven't really unlocked the power of `fold`. I just can't wrap my head around how to make it accumulate easily.

Just for fun, I want to give `accumulate-fields` a second try. With the `count-true` that I just defined, it becomes pretty trivial: `(count-true (map accumulate-field? full-entries))`. I could pretty easily write a higher-order function based around this structure that counts the number of elements of a list that match some criterion. Maybe I'll give that a shot at some point, since I seem to be using that sort of structure an awful lot. At any rate, here's what `accumulate-fields` looks like now:
```scheme
(define accumulate-fields
  (lambda (fields)
    (count-true (map accumulate-field? fields))))
```
Much better, right?

For a bit of random reflection on Lisp: I think that this challenge helped me quite a bit in writing better Lisp code, especially things like rewriting the iterator. The higher-order functions like `map` are what drew me to Lisp in the first place (along with the idea of immutability), so it makes sense that I like writing and reading my code a lot more when my code works that way. I don't like recursion schemes, but I love functions! And `map`ing uniquely matches the way my brain works.


## Part 2

The twist this time is that I have to validate the contents of each field. (I was wondering whether this would be a thing.) The good news is that my current method should be super easy to augment with this behavior. It'll be a simple addition to `accumulate-field`. I don't know exactly how to make it nice in that function yet, but it'll be easily encapsulated. The issue will then be how to check for each of these.

What comes to mind is that I could use `cond` to check for which field the current one is, and based on that call an associated function (basically like some tiny version of home-grown dynamic dispatch). The result of this function will replace the current check which is simply `(member name check-fields)`. The special cases are `cid`, in which case I always return `#f`, or if it's none of the fields, in which case I also return `#f`. So both of those can be wrapped into an `else`.

I'm going to start by setting up the `cond` statement and then work on each of the individual functions. Since I'm not going to have any of the functions set up yet, I want to be able to throw not-implemented errors. Although actually, this sounds more like something I should test one at a time and then wrap into the `cond`. (Otherwise, I would have to remove all the code that runs my current code, and I'd rather not.) So I'll start by writing the `cond`, move to a new file to try out the new functions, and add them back in when I'm done.

So first up, writing the `cond`. I'll start by basically just re-implementing the current check in more verbose form: any of the cases that match, I return `#t`, with the `else` returning `#f`. Rather simply, the new function looks like this (which works, according to the test input):
```scheme
(define accumulate-field?
  (lambda (field)
    (let ((name (car field)))
      (cond ((string=? name "byr") #t)
            ((string=? name "iyr") #t)
            ((string=? name "eyr") #t)
            ((string=? name "hgt") #t)
            ((string=? name "hcl") #t)
            ((string=? name "ecl") #t)
            ((string=? name "pid") #t)
            (else #f)))))
```
I could try to simplify this a little bit, but I'm not sure it's more readable:
```scheme
(define accumulate-field?
  (lambda (field)
    (let* ((name (car field))
           (name-check (lambda (str) (string=? name str))))
      (cond ((name-check "byr") #t)
            ((name-check "iyr") #t)
            ((name-check "eyr") #t)
            ((name-check "hgt") #t)
            ((name-check "hcl") #t)
            ((name-check "ecl") #t)
            ((name-check "pid") #t)
            (else #f)))))
```
I think this is probably harder to read because of the abstraction, and not much more concise, so I'll reverse it and use the whole version.

Now, to try to write some of these functions. First up, for `byr`. The condition here is that it has to be a 4-digit number between 1920 and 2002. Since there are multiple things to check, this is a job for `cond`. I can first check that it's a 4-character string, then move to the number checks, since `cond` has a guaranteed execution sequence. I'll do my number checking using `string->number`, which returns the number if it works or `#f` if it doesn't. Since I want to use this multiple times (first to check for a parse error, second to check the value), I probably want to bind it to a variable first up, outside of the `cond` to keep things from getting weirdly nested. I guess this makes for a potentially more logical sequence of checks if the number value is determined ahead of time: first check if it's a number, then check if it has the right length, then check if it has the right value. So here's a first try:
```scheme
(define check-byr
  (lambda (data)
    (let ((num (string->number data)))
      (cond ((not num) #f) ; If it's not a number, fail
            ((not (= (string-length data) 4)) #f); If it's not of length 4, fail
            (else (and (>= num 1920)
                       (<= num 2002))))))) ; Otherwise it's the right format, check value
```
My test cases that I came up with are:
```scheme
(define byr-test-strings '("2014" "1918" "2000" "02000" "ah" "2e03"))
(equal? (map check-byr byr-test-strings)
        '(#f #f #t #f #f #t))
```
Unfortunately, this doesn't work in the scientific notation case. But thankfully, according to a quick regex check, that doesn't appear in the test input. (It's an awfully specific case. It would be nice though if Scheme let you set the type of number to parse, though.) So I guess this function is good to go.

Next up: `iyr`.
This is pretty much the exact same as `byr`, except its range is different. It's 2010 through 2020. So I should just be able to copy the implementation of `check-byr`:
```scheme
(define check-iyr
  (lambda (data)
    (let ((num (string->number data)))
      (cond ((not num) #f) ; If it's not a number, fail
            ((not (= (string-length data) 4)) #f); If it's not of length 4, fail
            (else (and (>= num 2010)
                       (<= num 2020)))))))
```

`eyr` is the same thing. I guess I'll make a function out of year checking and I can call it with the year boundaries. So here's the new implementation:
```scheme
(define check-year-field
  (lambda (data first-year last-year)
    (let ((num (string->number data)))
      (cond ((not num) #f) ; If it's not a number, fail
            ((not (= (string-length data) 4)) #f); If it's not of length 4, fail
            (else (and (>= num first-year)
                       (<= num last-year)))))))

(define check-byr
  (lambda (data)
    (check-year-field data 1920 2002))) ; Otherwise it's the right format, check value

(define check-iyr
  (lambda (data)
    (check-year-field data 2010 2020)))
```

Now, `eyr` has the endpoints of 2020 and 2030. So follow that same structure:
```scheme
(define check-eyr
  (lambda (data)
    (check-year-field data 2020 2030)))

(define eyr-test-strings '("2020" "2030" "2025" "2000" "2040" "0200" "ah" "1e4"))
(equal? (map check-eyr eyr-test-strings)
        '(#t #t #t #f #f #f #f #f))
```

Now for a different type of field. `hgt` has a number followed by a unit, which is two characters. The unit could be either `cm` or `in`, and depending on that value the range of the number changes. First up, I need to figure out the unit and check it. Since it's the last two characters in the input string, I can simply divide the string in two: the first part that's the number, and then the unit that's the last 2 elements. This is just two applications of `substring`. So here's the first part: splitting it up.
```scheme
(define check-hgt
  (lambda (data)
    (let* ((unit-index (- (string-length data) 2))
           (number (substring data 0 unit-index))
           (unit (substring data unit-index (string-length data))))
      (list number unit))))

(map check-hgt '("60in" "190cm" "190in" "190"))
```
Now, I can check what the unit is. That's a fairly easy `cond` statement. (Using the `else` form, it gets me invalid units already.)
```scheme
(define check-hgt
  (lambda (data)
    (let* ((unit-index (- (string-length data) 2))
           (number (substring data 0 unit-index))
           (unit (substring data unit-index (string-length data))))
      (cond ((string=? unit "cm") "cm unit")
            ((string=? unit "in") "in unit")
            (else #f)))))
```
Up next, I need to check the number. This is 90% the same as `check-year-field`, so I guess I can refactor that too to be a general `check-number-field`:
```scheme
(define check-number-field
  (lambda (data min max len)
    (let ((num (string->number data)))
      (cond ((not num) #f) ; If it's not a number, fail
            ((not (= (string-length data) len)) #f); If it's not of length len, fail
            (else (and (>= num min)
                       (<= num max)))))))
(define check-year-field
  (lambda (data first-year last-year)
    (check-number-field data first-year last-year 4)))
```
Then I can use this to check the number part:
```scheme
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
```

Up next: `hcl`. This is a fairly simple format. It's a `#` followed by a collection of hex numbers: so 1–9 and a–f. It appears that they must be lower case.
This would be trivial with regex (`#[0-9a-f]{6}`), but I kind of want to try this with pure Scheme since it's relatively simple. 

The first part of the check is simple. I can use `char-at` and `char=?` to check that the first character is a `#`. That'll look like this, so far:
```scheme
(define check-hcl
  (lambda (data)
    (if (char=? (string-ref data 0) #\#)
        #t
        #f)))

(define hcl-test-strings '("#123abc" "#123abz" "123abc" "#123ab"))
(equal? (map check-hcl hcl-test-strings)
        '(#t #f #f #f))
(map check-hcl hcl-test-strings)
```
Although the `equal?` doesn't work yet, it correctly identifies the first character.

This will inevitably be a `cond`, so I might as well start that up now:
```scheme
(define check-hcl
  (lambda (data)
    (cond ((not (char=? (string-ref data 0) #\#)) #f)
          (else #t))))
```

The next condition I'll tackle is that the characters are valid. A first pass at this is:
```scheme
(define hex-number?
  (lambda (ch)
    (member ch '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0 #\a #\b #\c #\d #\e #\f)')))
(map hex-number? (string->list "1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
```
This works, but it is a little awkward. I think I could do this using `char>?` and `char<?`. It's based on Unicode codepoints, so I guess I'm diving into that now. The numbers are all sequential (from 0 to 9), so I can definitely do that. Then there's a gap for some punctuation and capital letters, then lower-case letters in order. So this is going to be relatively easy using two conditions (sounds like an `or` form). The result isn't a lot less awkward and a tiny bit more opaque, but it's fine too:
```scheme
(define hex-number?
  (lambda (ch)
    (or (and (char>=? ch #\0) (char<=? ch #\9))
        (and (char>=? ch #\a) (char<=? ch #\f)))))
```
I really don't know which of these to go with. I guess I'll go with the second and keep the first in a comment, because why not?

Now to finish this up. To check whether the number part works, I check whether all its characters pass `hex-number?`. That would work for `map`ing the function to `(string->list data)`. Except I need the string excepting the first character, which could easily be `(cdr (string->list data))`. I could also use `substring`, but I assume that's more work because it would have to allocate a whole string. And one more thought: is it possible to directly `map` to a string? The answer is no, unfortunately it expects a real list, but this is fine. (That would be a nice bit of homebrew syntactic sugar to make a super simple `string-map` wrapper function if I wind up dealing with this sort of string analysis frequently in these problems.) So here's my updated function:
```scheme
;; Checks for a 6 digit lower-case hex number. Essentially /#[0-9a-f]{6}/
(define check-hcl
  (lambda (data)
    (cond ((not (char=? (string-ref data 0) #\#)) #f)
          ((not (and (map hex-number? (cdr (string->list data))))) #f)
          (else #t))))
```

It doesn't work though. Time to debug. I'll make a test function out of the second `cond` condition (excepting the `not` probably):
```scheme
(define (test-hex data)
  (and (map hex-number? (cdr (string->list data)))))
```
The issue is that applying `and` to a list treats the list as the single condition, instead of treating the elements as the set of conditions. So I need to `map` and to the resulting list. Or actually `apply`. Except that `and` is a macro, so none of these will actually work! This is one thing that's starting to get me about Scheme: since macros look like functions, I often want to using higher-order functions on them. But that doesn't work, and it's often very opaque what's going to happen. It seems like it shouldn't be that hard to make sure that every element of a list is true, but it seems like it really is! This seems like a really basic thing to get hung up on.

Here's an implementation I found on Stack Overflow, which is basically what I would have done myself:
```scheme
;; Returns whether all elements of the list are true.
;; The empty case is considered to be true.
(define all
  (lambda (lst)
    (cond ((null? lst) #t)
          ((pair? lst) (all (cdr lst)))
          (else (error 'all "Expected list; got something else")))))
```
The difference is that it accounts for non-list arguments. Credit for accounting for non-list structures from Stack Overflow.

Except it doesn't actually work. So let's go in and fix it. I had a moment of being worried about tail recursion, but I double checked and each return expression in `cond` is in tail position! So I just have to make it able to return false. That's essentially another base case, since we can short-circuit, so I'll put it at the top. Although that requires using `car`, which assumes that `lst` is a pair, which I haven't verified yet. So again going to Stack Overflow (which I didn't really copy from, just skimmed and remade, badly I guess): instead of using it as a base case, `and` it with the tail of `lst`. Unfortunately, that requires that we go all the way down the list every time, so I'd still like to short-circuit. I think I'd rather make the error check at the front, so I can assume that `lst` is a pair for the rest. I think it'll be more readable and should be more efficient:
```scheme
;; Returns whether all elements of the list are true.
;; The empty case is considered to be true.
;; Credit for accounting for non-list structures from Stack Overflow.
(define all
  (lambda (lst)
    (cond ((null? lst) #t)
          ((not (pair? lst)) (error 'all "Expected list; got something else"))
          ((not (car lst)) #f) ; Short circuit if we encounter a false
          (else (all (cdr lst)))))) ; Otherwise, keep trying down the line
```
This one works! Hooray. And when I fix `check-hcl` to use this, I have
```scheme
;; Checks for a 6 digit lower-case hex number. Essentially /#[0-9a-f]{6}/
(define check-hcl
  (lambda (data)
    (cond ((not (char=? (string-ref data 0) #\#)) #f)
          ((not (all (map hex-number? (cdr (string->list data))))) #f)
          (else #t))))
```
And this works too. (At least for what I've implemented so far.) The last condition would be to make sure that it's of the proper length. That's a simple application of `string-length`. I'm going to move this up the line, so the most expensive operation (checking each letter) comes at the end. (At least I assume it's the most expensive. If not then it's probably even, in which case it's a wash, so may as well.)
```scheme
;; Checks for a 6 digit lower-case hex number. Essentially /#[0-9a-f]{6}/
(define check-hcl
  (lambda (data)
    (cond ((not (char=? (string-ref data 0) #\#)) #f)
          ((not (= (string-length data) 7)) #f)
          (else (all (map hex-number? (cdr (string->list data))))))))
```
And this works!

I can make this a lot less awkward by using `and` instead of a bunch of crazy `cond` conditions. It would probably be more readable if I had positive conditions, but doing a bunch of negatives is really odd. I'm basically replicating an `and` by saying that it has to match a bunch of conditions. And since `and` short-circuits, I think it's pretty much exactly equivalent (at least as far as my mental model, which is all I'm trying to replicate). So let's fix that up.
```scheme
;; Checks for a 6 digit lower-case hex number. Essentially /#[0-9a-f]{6}/
(define check-hcl
  (lambda (data)
    (and (char=? (string-ref data 0) #\#)
         (= (string-length data) 7)
         (all (map hex-number? (cdr (string->list data)))))))
```
And that works too! I'll also fix up `check-number-field`, which should be pretty much the same change.
```scheme
(define check-number-field
  (lambda (data min max len)
    (let ((num (string->number data)))
      (and num ; If it's not a number, this will be #f and fail
           (= (string-length data) len)
           (and (>= num min)
                (<= num max))))))
```

Then `ecl`: it's just one of a couple options, and I think it's all lower-case only. That's an easy job for `member`.
```scheme
(define check-ecl
  (lambda (data)
    (not (not (member data '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))))

(define ecl-test-strings '("brn" "jfd" "fdjsakl" "oth-er things"))
(equal? (map check-ecl ecl-test-strings)
        '(#t #f #f #f))
```
I don't love the `(not (not ...))` in there, but I'm not super sure what else to do about it. I could just remove it and trust that the logic in the calling code can handle implicit truth. I think I may do that for now, since I hate this. So now I have a very simple function: essentially just one statement.
```scheme
(define check-ecl
  (lambda (data)
    (member data '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))
```

Then I need to deal with `pid` (the last one!). Its only requirement is that it's a 9-digit number, including leading zeros. So that's basically 2 requirements: first, that it's a number; second, that the string is of length 9. (I can't check for the order of magnitude of the parsed number because it explicitly allows for leading zeros.)

First, checking for length. This is pretty simple: as before, I use `(= (string-length data) 9)`. Next, I need to check for a number. There are 2 ways to do this, I think. First, I can simply call `string->number` and make sure that it isn't false. Second, I can make sure that every character is a number. The second version should work, since a number is basically a collection of individual numeric characters. That would look something like `(all (map is-number (string->list data)))` for some function `is-number`. I'm not convinced this will be all that much faster than just using `string->number`, so I think I'll just go with the simpler and more expressive version. Why reinvent the wheel? (I've been doing plenty of that this project; I should take my chance to have a builtin rnrs version to use.)

Here's my first attempt:
```scheme
(define check-pid
  (lambda (data)
    (and (string->number data)
         (= (string-length data) 9))))

(define pid-test-strings '("000000001" "0123456789" "abcdefghi" "1234567e3"))
(equal? (map check-pid pid-test-strings) ; -> '(#t #f #f #t)
        '(#t #f #f #f))
```
Unfortunately, this doesn't work. In the off-chance I get an input in scientific notation, I'll incorrectly consider it a number. So this doesn't _quite_ work, and I need to figure out how to deal with that. There's nothing in the `string->number` function that lets me ignore inputs in exponent form. Also, exponent form doesn't determine number type, so I can't use something like `integer?`. And that's not to mention the fact that I can think of any other Scheme number format to create a number that won't fit the (sorta loose, but clear in spirit) spec here, like `1.1@1.764` (a complex number in polar form). So I guess I'll just use the longer version and check that every character is a number. So here's the new version:
```scheme
(define char-is-number?
  (lambda (ch)
    (and (char>=? ch #\0) (char<=? ch #\9))))
(define check-pid
  (lambda (data)
    (and (all (map char-is-number? (string->list data)))
         (= (string-length data) 9))))
```
And according to my tests, that works. Hooray. (I think I might have had another case where that wouldn't work, but I think I'm going to ignore it because I remember that there weren't any crazy fake-outs like that in a cursory check of the input. Design for the spec, and no more.)

And of course, `cid` is ignored no matter what, so I don't need to deal with that at all. So now I get to set all of these up in the main file and get it together! I define a new function `accumulate-field?-2` (I need to come up with a better format for these part 1/part 2 functions) based on my code from a while back that uses the functions instead of just returning `#t`:
```scheme
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
```
The rest of my code should be exactly the same. I just need to create new versions of them to use my new function instead of the old one:
```scheme
(define accumulate-fields-2
  (lambda (fields)
    (count-true (map accumulate-field?-2 fields))))

(define count-valid-entries
  (lambda (entries)
    (count-true (map valid-entry? entries))))

(count-valid-entries-2 (canonize-data (lines-from-filename "input.txt")))
```
The result from all of this is 208. That's the answer from the first part, so looks like I missed changing a name. And yeah: I forgot to change `valid-entry` to use `accumulate-fields-2`. So now I made
```scheme
(define valid-entry?
  (lambda (entry)
    (>= (accumulate-fields-2 entry) 7)))

(define count-valid-entries-2
  (lambda (entries)
    (count-true (map valid-entry?-2 entries))))
```
and I get 167. And that's correct! Hooray. (I really do need to find a better way of dealing with part 1/part 2 issues.)