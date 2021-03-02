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

Just for fun, I want to give `accumulate-fields` a second try. With the `count-true` that I just defined, it becomes pretty trivial: `(count-true (map accumulate-field? full-entries))`. I could pretty easily write a higher-order function based around this structure that counts the number of elements of a list that match some criterion. Maybe I'll give that a shot at some point, since I seem to be using that sort of structure an awful lot.

For a bit of random reflection on Lisp: I think that this challenge helped me quite a bit in writing better Lisp code, especially things like rewriting the iterator.
