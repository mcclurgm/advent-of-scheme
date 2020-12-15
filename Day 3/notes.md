# Day 3

## Part 1
Given a "map" in ASCII characters, with `.`="snow" and `#`=tree. This map repeats to the right arbitrarily many times.

Start in the top-left square (which is empty). You're given a slope of `a` down and `b` right. Follow that as many iterations as it takes to get to the bottom of the "map." Count how many times that lands on a `#`.

For part 1, the slope is given: `a` is 1 down, and `b` is 3 right. So after the first iteration (starting at `(0,0)`, I land on `(1,3)` (considering the vertical axis to be the first index).

So: this sounds like I'm going to need random access on the strings, since I will only access each line once and need an element that's not 1. I've been using `string->list` before, but that was when I was only iterating through the string in order. So if there's a `string->vector` function, that would be great. (It would also require that I know how vectors work, but I suppose it's about time I learned.)

Unfortunately, there's no `string->vector` function. This leaves me with 2 options: I could either index the string lists in (I believe?) linear time, or I could convert each string to a vector and then index the vectors in constant time. These should be equivalent asymptotically, since I can't imagine converting to a vector would be anything other than linear time. So maybe it just makes sense to use `list-ref`. Humph. Or alternatively, I could just use `string-ref` while I'm at it. If I'm super lucky it'll be implemented as a vector anyway, but I'm not so sure. 

Hey, it turns out from a quick experiment that strings are implemented to access in constant time! That's nice. I guess that's why there's only a `string->list` function, because `string->vector` would be sort of redundant. (We learned something today! Not _everything_ in Lisp is a list.) Here's what I got:
```racket
(define thing (make-string 1000 #\a))
(define lst (build-list 1000 values))
(define vec (make-vector 1000 #\a))
(time (for ([i (in-range 1000000)]) (string-ref thing 900)))
; cpu time: 4 real time: 4 gc time: 0
(time (for ([i (in-range 1000000)]) (list-ref lst 900)))
; cpu time: 1022 real time: 1021 gc time: 0
(time (for ([i (in-range 1000000)]) (vector-ref vec 900)))
; cpu time: 3 real time: 3 gc time: 0
```

For the next part, I imagine I'll have other values of `a` that aren't 1. So it would be nice to have a solution that can accept going non-sequentially. But I think for the moment, I'll just stick with a regular old list and use `cdr`, since that's a thing I can do for now and keep things simpler.

So I can generate a list of lines, like I have before. Then I'll want to convert each of those lines to a list, which I can do with the same boilerplate I used the first day. Since I'm iterating one line at a time, I can just `map` my way through the lines. For each of them, tell whether the index of the line is a `.` (so 0, no tree) or `#` (so 1, found a tree). Then at the end I can total up the trees using `(apply + ...)` as before.

Last item of business. I have to implement the repeating structure. This should be pretty easy using remainders: the first element past the end of the line (whose index would be the length of the line) I want to be index 0 to get the first element of the line for the repeat. This conveniently matches with `a % a = 0`. So the index I use can be `modulo index (length str)`. On to writing the code.

I'm starting with just one line that I hard coded.

One boring thing I got hung up on immediately: there's a unique function to get the length of a string, `string-length`. No polymorphism to be found here.

My wrap-around indexing works!
```scheme
(define test-line "a.##.......") ;; length 11
(define index 11)
(string-ref test-line (modulo index (string-length test-line)))
; #\a
```

My next step is to make a function to determine whether I land on a tree in a given line. My first attempt looks like this:
```scheme
(define is-tree
  (lambda (line index)
    (let* ((index-wrapped (modulo index (string-length line)))
           (ch (string-ref line index-wrapped)))
        (if (char=? (string-ref line (modulo index (string-length))))
            1
            0))))
```
I remain uncomfortable with how deeply nested all my code is. It's more than a little off-putting. I also feel like I rely a little too much on `let*`, which is probably a leftover from my time in imperative languages where I define data one after the next and then work on it. I'm basically doing a slightly Lisp-ier version of that, and it would be perfectly at home in something like Swift or Rust where variables are constant by default, with no significant change. I wonder if there's a more idiomatic way to do this? At any rate, though, it seems to work:
```scheme
(list (is-tree test-line 0) ;; 0
      (is-tree test-line 2) ;; 1
      (is-tree test-line 11) ;; 0
      (is-tree test-line 13)) ;; 1
```

One issue I realized. I can't _quite_ just use `map` on this, since I need to iterate the index by 3 each line. Options:
1. Roll my own recursion here. Highly specific but also error-prone and will be super not expressive.
2. Define the indices ahead of time. I'm not 100% sure how to do this, but I know that there will be a way to do it. This will be deterministic, so I can easily calculate ahead of time. I still need to figure out how to get the indices into a function though.
3. I was going to say that there was another one using `fold`, but now I don't think so. I will need the list of indices to work with.

Well, never mind. I looked around a little bit, and `map` works exactly how I want. If I give it 2 lists, it passes both elements to the function. So, 
```scheme
(map + '(1 2 3 4) '(10 20 30 40))
; '(11 22 33 44)
```
This is better anyway, since it's fully portable! So I will generate the list (presumably using something like `in-range`, I imagine they have a stepped range function) and pass it along with the list of lines into the function. The actual call will look something like this:
```scheme
(map is-tree lines indices)
```

To get the indices: There's unfortunately no `range` function in Scheme. Should I go roll my own or give up and just use the bigger library? I've tried to roll my own so that I can use a custom increment, but I'm not figuring it out super easily, so I guess I'll go with the library. `Pretty Big` here I come!

I first try to define `indices` like this:
```scheme
> (define indices (range 0
                         (length test-lines)
                         3))
'(0 3 6 9)
```
However, this doesn't work. It ends when the value reaches `(length test-lines)`, while I need it to keep going until it generates `(length test-lines)` values, if that distinction makes sense. I could easily enough do something like:
```scheme
> (define indices 
    (map (curry * 3)
         (range 0 (length test-lines))))
'(0 3 6 9 12 15 18 21 24 27 30)
```

This works! Now all I need to do is run the function.
```scheme
> (define results (map is-tree test-lines indices))
> (apply + results)
7
```
This is correct! Seems to work for part 1 now. Now all I need to do is set it up to run with the input file. I copy over `lines-from-filename`. Now I just need to make a function that will do this for me. Should be pretty simple: just change the `define`s into `let`s. Here's the full program now:
```scheme
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

(define number-of-trees
  (lambda (map-lines)
    (let* ((indices (map (curry * 3)
                         (range 0 (length map-lines))))
           (trees (map is-tree map-lines indices)))
      (apply + trees))))

(number-of-trees (lines-from-filename "input.txt"))
```
I get 169 from this. It's correct!