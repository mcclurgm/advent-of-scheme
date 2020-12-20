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

## Part 2
As I suspected, the new version is to check a variety of slopes. Unfortunately for me, this includes going multiple lines at a time, so I can't simply `map` my function to each line and consider that the total.

I need to figure out how to get every second element of the input list. Without changing the asymptotics of this algorithm, I could simply iterate through it and construct a list of every second element. This would be 2n iterations through the input lines, but that's "the same as" the n/2 iterations that I would do in an imperative, array-based language. It's not quite efficient, but it's probably workable. I would have to roll my own every-n-elements function. Alternatively, I could just convert the list of lines to a vector and run it with indices, like I would expect in an imperative language. I don't know how to do this well and idiomatically (it feels like it should be a `map` or `fold` again, but I don't know how best to go about it).

Well, I returned to the drawing board and made myself an `every-nth` function. It's not tail-recursive (at least every time), so it's not great, but it does work! Here it is:
```scheme
(define every-nth-helper
  (lambda (lst use? index)
    (if (null? lst)
        lst
        (let ((next-index (+ index 1))
              (next-lst (cdr lst)))
          (if (use? index)
              (cons (car lst) (every-nth-helper next-lst use? next-index)) ; (Not tail recursive)
              (every-nth-helper next-lst use? next-index))))))

;; Gets every nth element of lst, starting with start.
(define every-nth
  (lambda (lst n start)
    (let* ((use? (lambda (index)
                   (and (zero? (modulo (- index start) n))
                        (>= index start)))))
      (every-nth-helper lst use? 0))))
```

Now that I have this, I can do this properly! (I hope.)

Let's now do what I should have from the start. What will the function need to look like? My current function signature is `(number-of-trees-1 lines)`, where the iteration with each line is hard-coded. I'll need to do something like `(number-of-trees-2 lines vertical horizontal)`, where `vertical` and `horizontal` are the components of the slope. This makes sense, I think.

A slope of `v,h` means that every time I go down `v`, I go across `h`. This is convenient for my filtering method: once I get filtered down to every `v`th line, I will go across by `h` every time in the filtered list. Just like before! It's a little too abstracted from the idea of slopes for my taste, which means it's far less expressive. But I'll soldier on for a while, and see if I can get this method to work. It's much less change from before.

What do I need to change? First order of business is getting the proper set of lines. This means filtering the lines down to the ones that I'll actually hit with my vertical slope.

I start by copying the implementation from before, with the modified signature. If I call this, it should produce the same value as before:
```scheme
(define number-of-trees-2
  (lambda (lines v h)
    (let* ((indices (map (curry * 3)
                         (range 0 (length map-lines))))
           (trees (map is-tree map-lines indices)))
      (apply + trees))))
```
First issue. I forgot that I renamed `map-lines` to `lines` (which I think is a better name), so I fixed that. And then, indeed, it dutifully gives me the same answer as before.

I can now add a new binding to the `let*` form, for the filtered lines. (And a hopefully helpful comment that will at least alleviate the weird unintuitiveness of this code.) Then I use this filtered list in the original code. After some battle with parinfer, which doesn't seem to want to use the right indentation levels, my code looks like this:
```scheme
(define number-of-trees-2
  (lambda (lines v h)
    (let* ((indices (map (curry * 3)
                         (range 0 (length lines))))
           (filtered-lines (every-nth lines v 0)) ; Every line the vertical component of the slope will hit
           (trees (map is-tree filtered-lines indices)))
      (apply + trees))))
```
After realizing that I had been testing this on `v,h=1,1` which won't actually test the vertical slope, I found out that I need to do a little more reorganizing. `indices` is defined by the length of `lines`, while it should be defined by `filtered-lines`. This is because each new index corresponds to the next line that we land on. So I move the binding for `filtered-lines` up to the top, and try again. (Note to self: parinfer is a little finicky when it comes to reorganizing lines. If it or Atom makes a line indented too little, and I go to fix it, all the following lines will be indented to match. Not sure how to get around this, other than if I could just turn it off temporarily, but I don't know how to do this with the Atom version.) When applied to the test data we're given, I get the right answer! (For neglecting `h`, for the moment)
```scheme
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
(define number-of-trees-2
  (lambda (lines v h)
    (let* ((filtered-lines (every-nth lines v 0)) ; Every line the vertical component of the slope will hit
           (indices (map (curry * 3)
                     (range 0 (length filtered-lines))))
           (trees (map is-tree filtered-lines indices)))
      (display filtered-lines)
      (apply + trees))))
(number-of-trees-2 test-lines 2 1) ; -> 2, note 1 is ignored for 3
```

Now I just need to use `h`. This is pretty simple, since I just replace the hard-coded 3 with a reference to it. This works!
```scheme
(define number-of-trees-2
  (lambda (lines v h)
    (let* ((filtered-lines (every-nth lines v 0)) ; Every line the vertical component of the slope will hit
           (indices (map (curry * h)
                     (range 0 (length filtered-lines))))
           (trees (map is-tree filtered-lines indices)))
      (apply + trees))))
(number-of-trees-2 test-lines 2 1) ; -> 2, for real this time
```

So I try it out on the real input and... it doesn't work. I completely forgot what the new problem is, and didn't do all the slopes it wanted me to. So now to complete the new problem.

I need a function to multiply the slopes together. So time to make it. Well actually, I don't need a full function for this, unless I want to try a variety of slopes for fun. So I guess I'll make it anyway.

This may take a bit more engineering than I wanted. I want to be able to do a similar thing to before: `(apply * (map func slopes))`, where `slopes` is a list of `(v h)` pairs (as proper lists) and `func` is some magical function that does exactly what I want. Time to find out what that is, I guess.

In order to be able to map `func`, I need to make it so `func` can accept only one argument that's the pair of slopes. So this sounds like a job for currying! (This isn't in rnrs, but I'm already using a fancier dialect for `range`, and I used `curry` before without double checking where it's available. So time to update the header comment and just use Racket.) What do I need to do? The function needs to accept the pair only. So it needs to have the lines built in. This implies `(curry number-of-trees-2 lines ...)`. Then to get the pair into `number-of-trees-2` I need to use `apply`. So I can then `apply` this function to the pair. But how do I do this, when I'm using `map`? I guess I can do two levels of currying, just for fun.
```scheme
(curry apply (curry number-of-trees-2 lines))
```
Indeed this works! It's super weird, but it works. Theme of the day. I guess if I start to learn to write better Scheme, I can come back to this to see just how far I've come. As a small stop-gap, I can at least use `let*` to descriptively name parts of this:
```scheme
(define multiply-slopes
  (lambda (slopes lines)
    (let* ((trees-for-slope (curry number-of-trees-2 lines))
           (trees-in-map (curry apply trees-for-slope)))
      (apply * (map trees-in-map slopes)))))
```

I run this new function. However, I get the following error:
```scheme
> (define slopes '('(1 1) '(1 3) '(1 5) '(1 7) '(2 1)))
> (multiply-slopes slopes (lines-from-filename "input.txt"))
modulo: contract violation
  expected: integer?
  given: 'quote
  argument position: 2nd
  other arguments...:
   0
  context...:
   /home/michael/Documents/Lisp/Advent of Code/Day 3/day-3.ss:61:2: number-of-trees-2
   "/home/michael/Documents/Lisp/Advent of Code/Day 3/day-3.ss": [running body]
```
This is a pretty confusing error. By a stroke of luck though, I notice that it mentions 'quote, and I see that my original input has too many quotes! I remove the quotes on the inside of the list and it works. Result is 7560370818. And it's correct!

## Part 2 Enhancements
### Tail-recursive `every-nth`

While my solution works, it would be nice to get `every-nth` to work with tail recursion to make this more efficient. I'll give it a try.

My current solution is very close to tail-recursive. Both of the expressions in the `(if (use? index))` form are in tail position. The only thing is that it's the `cons` expression that's tail. So I should be able to call a hypothetical `every-nth-tail` with an argument that's the so-far-current version, and run the `cons` on the argument to that.

After fooling around with it a bit, I wind up with:
```scheme
;; For part 2: since vertical component of slope varies, I need to filter down
;; the list to be only the lines I will actually use
(define every-nth-tail
  (lambda (lst use? index result)
    (if (null? lst)
        lst
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
```

This doesn't work at all. It returns an empty list. I realized a potential solution: I was still returning an empty list in the base case, like I was building the result on my way back up from the base case, instead of on the way down. The proper way to do this is to return the fully-built list in the base case. The result then looks like this:
```scheme
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
```
