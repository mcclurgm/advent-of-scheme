# Notes for 2020 Day 2

## Part 2
Now, instead of specifying the number of times a character can appear, I'm given the locations. Again given `a-b c: str`, `a` and `b` refer to the locations where character `c` can appear, indexed at 1. `c` must appear at exactly one of those locations.

I'm already using the `string-ref` function, so I can just repurpose that for this version. Instead of going through `accumulate-char`, I can simply check whether `string-ref` has the right character at both indices.

Unfortunately, it looks like there's no `xor` function, according to the Dybvig Scheme book. But from some cursory googling, a hypothetical `(xor a b)` is the same as `(not (equal? a b))`. I suppose this makes sense, since the only way for `xor` to be true is if the arguments are unequal (`#t` and `#f`, or the other way round). It's a bit awkward, but I guess it works. And since booleans work with `eq?` (as far as I can tell), I can make this a little quicker by using `(not (eq? a b))`.

Since it looks like I'm going to have a long expression going, I implemented a quick and dirty `xor` function.

I had a small bump with 1-indexing: to convert from 1-indexed to 0-indexed, I _subtract_ 1, not add. I tried the other way round and wondered why I was getting out of bounds errors. Oops.

After fixing that, it seems to work.

## Refining
It feels like I could get this accumulator to work better. My current code is this:
```scheme
(define accumulate-char-tail
  (lambda (ch lst tot)
    (if (null? lst)
        tot
        (let* ((head (if (char=? (car lst) ch) 1 0))
               (cur (+ head tot)))
          (accumulate-char-tail ch (cdr lst) cur)))))
```
It's pretty nested, and it feels _almost_ like `fold`, but I haven't figured out how to get there yet.

Based on a great explanation on [StackOverflow](https://stackoverflow.com/questions/42144068/how-do-foldl-and-foldr-work-broken-down-in-an-example), `foldr` works something like this:
```scheme
(foldr - v '(1 2 3 4))
->
(- 1 (- 2 (- 3 (- 4 v))))
```

If I expand the recursion in `accumulate-char-tail` and move the accumulated value to the middle, I get
```scheme
(accumulate-char-tail #\a '(#\a #\b #\c) 0)
->
(accumulate-char-tail #\a '(#\b #\c) 1)
->
(accumulate-char-tail #\a '(#\c) 1)
->
(accumulate-char-tail #\a '() 1)
```

My original `accumulate-char` was something like this:
```scheme
(define accumulate-char
  (lambda (ch lst)
    (if (null? lst)
        tot
        (let ((head (if (char=? (car lst) ch) 1 0)))
          (+ head (accumulate-char ch (cdr lst)))))))
```
Expanding this:
```scheme
(accumulate-char-tail #\a '(#\a #\b #\c) 0)
->
(+ 1 (accumulate-char #\a '(#\b #\c)))
->
(+ 1 (+ 0 (accumulate-char #\a '(#\c))))
->
(+ 1 (+ 0 (0)))
```
Being more explicit about the result (using some shorthand for `char=?`):
```scheme
(accumulate-char #\a '(#\a #\b #\c) 0)
->
((if (= #\a #\a) 1 0) (accumulate-char #\a '(#\b #\c) 0))
->
((if (= #\a #\a) 1 0) (+ (if (= #\b #\a) 1 0) (accumulate-char #\a '(#\c))))
```

I'm still not convinced I can use fold without a bunch of gymnastics.
The problem is that I need to transform the value from the list into a different value (the character to either 1 or 0). I don't know if this is possible in foldr.
I could do a 2-step process using just builtins. First, map a function that defines the iterator at each index. Then foldr to total them. (Or there's the easier way, by using `(apply + lst)`.)

The result is this:
```scheme
(define accumulate-char-map
  (lambda (ch lst)
    (let ((iter-at-char
            (lambda (test)
              (if (char=? test ch) 1 0))))
      (apply + (map iter-at-char lst)))))
```

I'm not sure if it's much better or more expressive. It's certainly nice to not have to use a wrapper function. But I think it's certainly not as expressive as the original, non-tail-recursive form.
