# Notes for 2020 Day 2

## Part 2
Now, instead of specifying the number of times a character can appear, I'm given the locations. Again given `a-b c: str`, `a` and `b` refer to the locations where character `c` can appear, indexed at 1. `c` must appear at exactly one of those locations.

I'm already using the `string-ref` function, so I can just repurpose that for this version. Instead of going through `accumulate-char`, I can simply check whether `string-ref` has the right character at both indices.

Unfortunately, it looks like there's no `xor` function, according to the Dybvig Scheme book. But from some cursory googling, a hypothetical `(xor a b)` is the same as `(not (equal? a b))`. I suppose this makes sense, since the only way for `xor` to be true is if the arguments are unequal (`#t` and `#f`, or the other way round). It's a bit awkward, but I guess it works. And since booleans work with `eq?` (as far as I can tell), I can make this a little quicker by using `(not (eq? a b))`.

Since it looks like I'm going to have a long expression going, I implemented a quick and dirty `xor` function.

I had a small bump with 1-indexing: to convert from 1-indexed to 0-indexed, I _subtract_ 1, not add. I tried the other way round and wondered why I was getting out of bounds errors. Oops.