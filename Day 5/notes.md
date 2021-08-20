# Advent of Code Day 5

## Part 1

This seems to be a straight-up application of binary search. The basic problem is that I need to find the seat locations that are given basically in binary search form. They specify the row by a set of 7 binary instructions (forward or backward, so front half or back half of the search region) and the column by a set of 3 instructions (left or right). That makes 127 rows and 7 columns. (Isn't it convenient that this plane has seats in powers of two minus one?) Once I have the seat row and column, I find the "seat ID" by multiplying the row by 8 and adding the column.

So this seems like for each seat, there are 2 relevant functions. First, get the row and column from the instructions, sent as a string. Second, get the seat ID from the row and column.

I'll start with something super simple: finding the seat ID.
```scheme
(define seat-id
  (lambda (row column)
    (+ column (* row 8))))
(seat-id 70 7) ; 567
(seat-id 14 7) ; 119
(seat-id 102 4) ; 820
```
This checks out with all their test numbers, so I'll call it good. (How hard can it be to multiply and add 3 numbers?)

Next up, I need to figure out how to deal with the inputs and parse out a seat spec. The input is given as a string of 10 letters. The first 7 are either "F" or "B" and the last 3 are either "L" or "R". It seems pretty clear that I want to convert the string to a list and iterate through that. In some way, I want to make a generic binary search function that I can use on both of these. It needs to take a list of instructions (first or second half) and also probably a min and max of the currently valid range. This would be a natural recursion scheme. Take a list and the boundaries, and use that to find the next iteration's boundaries. Then use the `cdr` of the list as the instructions for the next iteration, and recurse with all of that. Well defined. The base case here would be when the range is only 2 elements long (I'm guaranteed to have a well-defined 2 case by construction of the 7- or 3-letter instructions and number of rows and columns, at least for now). Then, if it's first half, I pick the smaller of the two, and the bigger of the two if second half.

It makes sense to use a generic binary search instead of inventing the wheel twice. So I need to be able to use the strings from this function. There are 2 ways I could do this. First, I could make the generic function accept both L/R and F/B. Second, I could convert the L/R and F/B to a generic format (say, -1 and 1 or 0 and 1). I think I'll do the second, so the binary search function is a little more expressive and shorter. To convert, I can simply map a conversion function across the list. (Again, this all assumes that I have convert the string to a list of characters, which really just makes sense to me to do.) And anyway, if I make a generic one and then decide to scrap that idea, it's easy enough to change back.

One note for my future: it probably makes sense to use `values` in here instead of returning a list, although I don't really know how it works. Time to find out?

I guess before I go too far down any road, I should work on getting the inputs into a proper format. 
