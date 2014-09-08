Churro
======
Churro is a stack-based interpreted programming language in which the code is
entirely made up of churros.

Design Principles
-----------------
* Churros are magical and should therefore be the only syntax element.
* The language must include mutable state in order to irk Calvin Beck.

Language Concepts
-----------------
Churro operates on two structures: a stack and an array. Operations can only be
directly performed on data in the stack. The array is used to store data which
may need to be recalled later. It is zero-indexed and contains 9999 elements,
all of which are initialized to 0.

Syntax
------
The only syntax elements are churros. A churro might look like this:
`{o}=====}`

There are three important components to a churro:

1. Its orientation -- either left or right
2. Its filling -- either `o` (unfilled) or `*` (filled)
3. Its tail length

A `{` always indicates the beginning of a churro. Any other characters between
churros are ignored.

Literals
--------
Left-facing churros, also known as "literal churros," represent data. When a
literal churro is encountered, its value is pushed to the top of the stack.

The tail length of the churro -- that is, the number of `=` symbols -- is its
numerical value. Sign is indicated by a churro's filling, with unfilled churros
(`o`) being positive and filled churros (`*`) being negative. All data is stored
as integers.

For example, this churro stands for 3:
`{o}===}`

this churro stands for -9:
`{*}=========}`

and this tiny one stands for 0:
`{o}}`

Operators
---------
Right-facing churros, also known as "operator churros," are used to manipulate
data. They operate on values in the stack.

The functions are:

| Churro            | Operation                                                        |
| ----------------- | ---------------------------------------------------------------- |
| `{{o}`            | pop A; discard A                                                 |
| `{={o}`           | pop A, B; push (B + A)                                           |
| `{=={o}`          | pop A, B; push (B - A)                                           |
| `{==={o}`         | pop A; if A = 0, jump to churro after next occurence of {===={o} |
| `{===={o}`        | pop A; if A != 0, jump to churro after last occurence of {==={o} |
| `{====={o}`       | pop A, B; store B in memory location A                           |
| `{======{o}`      | pop A; push the value in memory location A to stack              |
| `{======={o}`     | pop A; print A as an integer                                     |
| `{========{o}`    | pop A; print A as an ASCII character                             |
| `{========={o}`   | read a single character from stdin and push it to the stack      |
| `{=========={o}`  | exit the program                                                 |

Filled operator churros have the same behaviour as unfilled churros, but instead
of popping values on the stack, they peek them.

Style
-----
Churros longer than a length of 10 have very little structural integrity, and so
for the sake of realism, they should be avoided. Adding two smaller churros
together is an acceptable solution.

Churros are also generally not stacked directly end-to-end, so leave a single
space between each churro. To avoid churros being cut in half (the horror!) by
console displays, keep lines below 80 characters in length.

In order to maintain the purity of the program's churro makeup, minimal (or no)
comments are recommended. Of course, this may make code more difficult to read,
so it is acceptable to distribute a "dirty" version of the code with comments
and extra whitespace so long as a "pure" version with minimal whitespace is also
included.

Installation
------------
Assuming you have Cabal installed, simply execute
`cabal install`
in this directory. You can then run
`churro <filename>.ch`
to execute a Churro program, such as one of the ones contained in the Samples
directory.