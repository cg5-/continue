
# Continue

Continue is a programming language I wrote for fun. It should not be used for anything.
It essentially supports generators (like Python or ECMAScript 6), only immutable (they
return the yielded value and a new generator without updating the old generator),
along with a *non-local yield* like Ruby's *non-local return*. For example:

    generator = {
        yield! 2;
        each ([1, 2, 3, 4], {x ->
            yield! x;
            yield! x*2;
        });
    };

`each` is a regular function which takes a callback block, but the generator
can still yield inside the callback, and when it resumes, it will still pick up where
it left off.

It's also the first "real" Haskell program I ever wrote. You probably shouldn't
emulate it.

## Language basics

### Blocks and yields

A *block* is a list of statements inside braces, with an optional argument. Blocks can only
take one argument, but that argument can be a tuple which is destructured, which is basically
the same thing as multiple arguments:

    printSum = {(x, y) ->
        println(x + y);
    };

Blocks can *yield* by writing any identifier followed by an exclamation mark:

    weirdBlock = {y ->
        weirdYield! y+1;
        differentKindOfYield!; // leaving off the argument defaults to the empty tuple ()
        x = yieldsAreExpressionsToo! 7;
        return! x/2; // return is just another way to yield
    };

### Advances

You can *advance* a block using syntax like:

    weirdBlock 7 >> weirdYield as (val, continuation);

(It's pretty rare to use this syntax outside of library code.)

After doing this, `val` is 8 and `continuation` can be though of as the block:

    {
        differentKindOfYield!;
        x = yieldsAreExpressionsTo! 7;
        return! x;
    }

We can continue until the block is completed:

    continuation >> differentKindOfYield as (val, continuation2); // leaving out the block arg defaults to ()
    // val == ()
    continuation2 >> yieldsAreExpressionsToo as (val, continuation3);
    // val == 7
    continuation3 16 >> return as (val, contination4); // x == 16 inside the block
    // val == 8 (i.e. 16 / 2)

The function call syntax `y = f x;` is essentially just sugar for `f x >> return as (y, _);` where the `_`
placeholder matches any value, then throws it away.

Blocks automatically yield `end!` when they're completed.

Most of the time you won't know in advance how the block will yield, so you can listen for multiple
yield types:

    block arg >>
        firstKindOfYield as (val, cont),
        anotherKindOfYield as (val, cont) [ /* statements in here are run if relevant */ ],
        thirdKindOfYield [ /* ... */ ]; // You can leave off the `as` entirely if you don't care

### Non-local yield

    wrapper = {block ->
        block >> weirdYield as (val, cont);
        println(val + 1);
        cont >> weirdYield as (val2, cont2);
        println(val2 + 2);
        return!;
    };

    doTwice = {block ->
        block >> end;
        block >> end;
        return!;
    };

    wrapper {
        doTwice {
            weirdYield! 5;
        };
    };

Since `doTwice` does not listen for `weirdYield`, the `weirdYield! 5` does a non-local yield up to `wrapper`.
This script prints 6, then 7.

If you want to be explicit, you can use the *qualified yield* to yield from a specific block:

    b = {'blah x ->
        doTwice {
            blah'weirdYield! x;
        };
    };

The `blah'weirdYield! x` will always yield back to the user of `b`, and never to `doTwice`. This is necessary
if you have two nested `each` loops and you want to `break!` from the outer one.

### The standard library

The standard library in `data/stdlib.ctu` defines functions which provide basic control structures:

* if and ifelse
* loops over lists and generators (including `continue`, and `break`)
* switch

As well as some fancier control structures:

* a mechanism somewhat like exception handling (closer to the `Either` monad, really)
* a mechanism similar to the list monad (called `quantum`)

By combining these structures together, and combining them with generators, you can write
some pretty ~~elegant~~ impossible to understand code. See `examples/eightqueens.ctu` for an example.

### Caveats

There are no mutable variables. Defining a variable with the same name as another always shadows it
and never overwrites it:

    x = 5;
    printX = {
        println x;
        return!;
    }
    x = 6;
    printX(); // prints 5

You can use the `state` function in the standard library to get mutable state back, but it's
not so convenient:

    state {
        set! 5;
        printState = {
            println get!;
            return!;
        };
        set! 6;
        printState(); // prints 6
    };

For passing data between iterations of loops, is probably easier to use `reduce`.

Functions can only access functions defined strictly before them. In particular,
functions can't directly access themselves. You can use the `recursive` function from the
standard library to write recursive functions.
