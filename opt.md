# Optimizations

## Intruduction

These optimizations are not necessary. However, you can take some of them to get a better compiler. And also, sometimes we suggest you to take such optimizations as bonus.

## What are them?

### a4

In live analysis, you can do something better for:
- Check the prediction sentence in If structure when compiling. If value of the Pred can be got in compilation, you don't bother to union two conflict sets of consequtive and alternative. Instead, you can just take one of them. This requires you to record the `true` live set and `false` live set when coming across an `if`.
- It is possible for a variable to be assigned at some point where it is not live. If this occurs, we could discard the assignment and ignore any variable and register references on the RHS of the assignment. If we do this, we'll usually end up with fewer conflicts.
