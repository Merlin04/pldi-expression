# Stateful Semantics

Here is a small language which builds on the expression language from this homework:

```
<comp> ::= skip
         | $id := <expr>
         | <comp> ; <comp>
         | doif <expr> { <comp> } doelse { <comp> }
         | while <expr> { <comp> }
```

(I used doif and doelse to avoid confusion because the semantics of these computations are different from the if expressions you just wrote in the last exercise.) This language represents stateful computations. Whereas expressions are evaluated to produce values, computations induce a change on the underlying state (in this case, an environment). Specifically,

- `skip` does nothing,
- `x := a` assigns the value produced by the expression `a` to the variable `x`,
- `c1 ; c2` executes a computation `c1` followed by a computation `c2`,
- `doif e1 { c1 } doelse { c2 }` runs `c1` if `e1` evaluates to `true` and `c2` otherwise, and
- `while e { c }` executes `c` repeatedly until `e` evaluates to `false`.

Here is an AST for the language:

```ocaml
type comp = Skip
  | Assign of string * expr
  | Sequence of comp * comp
  | Doif of expr * comp * comp
  | While of expr * comp
```

Write semantics for this language. This language is stateful, as we talked about in class on Wednesday. The state in this case is going to be a mapping from names to values. You may assume that there is already a relation `==>` which relates expressions to values. (NOTE: This exercise does not require writing any code. You do not have to write an interpreter for this language, just write the semantic rules.)

## Solution

```
---------------
<Skip, T> -> T

--------------------------
<x := a, T> -> T, x |-> a

<c1, T> -> T2   <c2, T2> -> T3
------------------------------
       <c1; c2, T> -> T3

   T |- e1 ==> false   <c2, T> -> T2
----------------------------------------
<doif e1 { c1 } doelse { c2 }, T> -> T2

   T |- e1 ==> true   <c1, T> -> T1
----------------------------------------
<doif e1 { c1 } doelse { c2 }, T> -> T1

   T |- e ==> false
-----------------------
<while e { c }, T> -> T

T |- e ==> true   <c, T> -> T1   <while e { c }, T1> -> T2
----------------------------------------------------------
                 <while e { c }, T> -> T2
```