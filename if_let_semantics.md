# `let` - substitution

```
e1 ==> v1   e2[v1/x] ==> v2
---------------------------
  let x = e1 in e2 ==> v2
```

# `let` - context

```
E |- e1 ==> v1   E[x -> v1] |- e2 ==> v2
----------------------------------------
      E |- let x = e1 in e2 ==> v2
```

# `if`

```
  c ==> false   e2 ==> v2
---------------------------
if c then c1 else c2 ==> v2

  c ==> true   e1 ==> v1
---------------------------
if c then c1 else c2 ==> v1
```