Pattern Miner
=============

URE based pattern miner.

If you just want to use the pattern miner, jump straight ahead to the
[Usage](#usage) Section, otherwise read on.

Problem and Terminology
-----------------------

The pattern miner attempts to solve the problem of finding frequent
patterns in the AtomSpace. The terminology used here is similar to the
one defined in this [overview](#chi2005) and the algorithm mimics the
typical algorithms of the subtree mining literature with the
additional twist that patterns are Atomese programs.

Let us recall the important terms

* *Data tree*: a tree (or hypergraph) that is part of the database to
  be mined. Can simply be called *tree*. Generally speaking any atom
  of an atomspace.
* *Database*: a collection of data trees.
* *Pattern tree*: a tree representing a pattern, that is capturing a
  collection of data trees. Can be simply called *pattern*.
* *Frequency*: number of data trees matching a given pattern divided
  by the size of the database.
* *Support*: number of data trees matching a given pattern.
* *Minimum support*: parameter of the mining algorithm to discard
  patterns with support below that value.
* *A priori property*: assumption that allows to systematically prune
  the search space. In its least abstract form, it expresses the fact
  that if a pattern tree has a certain frequency `f` then a
  specialization of it can only have a frequency that is equal to or
  lower than `f`.

Algorithm
---------

Pattern mining operates by searching the space of pattern trees,
typically starting from the most abstract pattern, the one that
encompass all data trees, construct specializations of it, retain
those that have enough support (support equal to or above the minimum
support), then recursively specialize those, and so on.

### Pattern Trees in Atomese

Here pattern trees are Atomese programs. So for instance the most
abstract pattern, called Top, is the following program

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Variable "$X")))
```

that is the identity. `Present` is an atomese construct that indicates
that its argument is a pattern present in the atomspace (or a given
database depending on the context). When passed to the pattern matcher
it becomes a pattern (see
https:://wiki.opencog.org/w/GetLink#Overview), resulting here in a
program that matches all atoms in the AtomSpace.

As another example, a pattern matching only `Inheritance` links would
look like

```scheme
(Lambda
  (VariableSet
    (Variable "$X")
    (Variable "$Y"))
  (Present
    (Inheritance
      (Variable "$X")
      (Variable "$Y"))))
```

Note: `VariableSet` can be used instead of `VariableList` when the
order of the variables in the variable declaration is irrelevant.
That is most certainly the case for the pattern miner as the only
thing that matters in that application is the frequency of the
pattern.  For that reason we strongly recommend to use `VariableSet`
whenever possible, such as for instance in the definition of the
initial pattern. Doing so is likely to speed up the search by many
folds.

Or, a slight specialization of the pattern above, matching only
`Inheritance` links with the same first and second argument would look
like

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Inheritance
      (Variable "$X")
      (Variable "$X"))))
```

### Algorithm Sketch

Given a collection of data trees `T`, a minimum support `ms` and an
initial collection of patterns `C` (containing at least the identity
pattern, Top), the pattern mining algorithm works as follows

1. Select a pattern `P` from `C`
2. Extract the valuation set of `P` over `T`, called `V`
3. Determine the shallow abstractions of `V`, called `A`
4. Specialize `P` by composing it with elements in `A`
5. Add the resulting specializations with enough support in `C`,
   discard the others
6. Repeat till termination

Let us now detail each step

#### Step 2: Extract Valuation Set

The valuation set of a pattern `P` over a collection of data trees `T`
is a set of mappings from the variables of `P` to subtrees (values) of
data trees of `T` such that substituting these variables by their
associated values produce matching data trees (or in order words data
trees in the satisfying set of the pattern).

For instance if `P` is

```scheme
(Lambda
  (VariableSet
    (Variable "$X")
    (Variable "$Y"))
  (Present
    (Inheritance
      (Variable "$X")
      (Variable "$Y"))))
```

and `T` is

```scheme
(Implication
  (Predicate "P")
  (Predicate "Q"))
(Inheritance
  (Concept "A")
  (Concept "B"))
(Inheritance
  (Concept "A")
  (Concept "C"))
(Inheritance
  (Concept "D")
  (Concept "D"))
```

then the satisfying set of `P` over `T` is

```scheme
(Inheritance
  (Concept "A")
  (Concept "B"))
(Inheritance
  (Concept "A")
  (Concept "C"))
(Inheritance
  (Concept "D")
  (Concept "D"))
```

and its valuation set `V` is

```
{(Variable "$X")->(Concept "A"), (Variable "$Y")->(Concept "B")}
{(Variable "$X")->(Concept "A"), (Variable "$Y")->(Concept "C")}
{(Variable "$X")->(Concept "D"), (Variable "$Y")->(Concept "D")}
```

#### Step 3: Determine Shallow Abstractions

A shallow abstraction of a valuation set `V` over a variable `X` is
any pattern that match the values associated to `X`. Possible patterns
are

1. Constant node, such as `(Concept "A")`
2. Variable node positioned in the definition of `P` after `X`, such
   as `(Variable "$Y")`. The reason it must be positioned after `X` is
   to avoid redundancy.
3. Lambda links, such as

```scheme
(Lambda
  (VariableLink
    (Variable "$Z")
    (Variable "$W"))
  (Present
    (Implication
      (Variable "$Z")
      (Variable "$W"))))
```

For example for the valuation set `V` defined above over variable
`(Variable "$X")`, the shallow abstractions would be

1. `(Concept "A")`
2. `(Concept "D")`
3. `(Variable "$Y")`

The last one comes the fact that in the last valuation of `V`, the
value associated to `(Variable "$Y")` is equal to the value associated
to `(Variable "$X")` as well, `(Concept "D")`, allowing to capture a
connection between `(Variable "$Y")` and `(Variable "$X")` as
potential pattern specialization.

Likewise the shallow abstractions of `(Variable "$Y")` would be

1. `(Concept "B")`
2. `(Concept "C")`
3. `(Concept "D")`

without `(Variable "$X")` because, in spite of having some value in
common, `(Concept "D")`, it is positioned before `(Variable "$Y")` in
the variable declaration of `P`.

Another example, if the valuation set is the following singleton
```
{(Variable "$X")->(Implication (Predicate "P") (Predicate "Q"))}
```
its shallow abstraction over its single variable `(Variable "$X")` is

```scheme
(Lambda
  (VariableSet
    (Variable "$Z")
    (Variable "$W"))
  (Present
    (Implication
      (Variable "$Z")
      (Variable "$W"))))
```
because it corresponds to a pattern matching value.

#### Step 4: Specialize with Shallow Abstractions

Given all shallow abstractions associated to a certain pattern `P`
over all its variables, we can compose `P` with each of them to
produce specializations. For instance reusing `P`, `T` and `V` as
defined in the section detailing
[Step 2](#step-2:-extract-valuation-set), let's recall that the shallow
abstractions over variable `(Variable "$X")` are `(Concept "A")`,
`(Concept "D")` and `(Variable "$Y")`. Likewise the shallow
abstractions over variable `(Variable "$Y")` are `(Concept "B")`,
`(Concept "C")` and `(Concept "D")`.

To carry out the composition `Put` is used as follows

```scheme
(Put
  P
  (List
    (Concept "A")
    (Variable "$Y")))
(Put
  P
  (List
    (Concept "D")
    (Variable "$Y")))
(Put
  P
  (List
    (Variable "$Y")
    (Variable "$Y")))
```

to substitute (or beta-reduce, as defined in the Lambda
Calculus) `(Variable "$X")` by `(Concept "A")`, `(Concept "D")` and
`(Variable "$Y")` in `P`, producing

```scheme
(Lambda
  (Variable "$Y")
  (Present
    (Inheritance
      (Concept "A")
      (Variable "$Y"))))
(Lambda
  (Variable "$Y")
  (Present
    (Inheritance
      (Concept "D")
      (Variable "$Y"))))
(Lambda
  (Variable "$Y")
  (Present
    (Inheritance
      (Variable "$Y")
      (Variable "$Y"))))
```

while keeping the variable `$Y` untouched.

Then

```scheme
(Put
  P
  (List
    (Variable "$X")
    (Concept "B")))
(Put
  P
  (List
    (Variable "$X")
    (Concept "C")))
(Put
  P
  (List
    (Variable "$X")
    (Concept "D")))
```

to substitute (or beta-reduce, as defined in the Lambda Calculus)
`(Variable "$Y")` by `(Concept "B")`, `(Concept "C")` and `(Concept
"D")` in `P`, producing

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Inheritance
      (Variable "$X")
      (Concept "B"))))
(Lambda
  (Variable "$X")
  (Present
    (Inheritance
      (Variable "$X")
      (Concept "C"))))
(Lambda
  (Variable "$X")
  (Present
    (Inheritance
      (Variable "$X")
      (Concept "D"))))
```

while keeping the variable `$X` untouched.

See https://wiki.opencog.org/w/PutLink for more information about `PutLink`.

#### Step 5: Add Resulting Specializations with Enough Support

Given all specializations (6 in total in this iteration example), we
now need to calculate the support of each of them against `T`, and
only the one reaching the minimum support can be added back to the
population of patterns `C`. Out of these 6 only one has enough support

```scheme
(Lambda
  (Variable "$Y")
  (Present
    (Inheritance
      (Concept "A")
      (Variable "$Y"))))
```

The others can be safely discarded because, according to the a priori
property, none of their subsequent specializations will reach the
minimum support.

One may notice that already in Step 4 we can avoid creating shallow
abstractions that we know will result into specializations that do not
have enough support, just by counting the number of valuations
matching a shallow abstraction. This is used in the forward URE
implementation as explained in Subsection
[Enumerating Specializations with Forward Chaining](#enumerating-specializations-with-backward-chaining).

### Heuristics

On top of that basic algorithm one can add various heuristics. We will
present one in particular called here *Incremental Conjunction
Expansion*.

A conjunction is the combination of different multiple patterns. For
instance given patterns

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Evaluation
      (Predicate "eat")
      (List
        (Variable "$X")
        (Concept "mexican-food")))))
```

and

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Inheritance
      (Variable "$X")
      (Concept "cat"))))
```

one can consider their conjunction, using `(Variable "$X")` as
connector

```scheme
(Lambda
  (Variable "$X")
  (Present
    (Evaluation
      (Predicate "eat")
      (List
        (Variable "$X")
        (Concept "mexican-food")))
    (Inheritance
      (Variable "$X")
      (Concept "cat"))))
```

Here `Present` has 2 clauses, and the pattern corresponds to a
conjunction of these 2 clauses. Thus when given this pattern to the
pattern matcher it will match all atoms representing cats eating
mexican-food.

The incremental conjunction expansion heuristic allows to combine
patterns (with minimal support) into conjunctions rather than merely
specializing them. Note that conjunction is sometimes called *n-grams*
due to some similarities with that computational linguistic notion,
here we consistently call it conjunction as it is more properly
reflect the intended meaning.

One can see that conjunction expansion is generally not a
specialization. For instance the conjunction of the following patterns
(properly alpha-converted for the expository purpose)

```scheme
(Lambda
  (VariableSet
    (Variable "$X")
    (Variable "$Y"))
  (Present
    (Inheritance
      (Variable "$X")
      (Variable "$Y"))))
```

```scheme
(Lambda
  (VariableSet
    (Variable "$Y")
    (Variable "$Z"))
  (Present
    (Inheritance
      (Variable "$Y")
      (Variable "$Z"))))
```

using `(Variable "$Y")` as connector, result into

```scheme
(Lambda
  (VariableSet
    (Variable "$X")
    (Variable "$Y")
    (Variable "$Z"))
  (Present
    (Inheritance
      (Variable "$X")
      (Variable "$Y"))
    (Inheritance
      (Variable "$Y")
      (Variable "$Z"))))
```

Given the following database

```
(Inheritance
  (Concept "A1")
  (Concept "B"))
(Inheritance
  (Concept "A2")
  (Concept "B"))
(Inheritance
  (Concept "A3")
  (Concept "B"))
(Inheritance
  (Concept "B")
  (Concept "C1"))
(Inheritance
  (Concept "B")
  (Concept "C2"))
(Inheritance
  (Concept "B")
  (Concept "C3"))
```

one can see that each pattern individually has a count of 6. Their
conjunction (connected by `(Variable "$X")`) however has a count of 9
(3*3 for considering all combinations of `A`s and `C`s). Thus the
conjunction cannot be a specialization. Therefore the a priori
property cannot apply to it. For that reason any use of the a priori
property will result in excessive pruning of the search. On one hand
it can speed it up, but on the other hand it makes the pattern miner
less open-ended as some desired patterns might be missed. To see that
assume that the minimum support is set to 9, in that case such a
conjunction above should be accepted, however because the incremental
conjunction expansion will only combine patterns with minimal support
(both 6 here) such combination will be missed.

### Unified Rule Engine Implementation

#### Motivation

Let us first explain why there is an incentive to implement such
algorithm in the URE, beside the coding simplifications that it may
offer. Pattern mining is a hard problem. Thus cleverly searching the
space of patterns is important. There are 2 places where careful
decisions matter, in Step 1, selecting the next pattern to specialize,
and in Step 4, selecting the shallow abstractions to be specialized
with. This highly resembles 2 steps of the URE algorithm, selecting
the next inference tree to expand, and selecting which node and rule
to expand it with. Since the URE has sophisticated inference control
mechanisms, amenable to self-learning, reframing the problem of
pattern mining onto the URE allows us to benefit from those control
mechanisms.

#### Forward or Backward?

There are at least two ways to implement this algorithm in the URE, a
way which is more amenable to backward chaining and another one more
amenable to forward chaining. The current implementation uses forward
chaining but both ways are presented here.

##### Enumerating Specializations with Backward Chaining

The backward chaining way would be to look for proofs of the theorem
that some initial pattern have some minimum support based on the
minimum support of its specializations. In other words, given the
following target to prove
```
minsup(P, T, ms)
```
where `P` is typically an abstract pattern, possibly the Top pattern,
`T` the database in consideration, `ms` the value of minimum support
parameter, and `minsup` a predicate asserting that `P` has minimum
support `ms`.

There would be two main inference rules, one based on the a priori
property
```
minsup(Ps, T, ms)
special(Ps, P)
|-
minsup(P, T, ms)
```
meaning that if `Ps` has enough support and is a specialization of
`P`, then `P` has enough support. And a second rule to evaluate by
direct calculation if some pattern has enough support
```
ms <= support(P, T)
|-
minsup(P, T, ms)
```
necessary when a pattern can no longer be specialized, or simply to
interrupt further specialization.

By mere virtue of attempting to proof the target the URE would
produced inference trees containing specializations, where the deeper
the more specialized. This however requires to either check the a
priori property in the inference control to make sure specializations
that do not have enough support are not being built for no reason
while inference trees are being expanded backward.

##### Enumerating Specializations with Forward Chaining

The forward chaining way starts with the initial pattern as source and
derive inferences with the rule
```
minsup(P, T, ms)
abstraction(S, P, T, ms)
|-
minsup((Put P S), T, ms)
```
where `abstraction` is a predicate that not only `S` is a shallow
abstraction of `P` over `T` but also if `P` is composed with such, the
resulting pattern will reach minimum support, which can indeed be
determined by looking at the valuation set used to generate the
shallow abstractions, as mentioned earlier.

The advantage of the forward technique is that it requires no control
for the a priori property as it is built into the rule.

In principle we would end up with 2 rules

1. Shallow abstraction rule, to produce all shallow abstractions of
   given pattern, database and minimum support, over all its variables.
2. Specialization rule, to produce specializations by composition a
   pattern with its shallow abstractions.

In practice these 2 rules have been packed into a single rule called
*Shallow Specialization* that produce in one step a 2-step process of
shallow abstraction followed by specialization

```
minsup(P, T, ms)
|-
minsup(Ps1, T, ms)
...
minsup(Psn, T, ms)
```

where `Ps1` to `Psn` are all specializations of `P` that have enough
support.

Such rule is expensive however

1. Its formula has been entirely implemented in C++, thus is very
   efficient.
2. The control mechanism is still in charge of making the most
   difficult decision of choosing the next pattern to specialize from.

The 2 rules to carrying out shallow abstraction and specialization in
2 steps is still available if the adventurous user wishes to
experiment with that.

Usage
-----

To invoke the pattern miner, within guile, you first need to import
the `miner` module

```scheme
(use-modules (opencog miner))
```

Then, simply call `cog-mine` on your database with a given minimum
support

```scheme
(cog-mine db #:minsup ms)
```

where `db` is either
1. a Scheme list of atoms
2. an Atomese List or Set of atoms
3. an atomspace (use `(cog-atomspace)` to get the current one)
4. a concept node such that all its members are data trees and `ms` is
   a Scheme number or an Atomese `NumberNode`.

`cog-mine` automatically configures the rule engine, calls it, returns
its results and removes the atoms that were temporarily created. The
results have the following form

```scheme
(Set
  P1
  ...
  Pn)
```

where `P1` to `Pn` are the patterns discovered by the pattern miner.

In addition `cog-mine` accepts multiple options such as

1. Initial pattern. All produced patterns will be specializations
   thereof.
2. Maximum number of iterations (default is 1000).
3. Whether to enable incremental conjunction expansion.
4. Maximum number of conjuncts (in case incremental conjunction
   expansion is enabled).
5. Surprisingness measure.

Providing an initial patterns can greatly speed up the search, as well
as limiting the number of conjuncts and variables. For instance the
following runs the pattern miner with a minimum support of 10, a
maximum number of 2 conjuncts and 3 variables.

```scheme
(cog-mine (Concept "db")
          #:minsup 10
          #:max-conjuncts 2
          #:max-variables 2)
```

where `(Concept "db")` contains (via using `MemberLink`) the data trees.

Beware that the search space will explode as a result of increasing
the number of conjuncts. To minimize the combinatorial explosion the
pattern miner uses by default an Incremental Conjunction Expansion
heuristic, as a result it might miss some patterns. This can be
disabled, see below for further help.

### Help and Examples

All available options are documented in
```
(help cog-mine)
```

Examples can be found in [examples/miner](../../../examples/miner).

Finally, if you wish to carry out manually the various steps
automatically handled by `cog-mine`, configuring the URE and such, the
`miner` module also provides all or most utilities you may need. The list
can be obtained with

```scheme
,in (opencog miner) ,b
```

and each function has an online help like `cog-mine`.

TODO
----

1. Support generic surprisingness, not just I-Surprisingness.
2. Support links such as `DefineLink`, `DefinedSchemaNode`,
   `ExecutionOutputLink`, etc.
3. Store more information about the pattern, such as frequency and
   surprisingness, and make accessible to the user.

References
----------

##### Chi2005 
Yun Chi et al. Frequent Subtree Mining -- An Overview. (2005)
