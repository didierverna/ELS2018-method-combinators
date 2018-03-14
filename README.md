# Companion code for my ELS 2018 paper #

## This is the abstract of the paper: ##

In traditional object-oriented languages, the dynamic dispatch algorithm is
hardwired: for every polymorphic call, only the most specific method is
used. CLOS, the Common Lisp Object System, goes beyond the traditional
approach by providing an abstraction known as "method combinations": when
several methods are applicable, it is possible to select several of them,
decide in which order they will be called and how to combine their results,
essentially making the dynamic dispatch algorithm user-programmable.

Although a powerful abstraction, method combinations are under-specified in
the Common Lisp standard, and the MOP, the Meta-Object Protocol underlying
many implementations of CLOS, only worsen the situation by either
contradicting it or providing obscure protocols. As a consequence, too much
freedom is granted to conforming implementations, the exact or intended
behavior of method combinations is unclear and not necessarily coherent with
the rest of CLOS.

In this paper, we provide a detailed analysis of the problems posed by method
combinations, the consequences of their lack of proper specification in one
particular implementation, and a MOP-based extension called "method
combinators", aiming at correcting these problems and possibly offer new
functionality.
