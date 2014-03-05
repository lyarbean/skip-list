About
=========
This is a Lock-free **Skip list** implemented in **Ada**, with RM interface.

Author
------
颜烈彬

License
-------
GPL3

Status
------
*Pre-Alpha*

Env := Gentoo GNU/Linux & Lenovo Y500 i7-3630QM.

Valgrind shows no memory leak, but some "still reachable" from task allocation.

Design
========

In "*A contention-friendly, non-blocking skip list*", Crain, Tyler et al, show that
one can do insertion and removal for skip list in two stages :

1. Eager abstract modification
1. Lazy selective structural adaptation

The fact is that, skip list is a multi-level linked list! One can link a node to list level by level.
However, obviously, *Lazy selective structural adaptation (LSSA)* takes more time than *Eager abstract modification (EAM)*.
If let a task to handle all LSSA, then all EAM tasks will block on the LSSA entry. But if to empoly more tasks, then how many are enough?
So, I don't do two-step insertion.

What about Removal? Removal of linked list is vulnerable to the ABA problem. Only DCAS can resolve, however, DCAS is unavailable.

As skip list is used for rare-removal situation, then logical removal may be enough.

Anyway, nodes removed but never reused are garbage, so I consider to introduce a task to remove them physically, lazily!

Interface
=========
The spec is similar to Ada RM's Ada.Containers'.

As Cursor is not controlled, one may have a valid cursor to a node, then later delete that node, and then call Element to the cursor, which gives you OMG! Well, Iterator in C++ has the same issue! Any solution? No, I don't have!

References
==========
1. Push, W. Concurrent Maintenance Of Skip Lists [1990]

2. Maurice Herlihy, Yossi Lev, Victor Luchangco, and Nir Shavit

    1. A Provably Correct Scalable Concurrent Skip List [2006]
    2. A Simple Optimistic skip-list Algorithm [2007]


3. Crain, T., Gramoli, V., & Raynal, M.

    1. A contention-friendly, non-blocking skip list [2012]
    1. No Hot Spot Non-Blocking Skip List [2013]

4. LevelDB
