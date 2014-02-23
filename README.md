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

###Two-step insertion

In "*A contention-friendly, non-blocking skip list*", Crain, Tyler et al, show that
one can play insertion and removal for skip list in two stages :

1. Eager abstract modification
1. Lazy selective structural adaptation

The fact is that, without stage 2, this skip list becomes a simple linked list!

However, I don't employ IndexItem.

###Logical-Removal

Removal of linked list is relevant to the ABA problem. I don't resolve it here. As LevelDB does, I just provide logical removal.

However, there is an issue. If there are many nodes get removed and never get reused, then they become garbage!

References
==========
1. Push, W. Concurrent Maintenance Of Skip Lists [1990]

2. Maurice Herlihy, Yossi Lev, Victor Luchangco, and Nir Shavit

    1. A Provably Correct Scalable Concurrent Skip List [2006]
    2. A Simple Optimistic skip-list Algorithm [2007]


3. Crain, T., Gramoli, V., & Raynal, M.

    1. A contention-friendly, non-blocking skip list [2012]
    1. No Hot Spot Non-Blocking Skip List [2013]

