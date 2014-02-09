Skip List
=========
This is a Lock-free skip list implementation in ada, with RM interface.

Author
======
颜烈彬

Status
======
*Alpha*

I do tests on Gentoo GNU/Linux with my Laptop (Lenovo Y500 i7-3630QM).

Valgrind shows no memory leak, but some "still reachable" from task allocation.

See skiplistdri

Design
========
1. CF insertsion

In "*A contention-friendly, non-blocking skip list*", Crain, Tyler et al, shows that
one can play skip-list insertion in two stages :

    1. Eager abstract modification
    2. Lazy selective structural adaptation

And one may think it as a Doubly linked list (DLL) insertion with some singly Linked list (SLL) insertions.
The virtue is that SLL insertions can be scheduled to do later, which makes it wait-free.

2. Non-Removal

As LevelDB does and also mentioned in "*A contention-friendly, non-blocking skip list*", one may let Nodes never be freed physically.

In our designe, to remove a node is to simply decrement its Visited. A node is invalid if its Visited is 0.

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

License
=======
GPL3
