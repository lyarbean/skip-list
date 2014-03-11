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

Env := Gentoo GNU/Linux & i7-3630QM.

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

What about Removal? Removal of linked list is vulnerable to the ABA problem. Only DCAS can resolve, however, DCAS is unavailable.

As skip list is used for rare-removal situation, like Dictionary, then logical removal may be enough.

Thus in this implementation

1. No two-step insertion
2. No physical removal!

N.B. Nodes removed but never reused are garbage, user is responsible to use Read-Write Lock to remove them.
I consider to add procedure Physical_Delete for high level use.

Interface
=========
The spec is akin to Ada RM's Containers.
However, no Variable_Indexing provided, because Variable_Indexing can change Element and then violate skip list's structure.

Please  try "for x of y loop" instead of cursor. A cursor is easy to be invalid as the node it refers may be removed from the list or Freed.
In later case, dereference to the node leads to segfault!
As Logical removal is applied in this implementation, one can call Is_Valid to verify a cursor.



References
==========
1. Pugh, W. Concurrent Maintenance Of Skip Lists [1990]

2. Maurice Herlihy, Yossi Lev, Victor Luchangco, and Nir Shavit

    1. A Provably Correct Scalable Concurrent Skip List [2006]
    2. A Simple Optimistic skip-list Algorithm [2007]


3. Crain, T., Gramoli, V., & Raynal, M.

    1. A contention-friendly, non-blocking skip list [2012]
    1. No Hot Spot Non-Blocking Skip List [2013]

4. LevelDB
