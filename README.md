Skip List
=========
This is a Lock-free skip list implementation in ada, with RM interface.

Author
======
颜烈彬

Status
======
*Alpha*

Not Done Yet

Design
========
1. Non-Removal

    As LevelDB does, no node physically gets removed from list. A node is removed and becomes invalid if its Visited is 0. However, there is an issue that, as removed nodes may no longer get activated and they get no chance to be reclaimed!

2. CF insertsion

    The CF Skip List, from Crain, Tyler et al, gives a simple and fast method for insertiion, as Doubly linked list (DLL) does.
In order to maintain the characteristic of Skip list, a lazy selective adaptation is used, which builds Forward Links for new Nodes parallelly.

References
==========
1. Push, W Concurrent Maintenance Of Skip Lists [1990]

2. Maurice Herlihy, Yossi Lev, Victor Luchangco, and Nir Shavit

    1. A Provably Correct Scalable Concurrent Skip List [2006]
    2. A Simple Optimistic skip-list Algorithm [2007]
 
4. Dmitry Vyukov, Concurrent Skip List [2010]

License
=======
GPL3
