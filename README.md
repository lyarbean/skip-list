Skip List
=========

This is a skip list implemented in ada, with RM interface and Lock-free.

Status
======
Not Done Yet

Features
========
1. Non-Removal
    
    As LevelDB does, no node physically gets removed from list. A node is removed and becomes invalid if its Visited is 0.

2. CF insertsion

    The CF Skip List, from Crain, Tyler et al, gives a new insert method, simple and fast, just as Doubly linked list does.
As to maintain the characteristic of Skip list, a lazy selective adaptation is used.

    In our Implementation, DLL insertion is used, but the lazy selective adaptation is not yet implemented. We may introduce to task to accomplish it, but may not as CF does!

License
=======
GPL3
