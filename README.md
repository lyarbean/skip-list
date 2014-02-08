Skip List
=========
This is a Lock-free skip list implemented in ada, with RM interface.

Author
======
颜烈彬

Author
======

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

License
=======
GPL3
