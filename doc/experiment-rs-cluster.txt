Experiment: An RServe Cluster
=============================
v2, 2020-06-08

Motivation
----------

To create a minimal implementation of distributed objects in R, with
transparent operations defined, in order to ascertain relevant associated
issues with further work on distributed computations using R.


Method
------

NOTE: R code located in link:../R/make-rs-cluster.R

Using the nectar cluster, the `hdp` node was used as a master with which to
control slave nodes `hadoop1` through to `hadoop8`.
RServe was used as the means for control and communication with the workers.
S3 classes were defined for `cluster`, `node`, `distributed.object` and
`distributed.vector`. 
Communication functions operate serially, but were written with future
parallelisation and speed in mind.

The `node` class contains information on connections to the worker nodes.
The `cluster` class is a collection of `node` 's.
The cluster is set up using `make_cluster`, which `ssh` into the hosts and
launches RServe, along with the relevant libraries and functions.
The global environment local to specific nodes can be checked with the `peek`
method, serving purely as a sanity check at present.
`send()` is a generic with methods defined for the `node` and `cluster` classes;
it takes objects from the master node and partitions the objects into equally
sized consecutive pieces and distributes them to the hosts referenced by the
`cluster` or `node` objects.
It can equally handle objects with smaller `split` sizes than there are nodes,
dispersing them maximally.
`send()` is used just to get data to the nodes to bootstrap the system,
 and wouldn't be used by the end-user.

`distributed.object` at present has no methods defined, serving as a
placeholder for an abstract distributed class.
`distributed.vector` inherits from `distributed.object`, and serves as a master
reference to data that may be spread across multiple nodes.
It contains a list of hostnames, the indices of the vector residing on each
node, and the name of the vector on the nodes, typically being a UUID generated
with the `distributed.vector` creation.

`receive` is the complement to `send`, giving a `distributed.object` as an
argument, and receiving the unsplit referent of the `distributed.object` as the
value.
The method will have additional usage as a remote version, which would enable
point-to-point communication through a node calling `receive` on some distributed
object, thereby requesting the referent from its location on all other nodes.
Such remote usage is not yet implemented due to difficulties with
point-to-point communication using RServe.
However, such functionality is essential, and is discussed further in the
successive sections.

As a means of testing operations between `distributed.vector` objects, S3 `Ops`
methods were defined, using a complex quoting function in order to call the
correct `.Generic` and reference the name of the vectors on the worker nodes.
They can interact with non-distributed objects, with the non-distributed
objects being coerced to distribted.
To enable interaction between vectors of different lengths, some means of
alignment must be defined, to allow elements at equivalent positions to be
processed at the same node.
This is still to be implemented, with further discussion given in the next
section.

No quality-of-life methods such as `print` were defined, with error-checking
and special case consideration being kept to a minimum, due to the primarily
exploratory nature of the implementation.

Outcome
-------

Basic tests revealed functional distributed vectors in the confines they were
defined with. 
A cluster was started and locally referenced. 
Numeric vectors of equal size were then sent to the cluster, with checking
revealing that the correct pieces of the vector were distributed across the
appropriate nodes. 
Simple arithmetic operations were performed using the vectors, of a transparent
form (e.g. `c <- a + b`, where `a` and `b` are distributed vectors).
The outcome was then sampled and found to match the expected output as
performed locally.

Relevant Points of Interest
---------------------------

The experiment raised several very important considerations that had not been
noted prior.

Memory management was a particular concern; 
management of reference and location of distributed objects emulates memory
management at a much lower level, introducing similar issues to those
encountered in systems-level programming.

The initial distribution of objects raises questions of appropriate algorithms
that take load-balancing and other factors into consideration.
One particular example is the question of what to do with vectors of different
length in their distribution across nodes;
if split equally across nodes, it is unlikely that elements at corresponding
posiions between the vectors, and for operations to take place, a significant
amount of data movement ("shuffling") will have to take place.
Consideration should be given to forms of distribution that minimise data
movement, perhaps through maximisation of correspondence with existing vectors,
while still avoiding misbalancing node memory.

Memory leaks, not much of a problem at the R level with garbage collection,
return to a potential problem with assignment of distributed objects being
fixed to their local R processes.
For example, with the following code consisting of distributed vectors: `c <- a
+ b`, what occurs is that on every node `a` and `b` exist on, they are summed
together, with the result saved as a new variable with a UUID name; a reference
to the name and locations is then stored locally in the variable `c`.
Were `a` and `b` not to be assigned, however, the result would still be saved
on all of the worker nodes, taking up memory, but without any local handle for
it.

This is a memory leak at a high level, and reassignment is even worse;
conceivably, there could be some side effect for the cases of non-assignment
and reassignment, though this would require a level of reflection whose
existence is currently unclear in R.

Dealing with objects of greater complexity such as matrices are certain to pose
problems, and it is unlikely that whatever evolution of this implementation
would perform better than something that has had years and teams worth of
effort poured into it, such as LAPACK or SCaLAPACK.

The need for data movement between nodes as in the case of aligning two vectors
to exist at equivalent positions at equivalent nodes for the sake of
processing, if it is to be done efficiently, requires point-to-point
communication.
The alternative is to have each node channel data through the master and then
on to the appropriate node, which would be a massive waste of resources.
This point-to-point communication is not so easy to perform in reality, as
RServe forks a fresh R session at every new connection, so objects that exist
in a particular node in connection with the master are not able to be
referenced in any other connection.

Next Steps
----------

The next steps in this experiment should involve introducing quality of life
aspects to distributed objects such as formal getters and setters, before it
becomes unmanageable.
Further methods for `distributed.vector` as well as a generalisation to vectors
of different lengths are necessary.
The implementation of operations between vectors of different lengths requires
elements of vectors at equivalent positions to be on the same node for
processing; this implies some kind of `align` method, which as discussed in the
previous section, would ideally require point-to-point communication, which
isn't so easily permitted through (ab)using RServe.
In turn, some custom solution would likely be required.
Upon implementing this, the system will be highly flexible, with a clean
demonstration of this begging for the right methods defined such that `summary`
and the like work smoothly.
This would lead naturally to the definition of `distributed.data.frame` objects
and the like.
Furthermore, a means of reading data from distributed storage to their local R
processes would likely yield very worthwhile insights to the process of
creation of a distributed R system.
Porting to S4 may be worthwhile, but it can be performed later.
And finally, a closer literature review on the issues raised and other
solutions will prove very valuable.
