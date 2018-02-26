(in-package :tables)

;;------------------------------------------------------------

(defmacro define-table (name (&key) &body columns)
  (let ((definiton (make-table-definition name columns)))
    `(progn
       (register-table ,definition)
       ',name)))

(defmacro define-join (&body who-knows)
  (declare (ignore who-knows))
  nil)

;;------------------------------------------------------------

(defun make-table-definition (name columns)
  (declare (ignore name columns))
  nil)

(defun register-table (definition)
  (declare (ignore definition))
  nil)


#+nil
(define-table test-table ()
  (position :type vec3)
  (rotation :type quaternion)
  (health :type (integer 0 100))
  (shot-recharge-time :type single-float))

#+nil
(define-table metadata ()
  (flags handy-flags))

#+nil
(define-join test-table 1-1 metadata)

#+nil
(define-join
  (test-table 1)
  (metadata 1))

#||
Join Strategies
---------------
1-to-1
1-to-1-exclusive

hmm structural is always 1 to 1


1-to-1 Join Strategies
----------------------
- structural
- structural-exclusive
- clustered structural (structural join inside cluster, clusters
                        mapped by index)
- indexed


1-to-* Join Strategies
----------------------
- Join from column of type X to cluster column of type X


*-to-1 Join Strategies
----------------------
Note: we need to ensure (one every add-row) that the new element does make
      there be 2 things with same 'index'. Otherwise it becomes *-to-2


*-to-* Join Strategies
----------------------
- all to all: visit second table once from every element in first table


structural & structural-exclusive will be baked in and not replicatable via
user code, however i want to provide ways to have user extensible indexes

||#

#|| Custom Indexing

An index can be too a table or not. The later case is for things like
spatial indexes.

The datatype of an index is user defined but cannot be soa, so that means
it's gonna be a packed type of some kind.

The index the system will allow will be almost idential to a query. The
columns required will inform the indexer when to mark the index as dirty
(if updating indicies lazily) or recompute (if updating is eager)

One thing though....we want to be able to specify different datastructures to
the clusters/chunks. In different cases a static array (cluster on small enum)
will be fine, in others a sparse array (many clusters) & for others a tree
(spatial query). How do we expose this?

'the index' is a datstructure that maps from some key to 'an index'.
a spatial index maps from a position and a depth/resolution to the indexes to
a set of entries.
In some cases it may make sense to have the data store mirror the index. hmm

A simple index may map a row-num in one table to a row-num in another table.
In this case is makes sense to elide the src row-num and just store the
destination row num with the source row.

Maybe we should think about indexes across tables seperately from indexes
within tables. We can take clustering and internal indexing treat them as
the same problem.

Hold on though maybe the tables should be tables (with clustering on vals etc)
and then make the join indexes the problem again.. the reason I say this is
that. I want multiple graphs over my data, not just spatial, tying spatial
to the storage is a bad idea.

Maybe the spatial hash is part of some index-to/from-external-system feature.
I mean it could be a table with index join to the entity table but it sounds
like a less frequent case and makes the definition of table weaker.

OK! so back to the snooker..


Join Indexing Strategies

Looking at the current join indexes 1-to-1 & *-to-* seem like they will be
baked into the system.

'Join from column of type X to cluster column of type X' looks like a condition
in the query to me. We are gonna need this kind of awareness in the query
compiler anyway as we want to be able to decompose/recompose the queries to
allow for potential speedups through threading (and good old branch prediction
stuff).

Then there is *-to-1 .. maybe just ban this and force the user to write the
join in reverse. One downside would be if there are many 1-to-* joins from some
table, then it may be harder to target a specific table.

.. fuck, it's hard to get back to the snooker. However thinking about how to
maintain the speed/cache promises with custom storage helps... but then
thinking about the query optimizer makes it a touch harder again. Nah, focus
on bridging to external systems for now.

||#

#|| Indexing to/from external systems

We need ways for things like foreign libs & other data system to reference (or
be referenced by) rows in tables.

each row contains a tpref. external system has add, remove, update query.
add & remove just take tpref. update takes tpref & value from specified column.
query take a value of type from specified column (and some other explicit args)
and writes a set of tprefs into a given buffer.

||#

;; misc: chunk size should probably be a power of 2 so finding the index of the
;;       correct chunk is a right shift


;; skiplists, indexing or anything that confounds locality reduces the ability
;; of simd to help. You'd have to pack data into vectors first and then
;; dispatch. Those temporaries can be precomputed and stored in a single buffer
;; however (or just use the scratch buffer)


;; maybe all tables start with 1 join 1 cluster and 1 chunk. The cluster is the
;; easiest to justify, when you add a cluster column this becomes the default
;; values's cluster.
;; Join is a little harder as it depends whether join will continue to own the
;; clusters or if it will become a parallel index to the tables chunks.
;; Either way a starting chunk shouldnt hurt. Then we only need logic for
;; adding new chunks when one is full rather that when none exist.. probably
;; a shitty reason though as then we should have 1 chunk for every cluster up
;; front.. can probably say every cluster object made gets a chunk though.
;;
;; After all that im not sure what the benefit of this was meant to be :D






#|| Memory freeing

When a block is not longer needed by a table (or maybe a chunk we will see) it
is kept for N frames regardless. This is because it is likely that certain
tables will fluctuate in size fairly wildly and we want to minimize the chance
that the very next query will ask for a block of exactly the size that was just
ditched and it will have already been given to something that could have used a
smaller buffer.

Maybe tables with same chunk size can eagerly steal from each other, but
a table cant steal an oversized chunk until it is fully released.

Feels like a chunk can be released & retired, and I'm not sure which way around
those statuses go.

||#
