(in-package :tables.lang)

#|| TO CONSIDER

id-stability != memory-stability. We can rearrange chunks in a table's
columns (perhaps invalidating ids) without invalidating pointers into the
chunks.

||#

#|| 1.

STRUCTUAL to chunk, STRUCTUAL to row
STRUCTUAL to chunk, DIRECT-REF to row
STRUCTUAL to chunk, INDEX to row
DIRECT-REF to chunk, STRUCTUAL to row
DIRECT-REF to chunk, DIRECT-REF to row
DIRECT-REF to chunk, INDEX to row
INDEX to chunk, STRUCTUAL to row
INDEX to chunk, DIRECT-REF to row
INDEX to chunk, INDEX to row

||#

#|| 2.

If a relationship is not structural then the src holds a ref into the dst

a ref is stable if the target either:
- never gets reordered
- uses some mechanism so the ref remains valid after reordering

A thing that presents unstable refs has to update the src whenever the
dst is reordered

An unstable ref can be made stable by using an index. That way the update
is the index rather than the src.

||#

#|| 0. Relationships

I STARTED WRITING THIS BEFORE THE PERMUTATIONS ABOVE (1). THE FOLLOWING
IS NOT CORRECT

There are many possible kinds of relationships between rows across tables

## table-structural-row-structural
this means that the src table has a offset from it's chunk id to the dst
table, however all chunks have the same offset so you can iterate across
both in lockstep.
Each row within the src chunk has same id as row in dst chunk

## table-structural-row-direct
chunks behave the same as table-direct however each row in the src chunk
can map to any row in the dst chunk by id.

## table-structural-row-indirect
chunks behave the same as table-direct however each row in the src chunk
can map to any row in the dst chunk by id.

## chunk-direct
each chunk in the src table has the index of the chunk in the dst table
which it relates to.

## chunk-indirect-row-direct
each chunk in the src table has the id to an index into the dst tables
chunks. Rows then relate in lockstep

## chunk-direct-row-indirect
chunks in the src table hold the id of the chunk in the dst table however
each row in the src chunk can map to any row in the dst chunk.

## row-direct
any row in the src table can map to any row in any chunk in the dst table
and does so via an id.

## row-indirect
any row in the src table can map to any row in any chunk in the dst table
and does so via an id to an index to that table.

||#
