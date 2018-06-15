`# Tables

Tables are structured as follows

```
structural-joins
 clusters
  chunks
   arrays/data
```

# Joins

The only two joins we need to support to begin with are structural and indexed.

The join has a primary table and secondary table

The secondary of the join is the one that the join is defined on

The primary can have more rows than the secondary (as the primary may be joined to multiple tables)

## Structural

In a structural join the secondary table stores the index of the join-object in the primary table

The nth row of the secondary then maps to the `(+ index n)`th row of the primary.

## Indexed

The first issue than comes to mind is that the index to the row would need to include the join-index, cluster-index & chunk-index too right?

Not sure how to omit this without
