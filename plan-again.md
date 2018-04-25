# Tables

Alright let's think about this again.

## System

'Tables' is the name of the `system`.

The `system` has `version` which is a monotonically increasing number.

The `system` has a `potential-future-version` which is a monotonically increasing number.

The `potential-future-version` will always be equal to or greater than the `version` of the `system`

The `version` increases when a `request` is made to modify the `system`

The `version` will never decrease; even if the modification failed.

Most `thing`s in the system hold the `version` number that the `system` had when the `thing` was created.

A `thing` which has a stored `version` is a `versioned-thing`

## Tables

A `table` holds data

A `table` is a `versioned-thing`

## Query

A `query` is a function that reads from one or more `table`s and writes into zero or more `table`s

A `query` is a `versioned-thing`

## Query-Set

A `query-set` holds a bunch of `queries` to be run simultaneously

A `query-set` is a `versioned-thing`

## Principle-Component

`table`s, `query`s & `query-set`s are the `principle-component `s of the `system`

## Flags

A `flag` is an object which can be checked for completion. Ideally it will be an atomic counter.

`flag`s are analogous to GL's fence and will form the heart of the `job-system`.

A `flag` is not `versioned-thing`

## Job-System

The `job-system` is a system that registers itself with `tables` into order handle dispatching and running the `job`s produced by Tables.

The `job-system` is not a `versioned-thing`.

## Job

a `job` is struct holding a closure which when run will do some work (all/part of a query) and then `raise` a `flag`
A `job` is a `versioned-thing`.

## gud-jerb

`gud-jerb` is the default `job-system`

## Requests

a `request` is a message to the tables system to do some work (usually run a query). 

When a request is `submitted` a `flag` is returned.

`request`s go into a `request-queue`.

`pump-request` will pop an entry from the `request` queue and push it to `handle-request`

`handle-request` will produce `job`s for the request.

A `request` is a `versioned-thing`.

## Memory

We will start with something like the basic tagged allocation detailed by NDog

### Pool

- 1 memory pool. Ask for chunk, giving an id. ID is associated with chunk

- pool is hashtable from ID's to arrays of pointers

- `take-block`
- `release-block`

### Allocator

todo

## Misc Grammar

We use `lock` & `unlock` as verbs and `locked` and `not-locked` to name the state a `lockable` thing can be in.

## Redefinition

The `redefinition-lock` is a boolean that says whether a `redefinition` to modify tables can be enqueued.

`with-no-tables-modified` can be used to `lock` all tables from redefinition during the scope.

On any recompile a `request` is made to modify the system

If the `redefinition-lock` is `not-locked` then the `request` is pushed onto the `request-queue`

If the `redefinition-lock` is `locked` then the `request` is pushed to the `pending-modifications` queue.

When the `redefinition-lock` is `unlocked` `request`s in `pending-modifications` queue are pushed, in the order they arrived, to the `request-queue`

When a `redefinition` `request` is popped by `pump-request` then only 

## Extra Libs

- Query cpu-flags
- High-Res Timer
- set thread affinity

## Ideas

- a `query` can only be bound to one `query-set`. If you need common behavior, factor this out into it's own
  function.

- originally has though of handling `frame`s in this too, but I want to see if I can avoid that and instead be more gpu like. We do work and provide `flag`s. The rest is left to the engine

- 'Query pagesize' lib. Not neccesary though. just start with 2mb

# NDog System

- `frame` piece of data to ultimately be shown on screen
 - 'A frame is defined by the stages the data goes through to become a displayed image' meaning that if it's in the rendering stage its currently a 'render frame' etc>
- A `frame-param` is the data for each displayed frame
 - one instance for each new frame to eventually be displayed
 - sent through all `stage`s of the engine
 - holds stuff like
  - frame number
  - delta time
  - skinning matrices
  - list of meshes to render
  - start/stop time for each `stage`
  - *really any data that gets generated that ultimately will contribute to displaying this frame on screen*
 - kinda like frame globals
 - good as
  - uncontended resource
- can global func to query if frame completed (by id). If true can free
- have 16 frameparams being rotated
 - can track state of 15 frames
 - obviously most data unreachable/stale as cant store 16 frames of memory. however still useful
- memory lifetimes
 - find cases like these
  - single game logic stage (scratch memory)
  - Double game logic stage [same stage one frame later] (low priority ray casts)
  - Game to render logic stage [cross stage memory] (object instance arrays)
  - Game to gpu stage [cross stage memory again] (skinning matrices)
 - have a pool of 2mb `block`s
  - block owned by some `tag`
  - can free all associated with a tag
  - allocator has two case
   - obtains 1 `block` and suballocates out of that
    - uses same block for each worker thread
	- lock around block
   - keep one 2mb block per thread, suballoc per thread
    -  avoids the lock you would need in the previous case
    - use thread-id as index to pick block
	- note that the suballoced memory is local to a `job`
	 - the job can move thread but next alloc will be from the 2mb block for that new thread.
   - when it runs out it obtains new `block` from pool
 - game code never touches block tag so same to query concurrently from memory pool
