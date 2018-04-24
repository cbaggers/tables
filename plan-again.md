# Tables

Alright let's think about this again.

A `table` holds data

A `query` is a function that reads from one or more `table`s and writes into zero or more `table`s

A `query-set` holds a bunch of `queries` to be run simultaneously

When an `query-set` is dispatched to run an `event` is returned.

An `event` is an object which can be checked for completion. Ideally it will be an atomic counter.

`event`s are analogous to GL's fence and will form the heart of the `job system`.

the `job system` is an optional system to be used alongside Tables which handles dispatching and running the `tasks` produced by Tables.

## Ideas

- a `query` can only be bound to one `query-set`. If you need common behavior, factor this out into it's own
  function.

- originally has though of handling `frame`s in this too, but I want to see if I can avoid that and instead be more gpu like. We do work and provide `event`s. The rest is left to the engine

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
