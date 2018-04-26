# Tables

Tables is a system that lets the user define and query tables of flat data.

Tables aims to 'be fast' by managing the memory for the tables itself in a more granular fashion and by adding
restrictions to allow processing the data in more cache friendly ways and compiling queries to SIMD instructions
where possible.

Tables must allow the user to redefine queries and tables 'live' and provide clear actionable information when
the change put the system in an invalid state.

## A note on style & terminology

When pluralizing any term in backquotes we use 's', even in cases where that would be incorrect in standard english. For example we will say `query`s not `queries`.

We use `lock` & `unlock` as verbs and `locked` and `unlocked` to name the state a `lockable` thing can be in.

## System

'Tables' is the name of the project, henceforth referred to as the `system`.

'Tables' allows the user to define `hub`s.

## Hub

A `hub` manages a number of `table`s, `query`s & `query-set`s

A `hub` is the `thing` a user's program interacts with to process data

`hub`s, and the `thing`s they manage, do not communicate with other `hub`s, or the `thing`s those `hub`s manage.

A `table`, `query` or `query-set` cannot belong to more that one hub.

A `hub` has a `version` which is a monotonically increasing number.

A `hub` has a `potential-future-version` which is a monotonically increasing number.

The `potential-future-version` of a `hub` will always be equal to or greater than the `version` of that `hub`

The `version` increases when a `request` is made to modify a `hub`

The `version` will never decrease; even if the modification failed.

Most `thing`s in the system hold the `version` number that a `hub` had when the `thing` was created.

A `thing` which has a stored `version` is a `versioned-thing`

## Table

A `table` holds data

A `table` is a `versioned-thing`

## Query

A `query` is a `thing` (internally a function) that reads from one or more `table`s and writes into zero or
more `table`s

A `query` is a `versioned-thing`

## Query-Set

A `query-set` holds a bunch of `query`s to be `run` simultaneously

A `query-set` is a `versioned-thing`

## Principle-Component

`table`s, `query`s & `query-set`s are the `principle-component `s of the `system`

## Flags

A `flag` is an object which can be checked to see whether is has been `raised`.

Ideally it will be an atomic counter.

`flag`s are analogous to GL's fence and will form the heart of the `job-manager`.

A `flag` is not `versioned-thing`

## Cord

A `cord` is a `thing` used in conjunction with some `flag`s. It is a `thing` that can have `draw`
called on it in order to signal to the associated `flag` that work is complete.

A `cord` cannot be `drawn` twice or become un-drawn

An example of use is a `flag` that is `raised` once 5 `job`s are completed. Each `job` gets a `cord`
and when all have been `drawn` the `flag` becomes `raised`.

## Job-Manager

The `job-manager` is code that registers itself with a `hub` into order handle dispatching and
`run`ning the `job`s produced by the `hub`.

The `job-manager` is not a `versioned-thing`.

## Job

a `job` is struct holding a closure which when `run` will do some work (all/part of a query) and then `raise`
a `flag`

A `job` is a `versioned-thing`.

## gud-jerb

`gud-jerb` is the default `job-manager`

## Requests

a `request` is a message to a `hub` to do some work (usually `run` a query on a `table`).

When a request is `submitted` a `flag` is returned.

`request`s go into a `request-queue`.

`pump-hub` will pop an entry from the `request` queue and push it to `handle-request`

`handle-request` will produce `job`s for the request.

A `request` is a `versioned-thing`.

## Memory

We will start with something like the basic tagged allocation detailed by NDog

### Memory-Pool

- Ask for chunk, giving an id. ID is associated with chunk

- pool is hashtable from ID's to arrays of pointers

- `take-block`
- `release-block`

### Allocator

todo

## Redefinition

A `redefinition` is a `request` who's `job`s make a change to a `hub`s `principle-component`s

A `redefinition` must have `upgrade` methods defined for it which specialize on each of the
`principle-component`s of the `system`.

The following describes a lifecycle of the `redefinition`

The user edits some code and compiles

A `redefinition` is made and passed to `validate-redefinition`

`validate-redefinition` compares with the current `system`

If deemed valid the `redefinition` is tagged with the `version` of the `hub` it was validated against.

The `redefinition` `request` is `submitted`. The `flag` is attached to the `redefinition` as well as
being returned.

when `handle-request` reaches the `redefinition` it creates & dispatches the `job`s as usual but then waits on
the `flag` in the redefinition.

On completion the `redefinition`'s success `flag` is checked and, if true, the `version` of the `hub` is set to
the `version` stored in the `redefinition`. `hub` version is asserted to be lower than the new `version`,
when this is not true we crash as this is a bug.

The following is how this affects other `request`s

Any `request` with a version lower than the current `version` of the `hub` is passed to `upgrade-request`

`upgrade-request` should never fail for non `redefinition` `request`s. The `validate-redefinition` process was
meant to ensure this. We make heavy use of asserts and fail hard if we find an issue as this is a bug.

`upgrade-request` for `redefinition`s will calling `validate-redefinition` again and `submit`ing the result. This
can fail and this is simply communicated to the user. The frequency of human updates is low enough that this is
not considered an issue.

Once a `request` has been processed by `upgrade-request` it is passed to `handle-request` as usual.

## Extra Libs

- Query cpu-flags
- High-Res Timer
- set thread affinity

## Questions

- 1 `memory-pool` for the `system` or 1 per `hub`?
- do we allow `run` on single query and treat as it's own `query-set`

## Ideas

- the simplest valid `job-manager` should be #'funcall

- assert that a given Tables layout is identical to a cffi struct layout allowing a direct byte copy.

- a `query` can only be bound to one `query-set`. If you need common behavior, factor this out into it's own
  function.

- originally has though of handling 'frames' in this too, but I want to see if I can avoid that and instead be
  more gpu like. We do work and provide `flag`s. The rest is left to the engine

- 'Query pagesize' lib. Not necessary though. just start with 2mb
