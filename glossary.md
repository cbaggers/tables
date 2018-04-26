# Glossary

## allocator

## block

A piece of memory managed by the `memory-pool`

## cord

A `thing` that can be `drawn` to signal completion of work to a flag

Once a `cord` is `drawn` it cannot be un-`drawn`

Not all `flag`s make explicit use of `cord`s.

## draw

The action that signals to a `flag` the completion of work

## drawn

The state of a `cord` once it has had `draw` called on it

## flag

An object which can be checked to see whether is has been `raised`.

It is typically use to signal completion of some work.

It's initial state is `not-raised`

Once a `flag` is `raised` it can never be un-`raised`

## gud-jerb

The default `job-manager` that ships alongside `Tables`

## handle-request

A function used internally that produces `job`s from a `request`

## hub

A `thing` that:

- manages a collection of `table`s, `query`s & `query-set`s

- when `pump-hub` is called the `hub` handles `request`s from the user (see `handle-request`) and dispatches
  `job`s to the `job-manager`

## job

A `job` is struct holding a closure which when run will do some work (often all/part of a query) and then `raise`
a `flag`

## job-manager

Code with a `hub` into order handle dispatching and running the `job`s produced by the `hub`.

## lock

The action that locks a `thing` (for example acquiring a mutex)

## lockable

A `thing` that something can `lock` (for example a mutex)

## locked

The state of a `lockable` once `lock` has been successfully called upon it.

## memory-pool

Only part of the `system` responsible for allocating & freeing memory via CFFI.

All `allocator`s get their memory from the `memory-pool`

## not-locked

The state of a `lockable` before being `locked` and after being `unlocked`

## not-raised

One of the two states of a `flag`. The other being `raised`

## potential-future-version

A monotonically increasing value stored internally in a `hub`.

## principle-component

A `table`, `query` or `query-set`

These are the principle `thing`s that the user works with when using `Tables`

## pump-hub

This will cause the hub to pop `request`s from it's internal queue and handle them (see `handle-request`)
dispatching the resulting jobs to `job-manager`

## query

A `query` is a `thing`that reads from one or more `table`s and writes into zero or more `table`s

## query-set

A `query-set` holds a bunch of `query`s to be `run` simultaneously

## raise

A linguistic shorthand. It refers to some action that causes `flag` to transition to the `raised` state.

## raised

One of the two states of a `flag`. The other being `not-raised`

## redefinition

A special kind of `request` which makes changes to the structure or behavior of `tables`s, `query`s
or `query-set`s.

## release-block

The name of the internal function that gives a `block` of memory back to the `memory-pool`

## request

a `request` is a message to a `hub` to do some work

## request-queue

A FIFO queue of `request`s to be handled (see `hub`).

## run

Used in conjunction with `query`s it means the operation defined by the query will be executed over some table/s
This does not say in what order, on what threads it will happen.

Used in conjunction with `job`s it means the inner closure is executed

## submit

The function (and act) that enqueues a `request` with the `hub`

## submitted

The implicit state of a `request` once `submit` has successfully been called on it.

## system

Refers to `Tables` as a whole in a way that, when spoken, is less easily the plural of `table`.

## table

A `table` holds data. It can have `query`s run on it.

## Tables (or TABLES when interned)

The name of the project.

## take-block

The name of the internal function, that gives a `block` of memory back to the `memory-pool`

## thing

Something in Tables. Used so as not to imply an deeper nature than it is a thing we can talk about.

To reiterate: Not necessarily an object.

## unlock

The state of a `lockable` after being `locked` and before being `unlocked`

## upgrade

The generic-function whos methods specialize on `principle-component`s and perform the modification
required by the `redefinition`

## upgrade-request

A function that takes a previously `submitted` `request` which, due to a `redefinition` is for an older `version`
of the `hub` and attempts to recreate it for the current `version` of the `hub`.

## validate-redefinition

A function that takes a `redefinition` and a `hub` and ensures that the `redefinition` will not put the `system`
in an invalid state. More concretely that it will not make queries fail or corrupt relationships between `tables`.

## version

A monotonically increasing number which represents the state, layout, relationships of & between the
`principle-component`s managed by a `hub`.

If a `request` has the same version as a `hub` then it is safe to assume that the `request` is valid for the
`hub` & the `thing`s it manages.

## versioned-thing

A `thing` along with a `version` number. The `version` is used to assert that this thing is `valid` for a
specific `hub`.
