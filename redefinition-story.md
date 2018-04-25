the system version is 105

the `potential-future-version` version is 105 or greater

you edit some code and compile

The `make-request` lock is `locked`

> Functions that produce `requests` are now blocked until the `redefinition` is complete
> (whether it succeeds or fails)
> New `Redefinition`s fail outright and let the user know a `redefinition` failed.
> This is so we don't have to fail which would require handling from the submitting code.

The `system` `lock`s the `potential-future-version`, increments it, returns the new value (`unlock`ing on the way)

The `potential-future-version` is attached to the `redefinition`

The `redefinition` is enqueued

The `redefinition` is turned into jobs and the `system` waits on the `flag`

When the `flag` is raised the systems sets the `version` to be the `potential-future-version` from
the `redefinition` and release the `make-request` lock

-------------------------------------
Ok issues with above are:
- if we block a load of enqueues what order do they add afterwards
 - that doesnt matter as we cant enforce that across threads anyway
- do we need such lockdown on making requests as if the query cant be updated the redef
  will fail anyway.
 - this is the important one

-------------------------------------

A `redefinition` is a `request` who's `job`s make a change to the `system`s `principle-component`s

A `redefinition` must have `upgrade` methods defined for it which specialize on each of the
`principle-component`s of the `system`.

The following describes a lifecycle of the `redefinition`

the system version is 105

the `potential-future-version` version is 105 or greater

you edit some code and compile

a `redefinition` is made and passed to `validate-redefinition`

`validate-redefinition` compares with the current `system`

if deemed valid the `redefinition` is tagged with the `version` of the `system` it was validated against.

the `redefinition` `request` is `submitted`. The `flag` is attached to the `redefinition` as well as
being returned.

when `handle-request` reaches the `redefinition` it creates & dispatches the `job`s as usual but then waits on
the `flag` in the redefinition.

On completion the `redefinition`'s success `flag` is checked and, if true, the `version` of the system is set to
the `version` stored in the `redefinition`. We assert `system` version is asserted to be lower than the new
`version`, when this is not true we crash as this is a bug.

The following is how this affects other `request`s

Any `request` with a version lower than the current `version` of the `system` is passed to `upgrade-request`

`upgrade-request` should never fail for non `redefinition` `request`s. The `validate-redefinition` process was meant to ensure this. We make heavy use of asserts and fail hard if we find an issue as this is a bug.

`upgrade-request` for `redefinition`s will calling `validate-redefinition` again and `submit`ing the result. This
can fail and this is simply communicated to the user. The frequency of human updates is low enough that this is
not considered an issue.

Once a `request` has been processed by `upgrade-request` it is passed to `handle-request` as usual.
