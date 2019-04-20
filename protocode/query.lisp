(in-package :tables.compile)

;;------------------------------------------------------------

(defmacro define-update-query (name uniforms columns &body body)
  (declare (ignore name columns uniforms body)))

(define-update-query move-step ((dt f32))
    (inputs
     (:> locations pos rot)
     (:> movement velocity))
  (let ((new-pos (+ pos (* velocity dt)))
        (new-vel (- velocity (* dt (* velocity 0.5)))))
    (outputs
     (:> pos new-pos)
     (:> velocity new-vel))))
;; here the symmetry between input and outputs is slightly misleading
;; as the first symbol in the input subclause is the table name and for
;; outputs its the destination column.

;;------------------------------------------------------------

(defmacro define-update-query (name uniforms inputs outputs &body body)
  (declare (ignore name columns uniforms body)))

(define-update-query move-step ((dt f32))
    (inputs
     (:> locations pos rot)
     (:> movement velocity))
    (outputs
     pos
     velocity)
  (let ((new-pos (+ pos (* velocity dt)))
        (new-vel (- velocity (* dt (* velocity 0.5)))))
    (values new-pos new-vel)))

;;------------------------------------------------------------

(defmacro define-update-query (name uniforms columns &body body)
  (declare (ignore name columns uniforms body)))

(define-update-query move-step ((dt f32))
    ((locations
      (pos :in/out)
      (rot :in))
     (movement
      (velocity :in/out)))
  (let ((new-pos (+ pos (* velocity dt)))
        (new-vel (- velocity (* dt (* velocity 0.5)))))
    (outputs
     :pos new-pos
     :velocity new-vel)))

;; or more compactly
(define-update-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in))
     (movement (velocity :in/out)))
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))))

;; keyword name is fine in outputs as matches using string= to name
;; of column in 'columns' declarations

;;------------------------------------------------------------

;; :rot wasnt being used, let's change that

(defmacro define-update-query (name uniforms columns &body body)
  (declare (ignore name columns uniforms body)))

(define-update-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)))
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))

;; This is fun as pos, velocity & rot are independantly calculable
;; and rot can mutate it's column storage directly as nothing else reads
;; from it

;; I think I like this format, it's pretty simple to work with, plays
;; well with the standard formatter and is pretty clear about where
;; values can come from and go to.

;;------------------------------------------------------------

;; Ok so that's the easy case, but we quietly introduced an implicit
;; join there with nary a care. How are the 'locations' and 'movement'
;; tables related to easy other?
;;
;; It's kind of goofy that these are seperate tables as they would be
;; perfectly fine as seperate columns in the same table. Ah well
;;
;; Anyhoo it's weird to iterate over whole tables when one table might
;; only map to a subsection of another table. For example 'creatures' &
;; 'metadata', there is metadata for more than just creatures in that
;; table.
;;
;; So maybe the first table specified in the query definition is the
;; primary table. Tables will have to check to make sure that the
;; primary table is equal to or a superset of the other tables.
;;
;; Hmm yeah.. multiple tables.. so each could have the same problem.
;; How should we resolve that? Well if the primary is the most
;; restrictive.. and if each have a valid relationship to the primary
;; then that should be enough.
;;
;; Hmm we need to keep an eye out for ways to construct relationships
;; which contradict each other. e.g.
;; A relates to B is this way
;; A relates to C the same way
;; B relates to C is a totally different way.
;;
;; When iterating this would mean that rows from B and C which werent
;; previously considered related are being pulled together for the same
;; A row.
;;
;; Not sure what to do about that.. it might be detectable or it might be
;; one of those things that is legal in the system but dodgy.
;;
;; Now in SQL we would use WHERE to control the relationship in the query
;; but we need something static (as this lets us maintain indexes etc)
;; So we will have a way to define a relationship between tables. Hey then
;; maybe for each non-primary table in the query we can specify the
;; relationship.
;;
;; That could be nice as it would let us have lots of potentially
;; conflicting relationships and we dont have to care. It's down to the
;; user to do their job properly.

(define-update-query move-step ((dt f32))
    ((locations (pos :in/out) ;; primary
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)
               :by some-relationship)) ;; explicit relationship
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))

;; How does this play with filter queries?

(defmacro define-filter-query (name uniforms columns &body body)
  (declare (ignore name columns uniforms body)))

(define-filter-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)
               :by some-relationship)
     (metadata (allegiance :in)))
  :where
  (eq allegiance :enemy)
  :do
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))

#||

My hunch would be that relationships apply first and filters apply
as we process. This implies that filters cant be used to limit which
chunks we take ownership of.
Or does it actually? we could compile them seperately and look for
optimizations (basically switching on clusters).

Hold on, I just pulled the update-query and filter-query shit out of
me butt. How do I update only some rows?
Well that would just be an `if` writing back the original value

Really? That sucks as we have no way to know we could `discard` and
save ourselves from the cost of writing to memory!

Actually.. maybe we do. We could look at the dependency and say
'oh this is just the source value, discard' There are cases that
wouldnt work of course, but if the user can use an explicit `discard`
then they could return the source value..
The nice bit is that if we split by cluster then one version will just
always write the src value to the destination which we can just remove
trivially.
It's interesting though as this 'discard' trick only works when we are
writing back to the same storage, otherwise we are copying anyway so
maybe it's actually better *not* to have the explicit discard as we
can't keep that promise in some cases anyway.

What cases do we have where we can avoid writes?
- when it's conditional on a cluster
- when the value is same as src and we are mutating the raw column data
- when a..


So what is the point of define-filter-query?
it almost implies that the results are going into some transient table
rather than a named one.. not sure how that would work though.
Or maybe it means that the contents of the destination tables are
wiped each time a new query writes to it? Or maybe it appends?

Hmm this suggests a different name that query though. I think the
original goal of filter-query was more along the lines of SELECT WHERE
but our WHERE is the static relationship thing.. nah wait WHERE is more
general than that, it is used for declaring the data to JOIN on but
also can be used for arbitrary data filtering.

What advantages do we get from spliting out the filter as opposed to
doing it inline?
Well the first is laziness. our compiler doesnt have to be smart, we
can avoid uncertainty by giving the user a way to say: 'hey this
defintely matters'. Also it means we can expect the user to have
thought of the cost of the operation in it's own regard wheras if
we extract something it might mean some duplication of work that they
werent expecting.
That seems like a pretty great reason to have this actually,
predictability is underrated :p omG so unDerratED mEeeh

MMAAAAYBE we dont need a dedicated filter query though. maybe we can do:
||#

(defmacro define-query (name uniforms columns &body body)
  (declare (ignore name columns uniforms body)))

(define-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)
               :by some-relationship))
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))
;; which is normal

(define-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)
               :by some-relationship))
  :do
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))
;; which is the same as the previous one

(define-query move-step ((dt f32))
    ((locations (pos :in/out)
                (rot :in/out))
     (movement (velocity :in/out)
               (rot-vel :in)
               :by some-relationship))
  :where
  (some-serious-check rot)
  :do
  (outputs
   :pos (+ pos (* velocity dt))
   :velocity (- velocity (* dt (* velocity 0.5)))
   :rot (+ rot (* rot-vel dt))))
;; which is a filtered query

#||

yeah that could work. We could have an implicit progn when no &key
divisors are used and none when they are. This just keeps things looking
sane-ish.


Let's talk about query execution.

Oh no wait, what was that shit we were yakking about where the result goes
into another table? Hmm the first part of it seems simple
||#

(define-query blerp ((dt f32))
    ((foo (a :in/out)
          (b :in))
     (bar (c :out)
          :by append))
  (outputs
   :c (+ a (* b dt))))

#||

Huh, that was interesting, noticed how we had to specify some new kind
of relationship for that to work syntactically. The desination couldnt
be primary as we werent going across it's rows.. but it also doesnt
have a relationship to foo that makes sense for this query

I wonder what other output only relationships we could have

- append is the one we had above
- replace maybe?

||#

#||

Ok NOW let's talk about query execution :)

A query ostensibly takes a table and modfies it.

A query-set groups queries that run simultaneously

When a query needs to write to a table that something else may be reading
it writes the results into a different location and merges them back into
the table at the end of the query-set's execution.

A query that is writing results into fresh memory needs to copy the
original data even if it wasnt mutating it.

This is extra work and we want to minimize that.

One way we can avoid work is taking advantage of how out data is split up.

Tables are split into chunks which are fixed sized subsections of the
table.

Chunks hold column-chunks which are straight-line malloc'd memory which
holds the number of instances of the column type that the chunk specifies.

This means that, when we have to copy, we need to copy at-least/only
the size of the column-chunk. Untouched column chunks can be kept as is.
(by at-least/only I meant that it's either a negative or positive thing
depending on your outlook).

Alright so the query is compiled, and lets assume that it can mutate the
storage directly. What now? Well then .. hmm actually we dont want to
have compiled to simd yet..or do we? hmm no in this case we can discard.

And now another fun thing... we are working with simd, so we can only skip
the write if all lanes are skipping it. How does that work and what
advantages do we get? Does the cpu know that if nothing if the mask says
'dont write anything' not to do some expensive shiz? What is the cost
difference between writing 1 lane and 4?
I'm not sure that there is a conditioanl store in sse2. So we may always
be writing if any lane changed. (clearly I know jack about sse right now)

So back to compiling, we really kinda want to process to ir. Do some
optimization and then do dependency analaysis to split the query.
We need to do that for the whole set as we need to know if anything is
reading something query N is writing to.
We check that no two queries in the query set mutate the same column

At this point we have the information to compile all the effective queries
into the lambdas that actually do the work. We should also have the
cluster mask which we can apply at dispatch to know what each thing can
skip.

Each query that mutates produces a new column-chunk and a ref to the thing
it's replacing.

At a higher level, the column ownership system locks the columns that are
going to be written to and gives ownership to this query-set-job.

This means that other query-sets that only depend on columns not mutated
by the former column set can in fact be dispatched overlapping more work.
Which is, of course, dope :p

So let's get back to query sets. When we compile them we need to:
- check that the queries compile
- split the queries into the effective queries
- add the outer loops to the queries
- ensure that at most one query is writing into a given column
- work out which queries write into a duplicate column
 - hmm dont we need this earlier in order to generate the 'else' case
   where we just copy the original data?

Yeah turns out we have more to do. I guess that we should start by
detecting the fixpoint of the compilation.

Hmm the bunch of the above really isnt the query-set's problem. It should
call compile and get back the functions and requirements for each query
all that splitting and loop shit should be handled already.



||#

(defun dispatch-query-set ()
  )

#||

Another thing to think about is loading in data from the host language.
maybe we can make 'load queries'

||#

(defmacro define-load-query (name uniforms src-data columns &body body)
  (declare (ignore name src-data columns uniforms body)))

(define-load-query blerp ((dt f32))
    (src-elem (array t (*)))
    ((foo a b)
     (bar c))
  (outputs
   :a (data-a src-elem)
   :b (data-b src-elem)
   :c (data-c src-elem)))

#||

the source type could be:

- array
- vector
- cffi array type

||#
