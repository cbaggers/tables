(in-package :tables.lang)

#||

Alright, it's planning time again. We need to come up with some idea
around tables and layout. I'm gonna level with ya though, I'm not awake
yet so I have no idea what this needs to entail.

Other than ya know, that we need to pass pointers or something to the
function so we need to know what that looks like

We can only load 8 16 32 or 64 bit 'segments' so we will need to know the
segment size and the offset from the pointer to the start of the segment

It might be worth writing uniforms into a buffer that is unpacked at the
top of the query. The reason is that if it needs to be passed to another
func we could just pass the pointer and that buffer will probably be in
cache.. My concern is about not being able to get the other values on
the stack and so we'd be passing multiple pointers.
Not sure.

Alright back to the snooker.

So the unsorted tables are generally split up into cluster and then
into chunks. We dont need to worry about traversing chunks as we will
leave this to the higher level dispatch code. This means we will always
be given straight chunks of memory (foreign arrays) to iterate over.

Each query is going to be handed only the columns it needs to operate on.
Each iteration the pointers will point to the entry in question.

(for now) the query doesnt need to think about stride or any other details
of the data. We wont worry about optimizations that ammount to reusing the
pointers somehow.

Each columns memory will have some head and tail space which is unused.
The head is to enable memory alignment and the tail is to allow overun by
vector instructions.

I havent yet worked out if the head/tail space is the same for all columns
or if each have their own depending on the type/alignment etc. I need to
look a bit more into vector instructions before I nail this down.

I can start by assuming data offset of zero and using cffi standard
packing. That should let me move quickly while not painting myself into a
corner.

The definition of record accessors (e.g. vec3-x) depend on the layout
of the data. We need to have them expand into (slot-value-q foo bar) and
those will be special forms. We can then compile those into
(load-i8 ptr 24) or whatever in the code. If we make ptr+ into one of our
special ops then it could be (load-i8 (ptr+ ptr 24)) and let the optimizer
take care of deduping that.

I still like the idea of a layout being a function that takes an aggregate
spec and returns a definite definition of what the data is and where.

Loads in turn will be compiled into (cffi:mem-ref ptr :uint8) or whatever.

For scalar columns .. wait lemme wind back a little. First off we want
to work out what to expand usages of the columns vals to. Is it a 'load'?
This feels weird for structs as they are an aggregate. However if we dont
then it's weird for scalars as the idea is that the thing passed to the
query function is the pointer. Ok for scalar columns let's have the
varying var expand to (load arg _type_) this will be cleaned up by
subsequent passes.

A slight shift to the left now, let's talk about the loop itself.
We are gonna have a function that loops over columns calling the query
func.

Is the func inline? well lets put it in a labels form and then we can
work the rest out later. The query func will take N args, each a pointer.
We will unpack the columns ptrs from the chunk passed to the query loop
and put them in local vars. Each iteration we increment the pointer by the
column stride.

Hmm we also have uniform values though. Ok let's assume we inline the
query func ourselves.

||#

#|| Mental Diversion

All uints should be 64 bit. Then all that matters is the operations you
apply to them. 8bit add is the lowest bits, 16bit..well you get the idea.

signed int is tricky as all the bits suddenly matter.

||#


#|| Another diversion

if record constructors were special somehow we could detect when one
is made with constants and make it a constant too. Then it could be
propagated.

Hmm. Actually this probably isnt needed as we will do record-decomposition
(or whatever it's called) and each slot will get it's own var anyway.

||#


#|| Constant diversionings

We don't need to give constant's their own var in stage-0.ast-to-ir as we
inline them in a sunsequent pass. It doesnt hurt to get this right earlier
in the process as all it means is that some other passes need to do less.

||#


#|| Compiling to standard lisp

All the dummy functions we have are basically a telling us the inbuilt
instruction set of tables. I think we should.. well there are a few things
here. First is we shoulddo that exercise of reviewing the instruction sets
of x64 processors and pick out the best ops for us to forumlate our lower
level from. We should look at llvm and see what ops they provide in their
IR as they are thinking much more about portability than I will be for a
long time.

Also we are limited by our simple set of types. So maybe that helps us
with regard to the low-level instructions we need.

I feel like tables could have a bytecode representation. So stage 1 does
all the big optimizations and then we lower to a bytecode representation
and ..

hehe well this isnt really about compiling to standard lisp per-say but it
doesnt hurt.

.. anyhoo we take the bytecode representaton and peephole the balls out of
it. Then we compile the bytecode to whatever.

So yeah working out the set of low level ops we need let's us define the
bytecode we will use.

One sad thing about the bytecode thing is that peephole is going to find
us optimizations which could be super useful.. for example if it finds out
something always has a constant value then propegating that info could be
huge for other passes. So maybe it's not as big a win as we would like?

Still.. a minimal set of ops. We need those. Then we can define the
translation to lisp, x64 etc.
||#
