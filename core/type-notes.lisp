#|| Types again

So today's issue is the design of types for tlang

I want to be explicit about the layout of types

    (define-block f32 ()
      (idk 1)
      (mantissa 24)
      (exponent 7))

but we also have fairly interesting types like bool

bool is 1bit of information, it could be backed in this way without loss

generally bool is worked with as a byte.

We need a way to express this that doesnt force bool to be a byte as there
are many times I want to pack them as a mask or as a flag in some small
struct.

Another important quality is that a type desribes a set of values.
Bool is actually `true` or `false`, but it's encodable in 1 bit.

This makes it somewhat different from f32 which is a specific layout and
standard for a floating point number.

Maybe bool is a trait, b1 & b8 would be implementations of this trait.

That would probably work but feels odd to pin down bool in that way.

I'd prefer to have a type, an encoding, and a place to encode into.


Maybe this is overthinking it. Maybe we just have functions to convert
to i8 etc

.. encode can be a function that takes a value, a position and a bit-block.
It should write the value into the position specified in the block.

    (& block (<< (coerce 1 '(block 32)) position))

for example would encode `True` into the 32 bit block. (<< x 0) of course
would always just reduce to x and coerce of any constant can happen at compile
time. So the case for bool would become (& block 1).

(block 32) or (bit 32)?

bit seems nicer in text than I thought it would.

that shift (<<) above seems redundent as we always know how to move a value
around, we just need to have it encoded as a (bit *)

bool can be a trait, boolean and (bool x) would be the implemtations where
`boolean` is a lisp `boolean` and `(bool x)` is a bool of n bits.

||#

#||

The primary place data exists is in cffi arrays, so encoded is the normal
state.

Maybe I need to look at what c compiles stuff to to get an idea of what to use.


one thing with tables is that it is kinda meant to be higher level. For example
we wouldnt really advise people to used a sized bool in a column as it limits
our ability to fold it in with other stuff.

However how far can we go with this? If someone puts a trait in a column what
does that mean? For example an 'addable' column is meaningless. So back to the
bools.. the fucking bools. We could special-case them, but that feels like
cheating.

Well one point is important. Tables is high level, we want the freedom to
somewhat control layout. We are gonna need to do this for simd stuff for sure.
That means we dont assume tight packing between consecutive elements in
columns. Instead it will be the correct packing for good simd use.

So maybe a bool is 1 bit. Maybe we reserve the right for us to pick any runtime
respresentation in code (so it can be lisp or asm) and we have some means of
specifying default layouts for structs (packed, c, etc)

or maybe we say screw it and all structs are bitstructs and you have to deal
with stuff yourself... sounds like it would make ffi harder than it needs to be
though.

||#

#||

How does the simd alignment stuff affect valid layouts for structs? We coudl
make a struct and then make a column from it. we might then have to do
*something* to unpack the data into a form we could address.. or we just have
to say fuck it and go linear.. which is the more likely case tbh

||#

#||

Hi folks, I've been musing on this and I'm wondering if you know of any systems
that handle types in a similar way (so I can borrow some ideas from them).

I have a toy language where I want to be able to define packed values.
For example and standard ieee754 float might be:

define f32
{
    sign 1;
    mantissa 24;
    exponent 7;
}

So far so good but then I started thinking about `Bool` as it's probably the
simplest complication to my scheme.

Bool is a type, a type is the set of all possibile values, in this case
it's `True` and `False`.

Now it is most commonly worked with as a byte but the information is encodable
in one bit of course. We often want to do that in network stream or as flags in
another packed value.

I could make a b1 and b8 types for 1bit bool and 8bit bool but this feels like cop-out

I'd rather have some way to define the type and it's 'encodings' seperately.

In this case the host language will be lisp so if I were to write a `Bool` to a
lisp stream it would default to being represented (encoded) as a lisp bool. But if
I were to write it to a binary stream then maybe I can choose the 1bit encoding.

Either way I'd like to be able to use the boolean type from the host language and
other encodings (e.g. the 8bit numeric bool) in the code without it always forcing
to one or the other (as unneccesary conversions just cost us cycles)

(Bool's behaviour could just be hard coded, but I like it as an example of this
case and it would be nice to be able to express it in the language)

Any ideas, critiques and papers are very welcome.
||#
