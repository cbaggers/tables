#||

Just a rough attempt to categorize behaviours across various deletion strats



## Hole Sentinel

Mark the deleted entry so you can tell if it's occupied.

#(a b c d e)
#(a b nil d e)

insert: scan for hole
iterate: check each place for sentinel
delete: add sentinel

hmm: for primitive types this means requiring more bits, then need to mask em
     out in simd.



## unsorted hole index collection

Keep a collection of the indicies of the deleted entries

#(a b c d e) + #()
#(a b c d e) #(2)

insert: pop hole index
iterate: check current index against hole collection.
delete: push index to hole collection

hmm: hole collection not fiendly to simd mask generation



## sorted hole index collection

Keep a collection of the indicies of the deleted entries

#(a b c d e) + #()
#(a b c d e) #(2)

insert: pop hole index
iterate: pop next hole index, check current index against hole.
delete: push into hole collection maintaining order.
        May require sort or order safe insert.



## hole co-collection

Seperate collection of bools to say whether is hole

#(a b c d e) + #(nil nil nil nil nil)
#(a b c d e) #(nil nil t nil nil)

insert: scan for hole
iterate: iterate both collections, check if hole from co-collection.
         set co-collection slot to nil
delete: set co-collection slot to t

hmm: more trivially simd friendly but phew a high cost.
     of course co-array can be array of masks so can be somewhat
     denser.



## Skip List

Seperate collection of distances to next element (details of which
elem holds the distance etc not discussed here but does matter)

#(a b c d e) + #(0 0 0 0 0)
#(a b c d e) #(0 0 1 0 0)

insert: scan for hole
iterate: iterate both collections, incrementing by (+ skip-dist 1)
delete: complex update of various skip fields

hmm: hmm



### Packed

Shuffle elems to fill hole

#(a b c d e)
#(a b d e)

insert: end
iterate: trivial, simd friendly
delete: would require data shuffle with simd-pack before writeback
        -or-
        would require post-process pass to fix up gaps. simd would
        return masks and those would be use as shuffle info

hmm: After first delete you need to touch all elems. Might be ok if
     need to check each row for shit. Not so great is conditional
     is based on a different column.

||#
