#||

In the latest update I rabbited on about the concept of a view.
This would be a way of saying:

> "hey the table X is just the columns A & B from Tables X, Y & Z"

This means that there are less cases where data is bound by lifetime across
tables. This seems like it maps better to that hot-cold example we saw back
in the early Jai examples too.

We could do shit like:

(deftable asteroids ()
  (hot pos-rot-vel)
  (metadata flags))

(deftable players ()
  (hot pos-rot-vel)
  (mdata flags)) ;; only giving it this name to show how it is used in view

(defview metadata ()
  (flags
   (asteroids metadata)
   (players mdata)))

And then make queries over the metadata view as if it was a table.

What other cases require a tight binding of data lifetime?


||#

(defrecord creature-stats ()
  (stat-0 i32)
  (stat-1 i32)
  (stat-2 i32)
  (stat-3 i32))

(deftable board-assets ()
  (pos vec3)
  (rot f32))

(deftable creature ()
  (base-asset-loader ..)
  (other-asset-loader ..)
  (health i32)
  (stats creature-stats))

(deftable tile ()
  (asset-loaders ..) ;; unknown number of asset loaders per tile
  )

(deftable asset-loaders ()
  (guid string) ;; eek
  (state ...) ;; table per asset kind? which kinds?
  ())
