# Gud-Jerb

`gud-jerb` starts a number of threads

Where possible it locks them to a cpu with affinity

It sits in a loop calling `pump-hub`

On errors it pushes *something* onto the `error-queue` and blocks until that error is handled
