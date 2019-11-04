# keep

This is a sketch of a library for resumable computations, which uses redis as a persistent store for thunk evaluations.

For now you'll need to scrub the store of closures whenever you switch executables in any meaningful capacity, but the Merkle store for "authenticated" structures should remain sound regardless.

Eventually I should supply command line tools to make this maintenance process somewhat automatic.

Heck, it probably doesn't even compile right now. Just stashing this for later exploration.
