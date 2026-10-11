# Threshold

A Haskell capsule traversal game: collect three beacons, push through a convex
block, climb a glTF triangle-mesh causeway, jump the raised threshold and reach
the cyan arch. A kinematic shuttle demonstrates a moving side route. The player
uses Bullet's upright capsule; movement, jump edges, camera, collection, sound
effects, completion and reset are Haskell systems.

From the repository root:

```sh
make app
./build/unholy projects/threshold/project.hj.json
cabal test threshold-tests --test-show-details=direct
python3 scripts/verify-threshold.py
```

Play in the editor with WASD/arrows and Space, or a controller's left stick and
south face button (A/Cross). R/Start resets. Stop retains the authored scene.
The Game inspector edits walk/jump speed and shuttle travel/period. Components
identify roles, so entity names can change. Build exports the selected game and
its geometry, HDR lighting and sounds.

The independent test traverses real Bullet geometry, proves a jump is required,
checks convex displacement and upright motion, then verifies collection/reset.
Desktop replay captures cover editor completion/reset/Stop and a controller
route in a relocated export. Controller replay is qualified separately from a
physical device, which has not been tested here.

`scripts/threshold-art.py` generates the original ramp asset and reuses the
engine samples' original studio environment and sounds. `cabal run
exe:threshold -- --write-scene projects/threshold` regenerates scene data.
The example also builds as a separate application using the public library.
