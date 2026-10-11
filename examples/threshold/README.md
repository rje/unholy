# Threshold

A Haskell capsule traversal game: collect three beacons, push through a convex
block, climb a glTF triangle-mesh causeway, jump the raised threshold and reach
the cyan arch. A kinematic shuttle demonstrates a moving side route. The player
uses Bullet's upright capsule; movement, jump edges, camera, collection, sound
effects, completion and reset are Haskell systems. The starting route crosses
original textured finite terrain generated in Haskell and colliding through
Bullet's matching static heightfield.

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
its geometry, terrain samples/texture, animated rig, HDR lighting and sounds.

The independent test traverses real Bullet geometry, proves a jump is required,
checks convex displacement and upright motion, then verifies collection/reset.
Desktop replay captures cover editor completion/reset/Stop and a controller
route in a relocated export. Controller replay is qualified separately from a
physical device, which has not been tested here.

`scripts/threshold-art.py` generates the original outward-facing ramp and
moss/stone texture, and reuses the
engine samples' original studio environment and sounds. `cabal run
exe:threshold -- --write-scene projects/threshold` regenerates scene data.
The example also builds as a separate application using the public library.

The original traveler rig has idle, walk and jump clips. Haskell selects the clip
from input/grounding and owns interpolation/skinning. Its upright Bullet capsule
remains the gameplay collider. `python3 scripts/traveler-art.py` regenerates the
original glTF art and the blended hierarchy fixture.

The terrain inspector provides Flat/Hills presets, spacing and center height;
`generateTerrain` and `setHeight` expose the whole grid to Haskell games.
`python3 scripts/verify-terrain.py` checks Add/presets, undo/redo, texture
attachment, save/reopen and export in a disposable project.
