# Circuit Breaker

A separately built Haskell game using persistent, typed authored components.
Roll onto each blue pressure pad and press **E** to open its linked door, then
reach the green portal. **WASD** moves, **Space** jumps, **R** resets.

```sh
cabal run circuit-breaker -- projects/circuit-breaker/project.hj.json
cabal test circuit-breaker-tests --test-show-details=direct
cabal run circuit-breaker -- --build projects/circuit-breaker/project.hj.json
```

The editor's **Game** inspector adds/removes components and edits their fields.
Player speed/jump, switch targets and door speed/travel persist in the scene.
Names are display labels: renaming every entity does not change gameplay.
Duplicate a gate assembly to copy its switch, door and internal target reference.
Saving the assembly as a prefab retains that link and placing it remaps the IDs.
Deleting a target clears the reference; Play explains which switch needs a door.
Prefab capture rejects links outside the captured assembly.

`Component a` supplies an explicit codec and field descriptions. The game decodes
configuration once at initialization, then keeps ordinary typed runtime state.
The example uses real Bullet triggers and kinematic doors, with no Lua scripts.

Create your own independent package:

```sh
python3 scripts/new-game.py /path/to/my-game --name my-game
```

This copies the playable starter and scene, and pins the public engine revision
in its Cabal project. `--engine-local /path/to/unholy` uses a development checkout.
The generated package needs no engine source edits or sample paths at runtime.
