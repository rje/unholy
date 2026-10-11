# Demo projects

Every new visible engine feature ships in a checked-in demo here. Extend an
existing demo when it fits; add a focused project when the feature needs clearer
presentation. Tests and generated scenes under `build/` supplement these demos.

Run `make app`, click Load, choose a project (More projects pages the list),
then Play. Stop to edit, Save to retain changes, and Build for a standalone
export. Each project contains its own required assets.

| Feature | Start here | What to try |
| --- | --- | --- |
| Instancing/culling | [Feature Courtyard](feature-courtyard/README.md) | Orbit around 128 columns with shared mesh/material and varied transforms |
| Skeletal animation | [Feature Courtyard](feature-courtyard/README.md), [Threshold](../examples/threshold/README.md) | Compare independent walk phases and a paused pose; select a clip or move/jump in Threshold |
| Finite terrain | [Feature Courtyard](feature-courtyard/README.md), [Threshold](../examples/threshold/README.md) | Edit Hills/Flat, spacing and center height; walk across the heightfield in Threshold |
| Imported PBR materials and HDR lighting | [Material Gallery](material-gallery/README.md), [Feature Courtyard](feature-courtyard/README.md) | Compare material slots/normal maps and roughness; rotate the HDR environment |
| Capsules, convex and triangle collision; controllers | [Threshold](../examples/threshold/README.md) | Push the block, climb the imported ramp and jump the threshold with keyboard or controller |
| Typed Haskell systems and authored fields | [Pulse Courier](../examples/pulse-courier/README.md), [Circuit Breaker](../examples/circuit-breaker/README.md) | Complete the games, edit Game fields and linked doors; inspect their independent packages |
| Lua, moving platforms, prefabs and audio | [Marble Run](marble-run/ART.md) | Ride the lift, collect gems, spawn crates, finish and reset |
| Dynamic rigid bodies | [Knockdown](knockdown/project.hj.json) | Ram the orange towers and watch real collisions topple them |
| PBR parameter editing and point lights | [Material Lab](material-lab/project.hj.json) | Compare metal/dielectric rows and edit material/light values |

`test/Samples.hs` lists all included demos for both asset/JSON and real graphics
checks. `make test` and `make render-test` exercise that list. New demos must
update it and this map. `python3 scripts/verify-demos.py` qualifies Feature
Courtyard's real Load/Play/Stop/Build workflow and relocated player. Gameplay
replays and feature-specific authoring checks are documented in each demo's
README and [the verification report](../docs/haskell-verification.md).
