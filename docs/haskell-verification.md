# Haskell library verification — 2026-10-10

## Reliability checkpoint

The next increment passes 18 engine integration groups, the separate Pulse
Courier gameplay test, and the 14 real GPU/audio groups. New regressions cover
complete project validation, top-level scene and declared game assets in
exports, reserved destinations (including case aliases), symlink ancestors,
and retention of the previous export after staging failure.

`python3 scripts/verify-reliability.py` compiles a disposable application against
the public library. Five real-editor runs deliberately fail initialize,
before-physics, after-physics, a lazy HUD character, and paused Step. Every run
returns to editing, starts Play again, and recovers an unsaved duplicated
assembly. Captures and run logs are under `build/verification/reliability/`.
The existing real-editor gizmo/undo/prefab/save check also passes.

A copied executable validates and simulates an absolute project from an
unrelated working directory without shader discovery. An exported project with
a top-level Lua file launches its real player from that unrelated directory
using sibling shader resources.

A CPU-only flat-world validation probe changed from 7.378 ms at 1,000 entities
and 77.650 ms at 4,000 to 0.265 ms and 0.953 ms in the retained runs. This
measures validation CPU cost, not game frame rate. The change removes quadratic
list deduplication while retaining all membership checks.

Qualified on Apple silicon macOS with GHC 9.14.1, raylib 6.0, Bullet 3.25,
and Cabal's bundled Lua 5.4.8. Linux remains configured but untested.

## Library and simulation

## Persistent Haskell authoring checkpoint

The authoring increment passes 20 engine integration groups and both independently
packaged game loops. Circuit Breaker completes with every entity renamed, then
restarts the completed runtime with both doors closed and authored data intact.
Resource checks exercise LIFO, once-only shutdown across returned handles, failed
initialization, and failing cleanup that preserves the original startup error.

`python3 scripts/verify-haskell-authoring.py` drives the actual editor. It edits
door speed to 9.451, undoes/redoes the whole slider gesture, duplicates a linked
assembly, saves and reloads. The cloned switch points to the cloned door. A scene
with an unset target opens for repair and fails explicit Play/build validation.
Captures and disposable scenes are in `build/verification/typed-authoring/`.

A generated package in `/tmp/unholy-m2-independent` compiles and passes its puzzle
test against the local library without engine source edits. Circuit Breaker's
real editor and its independent exported player both reach the completed circuit.
The captures are `build/verification/circuit-editor.png` and `circuit-export.png`.
The public-revision starter path is qualified in the clean-build checkpoint.

`cabal build all --enable-tests` builds the library, editor, independent sample
executable and both test packages. The engine's Haskell modules and native
adapters compile once into the library; the sample depends on that library.

`make test` passes 17 engine integration groups plus the independently packaged
Haskell game's complete gameplay check. Existing Lua samples retain their
completion, moving-platform, collision, scoring and reset checks. Added checks:

- All five Haskell primitives have finite positions/UVs, unit normals, outward
  nondegenerate triangle winding and the existing primitive dimensions.
- Haskell input bindings, focus suppression, typed custom components, live
  entity joins and component pruning.
- Pulse Courier uses no Lua; its public-API systems move, jump and dash through
  real Bullet physics, collect three triggers, finish, reset and preserve
  editor changes. Its required assets and compiled registration are checked.
- Runtime shutdown is idempotent across returned handles; a stopped runtime
  rejects stepping before touching closed Lua states.

`make render-test` passes 14 groups in a real graphics/audio context. This
includes the existing material-pixel, shadow-depth, import, picking, Retina,
OpenGL state and resource-lifetime checks. The new text-focus check preserves
an incomplete numeric edit buffer and suppresses/restores shortcuts. Audio
produced 16,320 nonzero mixer frames with a peak of 0.13935687 in the recorded
run. This is real mixer evidence, separate from headless audio-policy checks.

## Real editor and independent exported game

The app loads Pulse Courier through the normal project picker and plays it
using `test/pulse-editor-smoke.rae`. The captured real editor shows
**DELIVERY COMPLETE**, all relays charged, 49 remaining entities, 7 bodies and
no script errors. The game console identifies compiled Haskell systems.

`cabal run pulse-courier -- --build projects/pulse-courier/project.hj.json`
exports the independently built game executable, its assets, engine shaders
and relocated native libraries. Running the exported `unholy-player` from
its export directory with `test/pulse-player-smoke.rae` also completes the
three-relay delivery. Source Haskell files or a GHC installation are not
required to run the export.

Captures:

- `build/verification/haskell-library/pulse-editor.png`
- `build/verification/haskell-library/pulse-export.png`
- `build/verification/haskell-library/marble-editor.png`

`python3 scripts/verify-authoring.py` still passes the real editor's gizmo drag,
one undo/redo, parent propagation, prefab save/place and project save checks.
It uses a disposable fixture; the user's Marble Run recovery is preserved.
A fresh Marble Run editor replay also rolls, jumps, spawns a crate, resets and
spawns again; the captured live scene has its expected 179 entities / 27 bodies
and no script errors.

## Ownership and limits

Haskell now owns primitive generation, keyboard bindings, replay sequencing,
text focus and edit-buffer state, clipping interaction and the Unity-style
theme. Window/renderer/audio resources are explicit values; there are no
Haskell global engine singletons. The native UI adapter supplies scalar window,
font, event and raygui widget operations. Widget internals remain upstream
raygui. The platform mouse-edge hook stays Objective-C.

Bullet's solver/collision detector, file decoders, audio mixer, Lua interpreter
and instruction-budget hook remain established native integrations. GLSL still
runs GPU shading. File imports can create temporary native GPU data; built-in
primitives now avoid that path entirely.

The engine's own source is 3,244 Haskell lines, 271 native-adapter lines and
124 GLSL lines: about 89% Haskell by physical line count. This excludes upstream
libraries, the vendored widget header, tests, sample game code, generated scenes
and tooling. It describes engine ownership, not the implementation language of
the graphics driver, solver or decoder dependencies.

Compiled game source changes require a rebuild; no runtime Haskell plugin
loader is included. Persistent authored components use explicit typed codecs and
inspector descriptions; mutable session state stays separate. See the Haskell API guide
and sample README for these contracts. This is a local development build,
not a notarized release or cross-platform qualification.
