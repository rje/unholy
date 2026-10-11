# Haskell library verification — 2026-10-10

Qualified on Apple silicon macOS with GHC 9.14.1, raylib 6.0, Bullet 3.25,
and Cabal's bundled Lua 5.4.8. Linux remains configured but untested.

## Library and simulation

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
loader is included. Custom Haskell components are typed runtime state rather
than auto-generated inspector/serialization fields. See the Haskell API guide
and sample README for these contracts. This is a local development build,
not a notarized release or cross-platform qualification.
