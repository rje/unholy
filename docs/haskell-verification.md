# Haskell library verification — 2026-10-11

## Current qualification

The current macOS checkpoint builds the library, editor, benchmark and three
independently packaged games. `make test` passes 23 engine integration groups
and the Pulse Courier, Circuit Breaker and Threshold gameplay tests. `make render-test`
passes 21 groups in a real graphics/audio context. Editor replay checks cover
failure recovery, authored component fields/links, gizmo undo/prefabs and
imported material policy, animation configuration and finite terrain authoring.
The sections below retain the earlier checkpoints
and their measured evidence.

macOS qualification uses Apple M4, GHC 9.14.1, raylib 6.0, Bullet 3.25 and
Cabal's bundled Lua 5.4.8. Ubuntu 24.04 headless CI passes the library/editor
build, all 23 engine groups, three game suites and generated-package export at
the [terrain revision](https://github.com/rje/unholy/actions/runs/38124135219),
`f470787`. Linux desktop
graphics, editor interaction and audible output are unqualified.

## Finite terrain checkpoint — 2026-10-11

Haskell generates finite centered height grids, indexed triangles, normals,
repeating UVs and per-entity GPU geometry. Bullet supplies an owned static
heightfield with the same diagonal. The copied sample backing remains alive
until the collider is destroyed; its compound offset preserves the authored
entity origin under scale. Grid changes acquire replacement colliders before
releasing old bodies, and new renderer buffers before releasing old geometry.
Terrain is a first-class persisted component, retained by duplication/prefabs,
undo, project saves and export. The README records the bounded patch format;
chunk streaming, LOD, sculpting brushes and material layers are unsupported.

All 23 engine groups, three game suites and 21 GPU/audio groups pass. An
asymmetric four-height fixture checks rendered-triangle versus Bullet ray hits
on both sides of the diagonal under translation, yaw and nonuniform scale.
It also verifies collider replacement, failed replacement retaining the live
shape, unchanged body origin, JSON/prefab lifecycle and capsule traversal of
rolling terrain. GPU checks cover edited pixels/bounds, triangle picking and
disposal of old/new patch buffers.

Actual editor replays add Terrain, apply Hills, undo to Flat, redo, attach a
texture, save and reopen; the exported project retains the exact samples and
plays from /tmp. Attaching a static model clears incompatible terrain data and
converts its collider. Inspected captures and logs are under
`build/verification/terrain/`. Threshold crosses textured terrain before its
causeway, then completes/resets in the editor and relocated player. Visual
inspection also caught and corrected the original causeway's inward triangle
winding, removing underside/ground flicker. Center-height text entry itself
was not exercised by replay; height updates are covered through the API tests.

## Skeletal animation checkpoint — 2026-10-11

The first-class persisted Animator owns clip selection, pose time, rate, looping
and pause. Haskell advances time once per runtime tick, evaluates every node
(including non-joint parents), interpolates LINEAR/STEP tracks with shortest-path
quaternions, applies inverse binds and performs four-weight CPU skinning. cgltf
decodes accessors; raylib still supplies material-image decoding. The skinned mesh
node transform is ignored; the outer entity transform applies once. Per-entity
buffers borrow textures from an immutable source. Both render passes, bounds and
picking consume the same updated geometry. The format is bounded in README;
blending, root motion, morphs, GPU skinning and crowd performance remain outside
this checkpoint.

All 22 engine groups, three packaged game suites and 20 GPU/audio groups pass.
A transform fixture combines a nonidentity mesh node, non-joint parent and two
nonzero weights. It covers bind pose, known movement, looping/clamping, exact STEP
boundary, single-key tracks, antipodal quaternion interpolation and pause. Clone,
delete, prefabs, JSON and strict preflight retain the component. Missing-base
geometry and unsupported interpolation fail before unsafe native import. The GPU
fixture renders two independent poses, checks changed pixels/bounds/picking and
shadow depth, recovers from an invalid clip and verifies per-instance disposal.

Threshold's original rig responds with idle/walk/jump clips while its Bullet
capsule remains the collider. The real editor and relocated exported player still
complete the route; Reset/Stop retain authored scene bytes. Actual inspector
buttons persist pause/clamp while retaining authored time 0.5 after save/reopen.
Walk/jump and authoring captures were inspected in `build/verification/animation/`;
route/export captures are in `build/verification/threshold/`. A larger slope/minimum
shadow bias reduces the smooth-surface banding visible in the earlier capsule scene.

## Collider and controller checkpoint — 2026-10-11

Bullet now supports upright capsules, convex mesh hulls and static triangle mesh
colliders alongside boxes and spheres. Pinned cgltf supplies validated CPU
accessors; Haskell imports transformed triangles without a graphics context.
Physics replacement acquires all new bodies before discarding old ones, retaining
the live registry and pending teleports if a new asset fails. Native shapes own
borrowed triangle memory. Rotation locking is an authored body field.

Threshold is a separately packaged Haskell game with a capsule traveler, sloping
triangle-mesh causeway, pushable convex block, required jump, moving shuttle and
three collectible beacons. Its gameplay test proves grounded ramp traversal,
block displacement, jumping over a barrier that blocks walking, completion and
reset. The real editor replay completes, resets and stops; Stop restores the
authored scene. Controller replays complete both the local player and a relocated
export launched from /tmp. Captures were inspected; project bytes remain unchanged.
Receipts are in `build/verification/threshold/`.

The default controller policy lives in Haskell: radial 0.18 deadzone, analog
movement, south jump, west action and Start restart. Focus suppresses keyboard
and controller gameplay input. The GPU suite replays the native device adapter,
including held buttons and disconnection; no physical controller was available,
and remapping is not included. Independent rendered glTF geometry agrees with
CPU collider import under transformed parent nodes. All 21 engine groups, three
game suites and 19 GPU/audio groups pass.

## HDR environment checkpoint — 2026-10-11

The renderer owns floating-point HDR upload, 32px diffuse irradiance faces,
128px GGX prefiltered specular faces (seven levels) and a 128px split-sum BRDF
LUT. Convolution uses 256 samples. A pinned HDR-only stb decoder supplies CPU
samples; shader compilation, precomputation, texture caches and cleanup remain
in Haskell. Khronos's [reference IBL shader](https://github.com/KhronosGroup/glTF-Sample-Renderer/blob/main/source/Renderer/shaders/ibl.glsl)
provides the split-sum reference. This supports one global Radiance environment,
with cached intensity/rotation edits, consistent sky/reflection orientation and
sun/shadow coexistence. The studio HDR is original generated art.

All 17 real GPU/audio groups pass. New checks retain decoded radiance above 10,
verify constant-light irradiance/π and specular normalization on all six faces,
upper/lower orientation, roughness broadening of a bright patch, and actual IBL
lighting/rotation at a sphere interior pixel. They also cover zero intensity,
ambient fallback, short-buffer rejection, texture disposal and shared-context
2D/cubemap/seamless state restoration. The 20 engine groups and both separate
game suites pass, including headless HDR decode, corrupt/missing environment
rejection, top-level HDR export and prior-export preservation.

`python3 scripts/verify-environment.py` operates the actual app: asset attachment,
hover without clamping intensity 8/rotation 270, rotation slider, save/reopen,
asset export and identical local/relocated player pixels. The material-policy
editor replay also passes with HDR and identical exported gallery pixels.
Images were inspected. Receipts are in `build/verification/environment/` and
`build/verification/imported-materials/`; copied unit/GPU logs are in the former.
Linux graphics remains unqualified; this checkpoint proves macOS desktop behavior.

## Renderer scaling checkpoint — 2026-10-11

Haskell caches each entity's model/normal matrices and conservative world bounds,
groups opaque submeshes by VAO and their full effective material, and submits
compatible groups through a renderer-owned instance buffer. Model/normal
attributes use column-major packing. Camera-frustum culling affects the color
pass; the shadow pass retains every caster. Picking and overlays still use
retained CPU triangles and entity IDs. Pose/mesh changes refresh the cache, and
project switching clears it.

The GPU comparison renders scalar draws with culling disabled, then batched
draws with culling enabled. Mean RGBA error stays below 0.35 byte per channel;
this allows a few raster-edge changes from floating-point matrix association.
It covers rotated/nonuniform meshes, imported material slots mixed with entity
overrides, an invisible caster whose shadow affects visible pixels, edited poses,
mesh replacement, cache clearing and instance-buffer release. The actual
gizmo/undo/prefab/save replay also passes. Engine and independent game tests pass.

Matched Apple M4 runs use the same 2048 × 1536 viewport and warmed harness.
Both modes retain the new matrix cache, isolating batching/culling from that
earlier gain:

| Rendering workload | Scalar, unculled submission mean ms | Batched, culled mean ms | Color / shadow calls after batching |
| --- | ---: | ---: | ---: |
| Marble Run | 3.071 | 1.919 | 52 / 21 |
| Eight Marble Run geometry copies | 17.867 | 5.512 | 52 / 21 |
| 1,000 cubes | 4.338 | 1.490 | 1 / 0 |
| 4,000 cubes | 15.681 | 4.159 | 1 / 0 |

The expanded workload contains 2,560 submeshes, of which 2,460 are camera-visible.
It copies the actual level's resolved geometry/materials; it does not claim eight
simultaneous gameplay instances. Haskell allocation there falls from 11.0 to
8.1 MB/frame. Small scenes still incur preparation overhead, and the original
Marble Run's allocation is higher than the pre-cache baseline. This is useful
submission scaling, not an allocation-free renderer or a sustained-FPS claim.

`--scalar --unculled` on `unholy-bench --gpu` reproduces the reference mode.
Logs and editor/player captures are in `build/verification/scaling/`.

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
and Cabal's bundled Lua 5.4.8. This checkpoint did not test Linux.

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

## Static imported materials checkpoint

The 15 real GPU/audio groups now include retained glTF float factors and six
submesh slots, texture internal color-space formats, distinguishable metallic
and roughness channels, a numeric sRGB-to-linear pixel expectation, directional
normal-map pixels, mirrored UV handedness and degenerate UV fallback. They also
check explicit override save/reopen and repeated glTF/GLB texture release after
the temporary importer has been destroyed. Existing sample rendering still passes.

Headless checks detect missing imported images and unsupported imported material
features. They verify top-level glTF exports include their external buffers/images
and preserve the legacy entity override path for unsupported material features.

`python3 scripts/verify-materials.py` toggles material policy twice in the actual
editor, saves/reopens both choices, exports a model outside `assets/`, and launches
the export from `/tmp`. The local and relocated player captures match pixel for
pixel. Captures are under `build/verification/imported-materials/`. Matched
before/after captures use the same gallery, lighting and camera with the previous
renderer and current renderer: `build/verification/material-{before,after}.png`.

This qualifies opaque static UV0 materials with repeat/linear mip sampling,
without vertex colors, skinning, transparency, IBL or material extensions. The
README states the importer/node and external-URI limits.

## Earlier library and simulation checkpoint

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

## Clean public package and export

`scripts/new-game.py` generated `/tmp/unholy-public-starter` with its own Cabal
package, Haskell game source, project and assets. The default public dependency
was pinned to GitHub revision `e0407f254880f52c2a815d6870f7235552789b47`.
A fresh Cabal store at `/tmp/unholy-public-cabal-store` fetched that public
source and built the game and dependencies. Its independent test completes the
puzzle with renamed entities, real Bullet triggers/moving doors, then resets.
No local engine source package was used for this check.

The generated executable launched the actual editor and exported its project
from the generated package's directory, outside the engine checkout. The editor
and exported player both show **CIRCUIT COMPLETE** using
`test/circuit-editor-smoke.rae`. Shader discovery uses installed package data;
the export contains its sibling shaders and relocated native libraries. A second
exported-player replay from unrelated `/tmp` also reaches the completed circuit.
Captures and logs are retained in `build/verification/clean-public-package/`.

Reproduce with a new destination and store:

```sh
python3 scripts/new-game.py /tmp/my-unholy-game --name my-puzzle
cd /tmp/my-unholy-game
cabal --store-dir=/tmp/my-unholy-store build all
cabal --store-dir=/tmp/my-unholy-store test my-puzzle-tests --test-show-details=direct
cabal --store-dir=/tmp/my-unholy-store run my-puzzle -- --build project/project.hj.json
project/dist/my-puzzle/unholy-player --play project/dist/my-puzzle/project.hj.json
```

The generator pins the engine revision current at generation time. Native
dependencies still need installing for development; a fresh store means fresh
Cabal dependencies, not a machine without a compiler or native libraries.

## Linux headless CI

The [Ubuntu 24.04 workflow](https://github.com/rje/unholy/actions/runs/38124135219)
passes with GHC 9.14.1, raylib 6.0 built from its release tag, and the distribution's
Bullet package at engine revision `f470787`. It builds all library/editor/game/test
targets, runs 23 engine integration groups and all three independent game tests,
then generates a package
outside the checkout, builds/tests it, checks/exports its scene and runs the
exported player's headless check from an unrelated directory. This CI-generated
package uses the explicit local-checkout dependency; the macOS fresh-public
package check above separately qualifies the public Git revision path.

The workflow runs on pushes and pull requests. The benchmark's CPU-only path
also passes in this workflow. Graphics, real editor interaction and audible
output require separate desktop qualification; this CI run does not supply it.

## Representative performance baseline

`cabal run unholy-bench -- --gpu +RTS -T` ran on an Apple M4 with no background
compilation. Startup/import/shader compilation are excluded. Validation is
warmed before 400 measured iterations; a transform changes each time.
Simulation uses 240 warm and 240 measured 1/120-second ticks, neutral input,
and no audio device. These are idle scene workloads, not moving-player peaks.

| CPU operation | Mean ms | p95 ms | Haskell bytes/operation |
| --- | ---: | ---: | ---: |
| Validate 1,000 entities | 0.182 | 0.302 | 384,660 |
| Validate 4,000 entities | 0.903 | 0.962 | 1,536,733 |
| Marble Run tick | 0.463 | 0.536 | 423,480 |
| Pulse Courier tick | 0.026 | 0.029 | 107,558 |
| Circuit Breaker tick | 0.016 | 0.019 | 69,782 |

Graphics use the real renderer, scene camera/environment and a 2048 × 1536
framebuffer viewport. Each scene gets 20 warm and 120 measured frames.

| Scene | Submission mean / p95 ms | GPU query mean / p95 ms | Haskell bytes/frame |
| --- | ---: | ---: | ---: |
| Marble Run | 3.962 / 5.361 | 5.562 / 6.624 | 781,806 |
| Pulse Courier | 1.087 / 1.588 | 2.652 / 3.347 | 221,437 |
| Circuit Breaker | 0.607 / 0.884 | 2.166 / 2.720 | 131,613 |
| Material Gallery | 0.441 / 0.624 | 2.352 / 3.044 | 91,749 |
| 1,000 cubes | 9.284 / 13.116 | 2.530 / 3.352 | 2,471,941 |
| 4,000 cubes | 38.602 / 55.189 | 21.263 / 29.364 | 9,764,783 |

Submission brackets `drawScene`, including its shadow/viewport/composition
work. GPU queries bracket the same draws and block for the result every frame;
the elapsed interval can include waiting for CPU command submission. Allocation
includes the whole frame and benchmark harness, and excludes native/Bullet/GPU
memory. These numbers do not establish sustained frame rate or Linux performance.
The raw corrected run is `build/verification/performance/m4-baseline.log`.

Submission and per-frame allocation scale with every mesh because visibility
culling, batching and instancing are absent. The first optimization candidates
are frame transform reuse, culling and grouping material state/repeated geometry
in Haskell. This baseline does not justify replacing the ECS or Bullet solver.

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

The engine's own source is 4,524 Haskell lines, 401 native-adapter lines and
292 GLSL lines: about 86.7% Haskell by physical line count. This excludes upstream
libraries, the vendored widget header, tests, sample game code, generated scenes
and tooling. It describes engine ownership, not the implementation language of
the graphics driver, solver or decoder dependencies.

Compiled game source changes require a rebuild; no runtime Haskell plugin
loader is included. Persistent authored components use explicit typed codecs and
inspector descriptions; mutable session state stays separate. See the Haskell API guide
and sample README for these contracts. This is a local development build,
not a notarized release. Platform qualification is scoped at the top of this report.
