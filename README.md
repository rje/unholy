# Unholy

A small native Haskell 3D engine with a Unity-inspired editor, PBR materials,
Lua scripting, a typed ECS, Bullet physics, and load/save/build/play projects.
Named in honor of the original game jam. This is an experimental engine,
with six playable/inspectable projects and a small native desktop editor.

## Run on macOS

```sh
brew install ghc cabal-install pkg-config raylib bullet
cabal update
make app
open build/Unholy.app
```

Or use `make run` / `./build/unholy`. The default project is **Material Lab**.
Click **Load** to select a project, then **Play**. **More projects** pages through
the samples, including Circuit Breaker and Material Gallery.

```sh
./build/unholy projects/marble-run/project.hj.json
./build/unholy --play projects/knockdown/project.hj.json
make test
make render-test
python3 scripts/verify-authoring.py
```

Tested here with GHC 9.14.1, raylib 6.0, bundled Lua 5.4.8 and Bullet 3.25 on Apple
silicon. Cabal supplies Aeson, OpenGLRaw, linear and their dependencies. The source also has Linux
build configuration, but Linux has not been qualified.

## Editor

- **New / Load / Save**: create projects, select a sample or browse to a project
  file, and save the scene. Unsaved work gets a `.recovery` file on window close
  or exceptional exit. Haskell gameplay failures stop Play, report the error,
  and retain the editable scene so you can fix the game and play again.
- **Hierarchy**: select entities; **+ Add** creates primitives, empty entities or
  point lights. The inspector supports rename, duplicate and delete.
- **Inspector**: edit position, Euler rotation, scale, mesh and albedo texture
  paths, material color/metallic/roughness/emission, Lua scripts, Bullet box or
  sphere bodies, mass/friction/restitution, and point lights. Scroll for the
  lower components. Bodies support triggers and linear/angular damping. Set or
  clear a parent entity ID at the bottom; children retain their world pose.
  Duplicate/Delete operate on the whole selected assembly. Numeric fields support ordinary keyboard editing. Set a
  body to **kinematic** for Lua-driven moving platforms and obstacles.
- **Assets**: place `.lua`, `.obj`, `.gltf`, `.glb` or textures in a project's
  `assets/` directory. **Refresh assets** rescans it and reloads GPU assets.
  Click a `.prefab.json` asset, then click the scene to place a new assembly on
  the ground plane (Esc cancels). **Save prefab** in the inspector saves the
  selected entity and its descendants as a reusable project asset. Other asset
  types attach to the selected entity. Mesh paths are project
  relative; built-ins are `cube`, `sphere`, `plane`, `cylinder` and `torus`.
- **World / Camera**: edit the sun, ambient intensity, exposure, sky colors,
  atmospheric fog, sun shadows and game camera;
  copy the editor camera to the project, edit gravity and toggle the grid.
- **Play / Pause / Step**: Haskell game systems, Lua and Bullet run on a separate copy of the saved
  scene. Stop restores the editable scene. Step advances one 1/120 second tick.
  Toggle **Game camera / Editor camera** to inspect a running scene.
- **Build**: export the current editable scene and its assets to
  `<project>/dist/<name>/`. On macOS, double-click **Play.command**. Non-system
  dynamic libraries are bundled and their install paths relocated. Builds
  target the current OS and CPU architecture; this is not cross-compilation.
  Export copies the complete `assets/` tree plus direct scene references and
  the selected Haskell game's declared files, retaining relative paths.
  Asset symlinks and reserved export destinations are rejected. Keep multifile
  models and their dependencies beneath `assets/`; explicitly declare files
  elsewhere. A failed staging check leaves the previous export intact.

Viewport: **RMB drag** orbits, **MMB drag** pans, **wheel** zooms, **F** focuses
the selected entity. **W/E/R** select move/rotate/scale tools; drag the colored
axis or ring handles and hold **Shift** to snap (0.25 units / 15 degrees / 0.1
scale increments). Each completed drag is one undo entry. **F5** toggles play, **Cmd/Ctrl+S** saves,
**Cmd/Ctrl+Z** undoes, **Cmd/Ctrl+Shift+Z** redoes. Inspector edits are disabled
during play. Restart play after modifying a Lua source file to reload it.

## Included projects

| Project | What to do | Systems shown |
| --- | --- | --- |
| Material Lab | Inspect the two rows of spheres and animated ring | GGX metal/roughness, point lights, emissive materials, Lua animation |
| Material Gallery | Inspect six sculpted reliquaries | Static glTF submesh material slots, float factors, sRGB color textures, packed metallic/roughness source, normal maps, explicit entity overrides |
| Marble Run / Aether Gardens | Follow the blue lights, jump the gap, reach the checkpoint, cross the sliding bridge to the gold arch; ride the left lift to the bonus balcony | Textured beveled decks and suspended gardens, sun shadows/fog, kinematic platforms and rotating sweeper, parented platform assemblies, trigger checkpoints/finish, collectible gem prefabs, runtime crate spawning/destruction, collision sounds and ambience, win/restart |
| Pulse Courier | Roll through three orange relay cores and deliver to the green portal; E dashes | Independently packaged Haskell game, typed custom components and systems, trigger scoring, Haskell camera/audio, kinematic shuttles, all Haskell primitive geometry |
| Circuit Breaker | Roll onto blue pads, press E to open linked doors, reach the green portal | Persistent typed Haskell configuration, Game inspector, remapped entity links, prefab assemblies, Bullet triggers and moving doors |
| Knockdown | Ram all eight orange towers with the steel ball | Dynamic box/sphere collisions, rolling and toppling, Lua scoring, material changes, win/restart |

All games use **WASD/arrows**, **Space** to jump/hop, and **R** to restart.
The marble game's platform edges and its gap are intentional hazards.
**E** drops a physics crate in Marble Run. Collect all five gems, including the
one on the bonus balcony; finishing does not require all gems. **M** toggles sound. Marble Run includes wind, quiet ambient bells, rolling,
jump/landing effects and checkpoint/finish chimes. Pause suspends sound; Stop
ends it. The source art and generation notes are in
[projects/marble-run/ART.md](projects/marble-run/ART.md).

## Haskell games and library

`import Unholy` to author a game with statically typed state, explicit engine
resources and before/after-physics systems. Put custom components in
`Unholy.ECS.Components a`. The editor, exported player, tests and independently
built samples all use the same Cabal library. Persistent `Component a` definitions
provide explicit codecs and inspector fields; runtime state remains typed Haskell.
Game resources use an explicit scope with automatic cleanup on Stop or failure.

```sh
cabal build pulse-courier
cabal run pulse-courier -- projects/pulse-courier/project.hj.json
cabal run circuit-breaker -- projects/circuit-breaker/project.hj.json
python3 scripts/new-game.py /path/to/my-game --name my-game
```

See the [Haskell API guide](docs/haskell-api.md) and
[Pulse Courier source and instructions](examples/pulse-courier/README.md).
[Circuit Breaker](examples/circuit-breaker/README.md) demonstrates authored fields
and produces an independent starter package without editing engine source.
Haskell source changes require recompilation. Lua remains available for scripts.

## Lua

Each entity's script returns a module table. Closure variables survive between
ticks and are isolated per entity. Callbacks receive `(self, dt, input, scene)`;
`start` is optional and runs once when an entity acquires a script, including
entities spawned during play. Physics queries are available in `start`.

```lua
local elapsed = 0
return {
    start = function(self, dt, input, scene)
        print("Hello from " .. self.name)
    end,
    update = function(self, dt, input, scene)
        elapsed = elapsed + dt
        self.rotation.y = elapsed * 40
        if self.body then
            self.body.velocity.x = input.x * 4
        end
        local other = scene:get("Another entity") -- or a numeric entity ID
        if other then other.material.roughness = 0.3 end
        scene.hud = "A message\nA second line"
        -- One-shot sound, project-relative path, volume and playback pitch:
        -- scene:sound("assets/audio/jump.wav", 0.4, 1.0)
    end
}
```

Entity data: `id`, `name`, `position/rotation/scale` (`{x,y,z}`),
`material.color` (`{x,y,z}`), `material.metallic/roughness/emission`, and optional
`body.velocity` (`{x,y,z}`) / read-only `body.grounded`, `body.mass` and
`body.trigger`. `entity.alive` can be checked after destruction. Scripts can modify
transforms, materials and velocities of existing entities. `scene.by_id` and
`scene.by_name` expose live entity proxies. `scene.camera.eye/target/fov` is editable.
`input.x/z` are movement axes; `input.jump/restart/spawn` are held booleans (Space / R / E). `print`
appears in the editor console. Lua failures disable that script and report the
error; each callback has a 200,000-instruction limit to catch accidental loops.
Scripts are trusted local code with the standard Lua libraries available.
`scene:sound(path, volume, pitch)` plays a cached sound once. Call
`scene:loop(path, volume, pitch)` during updates to sustain an ambience or
rolling loop; volume zero stops it. Haskell caches at most 64 decoded clips,
keeps one loop voice and up to eight overlapping effect voices per clip, reuses
finished voices, and releases all project sounds on Stop. Pause remembers
active voices; M toggles master mute. Playback uses raylib's audio mixer.
Headless simulation validates referenced files without opening an audio device.

Prefabs and physics queries use project-relative paths and live entity proxies:

```lua
local crate = scene:spawn("assets/prefabs/physics-crate.prefab.json", {
    position={x=0,y=3,z=0}, rotation={x=0,y=45,z=0}, scale={x=1,y=1,z=1}
})
scene:teleport(crate, {x=2,y=3,z=0}) -- resets linear/angular motion
local hit = scene:raycast({x=0,y=10,z=0}, {x=0,y=-10,z=0}, crate)
if hit then print(hit.entity.name, hit.fraction) end
scene:destroy(crate) -- also removes children, scripts and physics bodies
scene.physics.gravity = {x=0,y=-18,z=0}
```

`spawn` also accepts a plain `{x,y,z}` position. Prefabs become ordinary editable
entities; they have no live link or override tracking. Each Lua VM caches loaded
prefabs until Stop. Runtime spawning is limited to 4096 active entities and 64
new script starts per tick. A destroyed proxy keeps `id` and `alive`; accessing
its engine components raises a Lua error. IDs are monotonic within a world.

Optional `on_collision(self, event, scene)` and `on_trigger(self, event, scene)`
callbacks receive `kind` (`enter`, `stay`, `exit`), `other` (a live entity or nil),
`other_id`, `point`, `normal`, `impulse` and `trigger`. The normal points toward
`self`; one event represents each touching entity pair per fixed tick. Triggers
use Bullet collision detection without physical response or grounding. Contact
callbacks run after physics; changes and newly spawned scripts are reconciled
before the next tick. Exit events may refer to a removed entity. Static triggers
need a dynamic or kinematic body to overlap, as in Bullet's normal collision
filtering. Raycasts return `entity`, `point`, `normal`, `fraction`, or nil.

## Architecture and limits

Haskell owns typed `IntMap` component stores, stable entity IDs, project JSON,
the editor, play state, undo/redo, the fixed 120 Hz simulation loop, and the 3D
renderer, scripting API, audio resource/playback management, physics body
lifetime/policy, collision-event dispatch, prefabs, parent transforms, primitive
geometry, input bindings, replay timing, text focus/edit buffers, clipping interaction
and the editor theme. Window, renderer and audio ownership is explicit; Haskell
uses no global engine singleton. Components
are **Name, Transform, Parent, Mesh, Material, Script, Light and Body**. Systems join the
tables they need. Small scalar FFI adapters call established native libraries:

- [OpenGLRaw](https://hackage.haskell.org/package/OpenGLRaw): direct graphics API
  bindings. Haskell creates/deletes GPU buffers, textures, shader programs and
  framebuffer targets, and submits the shadow, PBR, editor overlay and viewport
  composition passes. GPU shaders remain GLSL.
- [linear](https://hackage.haskell.org/package/linear): Haskell vector/matrix
  math for transforms, cameras, normal matrices and triangle picking.
- [raylib](https://github.com/raysan5/raylib): window/input, static mesh and image
  importing, audio decoding/mixing/playback, and the editor's 2D UI backend.
  Its model importer creates temporary native GPU data; Haskell copies the CPU
  geometry and supported imported material textures, then immediately releases the native
  model. All retained 3D resources and draw calls belong to the Haskell renderer.
- [raygui](https://github.com/raysan5/raygui): editor widgets, with a charcoal and
  blue theme. A pinned header and its license live in `native/vendor/`.
- [HsLua](https://hackage.haskell.org/package/hslua-core): protected Haskell
  callbacks and bundled Lua 5.4.8 for per-entity script instances. Cabal builds
  this matching interpreter; a system Lua installation is unnecessary.
- [Bullet](https://github.com/bulletphysics/bullet3): the established collision
  detector and rigid-body solver. Haskell owns the entity-to-body registry,
  create/remove/reconfigure decisions, collider scale, damping, CCD policy,
  project gravity, grounding thresholds and enter/stay/exit pair tracking.
  `native/physics.cpp` adapts Bullet objects, raw manifolds and closest ray hits.
- [Aeson](https://hackage.haskell.org/package/aeson): project JSON parsing/encoding.

`Unholy.Renderer` owns the render passes and per-window resource caches;
`Unholy.Renderer.GL` owns shader compilation, uploads, framebuffer allocation and
the shared UI context boundary; `Unholy.Renderer.Primitives` generates CPU geometry
for cube/sphere/plane/cylinder/torus and uploads it directly, without a native model.
`Unholy.Renderer.Assets` copies file importer data into Haskell-owned arrays; `Unholy.Renderer.Camera` owns camera/picking math. The renderer
runs on the window's bound OS thread and explicitly releases resources there.
It flushes pending UI batches and restores OpenGL state around its passes.
Viewport targets use actual framebuffer pixels, supporting Retina scaling.
`native/assets.c` is an importer ABI adapter and contains no scene drawing.

`Unholy.Script.API` exposes live proxies for entities and their components. Reads
look up current Haskell ECS data on demand; setters update only the requested
component. `scene:get`, `by_id`, and `pairs(scene.by_name)` keep the existing
sample syntax, and retained entity/vector references see the current world.
`Unholy.Script` owns VM lifetime, callback dispatch, logs/HUD, and rollback of ECS
and camera changes when a callback fails. Audio effects already triggered by
a failing script cannot be rolled back. Body grounding, entity IDs/names and
scene indexes are read-only; transform/material/vector fields remain editable.
Script fields on proxy tables persist as ordinary Lua data. `native/lua.c`
contains only the instruction-limit hook, whose error stays inside a protected
Lua call. Scripts remain trusted code, not a security sandbox.

`Unholy.Audio` owns decoded-clip caches, a bounded voice pool, loop playback, pause,
mute, and explicit release before the device closes. `native/audio.c` only
adapts raylib sound structs and meters the mixer on its audio thread. Headless
simulation checks audio file existence without initializing a device.

The local Cabal project relaxes only OpenGLRaw's published `base` and
`containers` upper bounds for GHC 9.14.1. The frozen versions are exercised by
the headless and GPU checks below.

PBR uses a Cook-Torrance GGX BRDF, Schlick Fresnel, Smith geometry, an
energy-conserving diffuse term, directional sun plus up to eight point lights,
ACES tone mapping and gamma conversion, an optional 2048-pixel directional
shadow map with 3x3 filtering, and distance fog. Ambient light is a simple approximation.
Static glTF/GLB imports retain per-submesh linear factors, base-color/emissive
textures, normal scale, occlusion, and metallic/roughness maps. The importer splits
the packed source's G/B channels; Haskell retains the resulting linear data maps.
The inspector's **Imported slots / Entity override** button persists that policy.
Existing scenes default to Entity override; glTF assets assigned in the editor use
Imported slots. Entity override applies one material's factors to all submeshes
and disables imported normal/data/emissive maps; a blank albedo path still uses
the imported base texture. An explicit albedo path overrides that texture.

The supported import subset is opaque, single-sided static triangles, UV0, no
vertex colors or material extensions, repeat wrapping and linear mipmap filtering.
Unsupported imported-material features fail check/Play/build with a diagnostic;
an entity override can still use their static geometry. glTF buffers and images
are included in checks and exports, including models outside `assets/`. External
URIs must be local paths without parent traversal or URL escaping. Raylib visits
all file nodes, so multi-scene selection is not supported. OBJ/IQM retain the
earlier static import path. Imported factors follow the
[glTF material specification](https://github.com/KhronosGroup/glTF/blob/main/specification/2.0/Specification.adoc#metallic-roughness-material).

This version has **no environment-map IBL, skeletal animation or terrain**.
Imported meshes are static; physics colliders are
explicit box/sphere approximations, independent of mesh geometry. Nonuniform
sphere scale uses the largest axis. Dynamic bodies must be assembly roots;
children may be visual meshes or static/kinematic colliders. Parent transforms
compose translation, quaternion rotation and scale; they do not represent shear
from rotated children under nonuniformly scaled parents. Use uniform parent
scale for such assemblies. Gizmos offer world translation/rotation and local
axis scaling. Prefab placement currently uses the y=0 plane.

The editor app launcher points at repository assets. Exported player folders
are self-contained for this macOS architecture, but not notarized distributions.
Linux export requires the native libraries installed on the destination machine.

The next targets follow these limits and the measured baseline:

1. Reduce renderer submission cost in Haskell: cache frame transforms, cull
   invisible geometry, group material state and instance repeated meshes. The
   4,000-cube baseline spends about 39 ms submitting draws on an M4; this needs
   work before larger scenes are a useful promise.
2. Add environment-map IBL to the Haskell renderer and exercise it in Material
   Gallery, with the existing sun/shadow path retained.
3. Reuse Bullet's capsule, convex and static triangle-mesh colliders for a sample
   that needs them. Collider authoring and lifetime stay in Haskell.
4. Broaden configurable input and controller support through the existing
   window backend. Add skeletal animation and terrain when a playable sample
   establishes their requirements.

These are remaining targets, not implemented features. Measurements and clean
public-package qualification are in [the current verification report](docs/haskell-verification.md).

## Verification tools

The current qualification and evidence boundaries are in
[docs/haskell-verification.md](docs/haskell-verification.md). Existing `project.hj.json` files
remain compatible; the project filename is retained.

```sh
./build/unholy --check projects/marble-run/project.hj.json
./build/unholy --simulate 360 projects/marble-run/project.hj.json
./build/unholy --build projects/marble-run/project.hj.json
./build/unholy --play projects/marble-run/project.hj.json --frames 120 --capture build/marble.png
./build/unholy --replay test/editor-smoke.rae --frames 460
```

`make test` checks camera unprojection and picking under rotated/nonuniform
transforms, ECS lifecycle, sample persistence, a falling sphere's Bullet
contacts, triggers/enter/stay/exit, raycast results, configurable gravity and
body cleanup; prefab disk/project persistence, recursive duplicate/delete,
spawned script lifecycle, transform gizmo math/snapping, collectible pickup and
crate reset; Lua/component/camera synchronization and errors, the marble course
through finish and restart using its real Lua + Bullet loop, and a physical
knockdown collision. It also checks that the marble rides the actual level's
kinematic lift and that the moving bridge and its decorations animate. The
headless tests also cover live references, instance isolation, failed-call
rollback, audio-path errors, overlapping voices, cache reuse, pause/mute and
cleanup through an injected device backend.
`make render-test` opens a real graphics context and checks GPU pixel changes
from live PBR edits, shadow depth output, UI state preservation, cached mesh
reuse, viewport resizing, picking, all sample projects, asset/shader error
handling, project-switch cleanup and renderer disposal. It also verifies
that sample sounds managed by Haskell produce nonzero native mixer output.
Captures go to
`build/verification/haskell-renderer/`. This suite requires a desktop display.
`scripts/generate-samples.py` regenerates the checked-in scene data.
`--replay` uses raylib's native automation-event format. The checked-in editor
smoke replay drives toolbar clicks, pause/step/stop, project selection, keyboard
movement and a window resize, captures screenshots and closes the window.
`scripts/verify-authoring.py` creates a disposable project under `build/`, drives
the real app through a gizmo drag and one undo/redo, saves a prefab, places its
assembly, saves the project, and checks the resulting scene files. Its captures
go to `build/verification/unholy-runtime/`.
`test/marble-runtime-smoke.rae` plays Marble Run, rolls, jumps, spawns crates
and restarts; use `--frames 270` for a bounded check.
`test/knockdown-smoke.rae` starts Play with F5 and rolls into the targets; run it
with the Knockdown project and `--frames 210` for a bounded editor/player check.

`cabal run unholy-bench -- +RTS -T` measures warm project validation and neutral
headless simulation ticks. Add `--gpu` before `+RTS` to measure warmed renderer
submission and serialized GPU elapsed queries in a desktop context. It also
reports Haskell allocation; the results are component costs, not sustained FPS.
