# HJ

A small native Haskell 3D engine with a Unity-inspired editor, PBR materials,
Lua scripting, a typed ECS, Bullet physics, and load/save/build/play projects.

## Run on macOS

```sh
brew install ghc cabal-install pkg-config raylib lua bullet
cabal update
make app
open build/HJ.app
```

Or use `make run` / `./build/hj`. The default project is **Material Lab**.
Click **Load** to switch to **Marble Run** or **Knockdown**, then **Play**.

```sh
./build/hj projects/marble-run/project.hj.json
./build/hj --play projects/knockdown/project.hj.json
make test
```

Tested here with GHC 9.14.1, raylib 6.0, Lua 5.5.1 and Bullet 3.25 on Apple
silicon. Cabal supplies Aeson and its dependencies. The source also has Linux
build configuration, but Linux has not been qualified.

## Editor

- **New / Load / Save**: create projects, select a sample or browse to a project
  file, and save the scene. Unsaved work gets a `.recovery` file on window close.
- **Hierarchy**: select entities; **+ Add** creates primitives, empty entities or
  point lights. The inspector supports rename, duplicate and delete.
- **Inspector**: edit position, Euler rotation, scale, mesh and albedo texture
  paths, material color/metallic/roughness/emission, Lua scripts, Bullet box or
  sphere bodies, mass/friction/restitution, and point lights. Scroll for the
  lower components. Numeric fields support ordinary keyboard editing.
- **Assets**: place `.lua`, `.obj`, `.gltf`, `.glb` or textures in a project's
  `assets/` directory. **Refresh assets** rescans it and reloads GPU assets.
  Click an asset to attach it to the selected entity. Mesh paths are project
  relative; built-ins are `cube`, `sphere`, `plane`, `cylinder` and `torus`.
- **World / Camera**: edit the sun, ambient intensity, exposure and game camera;
  copy the editor camera to the project and toggle the grid.
- **Play / Pause / Step**: Lua and Bullet run on a separate copy of the saved
  scene. Stop restores the editable scene. Step advances one 1/120 second tick.
  Toggle **Game camera / Editor camera** to inspect a running scene.
- **Build**: export the current editable scene and its assets to
  `<project>/dist/<name>/`. On macOS, double-click **Play.command**. Non-system
  dynamic libraries are bundled and their install paths relocated. Builds
  target the current OS and CPU architecture; this is not cross-compilation.

Viewport: **RMB drag** orbits, **MMB drag** pans, **wheel** zooms, **F** focuses
the selected entity. **F5** toggles play, **Cmd/Ctrl+S** saves,
**Cmd/Ctrl+Z** undoes, **Cmd/Ctrl+Shift+Z** redoes. Inspector edits are disabled
during play. Restart play after modifying a Lua source file to reload it.

## Included projects

| Project | What to do | Systems shown |
| --- | --- | --- |
| Material Lab | Inspect the two rows of spheres and animated ring | GGX metal/roughness, point lights, emissive materials, Lua animation |
| Marble Run | Roll along the blue path, jump the gap, reach the green checkpoint, turn right to the gold finish | Bullet sphere contacts, jump/grounding, Lua movement and checkpoint/respawn, live camera, timer, win/restart |
| Knockdown | Ram all eight orange towers with the steel ball | Dynamic box/sphere collisions, rolling and toppling, Lua scoring, material changes, win/restart |

Both games use **WASD/arrows**, **Space** to jump/hop, and **R** to restart.
The marble game's platform edges and its gap are intentional hazards.

## Lua

Each entity's script returns a module table. Closure variables survive between
ticks and are isolated per entity. Callbacks receive `(self, dt, input, scene)`;
`start` is optional and runs once before physics initialization.

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
    end
}
```

Entity data: `id`, `name`, `position/rotation/scale` (`{x,y,z}`),
`material.color` (`{x,y,z}`), `material.metallic/roughness/emission`, and optional
`body.velocity` (`{x,y,z}`) / read-only `body.grounded`. Scripts can modify
transforms, materials and velocities of existing entities. `scene.by_id` and
`scene.by_name` expose the snapshot. `scene.camera.eye/target/fov` is editable.
`input.x/z` are movement axes; `input.jump/restart` are held booleans. `print`
appears in the editor console. Lua failures disable that script and report the
error; each callback has a 200,000-instruction limit to catch accidental loops.
Scripts are trusted local code with the standard Lua libraries available.

## Architecture and limits

Haskell owns typed `IntMap` component stores, stable entity IDs, project JSON,
the editor, play state, undo/redo and the fixed 120 Hz simulation loop. Components
are **Name, Transform, Mesh, Material, Script, Light and Body**. Systems join the
tables they need. Small scalar FFI adapters call established native libraries:

- [raylib](https://github.com/raysan5/raylib): window/input, static mesh loading,
  GPU resources and OpenGL 3.3 drawing.
- [raygui](https://github.com/raysan5/raygui): editor widgets, with a charcoal and
  blue theme. A pinned header and its license live in `native/vendor/`.
- [Lua](https://www.lua.org/manual/5.5/): embedded per-entity script instances.
- [Bullet](https://github.com/bulletphysics/bullet3): rigid bodies, collision
  detection, gravity, friction, restitution and continuous collision settings.
- [Aeson](https://hackage.haskell.org/package/aeson): project JSON parsing/encoding.

PBR uses a Cook-Torrance GGX BRDF, Schlick Fresnel, Smith geometry, an
energy-conserving diffuse term, directional sun plus up to eight point lights,
ACES tone mapping and gamma conversion. Ambient light is a simple approximation.
This first version has **no shadows, environment-map IBL, normal maps, packed
metal/roughness textures, skeletal animation, audio, parenting, terrain or
runtime entity spawning**. Imported meshes are static; physics colliders are
explicit box/sphere approximations, independent of mesh geometry. Nonuniform
sphere scale uses the largest axis. The editor has component fields and camera
navigation; the drawn axes are visual guides, not draggable transform gizmos.

The editor app launcher points at repository assets. Exported player folders
are self-contained for this macOS architecture, but not notarized distributions.
Linux export requires the native libraries installed on the destination machine.

## Verification tools

```sh
./build/hj --check projects/marble-run/project.hj.json
./build/hj --simulate 360 projects/marble-run/project.hj.json
./build/hj --build projects/marble-run/project.hj.json
./build/hj --play projects/marble-run/project.hj.json --frames 120 --capture build/marble.png
./build/hj --replay test/editor-smoke.rae --frames 460
```

`make test` checks ECS lifecycle, sample persistence, a falling sphere's Bullet
contacts, Lua/component/camera synchronization and errors, the marble course
through finish and restart using its real Lua + Bullet loop, and a physical
knockdown collision. The headless tests complement native visual/UI checks.
`scripts/generate-samples.py` regenerates the checked-in scene data.
`--replay` uses raylib's native automation-event format. The checked-in editor
smoke replay drives toolbar clicks, pause/step/stop, project selection, keyboard
movement and a window resize, captures screenshots and closes the window.
