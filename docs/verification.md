# Unholy verification — 2026-10-10

This records the earlier authoring checkpoint. Current Haskell library/API,
renderer, generated-package and platform qualification is in
[Haskell library verification](haskell-verification.md).
The results below describe the earlier runtime-authoring pass.

Qualified locally on Apple silicon macOS with GHC 9.14.1, raylib 6.0,
Bullet 3.25 and Cabal's bundled Lua 5.4.8. This checkpoint did not test Linux.

## Automated integration

`make test`: 15 groups passed, covering the original samples and these additions:

- Prefab save/load, project persistence, parent transforms, recursive duplication
  with remapped IDs, and recursive deletion that preserves other instances.
- Trigger enter/stay/exit, ignored-entity raycasts with surface normals,
  configurable gravity, immediate body removal and idempotent close.
- Same-position teleport resets the angular motion of a genuinely tipping box.
- Dynamically spawned scripts start once; deleting an assembly removes its
  children, scripts and colliders. Retained destroyed proxies report `alive=false`.
- Gizmo handle capture/release, snapped translation/rotation and positive local
  scale. This pure math evidence is separate from the real editor check below.
- Marble Run gem pickup, E crate spawning, reset rebuilding all five gems,
  completion/restart and riding the actual moving lift using Lua + Bullet.
- Lua callback rollback, instruction limits, live references and audio policy.

`make render-test`: 13 groups passed in a real graphics/audio context. These
check live material pixels, shadow depth, all three gizmo overlays, OpenGL state
restoration, imported meshes/textures, picking, Retina viewport resizing, all
sample scenes, missing-asset/shader diagnostics and GPU resource disposal.
Haskell-managed sample audio produced nonzero output in the native mixer.
This verifies mixer output; headless tests alone do not establish audible sound.

## Actual editor and exported player

`python3 scripts/verify-authoring.py` creates a disposable project and drives
`build/Unholy.app/Contents/MacOS/Unholy` using `test/authoring-smoke.rae`.
It checks saved scene files after one gizmo drag, one undo/redo, prefab save,
placement of a new two-entity assembly, and project save. Child transforms and
parent remapping are asserted. Screenshots are in
`build/verification/unholy-runtime/authoring-{undo,redo,after}.png`.

The real Marble Run editor was played with `test/marble-runtime-smoke.rae`:
roll, jump, spawn a crate, restart, spawn another. The captured view shows the
course, a crate, five collectible gems, the new controls, and no script errors.
The headless completion check above provides the full-course evidence.

Both Marble Run and Knockdown were exported with `--build` and their bundled
`unholy-player` executables launched from inside the exported folders with
bounded replay/capture runs. The export includes prefab assets and Lua scripts.

- Editor: `build/verification/unholy-runtime/marble-editor.png`
- Marble player: `build/verification/unholy-runtime/marble-export.png`
- Knockdown player: `build/verification/unholy-runtime/knockdown-export.png`

The app has a valid local ad-hoc code signature. Exports target this OS and CPU
architecture; they are not notarized releases. Test windows close automatically.

## Ownership and current limits

The retained renderer, editor logic, ECS, project persistence, Lua API/lifetime,
audio policy, physics body registry/policy, contact dispatch, prefabs and parent
transforms are Haskell. Bullet still supplies collision detection and the rigid
body solver; raylib supplies window/input, import and audio backend operations;
raygui supplies widgets; Lua supplies its interpreter. GLSL executes on the GPU.
Line counts exclude those dependencies, generated sample data, tests and tools.
See the README for the current limits, including box/sphere colliders, uniform
parent scale for rotated assemblies, y=0 prefab placement and no prefab linking.
