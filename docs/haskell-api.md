# Authoring games in Haskell

`unholy.cabal` provides an importable library. Editor, player, tests, and the
independent Pulse Courier and Circuit Breaker packages link that library. Start with
`import Unholy`; game-owned components are in `Unholy.ECS`.

A game defines statically typed state and three hooks:

```haskell
data State = State { elapsed :: Float }

myGame :: SomeGame
myGame = game Game
  { identifier = "my-game"
  , requiredAssets = []
  , components = []
  , validateScene = const (Right ())
  , initialize = \_ project -> pure (State 0, project, [])
  , beforePhysics = \_ dt _ state project ->
      pure (state {elapsed = elapsed state + deltaTime dt}, project, [])
  , afterPhysics = \events _ _ _ state project ->
      pure (state, project, [HUD ("Contacts: " ++ show (length events))])
  }

main :: IO ()
main = runApplication [myGame]
```

Set the project JSON's `haskellGame` to `"my-game"`, or construct a project with
`(emptyProject "My Game") {haskellGame = Just "my-game"}`. Older project files
without this field continue to use Lua. Each application explicitly registers
its compiled games; an unknown or duplicate identifier reports an error.
Use `saveProject`, `loadProject`, and `buildProject` for persistence and export.
The player is the current executable, with its compiled registrations.
`validateProject` checks the whole document, including camera, environment,
physics and paths; the same contract applies to Haskell hook results. For a
custom exporter, use `buildProjectWithAssets requiredFiles root shaderRoot p`
to include game-owned files outside `assets/`. The editor/CLI passes the selected
game's `requiredAssets` automatically. The legacy `buildProject` wrapper includes
scene references and the complete `assets/` tree, with no extra declarations.

Headless `--check` and `--simulate` with an explicit project path need no shader
directory. Graphical applications locate exported sibling shaders, development
assets or installed Cabal data; `--engine-assets` explicitly selects a directory.

`System state` is an ordinary Haskell function receiving explicit `GameContext`,
`Step`, `Input`, typed state, and `Project`. `GameContext` owns the project asset
root, Bullet world, optional audio engine and a game resource scope. Headless simulation supplies no
audio device. `deltaTime` is the fixed tick length; `elapsedTime` is runtime
elapsed time. `Input` names movement axes, jump, restart and action. Input is
held state; use fields in your game state to detect edges, as Pulse Courier does.

The order each tick is Haskell before-physics → Lua update/reconciliation →
Bullet → Haskell after-physics → Lua contacts/reconciliation. The same world
and physics resources serve both languages. Engine scene components remain
in typed `IntMap` stores; `setTransform` propagates parent poses. Haskell hook
results validate world invariants before being committed. Compiled game code
is trusted code, and can perform ordinary IO. Effects are not transactional.

`Unholy.ECS.Components a` keeps game-owned components statically typed.
`query world components` joins against live entity IDs and `prune` releases
removed components. Put components in your game's state; the engine does not
require a heterogeneous store or `Dynamic`. For persistent authored configuration,
define a `Component a` with a stable key, default, explicit encode/decode functions
and `Field` descriptions. Register `SomeComponent` values in the game's `components`.
The editor's Game inspector edits number, text, boolean and entity-reference fields.
`setComponent` stores configuration; `readComponents` decodes it into typed stores.
Circuit Breaker demonstrates decoding once during initialization and keeping mutable
session state separately. Unknown component keys and fields survive saves.

Project schema 2 writes authored data and reads schema 1 scenes. Entity references
remap within duplicated assemblies and instantiated prefabs; same-world duplicates
retain external links. Prefab capture rejects external links. Deleting a target
clears its reference. Incomplete links remain editable and saveable; `validateScene`
enforces game-specific requirements before Play, simulation or export. Errors identify
the entity and component or field. Component and field registrations must be unique.

Use `acquireGameResource context allocate release` for game-owned IO resources.
The shared runtime scope releases in reverse acquisition order, exactly once, before
Bullet and audio shutdown. It closes on failed initialization, editor Stop, project
switch and gameplay failure. Cleanup attempts all registered releases even if one
fails; failed initialization retains its original exception. Acquisition and
registration are masked against asynchronous cancellation, like `bracket`.

`Log`, `HUD`, `Sound`, and `Teleport` effects route through explicit resources.
List sounds and other game-owned assets in `requiredAssets`; editor/CLI check
those before play/build. Sound paths are project-relative. `gamePhysics`
permits `raycastPhysics`; teleport effects clear body motion even when the
requested pose equals the current pose.

For a custom window loop, `withEngine title shaderRoot` supplies an explicit
`Engine` on a bound OS thread. `nextFrame` returns time and typed input;
`drawFrame` draws a project, and `engineAudio` supplies the optional audio
resource. Pair `startRuntimeWith registry audio root project` with `stopRuntime`
in `finally`; use `stepGame dt input runtime project` for typed stepping.
Runtime resource ownership survives returned state changes, so stopping the
initial handle also closes VMs spawned during later ticks. Shutdown is
idempotent. The raylib backend currently permits one window/audio device at
a time, and graphics calls must remain on the owning thread.

`Unholy.Native`, renderer modules, and other advanced modules remain exposed
for tooling and tests; the `Unholy` facade is the intended game-authoring surface.
There is no stable API/ABI guarantee at version 0.1. Haskell games require a
rebuild when source changes; there is no runtime GHC compiler or plugin loader.

`python3 scripts/new-game.py /path/to/my-game --name my-game` creates a separately
built playable package with its own scene and source, pinned to the public engine
revision. Use `--engine-local /path/to/unholy` during engine development.

## Collider and controller additions

`BodyShape` includes `Capsule`, `Convex` and `TriangleMesh`. `lockRotation=True`
keeps a dynamic capsule upright. Its unit radius is 0.5, cylindrical height 1
and total height 2; capsules require uniform scale. Convex and triangle bodies
use the entity's `Mesh.geometry`, decoded without a graphics context for glTF/GLB
or generated in Haskell for built-ins. Triangle bodies must be static and
nonkinematic. Node transforms are baked once, then the entity transform applies.
`createPhysicsAt projectRoot world` supplies project-relative file lookup; the
existing `createPhysics` uses `.`. Failed replacement acquisition preserves the
old registry, and Stop releases both Bullet shapes and their backing meshes.

`FrameInput.gamepad` contains the first controller's connection state, left
stick and held buttons. `gameInput` applies a radial 0.18 dead zone, combines
keyboard/stick movement with per-axis clamping and maps A/Cross to jump,
X/Square to action and Start to restart. Text focus neutralizes both keyboard
and controller game actions. These remain held actions across fixed substeps;
games such as Threshold track their own jump/restart edges. The held-input adapter retains this default mapping. Session games can use the
frame action bindings described below; a persisted remapping editor remains future work.

## Skeletal animation

`animators :: IntMap Animator` is a typed, persisted World store. Attach
`defaultAnimator { animationClip="walk" }` to a glTF/GLB mesh entity, then update
`animationClip`, `animationTime`, `animationRate`, `animationLoop` and
`animationPlaying` from a Haskell system. Time advances once per runtime tick;
set it to zero when changing clips if a fresh phase is desired. Empty clip means
bind pose. Paused authored time is rendered in the editor. Stop restores the
saved configuration, and cloning/prefabs retain it.

`Unholy.Asset.GLTF.loadAsset` is the shared CPU asset loader. Validated `Rig` and
`RigMesh` constructors are internal; public queries expose their retained data.
`Unholy.Animation.loadRig`, `validateAnimator`, `sampleGlobals`, `posePalette` and
`posedBounds` are headless APIs. cgltf supplies copied accessors; Haskell owns
interpolation, hierarchy evaluation and palette preparation. GPU shaders perform
rendered deformation in both PBR and shadow passes. `posedPositions` and
`posedMeshes` provide on-demand picking and a CPU reference. Entity transforms
apply once after deformation; the glTF skinned mesh node transform is ignored per
specification. Rigid attachments use their full node transform. Each renderer
instance borrows immutable source geometry/textures and owns a separate palette.
Use primitive gameplay colliders for animated characters. See README for the
bounded import format and unsupported animation features.

## Finite terrain

`terrains :: IntMap Terrain` stores centered XZ height grids. Heights are row
major (`z * terrainWidth + x`), with local Y as the height. `generateTerrain`
samples a Haskell height function; `spawnTerrain` attaches the mesh, material
and static Bullet `Heightfield` body together:

```haskell
let patch = generateTerrain 65 65 0.625 (\x z -> 0.5 * sin (x * 0.3) * cos (z * 0.2))
    (ground, nextWorld) = spawnTerrain "Rolling ground" patch emptyWorld
    edited = nextWorld { terrains = IM.adjust (setHeight (32 * 65 + 32) 1) ground (terrains nextWorld) }
```

Import `Data.IntMap.Strict` qualified as `IM` for the update. `setHeight` returns
the original grid for an invalid index or height. `validTerrain` checks the
complete grid; `terrainMesh` returns validated CPU geometry. Renderer and physics
replace owned resources when grid data changes. Static heightfields preserve
entity origin under translation, rotation and scale; they cannot be dynamic or
kinematic. JSON, clones and prefabs carry the samples inline, so exports need
no separate heightmap asset. Texture paths use the existing Mesh/Material policy.
See README for patch-size limits and the deliberately small inspector controls.

## Session UI, actions and levels

Use `gameWithInterface definition frameHook levels` for an interactive game.
The `Game` record and its fixed hooks retain their roles. `FrameSystem state`
receives `GameContext`, display-frame delta time, `ActionFrame`, state and project;
it runs once before zero or more 120 Hz physics ticks. Consume jump/interact/menu
edges there and queue the gameplay work in typed state for the next fixed tick.
`actionFrame bindings dt previous current` supports game-defined `Binding` data,
held/pressed/released actions, RMB/right-stick look and identified UI activations.
Right-stick input becomes neutral on disconnect. Bindings are configurable data;
there is no persisted remapping screen yet.

Return `Present (Presentation panel prompt)` for the built-in overlay. A `Panel`
has title/body text and `(buttonKey,label)` pairs. The session maps mouse clicks
and keyboard/controller focus/confirmation into `activatedButtons` once per
frame. `PauseSimulation True` freezes physics, scripts and animation while the
frame/menu hook continues; audio pauses too. `Request ExitGame` ends the player
or stops editor Play. The debugger's Pause/Step remains an editor control.

Declare levels as unique `(key,"act-two.hj.json")` pairs. Manifests must share
the entry project's asset root, end in `.hj.json` and use the same registered
game. `Request (LoadLevel key)` preflights the target, closes the current runtime,
then initializes the next. `Request (LoadLevelWith key payload)` also passes
an optional game-owned Aeson `Value` through `GameContext.levelData`; this is
transient handoff data, independent of filesystem saves. `enteringLevel` tells
initialization whether a scene transition is in progress. Failed preflight
keeps the current runtime; failed initialization provides Retry/Exit. Stop
always restores the editor's original authored document. Build discovers every
declared level's dependencies, including models and HDR files outside assets/.

`Unholy.Session` is an abstract stable owner for custom hosts: bracket
`startSession namespace registry audio root project` with `closeSession`, feed
`advanceSession dt frameInput`, and read `sessionProject`, `sessionPresentation`,
`sessionGeneration` and `sessionQuit`. Close is idempotent. The existing low-level
Runtime stepping API remains available for fixed-tick headless applications.
The default editor and player use Session. Replays use a fixed 60 Hz display
clock, with two fixed simulation ticks per frame when unpaused.

## Checkpoints and character movement

`GameContext.saveRoot` is outside the authored/exported project. The default
session namespaces are `preview` and `player`, then the registered game ID.
`UNHOLY_SAVE_DIR` overrides the platform application-data base. Games own the
Aeson schema and version policy: `readCheckpoint path` returns missing/corrupt
results without throwing synchronous errors, and `writeCheckpoint path value`
replaces a completed sibling temporary file atomically. A failed encode/write
preserves the previous file. This is atomic replacement, not a power-loss/durable
fsync guarantee. Save logical progress and named spawn marks; recreate transient
physics/script state from authored scenes. Last Cue also retains progress in
memory when storage is unwritable.

`driveCharacter physics settings yaw jumpEdge input entity world` drives an
upright dynamic capsule, normalizes diagonal speed, reports facing and performs
bounded solid-ray step-up checks. `followCamera` computes an orbital camera with
a solid-only obstruction ray; triggers do not block it. This is a small motor
for unit capsules and simple stages, without arbitrary capsule resizing,
full swept-volume camera collision, slope sliding or navmesh navigation.

`Animation.crossfade duration clip animator` starts a two-clip local-TRS fade,
retains independent source/destination clocks and ignores repeated selection
of the destination clip. Pause freezes both clocks; completion drops the source.
An interrupted fade starts from the previous destination clip, rather than
retaining a full blended pose/graph. Skinning, posed bounds, picking and the GPU
material/shadow passes use the same blended hierarchy. The optional
`animationBlend` persists and defaults to Nothing in older scene JSON.

Source compatibility: `Game` hooks and `Input` retain their original shapes;
`GameContext`, `Gamepad` and `Animator` gain fields, changing positional
constructor arities. Prefer `defaultAnimator` and `neutralGamepad` record updates
when constructing gameplay data. This experimental version does not promise a
stable source ABI. Existing authored project JSON remains compatible.
