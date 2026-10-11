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
