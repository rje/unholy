# Authoring games in Haskell

`unholy.cabal` provides an importable library. Editor, player, tests, and the
independent `examples/pulse-courier` package link that library. Start with
`import Unholy`; game-owned components are in `Unholy.ECS`.

A game defines statically typed state and three hooks:

```haskell
data State = State { elapsed :: Float }

myGame :: SomeGame
myGame = game Game
  { identifier = "my-game"
  , requiredAssets = []
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

`System state` is an ordinary Haskell function receiving explicit `GameContext`,
`Step`, `Input`, typed state, and `Project`. `GameContext` owns the project asset
root, Bullet world and optional audio engine. Headless simulation supplies no
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
require a heterogeneous store or `Dynamic`. These components are runtime-only;
provide your own persistence if needed. Built-in World components serialize in
project JSON and appear in the editor.

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
