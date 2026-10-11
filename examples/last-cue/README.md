# Last Cue independent game

A separately compiled Haskell application using the public Unholy library.
The [checked-in project](../../projects/last-cue/README.md) contains both acts,
original assets, controls and research references.

From the engine checkout:

```sh
cabal run last-cue -- projects/last-cue/project.hj.json
cabal test last-cue-tests --test-show-details=direct
```

`LastCue.hs` registers typed stage-role fields and ordinary Haskell game state.
`gameWithInterface` adds its display-frame/menu hook and declared levels while
fixed hooks retain the gameplay and Bullet contacts. Durable checkpoints and
transient transition data use the same game-owned, versioned Aeson schema.
The test traverses both acts using actual capsule motion and moving lifts,
checks fresh-session Continue and pause, proves enemy capture/Retry, and permits
unsaved play with an unwritable storage destination.

The independent executable's Build exports its compiled registration; game code
is linked at build time. `--write-scenes ROOT` regenerates both authored manifests
and the entry scene. Reuse the assets from `projects/last-cue/assets/` when
running those generated files.
