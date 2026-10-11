# Feature Courtyard

A focused, inspectable showcase of recent engine features. Load it from the
editor's Load picker (use More projects), then Play. Toggle Editor camera to
orbit/pan/zoom during Play, or Stop to edit the scene.

- The four plinths show idle, two independently phased walk animations sharing
  the original traveler asset, and a paused jump pose. Select a traveler and
  scroll to Animator to change clip, time, rate, loop or pause.
- The five gold spheres vary roughness from 0.06 to 0.9 under the same HDR
  environment. Change environment intensity/rotation in the World inspector.
- The 128 teal columns share one cube mesh/material while varying scale and
  rotation. They exercise the renderer's instancing/culling path; this scene is
  a visual showcase, without an FPS claim.
- The surrounding hills are an editable 65 x 65 terrain patch with a matching
  static Bullet heightfield. Select Terrain and scroll to its presets, spacing
  and center height controls. Threshold supplies playable terrain traversal.
- The gold sculpture rotates through Lua while Haskell advances skeletal poses.

Build exports this scene and all its assets. Try the standalone player with:

```sh
./build/unholy --play projects/feature-courtyard/project.hj.json
```

`python3 scripts/feature-courtyard.py` regenerates the scene and copies its
original traveler, moss texture and studio HDR from Threshold. Assets use the
repository's MIT license and are included locally; no downloads are needed.
