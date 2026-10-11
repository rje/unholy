# Feature demos

New user-visible engine features must ship with a checked-in demo under
`projects/`. Extend a suitable existing project or add a focused project. A test
fixture under `test/` or a disposable scene under `build/` does not replace a demo.

- Make the feature visible through the normal editor Load/Play workflow and
  include the assets needed by a relocated Build export.
- Keep playable games for gameplay systems; focused inspectable scenes are
  appropriate for rendering and authoring features.
- Explain what to try in the demo's README and update the root README's demo map.
  Use original assets or record their source and license.
- Register new demos in `test/Samples.hs`. The shared headless and graphics
  checks must load every included demo, validate assets/JSON and draw it.
- Exercise the changed feature in the real editor/player and exported project
  proportionately. Preserve user projects and recovery files; use `build/` for
  disposable verification.

Keep engine policy and systems in Haskell and reuse established native libraries
for their existing roles. Do not create a new engine subsystem just for a demo.
