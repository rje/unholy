# Astra review of Unholy — 2026-10-11

Reviewed engine revision: `49bd449decbb4d66f5825ee57bcfc317fcd8084d`.
This is a read-only GPT-6 Astra review of the engine's Haskell architecture,
public interfaces, import/animation path and resource ownership. The findings
below are a synthesis of that review, checked against the source. Evidence is
source inspection plus a focused audio-backend failure probe; this review did
not measure frame times or change engine code.

## Verdict

The engine is Haskell-forward and broadly idiomatic in its architecture, with
less discipline in implementation readability and public boundaries. Its
foundations are worth retaining:

- Immutable `World` transformations and typed component stores.
- Statically typed game state, with existential packaging confined to the
  registration boundary rather than a pervasive `Dynamic` store.
- Explicit authored-component codecs and shared scene validation.
- Pure transform and animation evaluation, separated from native effects.
- Explicit native resources, a game-resource scope and transactional Bullet
  replacement when colliders change.
- Reuse of established decoders, physics and scripting libraries.

OpenGL state, caches and native handles in `IO` are reasonable Haskell. The next
improvements are stronger ownership and import contracts, clearer code and GPU
vertex deformation. They do not require a new ECS framework or an effect-system
rewrite.

## Prioritized findings

### 1. Audio ownership after backend failure — reproduced correctness bug

[Audio.hs, lines 51–78](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Audio.hs#L51)
allocates a voice alias, invokes volume/pitch/playback/pause callbacks, and only
then publishes the alias in the engine registry. If a callback throws, the
engine no longer knows that it owns the alias.

Astra reproduced this in GHCi against the built library with an injectable
`Backend`: loading returned source handle 1, alias allocation returned handle 2,
and `volumeVoice` threw an `IOException`. `playOn` failed; after `closeEngine`
the unload log contained only `[(1,False)]`. Alias 2 was never released. This is
a failure-path bug, not evidence of leaking during successful playback.

Fix acquisition/publication under masking and publish ownership before fallible
configuration, or use `bracketOnError` until registry publication. Cleanup should
detach the registry before callbacks and attempt every release even when one
fails. Add focused failure-injection checks for those ownership transitions.

### 2. Two glTF geometry interpretations — next-feature prerequisite

[Animation.hs, lines 116–129](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Animation.hs#L116)
restricts geometry layouts to protect raylib's second import. The animation
parser also requires a skin on every mesh node, rejects additional attributes
such as tangents, and accepts only float skin weights. These are documented
limitations, rather than newly demonstrated crashes.

[Renderer/Assets.hs, line 48](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Renderer/Assets.hs#L48)
starts the native model import before material metadata validation.
[native/assets.c, line 47](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/native/assets.c#L47)
recovers imported material images through temporary GPU textures and readback.
Haskell then pairs separately obtained rig primitives and model materials by
their ordering.

Extend the existing cgltf-backed CPU asset boundary into one authoritative,
validated geometry/node/material representation. Decode images separately.
Retain cgltf and the existing image decoders; do not write another format parser
or broaden guards while preserving two independent geometry interpretations.
Use an actual conventional exported asset to choose the needed import support.

### 3. Animation allocation and repeated evaluation — source-backed scaling cost

[Animation.hs, line 214](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Animation.hs#L214)
creates full deformed vertex arrays, including small lists/vectors per vertex.
[Renderer.hs, line 220](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Renderer.hs#L220)
uploads them and reconstructs CPU positions/bounds whenever a pose changes.
Adding shader deformation while retaining this work would preserve much of the
CPU/allocation cost.

[Animation.hs, lines 187–200](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Animation.hs#L187)
walks each node's ancestors independently, recalculating shared transforms.
Validate and retain a parent-before-child evaluation order, then compute globals
once into a vector. Ordinary rendering should upload joint palettes and reuse
immutable geometry, without calling `posedMeshes` for every animated vertex.
These costs are apparent in the source; their current frame-time contribution
was not measured in this review.

### 4. Validated assumptions escape public boundaries — API improvement

[Animation.hs, line 3](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Animation.hs#L3)
exports rig constructors, but evaluation assumes valid indices, matching arrays
and nonempty keyframes. Loader-created values satisfy many of those conditions;
unchecked indexing alone does not demonstrate a bug in the current demos.
Renderer modules also expose raw structures that carry GPU ownership.

Make retained `Rig` and GPU ownership types abstract, expose useful read-only
queries, and keep constructors in internal modules. Keep ordinary scene records
convenient and continue validating their invariants at shared boundaries. A
wholesale smart-constructor conversion would add unnecessary authoring friction.

### 5. Compressed expressions obscure policies — maintainability improvement

[Types.hs, line 84](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Types.hs#L84),
[Project.hs, line 94](https://github.com/rje/unholy/blob/49bd449decbb4d66f5825ee57bcfc317fcd8084d/src/Unholy/Project.hs#L94)
and the editor inspector put substantial policies into dense expressions.
Component additions, invariant auditing and ownership review become harder than
the underlying design requires.

Use conventional formatting and named operations as those sections change.
Separate import decoding, pure pose evaluation and GPU preparation at their
existing responsibility boundaries. This is a readability/cohesion improvement,
not a reason to add more typeclasses or rewrite the engine.

## Next milestone: conventional import and GPU deformation

The user's next priority is mesh import and animation, with rendered deformation
on the GPU. This section is an implementation target; the current renderer still
uses CPU skinning.

1. **One validated CPU asset description.** Preserve stable primitive/material
   identities, the selected scene, full joint ancestry, rigid versus skinned
   primitives, bind vertices, indices, joints and normalized weights. Respect
   the distinct transform rules for skinned meshes and rigid joint attachments.
   The [glTF skin specification](https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#skins)
   defines joint ordering, inverse-bind matrices and these transform rules.
2. **Haskell pose evaluation and ownership.** Haskell samples clips, interpolates
   rotations, evaluates the hierarchy, applies inverse binds, owns retained
   resources and schedules uploads. Upload immutable geometry once; update
   per-character joint palettes as poses change.
3. **GPU material and shadow deformation.** Both vertex passes use the same
   skinning convention. Check palette capacity against the device, and reserve
   vertex attribute locations deliberately: instance matrices currently use
   locations 3–10. Normal deformation must remain consistent with the reference.
4. **Correct bounds and picking.** Use conservative posed bounds from per-joint
   influence bounds. Retain CPU deformation as a test reference and for exact
   picking on demand, rather than unconditional per-frame vertex work.
5. **A visible checked-in demo.** Include a licensed conventional asset, its
   provenance, independent character poses and a rigid accessory. Extend a
   suitable existing project or add a focused animation project.

Qualification must compare CPU/GPU poses and shadow silhouettes, check culling
and picking at extended poses, exercise clip selection/pause/time edits and
reload/Stop/restart, and run a relocated Build export. Record animated-scene CPU
time, allocation and upload volume before claiming a performance improvement.

Address the reproduced audio ownership bug in a small separate change. Then
consolidate the import representation and implement GPU deformation with the
target asset and demo. Formatting and boundary improvements should accompany
the touched modules rather than become an engine-wide detour.
