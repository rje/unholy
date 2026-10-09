# Aether Gardens

Marble Run's playable route now sits in a suspended limestone garden. Beveled
decks, brass hulls, cyan inlays, fluted pillars, a horseshoe finish arch and
faceted hanging islands are ordinary project assets, editable in the ECS
inspector. The left-hand lift rises to a bonus balcony with a rotating sweeper.
The final bridge slides sideways. Both use Bullet kinematic contacts; their
visible trim follows the collider in Lua.

## Recreate the assets

The generated meshes and WAVs are checked in. Blender and Python are only needed
to regenerate them, not to load/play/build the project.

```sh
/Applications/Blender.app/Contents/MacOS/Blender --background --factory-startup --python scripts/marble-art.py
python3 scripts/marble-audio.py
python3 scripts/generate-samples.py
```

`scripts/marble-art.py` uses Blender bevel and normal modifiers and exports
13 OBJ meshes to `assets/models/`. Meshes for the main decks have unit bounds
to match their explicit box colliders, with UVs sized for the stone paving.
Railings and the gameplay decks collide; shrubs, pillars and distant scenery
are decorative. Rounded deck corners retain a small box-collider approximation.

`scripts/marble-audio.py` renders nine original mono PCM WAVs to `assets/audio/`:
start, jump, landing, fall, checkpoint and finish cues, plus wind, rolling and
ambient bell loops. raylib handles decoding, pitch, mixing and device playback.
M mutes; editor Pause suspends clips and Stop ends them.

## Texture provenance and prompts

Two textures were generated with the built-in image generation tool using the
imagegen skill, then copied into this project. They are albedo-only materials;
the renderer supplies their lighting and specular response.

- `assets/textures/temple-stone.png`
- `assets/textures/porcelain-marble.png`

Stone prompt:

> Use case: stylized-concept. Asset type: a single seamless tiling albedo texture for a real-time 3D game sky temple stone race track. Generate a square 1024x1024 texture swatch, orthographic straight down, filling the entire image edge to edge. Pale warm ivory limestone paving, large rectangular hand-cut stone slabs in staggered rows, fine mineral speckle, faint fossil lines and natural subtle warm variation. Hairline recessed joints, lightly worn edges, tasteful very subtle geometric engraved motifs. Beautiful stylized physically based game material, clean readable shapes at a distance and rich small detail up close. Tileable opposite edges, flat diffuse illumination with no directional shadows, no vignette, no perspective, no objects, no text, no watermark, no border. Neutral ivory and desaturated beige palette; avoid grass/moss and large cracks. This will be tinted by the game shader.

Marble prompt:

> Use case: stylized-concept. Asset type: seamless square albedo material texture for a polished game marble sphere. Generate a square 1024x1024 seamless tiling texture swatch filling the entire image edge to edge, no perspective. Deep midnight cobalt blue glazed porcelain with broad flowing ivory-white marble swirls and hairline muted metallic-gold veins, elegant sky-temple racing ball material. Organic curving bands of white and blue of varied width, strong readable material detail, subtle fine ceramic mineral speckle. Flat diffuse illumination with no baked specular highlights or directional shadows; this texture will be mapped to a sphere and lit in the actual PBR engine. No sphere depicted, no objects, no border, no vignette, no text, no watermark. Opposite edges seamlessly tile.

## Rendering sources

The directional depth target and filtered sampling follow
[raylib's shadowmap rendering example](https://github.com/raysan5/raylib/blob/master/examples/shaders/shaders_shadowmap_rendering.c).
Moving-platform velocity comes from
[Bullet's kinematic motion-state calculation](https://github.com/bulletphysics/bullet3/blob/master/src/BulletDynamics/Dynamics/btRigidBody.cpp),
so contacts respond to platform motion.
