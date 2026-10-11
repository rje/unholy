# Last Cue assets and references

The theatre kit is original Blender-authored game art: worn oak, soot brick,
petrol painted scenery, oxblood velvet, black iron and aged brass. Construction
references come from [Alexandra Palace's preserved stage machinery](https://www.alexandrapalace.com/blog/scanning-stage-machinery/)
and the [V&A's theatrical scenery history](https://www.vam.ac.uk/articles/the-story-of-pantomime).
These are research references; no photographs, scans or third-party models are
redistributed.

## Editable sources

- [theatre-kit.blend](art/theatre-kit.blend): named modular collections for the
  deck, brick walls, scenic flat, winch, head block pulley, keys, lined curtains,
  proscenium, counterweights, lift, gallery, exit portal and worklight housing.
- [stagehand.blend](art/stagehand.blend): indigo work jacket, flat cap,
  neckerchief, tool belt and boots; a deforming torso and articulated limbs.
- [performer.blend](art/performer.blend): a separate porcelain-faced mechanical
  performer with a burgundy tailcoat and exposed brass clockwork.

Both characters retain an armature and idle, walk and jump Actions. The files
open at meter scale; the kit is arranged as a workshop for browsing. The
source files live outside assets/ so desktop builds ship only runtime art.
The checked-in glTF exports use opaque core PBR materials, UV0 textures,
triangles, linear sampled animation and four skin influences. Unholy performs
animation sampling in Haskell and vertex deformation on the GPU.

The deck has individual staggered boards and nail heads. Blender bakes its
restrained wood albedo and tangent normal map; the previous busy floor image
has been replaced. Curtains have modeled folds, lining and gold hems. The
winch has a wound rope drum, bearings, handwheel, crank, brake and anchor bolts.
The head blocks have wooden cheeks, iron straps, a sheave and a suspension eye.
Scenery flats have painted architectural panels, stretcher frames and folding
braces. The proscenium has layered cornices, fluting, dentils and a cartouche.

Geometry, rig, animations, procedural wood textures, audio and authoring code
use the repository's MIT license. Six original PCM WAVs are retained from the
first art pass. studio.hdr reuses our original procedural studio environment.

## Regeneration

Tested with Blender 5.1.1. The Python driver finds Blender on PATH or in its
standard macOS app location; BLENDER can specify another executable.

```sh
python3 scripts/last-cue-art.py
cabal run last-cue -- --write-scenes projects/last-cue
```

[scripts/blender-last-cue.py](../../scripts/blender-last-cue.py) is the complete
Blender modeling, baking, rigging and glTF export recipe. It preserves the
existing box collider dimensions when exporting normalized deck, wall, gate,
lift, gallery and portal assets. Most bevels are geometry; repeated bricks and
boards use a single bevel segment to keep the game meshes modest. To rebuild
only the characters while iterating on the rig:

```sh
/Applications/Blender.app/Contents/MacOS/Blender -b \
  --python scripts/blender-last-cue.py -- --characters-only
```

## Retained velvet texture

assets/velvet.png was generated with the built-in imagegen tool using the
imagegen skill in the first pass. It remains unchanged; Blender supplies folds
and lining. The original prompt follows.

## Curtain velvet

Use case: stylized-concept. Asset type: seamless square diffuse albedo texture for velvet stage curtains in an original 3D theatre game Last Cue. A uniform straight-on 1024x1024 material swatch of worn deep oxblood crimson theatrical velvet, subtle soft woven pile and broad irregular faded patches. Texture only: do not draw large curtain folds because the 3D mesh supplies folds. Flat neutral illumination, no directional shadows or gloss, no props, no lettering, no border, no perspective. Opposite edges tile seamlessly. Rich muted red palette with restrained variation, readable in warm stage lighting.
