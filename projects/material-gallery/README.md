# Material Gallery

Six sculpted glTF reliquaries with distinct material slots: factor-only ruby and
cobalt, patterned bronze with packed metallic/roughness, normal-mapped silver,
emissive jade and a linear factor multiplied by an sRGB texture.

Load this project in the editor and Play to inspect the authored camera. The
single imported entity preserves all six slots. Its Entity inspector switches
between Imported slots and Entity override, and saves that choice.

The included studio HDR lights reflections and diffuse surfaces. In World,
change environment intensity/rotation or clear its path to compare the ambient
fallback. Feature Courtyard adds a focused metal roughness row for this system.

These assets are original procedural work under the repository's MIT license.
`python3 scripts/material-gallery.py` regenerates the gallery, compact GPU-test
cards and external buffers/images. Regeneration requires Pillow. No network
downloads or external model licenses are required.
