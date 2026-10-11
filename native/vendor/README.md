raygui.h is vendored from raysan5/raygui at commit
9cb5cfa73290ba920d071f57c1d06a4daee22f52 (raygui 5.1 development version).
Source: https://github.com/raysan5/raygui/tree/9cb5cfa73290ba920d071f57c1d06a4daee22f52

The upstream zlib license is preserved in the header. This copy is unmodified.
raylib and Bullet are linked as system libraries; they are not vendored here.
Cabal builds the matching Lua 5.4.8 bundled with HsLua's lua package; a system
Lua installation is not linked.

`stb_image.h` is pinned to nothings/stb commit
`2c980bb59875b0d32144a71867fbdebb2f77cd20` (stb_image v2.30),
from <https://github.com/nothings/stb>. Its MIT/public-domain license is included
at the end of the header. Only HDR decoding is compiled, with private static
symbols; Haskell copies linear float samples and owns all retained GPU data.
This avoids depending on raylib's optional `SUPPORT_FILEFORMAT_HDR` switch.
