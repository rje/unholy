#!/usr/bin/env python3
"""Original linear Radiance studio environment; no downloaded art required."""
import math
from pathlib import Path


def write_hdr(path, width, height, sample):
    path.parent.mkdir(parents=True, exist_ok=True)
    data = bytearray(f'#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y {height} +X {width}\n'.encode())
    for y in range(height):
        row = []
        for x in range(width):
            color = sample((x + .5) / width, (y + .5) / height)
            peak = max(color)
            if peak < 1e-32:
                row.append((0, 0, 0, 0))
            else:
                mantissa, exponent = math.frexp(peak)
                scale = mantissa * 256 / peak
                row.append((*[min(255, int(c * scale)) for c in color], exponent + 128))
        data.extend((2, 2, width >> 8, width & 255))
        for channel in range(4):
            for start in range(0, width, 128):
                chunk = row[start:start + 128]
                data.append(len(chunk))
                data.extend(pixel[channel] for pixel in chunk)
    path.write_bytes(data)


def studio(u, v):
    phi = (u - .5) * 2 * math.pi
    theta = v * math.pi
    d = (math.sin(theta) * math.cos(phi), math.cos(theta), math.sin(theta) * math.sin(phi))
    base = [.018 + .018 * max(d[1], 0), .024 + .025 * max(d[1], 0), .045 + .035 * max(d[1], 0)]
    # Two softboxes and a narrow overhead strip. Values are linear radiance.
    for direction, color, size in [((-.7, .6, -.3), (12, 7, 3), 28), ((.3, .35, .85), (2, 6, 15), 36), ((0, 1, 0), (2.5, 3, 4), 65)]:
        length = math.sqrt(sum(c*c for c in direction))
        angle = sum(a*b/length for a, b in zip(d, direction))
        power = math.exp((angle - 1) * size)
        base = [a + b*power for a, b in zip(base, color)]
    return base


if __name__ == '__main__':
    root = Path(__file__).resolve().parents[1]
    write_hdr(root/'projects/material-gallery/assets/studio.hdr', 256, 128, studio)
