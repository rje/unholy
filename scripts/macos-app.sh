#!/bin/sh
set -eu
mkdir -p build/HJ.app/Contents/MacOS
cp build/hj build/HJ.app/Contents/MacOS/HJ
cat > build/HJ.app/Contents/Info.plist <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleExecutable</key><string>HJ</string>
<key>CFBundleIdentifier</key><string>dev.hj.editor</string>
<key>CFBundleName</key><string>HJ Editor</string>
<key>CFBundleDisplayName</key><string>HJ Editor</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>CFBundleVersion</key><string>0.1.0</string>
<key>NSHighResolutionCapable</key><true/>
</dict></plist>
PLIST
printf 'APPL????' > build/HJ.app/Contents/PkgInfo
codesign --force --sign - build/HJ.app
echo "Editor launcher: build/HJ.app"
