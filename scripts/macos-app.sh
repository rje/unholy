#!/bin/sh
set -eu
mkdir -p build/Unholy.app/Contents/MacOS
cp build/unholy build/Unholy.app/Contents/MacOS/Unholy
cat > build/Unholy.app/Contents/Info.plist <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
<key>CFBundleExecutable</key><string>Unholy</string>
<key>CFBundleIdentifier</key><string>dev.unholy.editor</string>
<key>CFBundleName</key><string>Unholy Editor</string>
<key>CFBundleDisplayName</key><string>Unholy Editor</string>
<key>CFBundlePackageType</key><string>APPL</string>
<key>CFBundleVersion</key><string>0.1.0</string>
<key>NSHighResolutionCapable</key><true/>
</dict></plist>
PLIST
printf 'APPL????' > build/Unholy.app/Contents/PkgInfo
codesign --force --sign - build/Unholy.app
echo "Editor launcher: build/Unholy.app"
