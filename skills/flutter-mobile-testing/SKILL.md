---
name: flutter-mobile-testing
description: Use when testing a Flutter mobile app interactively — taking screenshots, tapping, typing, swiping on iOS Simulator or Android emulator to verify app behavior visually or via UI tree.
---

# Flutter Mobile Testing

Interactive mobile testing loop: observe → act → verify. Works across all providers.

## Setup

**Detect running device:**
```bash
flutter devices
```
Output uses `•` as separator: `iPhone 15 Pro • A1B2C3D4-... • ios • iOS 17.0`
Extract the second field (UDID) for iOS, device serial/name for Android.

**Auto-detect platform:**
```bash
xcrun simctl list devices booted 2>/dev/null | grep -q Booted && echo ios || echo android
```

## Testing Loop

```
screenshot → observe → identify target → act → screenshot → verify
```

Repeat until goal is achieved or assertion fails.

## Path A — Visual (image-capable providers)

```bash
# 1. Capture
xcrun simctl io booted screenshot /tmp/djinni-screen.png   # iOS
adb exec-out screencap -p > /tmp/djinni-screen.png         # Android

# 2. Observe — use Read tool on /tmp/djinni-screen.png
# 3. Identify x,y of target element visually
# 4. Act (see commands below)
# 5. Repeat
```

## Path B — Text fallback (text-only providers)

```bash
# iOS: dump accessibility tree
xcrun simctl io booted ui > /tmp/djinni-ui.json

# Android: dump UI hierarchy
adb shell uiautomator dump /sdcard/window_dump.xml
adb pull /sdcard/window_dump.xml /tmp/djinni-ui.xml
```

Parse element bounds from the tree to get `x,y` coordinates before acting.

## Interaction Commands

| Action | iOS | Android |
|--------|-----|---------|
| Tap | `xcrun simctl io booted tap <x> <y>` | `adb shell input tap <x> <y>` |
| Type | `xcrun simctl io booted sendstring "<text>"` | `adb shell input text "<text>"` |
| Swipe | `xcrun simctl io booted swipe <x1> <y1> <x2> <y2> <ms>` | `adb shell input swipe <x1> <y1> <x2> <y2> <ms>` |
| Home | `xcrun simctl io booted button home` | `adb shell input keyevent KEYCODE_HOME` |
| Back | `xcrun simctl io booted button sideButton` | `adb shell input keyevent KEYCODE_BACK` |
| Scroll down | swipe from center upward | swipe from center upward |

## Flutter Integration Tests (automated)

```bash
# Run integration tests on connected device
flutter test integration_test/ -d <device_id>

# Drive with custom test script
flutter drive --driver=test_driver/main.dart --target=integration_test/app_test.dart -d <device_id>
```

## Common Patterns

**Find element by scrolling:**
1. Screenshot → element not visible → swipe up 300px → screenshot → check

**Text field input:**
1. Tap field → `sendstring` / `input text` → screenshot to confirm

**Wait for navigation:**
After tap, take screenshot immediately — if screen unchanged, wait 1s and retry (max 3x).

**Coordinates from UI tree (Path B):**
- iOS JSON: `.children[].frame` → `{x, y, width, height}` → center = `{x + w/2, y + h/2}`
- Android XML: `bounds="[x1,y1][x2,y2]"` → center = `{(x1+x2)/2, (y1+y2)/2}`
