# Notes for agents

This is for durable agent notes for helpful development information that can't be obtained from the code source.

The GitHub Pages release is maintained in `docs/`, including its separate `docs/manifest.json`; update the release copy when changing release-specific PWA settings in the source manifest.

All test source files belong under the root `test/` directory, mirroring the
production namespace path. Do not place `*-test.cljs` files under `src/`.

## Android/PWA state

Android installed PWAs can retain a stale document URL and an older cached app
revision. Reinstalling the PWA refreshes its installed state. Active-session
persistence is independent of URL routing; URL fragments remain authoritative
for explicit document links, while a URL without a fragment restores the last
active session.
