# Notes for agents

## Updating

This is for durable agent notes for helpful development information that can't be obtained from the code source.

## Windows environment

The restricted PowerShell runner may fail before executing a command with
`CreateProcessAsUserW failed: 1920`. This is a sandbox process-launch issue,
not a repository or PowerShell problem. Retry read-only repository commands
with the elevated shell context when necessary; the repository is at
`C:\Users\mike\Dev\yetipad-app`.

The project uses PowerShell in this environment. Prefer `rg` for searches and
`git` for repository inspection. Avoid assuming that a failed shell launch
means the workspace path is unavailable.

## Release and deployment

GitHub Pages intentionally uses two workflow runs for a tag release: the tag
push run dispatches the actual deployment run from `main`, and that second run
checks out the release tag. The first run is only a dispatcher.

The Pages build generates `docs/service-worker.js` from
`build-service-worker.cljs`. Static files matching the configured cache glob
are revisioned and precached. Do not reintroduce a catch-all `CacheFirst`
runtime route, because it can preserve stale application assets.

The GitHub Pages release is maintained in `docs/`, including its separate `docs/manifest.json`; update the release copy when changing release-specific PWA settings in the source manifest.

All test source files belong under the root `test/` directory, mirroring the
production namespace path. Do not place `*-test.cljs` files under `src/`.

## Android/PWA state

Android installed PWAs can retain a stale document URL and an older cached app
revision. Reinstalling the PWA refreshes its installed state. Active-session
persistence is independent of URL routing; URL fragments remain authoritative
for explicit document links, while a URL without a fragment restores the last
active session.
