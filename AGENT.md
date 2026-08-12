# Notes for agents

This is for durable agent notes for helpful development information that can't be obtained from the code source.

The GitHub Pages release is maintained in `docs/`, including its separate `docs/manifest.json`; update the release copy when changing release-specific PWA settings in the source manifest.

## Android startup persistence

Chrome Android/PWA relaunch can retain a URL such as `/?open=(:log)#<doc-id>`.
Accountant therefore enters the URL-driven navigation branch, which otherwise
bypasses `restore-active-session!!`. On the first navigation only, compare the
URL fragment with the persisted active-session document ID. If they match,
prefer the persisted open-item list because the retained URL query may be
stale; keep explicit URL state for a different-document deep link. Subsequent
navigation remains URL-driven.

The one-shot startup guard must only be consumed by a navigation callback that
contains a document fragment. Accountant may invoke the handler first for `/`;
consuming the guard there causes the retained Android URL callback to skip the
session reconciliation.

Active-session state is lifecycle-critical. It is written synchronously to
Web Storage with a localForage fallback; `visibilitychange` and `pagehide`
both trigger a final persistence attempt. The existing Log view records
`configure-navigation!`, `initial-document-navigation`, active-session reads,
and writes. The manifest version is logged as `app-version <version>`.
