# Changelog for notifications-tray-icon

## 0.2.0.2

- Fix polling threads silently dying on transient network errors

## 0.2.0.0

- Add Gmail notification tray icon with browser-based OAuth2 loopback flow
- Add Gitea notification tray icon with REST API client
- Refactor CLI to use subcommands (github, gmail, gitea, sample)
- Bundle SVG icons (gmail, github, gitea, notification-indicator) with IconThemePath
- Fix build compatibility with GHC 9.10 and latest dependencies
- Update CI to use cachix/install-nix-action v30

## 0.1.1.0

- Initial release
