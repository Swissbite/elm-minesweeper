# Changelog

## v1.2.0 - Resume Interrupted Games (July 2026)
- **Game Persistence**: A running game is saved to `localStorage` on every clock tick, so a reload or navigation no longer loses the board or the elapsed play time.
- **Resume Tile**: An interrupted game is offered as a full-width "Resume game" tile above the difficulty tiles on the selection view (mobile first). A restored game starts paused and continues exactly where it left off.
- **Cheat Protection**: The stored game is guarded by a salted checksum (static application salt combined with a random per-browser salt) against manual edits, and the whole payload is obfuscated with a salt-derived XOR keystream and hex encoding so mine positions cannot be read out of the browser's dev tools. Deliberately obfuscation, not cryptography - documented in SPEC.md.
- **Persistence Schema**: New versioned `runningGame` entry in `localStorage`; invalid, tampered, or foreign saves are discarded and cleaned up automatically. The save is removed when a game finishes, is given up, or a new game is created.
- **Fixes**: The finished-game history is now only written when a game actually finishes instead of on every cell click.

## v1.1.0 - Theming & Mobile Support (May 2026)
- **Theming**: Added a new dynamic dark/light theme switch.
- **Mobile Support**: Improved touch targets and responsive UI layout for smaller devices (phones and tablets). Minimum touch targets enforced to 44px for accessibility.
- **Architecture Refactoring**: Better separation of concerns (e.g., extracting `Theme` module, organizing game logic into `Internal.elm`).
- **Resilience**: Added error handling for browser features like `localStorage` to ensure graceful degradation.

## v1.0.0 - Initial Release (2023 / 2024)
- **First Playable Version**: Fully working Minesweeper web application in Elm.
- **Core Mechanics**: Marking flags, opening cells, win/loss detection.
- **Game History**: Saved game statistics in `localStorage`.
- **Difficulty Levels**: Support for multiple difficulties (Beginner, Intermediate, Expert) and custom boards.
