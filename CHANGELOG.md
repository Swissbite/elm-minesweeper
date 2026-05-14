# Changelog

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
