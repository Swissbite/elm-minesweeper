# Elm Minesweeper Workspace Instructions

## Quick Start

**Language**: Elm 0.19.1 (functional programming)  
**Framework**: elm-ui (UI library), Parcel 2.16.0 (bundler)  
**Build tool**: npm  
**Node Version**: v20 (defined in `.nvmrc`)  

### Initial Setup

1. Install [nvm](https://github.com/nvm-sh/nvm) (macOS/Linux) or [nvm-windows](https://github.com/coreybutler/nvm-windows) (Windows)
2. Run `nvm use` to activate Node v20 from `.nvmrc`
3. Run `npm install` (postinstall hook runs `elm-tooling install` automatically)

### Essential Commands

| Command | Purpose |
|---------|---------|
| `nvm use` | Activate Node v20 from `.nvmrc` (run first!) |
| `npm run start` | Start local dev server with hot reload on `localhost:1234` |
| `npm run test` | Run elm-test suite |
| `npm run format` | Auto-format all Elm files |
| `npm run format-validate` | Validate formatting without changes |
| `npm run build` | Build for production to `build/` directory |

**Auto-install hook**: `postinstall` runs `elm-tooling install` to manage Elm dependencies.

---

## Architecture & Code Organization

The project follows **Elm best practices** from the [Structuring Web Apps](https://guide.elm-lang.org/webapps/structure.html) pattern. Three conceptual layers:

### Strict Module Separation

1. **[Types.elm](../../src/Types.elm)** — Type-only module
   - All types and type aliases live here
   - **NEVER** includes functions, helpers, or transformations
   - Other modules depend on it; it depends on nothing else
   - Examples: `Msg`, `GameMsg`, `Model`, `Flags`, `Coordinate`

2. **[Styles.elm](../../src/Styles.elm)** — Style utilities (reusable UI components)
   - Color definitions, icons, predefined elements (buttons, toggles, etc.)
   - **ZERO dependencies** on Types.elm or model code
   - Provides pure styling functions
   - Example: button styles, color palette

3. **[Main.elm](../../src/Main.elm)** — Application coordinator
   - Entry point (`main : Program Flags Model Msg`)
   - URL routing via `Url.Parser`
   - Delegates game logic to `Game.Game` and history to `Game.History`
   - Handles screen size subscriptions

### Feature Modules

- **[Game/Game.elm](../../src/Game/Game.elm)** — Core minesweeper game logic
  - Exposes: `initModel`, `update`, `view`, `subscriptions`
  - Uses `GameMsg` for internal messages
  - Contains game data structures: `PlayGameGrid`, `Coordinate`, etc.

- **[Game/History.elm](../../src/Game/History.elm)** — Game history tracker
  - Sortable history of won/lost games
  - Uses `GameHistoryMsg` for internal messages

- **[Game/Internal.elm](../../src/Game/Internal.elm)** — Shared game types and helpers
  - Internal types used by both Game and History modules

- **[Colors.elm](../../src/Colors.elm)** — Color definitions (if separate from Styles)

- **[ErrorPage404.elm](../../src/ErrorPage404.elm)** — 404 error view

---

## Code Style & Conventions

### Naming
- **Type names**: PascalCase (`GameMsg`, `PlayGameGrid`, `Coordinate`)
- **Function names**: camelCase (`initModel`, `decodeStoredFinishedGameHistory`)
- **Module names**: PascalCase, match filename exactly

### Message Routing
- `Main.Msg` dispatches to sub-modules via wrapper types:
  - `GameView GameMsg` → routed to `Game.update`
  - `GameHistory GameHistoryMsg` → routed to `GameHistory.update`
- Each module exports its own `update`, `subscriptions`, and `view` functions

### Module Documentation
- Use module docstrings (triple-brace `{-| ... -}`) to explain purpose
- Example: `{-| Game module for rendering the complete game... -}`

### File Headers
- All files include AGPL-3.0-or-later license header for compliance

---

## Key Dependencies

### Direct Dependencies
- **elm/browser** (1.0.2) — Elm runtime
- **elm/core** (1.0.5) — Standard library
- **mdgriffith/elm-ui** (1.1.8) — Declarative UI
- **elm/random** (1.0.0) — Mine placement
- **elm/time** (1.0.0) — Game timer
- **LesleyLai/elm-grid** (1.0.1) — Grid structure for board

### Dev Dependencies
- **parcel** (2.16.0) — Bundler with hot reloading
- **elm-test** (0.19.1-revision12) — Unit testing
- **elm-format** → Enforced via npm script

---

## Common Development Tasks

### Add a New Feature
1. **Define types** in `Types.elm` (or `Game/Internal.elm` if game-local)
2. **Create message** (add to `Msg` or feature-specific `*Msg` type)
3. **Implement update logic** in appropriate module (Main, Game, or History)
4. **Add view** in same module or as a sub-view
5. **Test-drive**: `npm run test` before committing
6. **Format**: `npm run format` before PR

### Refactor Code
- Types.elm changes can cascade heavily — plan carefully
- Use elm-format to maintain consistency: `npm run format`
- Run tests to catch missing updates: `npm run test`

### Debug Game Logic
- Game state flows: `Main.elm` → `Game.Game.update` → `GameModel`
- Check [Game/Game.elm](../../src/Game/Game.elm) for state machine (e.g., `gameBoardStatus`)
- Use `Debug.log` sparingly (compiles to `console.log` in JS)

### Test Additions
- Place tests in `tests/` directory
- Follow naming: `GameTests.elm`, `HistoryTests.elm`
- Run: `npm run test`
- Current tests in [tests/GameTests.elm](../../tests/GameTests.elm)

---

## Git & PR Workflow

**Current branch**: `copilot/update-parcel-dependencies`  
**Active PR**: #79 (upgrading @parcel/* packages from 2.13.2 to 2.16.0)

### Before Committing
```bash
npm run format          # Auto-format code
npm run format-validate # Verify formatting
npm run test            # Run all tests
npm run build           # Test production build
```

### Deployment
- Automated via GitHub Actions: `.github/workflows/main.yml`
- Publishes to: https://swissbite.github.io/elm-minesweeper/
- `githubPagePathPrefix = "elm-minesweeper"` in Main.elm handles routing

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Hot reload not working | Restart `npm run start` |
| Elm compiler errors | Check `elm.json` and ensure all deps are installed via `elm-tooling install` |
| Test file not found | Run `npm run test` — it auto-compiles |
| Formatting conflicts | Run `npm run format` to auto-fix |
| Build size large? | Check `npm run build` output; Parcel handles tree-shaking |

---

## Document References

- **Elm Guide**: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
- **elm-ui Docs**: [https://github.com/mdgriffith/elm-ui](https://github.com/mdgriffith/elm-ui)
- **Project README**: [README.md](../../README.md) — Design goals, features, and setup
- **License**: [LICENSE](../../LICENSE) — AGPL-3.0-or-later

---

## Notes for AI Agents

- **Elm is pure-functional**: No side effects in functions. Use `Cmd` for effects.
- **Immutability**: All values are immutable. Use record updates for state changes: `{ model | gameRunningTimes = newTimes }`
- **Type safety**: Elm catches most bugs at compile time. Trust the compiler.
- **Module exports**: Check the module docstring or top-level `exposing` to understand what's public.
- **Current task context**: Review the active PR and recent commits to align with ongoing work.
