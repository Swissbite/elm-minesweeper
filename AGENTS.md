# Elm Minesweeper - Project Context

This project is a functional programming exploration into building a Minesweeper web application using **Elm**. It draws inspiration from Gnome Mines and "Minesweeper - The Clean One" (Android).

For detailed technical requirements and mechanics, see the [Technical Specification](SPEC.md).

## Project Overview

*   **Core Technology:** [Elm 0.19.1](https://elm-lang.org/)
*   **UI Framework:** [elm-ui](https://github.com/mdgriffith/elm-ui) for layout and styling.
*   **Data Structures:** Uses [elm-grid](https://github.com/LesleyLai/elm-grid) for the game board.
*   **Bundler:** [Parcel](https://parceljs.org/) for building and local development.
*   **Testing:** [elm-test](https://github.com/elm-explorations/test) for unit and fuzz testing.
*   **Deployment:** Automated builds via GitHub Actions, published to GitHub Pages.

## Architecture and Design

The project follows the [official Elm guide for structuring web apps](https://guide.elm-lang.org/webapps/structure.html) and "The Life of a File" patterns.

### Key Modules

*   **`src/Main.elm`**: The application entry point (`Browser.application`). Handles routing, global state initialization, and coordinates between `Game` and `History` views.
*   **`src/Types.elm`**: **Strictly for Type Definitions.** Contains all `Msg`, `Model`, and domain-specific types. No functions or logic should be placed here.
*   **`src/Styles.elm`**: Reusable styles, colors (referencing `Colors.elm`), and icons. It has zero dependencies on `Types.elm`.
*   **`src/Game/`**:
    *   **`Game.elm`**: Main game logic, state management, and rendering. Uses a dedicated `GameMsg` type.
    *   **`Internal.elm`**: Helper functions, random grid generation logic, and JSON encoders/decoders for game state.
    *   **`History.elm`**: Logic for tracking, displaying, and filtering game results.
*   **`src/Ports.elm`**: Defines ports for persisting game history to local storage.
*   **`src/index.js`**: JavaScript entry point. Initializes the Elm application with flags (window dimensions, initial path, and game history from `localStorage`) and handles port subscriptions for data persistence.
*   **`src/Colors.elm`**: Centralized color palette.

## Development Workflow

### Persistence
- Game history is stored in the browser's `localStorage` under the key `finishedGameHistory`. It is passed to Elm as a flag on initialization and updated via the `storeFinishedGameHistory` port.

### Prerequisites
- Node.js (Version managed via `.nvmrc`)
- [nvm](https://github.com/nvm-sh/nvm)

### Getting Started
1.  `nvm use`
2.  `npm install`

### Key Commands
- **Development Server:** `npm run start` (Parcel with hot reloading)
- **Run Tests:** `npm run test`
- **Production Build:** `npm run build`
- **Format Code:** `npm run format`
- **Validate Formatting:** `npm run format-validate` (Used in CI)

## Development Conventions

*   **Surgical Logic Separation:** Keep `Types.elm` pure of logic. Any transformations or helper functions belong in the module that manages the data (e.g., `Game/Internal.elm`).
*   **Styling:** Prefer `elm-ui` primitives over raw CSS. Shared styles should be added to `Styles.elm`.
*   **Routing:** The application uses path-based routing (e.g., `/history`). Note the `githubPagePathPrefix` ("elm-minesweeper") used for compatibility with GitHub Pages.
*   **Testing:** New features or bug fixes in game logic should be accompanied by tests in `tests/GameTests.elm`.

## Important Files
- `elm.json`: Elm project configuration and dependencies.
- `package.json`: NPM scripts and development tools (Parcel, etc.).
- `README.md`: High-level project goals and setup instructions.
- `LICENSE`: AGPL-3.0-or-later.
