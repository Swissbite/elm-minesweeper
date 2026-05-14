# Project Specification: Elm Minesweeper

## 1. Goal and Core Philosophy
The goal of this project is to provide a fully functional, visually polished Minesweeper web application that replicates the feel of classic implementations (Gnome Mines) while incorporating modern touch-friendly elements (inspired by "Minesweeper - The Clean One").

### Key Mandate: Zero Server Logic
**The application must run entirely in the browser.**
- **No Backend:** All logic, state management, and persistence must happen on the client side.
- **Static Hosting:** The build output consists of static assets (HTML, JS, CSS, images) suitable for hosting on GitHub Pages or any static file server.
- **Persistence:** User history and settings are stored locally in the browser's `localStorage` (with graceful degradation if unavailable).

## 2. Technical Stack
- **Language:** Elm 0.19.1
- **UI & Layout:** `elm-ui` (Typed UI composition, replacing manual CSS)
- **Data Structures:** `elm-grid` for 2D board representation.
- **Build System:** Parcel 2.x
- **Testing:** `elm-test` (Unit and Fuzz testing)

## 3. Game Mechanics

### 3.1. Grid Generation
- **Safe First Click:** The first cell clicked is guaranteed to be an `EmptyCell` (no mines in the immediate 3x3 area). The grid is generated *after* the first click.
- **Difficulty Levels:**
  - **Small:** 8x8, 10 mines
  - **Medium:** 16x16, 40 mines
  - **Advanced:** 30x16, 99 mines
  - **XXL:** 30x30, 200 mines

### 3.2. Interactions
- **Reveal:** Left-click/Tap to open a cell.
- **Flag:** Right-click or toggle "Flag Mode" to mark suspected mines.
- **Chording:** Clicking an already opened number cell, when the required number of flags are placed around it, will automatically open all remaining adjacent cells.
- **Recursive Opening:** Opening an `EmptyCell` automatically reveals all adjacent non-mine cells recursively.

### 3.3. Win/Loss Conditions
- **Win:** All non-mine cells are opened.
- **Loss:** Any mine cell is opened.
- **End State:** The full board is revealed, highlighting the cause of loss (exploded mine) or celebrating the win.

## 4. UI/UX Requirements
- **Responsive Design:** Adapts between Mobile (Phone/Tablet) and Desktop layouts using `Element.classifyDevice`.
- **Accessibility & Touch:** Minimum touch targets enforced to 44px for touch interfaces.
- **Theming:** Dynamic dark/light theme switch support.
- **Game History:** A dedicated view to browse past games, filterable by result (Won/Lost) and sortable by duration, date, or difficulty.
- **Visual Feedback:** Interactive feedback for cell hovering, clicking, and state changes (Reveal vs. Flag mode).

## 5. Persistence Schema
Data is persisted as a JSON string in `localStorage` under the key `finishedGameHistory`.

### Versioning
- **Version 1 (Current):** Includes `grid`, `result` ("won"/"lost"), `duration` (ms), and `posix` (timestamp).
- **Migration:** The application includes decoders to transparently upgrade Version 0 (legacy) data to the current schema.

## 6. File Map

### Core Application
- `src/Main.elm`: Application entry, routing, and top-level update loop.
- `src/index.js`: JS bootstrap, `localStorage` bridge, and port subscriptions.
- `src/Types.elm`: Global type definitions (Model, Msg, GameState).
- `src/Theme.elm`: Theming types and definitions (dark/light mode).
- `src/Styles.elm`: UI component definitions and reusable styling attributes.
- `src/Colors.elm`: Standardized color palette.

### Game Logic
- `src/Game/Game.elm`: Main game view and update logic.
- `src/Game/Internal.elm`: Grid generation, JSON encoding/decoding, and time formatting.
- `src/Game/History.elm`: History view and filtering/sorting logic.

### Infrastructure
- `src/Ports.elm`: Elm-to-JS communication for persistence.
- `src/StaticHelper.elm`: Configuration for deployment paths (GitHub Pages prefix).
- `public/`: Static assets (index.html, manifest, icons, background images).
- `tests/GameTests.elm`: Test suite for grid logic and decoders.
