# Elm Minesweeper

[![Build and deploy elm-minesweeper](https://github.com/Swissbite/elm-minesweeper/actions/workflows/main.yml/badge.svg)](https://github.com/Swissbite/elm-minesweeper/actions/workflows/main.yml)  [![AGPL v3 logo](https://www.gnu.org/graphics/agplv3-88x31.png)](https://www.gnu.org/licenses/agpl-3.0.html)

This project is just something for me to play around 
with [elm-lang](https://elm-lang.org/), [elm-ui](https://github.com/mdgriffith/elm-ui) 
and to wrap my head around functional programming at it's purest.

## Table of Content
- [The goal](#the-goal)
- [Set up the project](#setup-the-project)
- [Project structure](#project-structure)

## The goal
The goal of this **side project** is to have a working minesweeper web application (as a thousand other in the web)
in a similar behaviour as the [Gnome Mines](https://wiki.gnome.org/Apps/Mines) application.

I also like the way the android game 
[Minesweeper - The Clean One](https://play.google.com/store/apps/details?id=ee.dustland.android.minesweeper) 
solved some issues for touch devices.

Features it should have (in no particular order):

- Multiple difficulties ✔
- A current played game history ✔
- Able to pause and resume the game
- Able to switch between click or mark fields ✔
- See the result board after finished the game
  - If the game has been won or lost ✔
  - The mines should be revealed on the board ✔
  - The end state of the board ✔
  - Time it took for the game ✔
- After game finish:
   - Able to start a new game with the same difficulty ✔
   - Back to overview to create a new game ✔
- Click on a number, where are enough marks to open all remaining items should work ✔
- Open a blank field should open all other blank fields and the neighbour number fields ✔
- Have some game information like
  - how many mines are still missing ✔
  - how many have been marked ✔
  - how much time it took for the game ✔
- Have the game automated build and published to https://swissbite.github.io/elm-minesweeper/ ✔

This project will be an on/off project ;-)


## Setup the project

Use [nvm](https://github.com/nvm-sh/nvm) or [nvm-windows](https://github.com/coreybutler/nvm-windows) to have the correct node version installed.
On Microsoft Windows, you may consider to use the dev environments on Docker for Desktop.

1. `nvm use`
2. `npm install`

### Local development

- Run `npm run start` to start a local development server with auto-reload and all other features

### Run tests
- `npm run test`

### Validate format
 - `npm run format-validate`


## Project structure

Currently, I'm following [Structuring Web Apps](https://guide.elm-lang.org/webapps/structure.html) and the [Live of a file](https://youtu.be/XpDsk374LDE)

To simplify lookups as a developer, I split it in three main files:
- [Types.elm](./src/Types.elm): All types and type aliases. ***No*** functions, no helpers, no transformers.
- [Styles.elm](./src/Styles.elm): Make styles reusable. Define colors, icons, predefined elements (like the toogle element). But does not hold any model related code. Styles is has zero dependencies to [Types.elm](./src/Types.elm)
- [Main.elm](./src/Main.elm): The main startup application. Responsible for initialize the application. Coordinates update subscription and view between the different elements.
- [Game/Game.elm](./src/Game/Game.elm):Has its own msg type, GameMsg. Responsible for the complite minesweeper game flow. Extracted into own module to be able to add additional views like an About view or other stuff.
- [Game/History.elm](./src/Game/History.elm): The game history of lost / won games. Sortable.

## License
Elm Minesweeper is licensed under the [GNU Affero General Public License version 3](LICENSE) or later.