# Elm Minesweeper

This project is just something for me to play around 
with [elm-lang](https://elm-lang.org/), [elm-ui](https://github.com/mdgriffith/elm-ui) 
and to wrap my head around functional programming at it's purest.

## Table of Content
- [The goal](#the-goal)
- [Set up the project](#set-up-the-project)
- [Basic game flow](#basic-game-flow)

## The goal
The goal of this **side project** is to have a working minesweeper web application (as a thousand other in the web)
in a similar behaviour as the [Gnome Mines](https://wiki.gnome.org/Apps/Mines) application.

I also like the way the android game 
[Minesweeper - The Clean One](https://play.google.com/store/apps/details?id=ee.dustland.android.minesweeper) 
solved some issues for touch devices.

Features it should have (in no particular order):

- Multiple difficulties ✔
- A current high score
- Able to pause and resume the game
- Able to switch between click or mark fields ✔
- See the result board after finished the game
  - If the game has been won or lost ✔
  - The mines should be revealed on the board ✔
  - The end state of the board ✔
  - Time it took for the game
- After game finish:
   - Able to start a new game with the same difficulty ✔
   - Back to overview to create a new game ✔
- Click on a number, where are enough marks to open all remaining items should work ✔
- Open a blank field should open all other blank fields and the neighbour number fields ✔
- Have some game information like
  - how many mines are still missing
  - how many have been marked
  - how much time it took for the game
- Have the game automated build and published to https://swissbite.github.io/elm-minesweeper/ ✔

This project will be an on/off project ;-)


## Set up the project

1. Install [`elm-app`](https://github.com/halfzebra/create-elm-app), a super-power tool for 
   bootstraping elm applications by `npm install -g create-elm-app`
2. Follow [Elm-App Setup](Elm-App%20Setup.md)

## Project structure

Currently, I'm following [Structuring Web Apps](https://guide.elm-lang.org/webapps/structure.html) and the [Live of a file](https://youtu.be/XpDsk374LDE)

To simplify lookups as a developer, I split it in three main files:
- [Types.elm](./src/Types.elm): All types and type aliases. ***No*** functions, no helpers, no transformers.
- [Styles.elm](./src/Styles.elm): Make styles reusable. Define colors, icons, predefined elements (like the toogle element). But does not hold any model related code. Styles is has zero dependencies to [Types.elm](./src/Types.elm)
- [Main.elm](./src/Main.elm): The main startup application. At the write of this line, it holds everything. This may be splited up into different files per screen. But still following the advises of the links above.

## License
See [LICENSE](LICENSE)

