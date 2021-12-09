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

This project will be an on/off project ;-)


## Set up the project

1. Install [`elm-app`](https://github.com/halfzebra/create-elm-app), a super-power tool for 
   bootstraping elm applications by `npm install -g create-elm-app`
2. Follow [Elm-App Setup](Elm-App%20Setup.md)

## License
See [LICENSE](LICENSE)

## Basic game flow
From initialize a game to finish the game.

In [Game.elm](src/Game.elm) is a type definition `GameStatus` which describes a current state of the 
game and the game field. The possible states are:

1. `NoGame`: Represents the start of the application. No definition of the playground has been defined
2. `InitGame PlaygroundDefinition`: The initialisation of the game has been set. We know the dimensions and
   the amount of mines. But the game has not yet started. The game starts by clicking on any field.
3. `RunningGame GameField`: The current running game. Fields can either be marked or opened. The time counter is running
4. `PausedGame GameField`: Still the game field available, but no updates are allowed. 
   The UI may hide the visual representing of the board to reduce cheating
5. `FinishedGame FinishStatus GameField`: The end of the game. The won/lost is set by `FinishStatus`, the `GameField` 
   represents the last state of the field. 
