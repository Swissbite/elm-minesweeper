# Elm Minesweeper

This project is just something for me to play around 
with [elm-lang](https://elm-lang.org/), [elm-ui](https://github.com/mdgriffith/elm-ui) 
and to wrap my head around functional programming at it's purest.

The goal of this **side project** is to have a working minesweeper web application (as a thousand other in the web)
in a similar behaviour as the [Gnome Mines](https://wiki.gnome.org/Apps/Mines) application.

I also like the way the android game 
[Minesweeper - The Clean One](https://play.google.com/store/apps/details?id=ee.dustland.android.minesweeper) 
solved some issues for touch devices.

Features it should have:

- Multiple difficulties
- A current high score
- Able to pause and resume the game
- Able to switch between click or mark fields
- Click on a number, where are enough marks to open all remaining items should work
- Open a blank field should open all other blank fields and the neighbour number fields
- Have some game information like
  - how many mines are still missing
  - how many have been marked
  - how much time it took for the game

This project will be an on/off project ;-)


## Set up the project

1. Install [`elm-app`](https://github.com/halfzebra/create-elm-app), a super-power tool for bootstraping elm applications by `npm install -g create-elm-app`
2. Follow [Elm-App Setup](Elm-App%20Setup.md)

## License
See [LICENSE](LICENSE)
