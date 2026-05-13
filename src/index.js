/**
  This file is part of Elm Minesweeper.

  Elm Minesweeper is free software: you can redistribute it and/or modify it under
  the terms of the GNU Affero General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later version.

  Elm Minesweeper is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with
  Elm Minesweeper. If not, see <https://www.gnu.org/licenses/>.

*/

import './main.css';
import { Elm, Main } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const localStoreFinishedGameHistoryKey = 'finishedGameHistory';
const localStoreThemeKey = 'themePreference';

const storedFinishedGameHistory = localStorage.getItem(localStoreFinishedGameHistoryKey);
const finishedGameHistory = storedFinishedGameHistory ? JSON.parse(storedFinishedGameHistory) : "[]";
const pathname = window.location.pathname;

let themePref = localStorage.getItem(localStoreThemeKey);
if (!themePref) {
  themePref = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    history: finishedGameHistory,
    height: window.innerHeight,
    width: window.innerWidth,
    initPath : pathname,
    theme: themePref
  }
});


app.ports.storeFinishedGameHistory.subscribe(function(finishedGameHistory) {
  if (finishedGameHistory.length > 0) {
    const historyAsJson = JSON.stringify(finishedGameHistory);
    localStorage.setItem(localStoreFinishedGameHistoryKey, historyAsJson);
  }
});

app.ports.storeTheme.subscribe(function(theme) {
    localStorage.setItem(localStoreThemeKey, theme);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
