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
const localStoreRunningGameKey = 'runningGame';
const localStoreRunningGameSaltKey = 'runningGameSalt';

const storedFinishedGameHistory = localStorage.getItem(localStoreFinishedGameHistoryKey);
const finishedGameHistory = storedFinishedGameHistory ? JSON.parse(storedFinishedGameHistory) : "[]";
const pathname = window.location.pathname;

let themePref = localStorage.getItem(localStoreThemeKey);
if (!themePref) {
  themePref = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
}

// Random per-browser salt for the running game checksum. Generated once on
// first start; without it a stored running game cannot be validated, so a
// missing or unwritable salt simply invalidates any existing save.
function generateRunningGameSalt() {
  const words = new Uint32Array(4);
  if (window.crypto && window.crypto.getRandomValues) {
    window.crypto.getRandomValues(words);
  } else {
    for (let i = 0; i < words.length; i++) {
      words[i] = Math.floor(Math.random() * 4294967296);
    }
  }
  return Array.from(words).map((word) => word.toString(16).padStart(8, '0')).join('');
}

let runningGame = "";
let runningGameSalt = "";
try {
  runningGame = localStorage.getItem(localStoreRunningGameKey) || "";
  runningGameSalt = localStorage.getItem(localStoreRunningGameSaltKey) || "";
  if (!runningGameSalt) {
    runningGameSalt = generateRunningGameSalt();
    localStorage.setItem(localStoreRunningGameSaltKey, runningGameSalt);
  }
} catch (e) {
  console.warn("Could not read the running game from local storage", e);
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    history: finishedGameHistory,
    height: window.innerHeight,
    width: window.innerWidth,
    initPath : pathname,
    theme: themePref,
    runningGame: runningGame,
    runningGameSalt: runningGameSalt
  }
});


app.ports.storeFinishedGameHistory.subscribe(function(finishedGameHistory) {
  if (finishedGameHistory.length > 0) {
    const historyAsJson = JSON.stringify(finishedGameHistory);
    try {
      localStorage.setItem(localStoreFinishedGameHistoryKey, historyAsJson);
    } catch (e) {
      console.warn("Could not save game history to local storage", e);
    }
  }
});

app.ports.storeRunningGame.subscribe(function(runningGameAsJson) {
  try {
    localStorage.setItem(localStoreRunningGameKey, runningGameAsJson);
  } catch (e) {
    console.warn("Could not save the running game to local storage", e);
  }
});

app.ports.clearRunningGame.subscribe(function() {
  try {
    localStorage.removeItem(localStoreRunningGameKey);
  } catch (e) {
    console.warn("Could not clear the running game in local storage", e);
  }
});

app.ports.storeTheme.subscribe(function(theme) {
    try {
      localStorage.setItem(localStoreThemeKey, theme);
    } catch (e) {
      console.warn("Could not save theme to local storage", e);
    }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
