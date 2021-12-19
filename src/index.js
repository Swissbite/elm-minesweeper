import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: Math.floor (Math.random() * Number.MAX_VALUE)
});

app.ports.storeFinishedGameHistory.subscribe(function(finishedGameHistory) {
  if (finishedGameHistory.length > 0) {
    const historyAsJson = JSON.stringify(finishedGameHistory);
    localStorage.setItem('finishedGameHistory', historyAsJson);
    console.log('saved state', historyAsJson);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
