import './main.css';
import { Elm, Main } from './Main.elm';
import * as serviceWorker from './serviceWorker';



// Single Page Apps for GitHub Pages
// MIT License
// https://github.com/rafgraph/spa-github-pages
// This script checks to see if a redirect is present in the query string,
// converts it back into the correct url and adds it to the
// browser's history using window.history.replaceState(...),
// which won't cause the browser to attempt to load the new url.
// When the single page app is loaded further down in this file,
// the correct url will be waiting in the browser's history for
// the single page app to route accordingly.
function fixHistoryForSPARedirect(l) {
  const pathname = l.pathname;
  if (l.search[1] === '/' ) {
  const decoded = l.search.slice(1).split('&').map(function(s) { 
      return s.replace(/~and~/g, '&')
  }).join('?');
  window.history.replaceState(null, null,
      l.pathname.slice(0, -1) + decoded + l.hash
  );
  }
  l.pathname
}

const localStoreFinishedGameHistoryKey = 'finishedGameHistory';

const storedFinishedGameHistory = localStorage.getItem(localStoreFinishedGameHistoryKey);
const finishedGameHistory = storedFinishedGameHistory ? JSON.parse(storedFinishedGameHistory) : "[]";
const pathname = window.location.pathname;

fixHistoryForSPARedirect(window.location);

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    history: finishedGameHistory,
    height: window.innerHeight,
    width: window.innerWidth,
    initPath : pathname
  }
});


app.ports.storeFinishedGameHistory.subscribe(function(finishedGameHistory) {
  if (finishedGameHistory.length > 0) {
    const historyAsJson = JSON.stringify(finishedGameHistory);
    localStorage.setItem(localStoreFinishedGameHistoryKey, historyAsJson);
    console.log('saved state', historyAsJson);
  }
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
