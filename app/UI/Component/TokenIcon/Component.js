const rawSVG = require("svg-inline-loader!./token-icon.svg");
const tokenIconConfig = require('./config.json');

// halogen will effectively re-render the whole DOM every
// time there's a child change -- and there's  no sense in re-creating
// an element tree every time the view changes -- so we can just hold
// on to previous generated DOM elements here
let elementCache = {};


function buildClassNameFromProps(props) {
  let built = "token-icon";
  for (const prop in tokenIconConfig.defaultStyle) {
    built += ` ${prop}-${tokenIconConfig.defaultStyle[prop]}`;
  }
  return built;
}

function createFromProps(props) {
  const div = document.createElement('div');
  div.className = buildClassNameFromProps(props);
  div.innerHTML = rawSVG;
  return div;
}

function getOrCreateFromCacheRef(cacheRef, props) {
  const inCache = cacheRef ? elementCache[cacheRef] : undefined;
  if (inCache) {
    return inCache;
  } else {
    const e = createFromProps(props);
    elementCache[cacheRef] = e;
    return e;
  }
}

exports.insertTokenIconWidget = function(cacheRef) {
  return function(props) {
      return function(elem) {
        return function (_eff) {
          const widget = cacheRef && typeof(cacheRef) === 'string' && cacheRef != "" ? 
            getOrCreateFromCacheRef(cacheRef, props) : createFromProps(props);
          elem.appendChild(widget);
        }
     }
  }
}

exports.iconDivClass = buildClassNameFromProps;