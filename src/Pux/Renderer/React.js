'use strict';

/* jshint node:true */
// module Pux.Renderer.React

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

var class_cache = {};

var props_cache = {
  index: 0
};

exports.renderToDOM_ = function (selector) {
  var ReactDOM = (typeof require === 'function' && require('react-dom'))
              || (typeof window === 'object' && window.ReactDOM);

  return function (reactClass) {
    return function () {
      ReactDOM.render(React.createElement(reactClass), document.querySelector(selector))
    };
  };
};

exports.renderToString_ = function (reactClass) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);

  return function () {
    return ReactDOMServer.renderToString(React.createElement(reactClass));
  };
};

exports.renderToStaticMarkup_ = function (reactClass) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);

  return function () {
    return ReactDOMServer.renderToStaticMarkup(React.createElement(reactClass));
  };
};

// Return a React component from virtual DOM signal.
exports.toReact = function (vdomSignal) {
  var isBrowser = typeof window === 'object';

  // Sets the focus of element with "data-focused" attribute (`focused` constructor).
  // Provides declarative focus control.
  function setFocus () {
    if (isBrowser && window.__puxActiveElement === true) {
      if (window.__puxActiveElement !== document.activeElement) {
        var el = window.__puxActiveElement = document.querySelector('[data-focused]')
        if (el !== null && document.activeElement !== el) {
          el.focus();
        }
      }
    }
  }

  return React.createClass({
    componentWillMount: function () {
      var ctx = this;
      var subscribed = false;
      vdomSignal.subscribe(function () {
        if (subscribed === true) {
          ctx.forceUpdate();
        } else {
          subscribed = true;
        }
      });
    },
    componentDidMount: setFocus,
    componentDidUpdate: function () {
      props_cache.index = 0;
      setFocus();
    },
    render: function () {
      var vdom = vdomSignal.get();

      if (vdom.length === 1) return vdom[0];

      // Wrap multiple root elements in a div
      return React.createElement('div', null, vdom);
    }
  });
};

// Create an HTML constructor for a React class using a unique key.
// When rendered this element is replaced by the class.
exports.registerClass = function (reactClass) {
  return function (key) {
    class_cache[key] = reactClass;
    return function (markup) {
      return markup;
    };
  };
};

exports.registerProps = function (props) {
  var key = String(++props_cache.index);
  props_cache[key] = props;
  return function (attr) {
    return attr(key);
  };
};

exports.reactAttr = function (str) {
  return str;
};

exports.reactHandler = function (input) {
  return function (handler) {
    return function (ev) {
      if (!ev || ev.nativeEvent === undefined) {
        input(handler(ev))();
      } else {
        input(handler(ev.nativeEvent))();
      }
    };
  };
};

// Wraps memoized views in a component class which only re-renders if the state
// has changed.
var PureComponent = React.createClass({
  shouldComponentUpdate: function (nextProps) {
    if (nextProps.state.st === undefined) return true;
    return nextProps.state.st !== this.props.state.st;
  },
  render: function () {
    return this.props.children;
  }
});

exports.reactElement = function (name, attrs, children) {
  // convert smolder attribute names to react attribute names
  var reactAttrs = {};
  for (var key in attrs) {
    if (attrMap[key]) {
      reactAttrs[attrMap[key]] = attrs[key];
    } else {
      reactAttrs[key] = attrs[key];
    }
  }
// MODIFIED
  if (reactAttrs.ref !== undefined){
    console.log("React Ref is here!  It's name is " + reactAttrs.ref);
  }

  if (reactAttrs.dangerouslySetInnerHTML !== undefined) {
    reactAttrs.dangerouslySetInnerHTML = { __html : reactAttrs.dangerouslySetInnerHTML };
  }

  // Support declarative focus attribute
  if (reactAttrs.focused !== undefined) {
    if (typeof window === 'object') {
      window.__puxActiveElement = true;
      reactAttrs['data-focused'] = 'focused';
    }
  }

  // Parse inline style, because React expects a map instead of a string.
  // Skipped if Preact is detected, because it supports a string.
  if (reactAttrs.style !== undefined) {
    reactAttrs.style = reactAttrs.style.split(';').reduce(function (prev, curr) {
      if (!curr) return prev;
      var prop = curr.split(':');
      var key = prop[0].replace(/^ */, '').replace(/ *$/, '').replace(/(-\w)/g, function (m, w) {
        return w[1].toUpperCase();
      });
      var val = prop[1].replace(/^ */, '').replace(/ *$/, '');
      prev[key] = val;
      return prev;
    }, {});
  }

  if (name === 'style') {
    // Convert style element children to string
    if (children !== null && children.length) {
      reactAttrs.dangerouslySetInnerHTML = { __html: children.join(' ') };
      children = null
    }
  } else if (name === 'reactclass') {
    // Support rendering of foreign react classes registered through
    // `registerClass`
    var component = class_cache[reactAttrs['data-pux-react-class']];
    var props = props_cache[reactAttrs['data-pux-react-props']];

    if (props === undefined) props = {};

    for (var key in reactAttrs) {
      if (key !== 'data-pux-react-class') {
        props[key] = reactAttrs[key];
      }
    }

    if (component) {
      return React.createElement(component, props, children);
    } else {
      return React.createElement('div', reactAttrs, children);
    }
  }

  // Eliminate React "key" errors for parents with a single child
  // (React checks for keys when children is passed as an array)
  if (children !== null && children.length === 1) {
    children = children[0];
  }

  // Cache react element. If the same node is rendered again the cached element will be used.
  if (name === 'thunk') {
    console.log("We have a thunk");
    return React.createElement(PureComponent, reactAttrs, children);
  }

  // MODIFIED - Add lifecycle functions
  if (name === 'lifecycle') {
    console.log("Found lifecycle component");
  var lifeCycleMap = reactAttrs.lifecycle.lifeCycles;
  console.log("Lifecycles are " + lifeCycleMap);
  // strip out our special lifecycle prop
  var reactProps = {};
  for (var k in reactAttrs) {
    if (k !== 'lifecycle') {
      reactProps[key] = reactAttrs[key];
    }
  }

    return React.createElement(LifeCycleComponent(lifeCycleMap), reactProps, children);
  }

  return React.createElement(name, reactAttrs, children);
};

exports.reactText = function (string) {
  return string;
};

//MODIFIED
// LifeCycle functions
exports.wrapLifeCycles = function(toNullable){
  return function(wrapper){
    return function(lifeCycles){
      return function(component){
          return wrapper(new PuxLifeCycleString(maybeToNullable(toNullable, lifeCycles)))(component);
      };
    };
  };
};

//MODIFIED
function maybeToNullable(toNullable, lifecycles){
    var nulled = {};
    for (var key in lifecycles) {
        nulled[key] = toNullable(lifecycles[key]);
    }
    console.log("Converted to Nullable.  Here is the result " + nulled);
    return nulled;
}

//MODIFIED
// Used to store out-of-band lifecycle objects as a String type safely.
// In the worst case the attribute is manipulated, it will be treated as an
// empty string and any renderer cache will always miss.
function PuxLifeCycleString (lifeCycles) {
  this.lifeCycles = lifeCycles;
}

//MODIFIED
PuxLifeCycleString.prototype.toString = function () {
  return '';
};

//MODIFIED
// Wraps lifecycle views in a component class
function LifeCycleComponent(lifecycles){
  var lifeCycleClass = {};

  if (lifecycles.componentWillMount){
      console.log("componentWillMount lifecycle has been found");
      lifeCycleClass.componentWillMount = function(){
        return lifecycles.componentWillMount(this)();
      };
  }

  if (lifecycles.componentDidMount){
      console.log("componentDidMount lifecycle has been found");
      lifeCycleClass.componentDidMount = function(){
        return lifecycles.componentDidMount(this)();
      };
  }

  if (lifecycles.shouldComponentUpdate){
      console.log("shouldComponentUpdate lifecycle has been found");
      lifeCycleClass.shouldComponentUpdate = function(nextProps, nextState){
        return lifecycles.shouldComponentUpdate(this)(nextProps)(nextState)();
      };
  }

  if (lifecycles.componentWillReceiveProps){
      console.log("componentWillReceiveProps lifecycle has been found");
      lifeCycleClass.componentWillReceiveProps = function(nextProps){
        return lifecycles.componentWillReceiveProps(this)(nextProps)();
      };
  }

  if (lifecycles.componentWillUpdate){
      console.log("componentWillUpdate lifecycle has been found");
      lifeCycleClass.componentWillUpdate = function(nextProps, nextState){
        return lifecycles.componentWillUpdate(this)(nextProps)(nextState)();
      };
  }

  if (lifecycles.componentDidUpdate){
      console.log("componentDidUpdate lifecycle has been found");
      lifeCycleClass.componentDidUpdate = function(prevProps, prevState){
        return lifecycles.componentDidUpdate(this)(prevProps)(prevState)();
      };
  }

  if (lifecycles.componentWillUnmount){
      console.log("componentWillUnmount lifecycle has been found");
      lifeCycleClass.componentWillUnmount = function(){
        return lifecycles.componentWillUnmount(this)();
      };
  }

    lifeCycleClass.render = function () {
      return this.props.children;
    };

    return React.createClass(lifeCycleClass);
}

//MODIFIED
exports.getRefImp = function(name){
  return function(ctx){
    return function(){
      return ctx.name;
    };
  };
};



// Normalize Smolder attribute names with React attribute names
var attrMap = {
  'accesskey': 'accessKey',
  'allowfullscreen': 'allowFullScreen',
  'allowtransparency': 'allowTransparency',
  'autocomplete': 'autoComplete',
  'autofocus': 'autoFocus',
  'autoplay': 'autoPlay',
  'cellpadding': 'cellPadding',
  'cellspacing': 'cellSpacing',
  'charset': 'charSet',
  'class': 'className',
  'classid': 'classID',
  'colspan': 'colSpan',
  'contextmenu': 'contextMenu',
  'crossorigin': 'crossOrigin',
  'datetime': 'dateTime',
  'enctype': 'encType',
  'formaction': 'formAction',
  'formenctype': 'formEncType',
  'formmethod': 'formMethod',
  'formnovalidate': 'formNoValidate',
  'formtarget': 'formTarget',
  'frameborder': 'frameBorder',
  'for': 'htmlFor',
  'inputmode': 'inputMode',
  'keyparams': 'keyParams',
  'keytype': 'keyType',
  'marginheight': 'marginHeight',
  'marginwidth': 'marginWidth',
  'maxlength': 'maxLength',
  'mediagroup': 'mediaGroup',
  'minlength': 'minLength',
  'novalidate': 'noValidate',
  'radiogroup': 'radioGroup',
  'readonly': 'readOnly',
  'rowspan': 'rowSpan',
  'spellcheck': 'spellCheck',
  'srcdoc': 'srcDoc',
  'srclang': 'srcLang',
  'srcset': 'srcSet',
  'tabindex': 'tabIndex',
  'usemap': 'useMap'
};
