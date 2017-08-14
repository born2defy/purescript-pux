<div align="center">
<h1>PUX</h1>
<p align="center">
<em>Build type-safe web applications with PureScript.</em>
</p>
<a href="https://www.purescript-pux.org">Documentation</a>
| <a href="https://github.com/alexmingoia/purescript-pux/tree/master/examples/">Examples</a>
| <a href="https://gitter.im/alexmingoia/purescript-pux">Chat</a>
</div>

<hr />

[![Latest Release](http://img.shields.io/github/release/alexmingoia/purescript-pux.svg)](https://pursuit.purescript.org/packages/purescript-pux)
[![ComVer](https://img.shields.io/badge/comver-compliant-brightgreen.svg)](https://github.com/staltz/comver)
[![Build Status](https://travis-ci.org/alexmingoia/purescript-pux.svg?branch=master)](https://travis-ci.org/alexmingoia/purescript-pux)
[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/alexmingoia/purescript-pux)

====================================================
=============== FORK MODIFICATIONS =================
====================================================

This library has been modified in the following ways:

1) To allow for usage of the new stack-safe
implementation of the Smolder Free Monad (v10+).  
These modifications have been taken directly from
Bodil Stokke's existing pull request.


=====================mapEvent======================
In the HTML.purs file, the mapEvent function has been
modified to use Smolder 10+'s built-in mapEvent
functionality.  This implementation is my own
(using composition rather than direct application).

-- | MODIFIED - Changed to use smolder's mapEvent function
mapEvent :: ∀ a b. (a -> b) -> HTML a -> HTML b
mapEvent f = Smolder.mapEvent (\g -> f <<< g)

We take the existing handler function and simply compose
it with the contravariant mapping function (a -> b).

=====================Renderer.React======================
In the Renderer.React.purs file, the below functions have been
modified using Bodil Stokke's implementation from her existing pull request to
utilize Smolder 10+'s Free Monad implementation

renderItem :: ∀ e. (e -> ReactAttribute) -> NaturalTransformation (MarkupM e) (State (Array ReactElement))
renderItem input (Element n c a e r) =
  let kids = renderNodes input c
      el   = runFn3 reactElement n (renderAttrs input a e) (toNullable (Just kids))
  in  state \s -> Tuple r $ snoc s el

renderItem input (Content t r) = state \s -> Tuple r $ snoc s $ reactText t
renderItem input (Empty r) = pure r

renderNodes :: ∀ e. (e -> ReactAttribute) -> Markup e -> Array ReactElement
renderNodes input m = execState (foldFree (renderItem input) m) []




2) To expose the Pux main callback thread to
external react components via a prop called
sendToPux allowing outside components to send values
to the Pux main thread.  The mapToPux attribute building
function and the prop type are exposed in a private library
(Pux.More)

=====================mkSend FFI======================
-- | This allows the pux callback to be embedded in a String valued Attribute
-- | It is used in the modified version of renderAttrs
foreign import mkSend :: ∀ e. String -> (e -> ReactAttribute) -> ReactAttribute

=====================renderAttrs======================
-- | The attributes toTupleA fucntion has been modified to pattern match on the
-- | sendToPux attribute and embed the pux callback into the attribute value
renderAttrs :: ∀ e. (e -> ReactAttribute) -> CatList Attr -> CatList (EventHandler e) -> StrMap ReactAttribute
renderAttrs input attrs handlers = StrMap.fromFoldable tuples
  where
  tuples = map toTupleA attrs <> map toTupleH handlers
  toTupleH (EventHandler key value) = Tuple key (input value)
  toTupleA (Attr "sendToPux" send) = Tuple "sendToPux" (mkSend send input)  -- | MODIFIED
  toTupleA (Attr key value) = Tuple key (reactAttr value)


====================================================
================ END MODIFICATIONS =================
====================================================


Pux is a PureScript library for building web applications. Interactive
UI is modeled as a single state transition function,
`Event -> State -> (State, HTML)` which is run for every event. Pux also
provides tooling such as:

- Isomorphic routing and rendering
- Hot reloading
- Render to React (or any virtual DOM library)
- Time-travelling debug extension

### Quick start

The [starter app](http://github.com/alexmingoia/pux-starter-app) provides
everything you need to get started:

```sh
git clone git://github.com/alexmingoia/pux-starter-app.git my-awesome-pux-app
cd my-awesome-pux-app
npm install
npm start
```

### Example

The following chunk of code sets up a basic counter that can be incremented and
decremented:

```purescript
module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = Int

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
```
