module LifeCycleTest where

import Pux
import Text.Smolder.HTML

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import Data.Maybe (Maybe(..))
import Prelude (Unit, bind, const, discard, show, unit, ($), (<>))
import Pux.DOM.HTML (HTML, memoize)
import Pux.Renderer.React (ReactLifeCycles, addLifeCycles, lifeCycles, renderToDOM, addRef, ref, getRef)
import Text.Smolder.Markup (empty, text, (!))

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: unit
    , view
    , foldp: \_ _ -> noEffects unit
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input

view :: ∀ ev. Unit -> HTML ev
view  x = do
  refDiv
  memoDiv x

refDiv :: ∀ ev. HTML ev
refDiv = addLifeCycles lifeCyclesTest $
  div do
    span ! ref "myDiv" $ text "I am the div that has lifecycle functions"

memoDiv :: ∀ ev. Unit -> HTML ev
memoDiv = memoize \_ -> span $ text "memoized"

lifeCyclesTest :: ∀ eff. ReactLifeCycles (dom :: DOM, console :: CONSOLE | eff)
lifeCyclesTest = lifeCycles {
    componentWillMount = Just \ctx -> do
      log "We will soon be mounting!"
      addRef ctx
  , componentDidMount = Just \ctx -> do
      mref <- getRef "myDiv" ctx
      case mref of
        Nothing -> log "Ref isn't found"
        Just e  -> do
          rect <- getBoundingClientRect e
          log $  "Ref found.  Bounding rectangle is: left " <> show rect.left <> ", right " <> show rect.right
}
