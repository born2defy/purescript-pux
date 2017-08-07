module Pux.DOM.HTML
  ( HTML
  , child
  , mapEvent
  , memoize
  , style
  ) where

import CSS.Render (render, renderedSheet)
import CSS.Stylesheet (CSS)
import Control.Monad.Free (liftF)
import Data.CatList (snoc)
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Unit (unit)
import Prelude ((<<<))
import Pux.DOM.Events (DOMEvent)
import Text.Smolder.HTML (style) as E
import Text.Smolder.Markup (MarkupM(Element), Markup, Attr(Attr), text)
import Text.Smolder.Markup (mapEvent) as Smolder


-- | A type for HTML markup, parametized by the events it may trigger. It is a
-- | synonym for the `Markup` monad from
-- | [purescript-smolder](https://pursuit.purescript.org/packages/purescript-smolder).
type HTML ev = Markup (DOMEvent -> ev)

-- | Memoize child view and map event handlers with parent event type.
-- |
-- | It's important that `child` is only used at a top-level declaration and
-- | not inside a view. This is because PureScript is eagerly evaluated like
-- | JavaScript. If `child` is used inside a view it will recreate the memoized
-- | function every time the view is called.
child :: ∀ s a b. (a -> b) -> (s -> HTML a) -> (s -> HTML b)
child f view = memoize $ \s -> mapEvent f (view s)

-- | Map HTML with event type `a` to HTML with event type `b`.
-- |
-- | It's important that `memoize` is only used at a top-level declaration –
-- | not inside a view. This is because PureScript is eagerly evaluated like
-- | JavaScript. If `memoize` is used inside a view it will recreate the memoized
-- | function every time the view is called.

-- | MODIFIED - Changed to use smolder's mapEvent function
mapEvent :: ∀ a b. (a -> b) -> HTML a -> HTML b
mapEvent f = Smolder.mapEvent (\g -> f <<< g)

-- | Memoize view. Uses JavaScript equality to match states.
-- |
-- | It's important that `memoize` is only used at a top-level declaration and
-- | not inside a view. This is because PureScript is eagerly evaluated like
-- | JavaScript. If `memoize` is used inside a view it will recreate the memoized
-- | function every time the view is called.
-- | MODIFIED - Lifted into Free Monad
memoize :: ∀ st ev. (st -> HTML ev) -> (st -> HTML ev)
memoize = memoize_ wrapper
  where
  -- | Wraps memoized vdom in a thunk element that stores the current state
  -- | out-of-band, which allows renderers to cache views by state.
  wrapper s c = liftF $ Element "thunk" c (snoc mempty (Attr "state" s)) mempty unit

foreign import memoize_ :: ∀ st ev. (String -> HTML ev -> HTML ev) -> (st -> HTML ev) -> (st -> HTML ev)

-- | Render CSS stylesheet and return a style element.
style :: ∀ ev. CSS -> HTML ev
style css = E.style $ text $ fromMaybe "" (renderedSheet (render css))
