module Pux.Renderer.React
  ( dangerouslySetInnerHTML
  , renderToDOM
  , renderToString
  , renderToStaticMarkup
  , renderToReact
  , reactClass
  , reactClassWithProps
    -- | MODIFIED
  , addLifeCycles
    -- | MODIFIED
  , lifeCycles
    -- | MODIFIED
  , ReactLifeCycles
    -- | MODIFIED
  , ref
    -- | MODIFIED
  , getRef
    -- | MODIFIED
  , addRef
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Free (foldFree, liftF)
import Control.Monad.State (State, execState, state)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import Data.Array (snoc)
import Data.CatList (CatList)
import Data.CatList (snoc) as CatList
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.NaturalTransformation (NaturalTransformation)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.StrMap (StrMap)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (data_)
import React (ComponentDidMount, ComponentDidUpdate, ComponentWillMount, ComponentWillReceiveProps, ComponentWillUnmount, ComponentWillUpdate, ReactClass, ReactElement, ReactProps, ReactRefs, ReactThis, ReadOnly, ShouldComponentUpdate)
import Signal (Signal, (~>))
import Signal.Channel (CHANNEL, Channel, channel, send)
import Text.Smolder.Markup (Attr(..), Attribute, EventHandler(EventHandler), Markup, MarkupM(..), attribute, parent, (!))

-- | ```purescript
-- | main = do
-- |  app <- start
-- |    { initialState
-- |    , view
-- |    , foldp
-- |    , inputs: [] }
-- |
-- |  renderToDOM "#app" app.markup app.input
-- | ```
renderToDOM :: ∀ ev fx
               .  String
               -> Signal (HTML ev)
               -> Channel (List ev)
               -> Eff (channel :: CHANNEL | fx) Unit
renderToDOM selector markup input =
  renderToDOM_ selector =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned
-- | includes React-specific attributes for fast mounting in the browser.
renderToString :: ∀ ev fx
                  .  Signal (HTML ev)
                  -> Eff (channel :: CHANNEL | fx) String
renderToString markup = do
  input <- channel Nil
  renderToString_ =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned is
-- | stripped of all React-specific attributes.
renderToStaticMarkup :: ∀ ev fx
                        .  Signal (HTML ev)
                        -> Eff (channel :: CHANNEL | fx) String
renderToStaticMarkup markup = do
  input <- channel Nil
  renderToStaticMarkup_ =<< renderToReact markup input

-- | Return a ReactClass from a component's HTML signal.
renderToReact :: ∀ ev props fx
                 .  Signal (HTML ev)
                 -> Channel (List ev)
                 -> Eff (channel :: CHANNEL | fx) (ReactClass props)
renderToReact markup input =
  pure $ toReact $ markup ~> renderNodes (reactHandler (hook input))

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class.
reactClass :: ∀ ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
reactClass component key' = \children ->
  registerClass component key'
    $ parent "reactclass" children ! (data_ "pux-react-class" key')

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class. The returned constructor
-- | takes an arbitrary props argument, which will be passed to the React class
-- | when rendered.
reactClassWithProps :: ∀ ev props. ReactClass props -> String -> (props -> HTML ev -> HTML ev)
reactClassWithProps component key' = \props children ->
  registerClass component key'
    $ parent "reactclass" children
      ! registerProps props (data_ "pux-react-props") ! data_ "pux-react-class" key'

dangerouslySetInnerHTML :: String -> Attribute
dangerouslySetInnerHTML = attribute "dangerouslySetInnerHTML"

-- | MODIFIED
ref :: String -> Attribute
ref = attribute "ref"

-- | MODIFIED
type ReactLifeCycles eff
  = { componentWillMount        :: Maybe (ComponentWillMount Unit Unit eff)
    , componentDidMount         :: Maybe (ComponentDidMount Unit Unit eff)
    , componentWillReceiveProps :: Maybe (ComponentWillReceiveProps Unit Unit eff)
    , shouldComponentUpdate     :: Maybe (ShouldComponentUpdate Unit Unit eff)
    , componentWillUpdate       :: Maybe (ComponentWillUpdate Unit Unit eff)
    , componentDidUpdate        :: Maybe (ComponentDidUpdate Unit Unit eff)
    , componentWillUnmount      :: Maybe (ComponentDidUpdate Unit Unit eff)
    }

-- | MODIFIED
lifeCycles :: ∀ eff. ReactLifeCycles eff
lifeCycles =
  { componentWillMount        : Nothing
  , componentDidMount         : Nothing
  , componentWillReceiveProps : Nothing
  , shouldComponentUpdate     : Nothing
  , componentWillUpdate       : Nothing
  , componentDidUpdate        : Nothing
  , componentWillUnmount      : Nothing
  }

-- | MODIFIED
addLifeCycles :: ∀ eff ev.  ReactLifeCycles eff -> HTML ev -> HTML ev
addLifeCycles = wrapLifeCycles toNullable wrapped
  where
  wrapped l c = liftF $ Element "lifecycle" c (CatList.snoc mempty (Attr "lifecycle" l)) mempty unit


getRef :: ∀ p s eff. String -> ReactThis p s -> Eff (dom :: DOM, refs :: ReactRefs ReadOnly | eff) (Maybe HTMLElement)
getRef name ctx = map toMaybe $ getRefImp name ctx

-- | MODIFIED
foreign import wrapLifeCycles :: ∀ eff ev a. (Maybe a -> Nullable a) -> (String -> HTML ev -> HTML ev) -> ReactLifeCycles eff -> HTML ev -> HTML ev
foreign import getRefImp :: ∀ p s eff. String -> ReactThis p s -> Eff (dom :: DOM, refs :: ReactRefs ReadOnly | eff) (Nullable HTMLElement)
foreign import addRef :: ∀ p s eff. ReactThis p s -> Eff (props :: ReactProps | eff) Unit

foreign import toReact :: ∀ props. Signal (Array ReactElement) -> ReactClass props
foreign import registerClass :: ∀ ev props. ReactClass props -> String -> HTML ev -> HTML ev
foreign import registerProps :: ∀ props. props -> (String -> Attribute) -> Attribute
foreign import renderToDOM_ :: ∀ props fx. String -> ReactClass props -> Eff fx Unit
foreign import renderToString_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import renderToStaticMarkup_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import reactElement :: Fn3 String (StrMap ReactAttribute) (Nullable (Array ReactElement)) ReactElement
foreign import reactText :: String -> ReactElement
foreign import reactHandler :: ∀ a e fx. (a -> Eff (channel :: CHANNEL | fx) Unit) -> e -> ReactAttribute
foreign import reactAttr :: String -> ReactAttribute

foreign import data ReactAttribute :: Type

-- | MODIFIED - for Free Monad in Smolder
renderItem :: ∀ e. (e -> ReactAttribute) -> NaturalTransformation (MarkupM e) (State (Array ReactElement))
renderItem input (Element n c a e r) =
  let kids = renderNodes input c
      el   = runFn3 reactElement n (renderAttrs input a e) (toNullable (Just kids))
  in  state \s -> Tuple r $ snoc s el

renderItem input (Content t r) = state \s -> Tuple r $ snoc s $ reactText t
renderItem input (Empty r) = pure r

-- | MODIFIED - for Free Monad in Smolder
renderNodes :: ∀ e. (e -> ReactAttribute) -> Markup e -> Array ReactElement
renderNodes input m = execState (foldFree (renderItem input) m) []

renderAttrs :: ∀ e. (e -> ReactAttribute) -> CatList Attr -> CatList (EventHandler e) -> StrMap ReactAttribute
renderAttrs input attrs handlers = StrMap.fromFoldable tuples
  where
  tuples = map toTupleA attrs <> map toTupleH handlers
  toTupleH (EventHandler key value) = Tuple key (input value)
  toTupleA (Attr key value) = Tuple key (reactAttr value)

hook :: ∀ a fx. Channel (List a) -> (a -> Eff (channel :: CHANNEL | fx) Unit)
hook input = \a -> do
  send input (singleton a)
