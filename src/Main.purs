module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (throwException, EXCEPTION)
import Data.Functor
import Data.Maybe
import Data.Foldable

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed (style) as P
import Halogen.HTML.Events.Forms.Extra (onFilesChange) as E

import DOM (DOM)
import DOM.HTML.Types (htmlElementToEventTarget)
import DOM.Event.EventTarget (dispatchEvent)
import DOM.Event.Types (EventType, MouseEvent, mouseEventToEvent)
import DOM.Event.EventTypes (click) as EventTypes
import DOM.HTML.Types (HTMLElement)
import DOM.File.Types (FileList())

import CSS as CSS

import Halogen.FileDrop

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui $ initialState defaultOptions
  onLoad $ appendToBody app.node
