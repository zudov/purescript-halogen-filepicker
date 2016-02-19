module Halogen.FileDrop
  ( ui
  , initialState
  , defaultOptions
  , Query()
  , State()
  , Options()
  ) where


import Prelude

import Control.Apply
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Eff.Exception (throwException, EXCEPTION)
import Data.Functor
import Data.Maybe
import Data.Foldable

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Core (prop, propName, attrName)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed.Extra as E
import Halogen.HTML.Events.Handler (preventDefault, stopPropagation) as E
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed (style) as P
import Halogen.HTML.Events.Forms.Extra (onFilesChange, onFilesDrop) as E

import DOM (DOM)
import DOM.HTML.Types (htmlElementToEventTarget)
import DOM.Event.EventTarget (dispatchEvent)
import DOM.Event.Types (EventType, MouseEvent, mouseEventToEvent)
import DOM.Event.EventTypes (click) as EventTypes
import DOM.HTML.Types (HTMLElement)
import DOM.File.Types (FileList())

import CSS as CSS

import Unsafe.Coerce (unsafeCoerce)

foreign import newMouseEvent :: EventType -> MouseEvent

data Query a
  = OpenFilePicker a
  | FilesChange FileList a
  | SetUploadElement HTMLElement a
  | SetDragOver Boolean a

type State =
  { uploadElement :: Maybe HTMLElement
  , dragover :: Boolean
  , options :: Options
  }

initialState :: Options -> State
initialState options =
  { uploadElement: Nothing
  , dragover: false
  , options
  }

type Options
  = { view :: { dragover :: Boolean, multiple :: Boolean } -> ComponentHTML Query
    , multiple :: Boolean
    }

defaultOptions :: Options
defaultOptions
  = { view: \({ multiple, dragover }) ->
        H.div_
          [ H.button
            [ E.onClick (E.input_ OpenFilePicker) ]
            [ H.text if multiple
                     then "Select a file"
                     else "Select some files"
            ]
          , H.div
              [ E.onDragEnter (E.input_ (SetDragOver true))
              , E.onDragOver (\_ -> E.preventDefault
                                 *> E.stopPropagation
                                 $> action (SetDragOver true))
              , E.onDragExit (E.input_ (SetDragOver false))
              , E.onFilesDrop (E.input FilesChange)
              ]
              [ H.text (if dragover
                        then "DROP IT!"
                        else "Or just drop it here") ]
          ]
    , multiple: true
    }

multiple :: ∀ r i. Boolean -> P.IProp (multiple :: P.I | r) i
multiple = unsafeCoerce multiple'
  where
    multiple' :: ∀ i. Boolean -> Prop i
    multiple' = prop (propName "multiple") (Just $ attrName "multiple")

ui :: ∀ m eff. (MonadEff (dom :: DOM, err :: EXCEPTION, console :: CONSOLE | eff) m)
   => Component State Query m
ui = component render eval

render :: ∀ m. State -> ComponentHTML Query
render state =
  H.div_
    [ H.input
      [ P.inputType P.InputFile
      , multiple state.options.multiple
      , E.onFilesChange (E.input FilesChange)
      , P.initializer (action <<< SetUploadElement)
      , P.style do
          CSS.display CSS.displayNone
      ]
    , state.options.view
        { dragover: state.dragover
        , multiple: state.options.multiple
        }
    ]

eval :: ∀ m eff. (MonadEff (dom :: DOM, err :: EXCEPTION, console :: CONSOLE | eff) m)
     => Natural Query (ComponentDSL State Query m)
eval (OpenFilePicker next) = next <$ do
  gets _.uploadElement
    >>= traverse_
          (htmlElementToEventTarget
             >>> dispatchEvent
                  (mouseEventToEvent (newMouseEvent EventTypes.click))
             >>> liftEff')
eval (SetUploadElement element next) = next <$ do
  modify (_ { uploadElement = Just element })
eval (FilesChange fileList next) = next <$ do
  liftEff' $ logAny fileList
eval (SetDragOver dragover next) = next <$ do
  modify (_ { dragover = dragover })
