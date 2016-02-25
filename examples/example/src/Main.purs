module Main where

import Prelude (Unit, ($), bind, unit, pure, const, (>>>), (>>=), show, (<>), (<<<))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Maybe (Maybe(..))
import Data.Functor ((<$), ($>))
import Data.Foldable (traverse_)

import Halogen (HalogenEffects, ComponentDSL, Natural, ComponentHTML, Component,
                runUI, component, modify, liftEff', gets, action)
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler (preventDefault, stopPropagation)
import Halogen.HTML.Indexed as H
import Halogen.HTML.CSS.Indexed (style) as P

import DOM.File.Types (FileList)
import DOM.File.FileList as FileList

import CSS as CSS

import Halogen.FilePicker (FilePicker, filePicker, openFilePicker, initFilePicker, onFilesChange, multiple)
import Halogen.FileDrop (onFilesDrop, onDragEnter, onDragOver, onDragLeave)

type State
  = { files :: Maybe FileList
    , filePicker :: Maybe FilePicker
    , dragover :: Boolean
    }

initialState :: State
initialState = { files: Nothing, filePicker: Nothing, dragover: false }

data Query a
  = SetFiles (Maybe FileList) a
  | SetFilePicker FilePicker a
  | SetDragOver Boolean a
  | OpenFilePicker a


ui :: ∀ eff. Component State Query (Aff (HalogenEffects eff))
ui = component render eval
  where
  render :: State -> ComponentHTML Query
  render { files: Nothing, dragover } =
    H.div_ 
      [ H.button
          [ E.onClick (E.input_ OpenFilePicker) ]
          [ H.text "A nicer looking filepicker" ]
      , filePicker
          [ onFilesChange (E.input (SetFiles <<< Just))
          , multiple true
          , P.style do
              CSS.display CSS.displayNone
          , initFilePicker (action <<< SetFilePicker)
          ]
      , H.div
          [ onDragEnter
              \_ -> preventDefault
                 *> stopPropagation
                 $> action (SetDragOver true)
          , onDragOver
              \_ -> preventDefault
                 *> stopPropagation
                 $> action (SetDragOver true)
          , onDragLeave
              \_ -> preventDefault
                 *> stopPropagation
                 $> action (SetDragOver false)
          , onFilesDrop (E.input (SetFiles <<< Just))
          ]
          [ H.text if dragover
                   then "DROP IT!"
                   else "Or drop them here"
          ]
      ]

  render { files: Just files } =
    H.div_
      [ H.text (show (FileList.length files) <> " file(s) selected")
      , H.button
         [ E.onClick (E.input_ (SetFiles Nothing)) ]
         [ H.text "Try again" ]
      ]


  eval :: Natural Query (ComponentDSL State Query (Aff (HalogenEffects eff)))
  eval (SetFiles files next) = next <$ do
    modify (_ { files = files, dragover = false })
  eval (SetFilePicker filePicker next) = next <$ do
    modify (_ { filePicker = Just filePicker })
  eval (OpenFilePicker next) = next <$ do
    gets _.filePicker >>= traverse_ openFilePicker >>> liftEff'
  eval (SetDragOver dragover next) = next <$ do
    modify (_ { dragover = dragover })


main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
