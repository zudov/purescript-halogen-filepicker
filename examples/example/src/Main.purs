module Main where

import Prelude
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Console
import Data.Maybe (Maybe(..))
import Data.Functor ((<$))
import Data.Foldable (traverse_)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Forms as E
import Halogen.HTML.Events.Handler (EventHandler(), preventDefault, stopPropagation)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (I)
import Halogen.HTML.Properties.Indexed.Extra as P
import Halogen.HTML.CSS.Indexed (style) as P

import DOM.File.Types (FileList)
import DOM.File.FileList as FileList

import CSS as CSS

import Halogen.FilePicker (FilePicker, filePicker, openFilePicker, initFilePicker, onFilesChange, multiple)

type State
  = { files :: Maybe FileList
    , filePicker :: Maybe FilePicker
    }

initialState :: State
initialState = { files: Nothing, filePicker: Nothing }

data Query a
  = SetFiles (Maybe FileList) a
  | SetFilePicker FilePicker a
  | OpenFilePicker a

ui :: ∀ eff. Component State Query (Aff (HalogenEffects eff))
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render { files: Nothing } =
      H.div_ 
        [ H.button
            [ E.onClick (E.input_ OpenFilePicker) ]
            [ H.text "A nicer looking filepicker" ]
        , filePicker
            [ onFilesChange (E.input (SetFiles <<< Just))
            , P.style do
                CSS.display CSS.displayNone
            , initFilePicker (action <<< SetFilePicker)
            , multiple true
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
      modify (_ { files = files })
    eval (SetFilePicker filePicker next) = next <$ do
      modify (_ { filePicker = Just filePicker })
    eval (OpenFilePicker next) = next <$ do
      gets _.filePicker >>= traverse_ openFilePicker >>> liftEff'
              

main :: Eff _ Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
