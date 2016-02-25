module Halogen.FileDrop where

import Prelude ((<$>), pure, const)

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Foreign (unsafeReadTagged, toForeign)
import Data.Foreign.Index (prop)

import Halogen.HTML.Core (eventName, handler, handler')
import Halogen.HTML.Events.Handler (EventHandler(), preventDefault, stopPropagation)
import Halogen.HTML.Properties.Indexed (IProp)
import Halogen.HTML.Events (EventProp)
import Halogen.HTML.Events.Indexed (IEventProp)

import DOM.File.Types (FileList())
import Unsafe.Coerce (unsafeCoerce)

-- | TODO: Move to purescript-halogen
onFilesDrop
  :: ∀ r i.
     (FileList -> EventHandler i)
  -> IProp r i
onFilesDrop f = unsafeCoerce onFilesDrop'
  where
  onFilesDrop' = handler' (eventName "drop") handle
    where
    handle { dataTransfer } =
      either
        (const (pure Nothing))
        (\files -> preventDefault
                *> stopPropagation
                *> (Just <$> f files))
        (unsafeReadTagged "FileList" =<< prop "files" (toForeign dataTransfer))

-- | TODO: Move to purescript-halogen
onDragEnter :: ∀ r i. IEventProp r () i
onDragEnter = unsafeCoerce onDragEnter'
  where
    onDragEnter' :: EventProp () i
    onDragEnter' = handler (eventName "dragenter")

-- | TODO: Move to purescript-halogen
onDragLeave :: ∀ r i. IEventProp r () i
onDragLeave = unsafeCoerce onDragLeave'
  where
    onDragLeave' :: EventProp () i
    onDragLeave' = handler (eventName "dragleave")

-- | TODO: Move to purescript-halogen
onDragOver :: ∀ r i. IEventProp r () i
onDragOver = unsafeCoerce onDragOver'
  where
    onDragOver' :: EventProp () i
    onDragOver' = handler (eventName "dragover")

-- | TODO: Move to purescript-halogen
onDragEnd :: ∀ r i. IEventProp r () i
onDragEnd = unsafeCoerce onDragEnd'
  where
    onDragEnd' :: EventProp () i
    onDragEnd' = handler (eventName "dragend")

-- | TODO: Move to purescript-halogen
onDragExit :: ∀ r i. IEventProp r () i
onDragExit = unsafeCoerce onDragExit'
  where
    onDragExit' :: EventProp () i
    onDragExit' = handler (eventName "dragexit")
