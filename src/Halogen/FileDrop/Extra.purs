module Halogen.HTML.Events.Indexed.Extra
  ( module Halogen.HTML.Events.Indexed
  , onDragEnter
  , onDragOver
  , onDragEnd
  , onDragExit
  ) where

import Halogen.HTML.Events (EventProp)
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Core (handler, eventName)

import Unsafe.Coerce (unsafeCoerce)

onDragEnter :: ∀ r i. E.IEventProp r () i
onDragEnter = unsafeCoerce onDragEnter'
  where
    onDragEnter' :: EventProp () i
    onDragEnter' = handler (eventName "dragenter")

onDragOver :: ∀ r i. E.IEventProp r () i
onDragOver = unsafeCoerce onDragOver'
  where
    onDragOver' :: EventProp () i
    onDragOver' = handler (eventName "dragover")

onDragEnd :: ∀ r i. E.IEventProp r () i
onDragEnd = unsafeCoerce onDragEnd'
  where
    onDragEnd' :: EventProp () i
    onDragEnd' = handler (eventName "dragend")

onDragExit :: ∀ r i. E.IEventProp r () i
onDragExit = unsafeCoerce onDragExit'
  where
    onDragExit' :: EventProp () i
    onDragExit' = handler (eventName "dragexit")

module Halogen.HTML.Events.Forms.Extra
  ( onFilesChange
  , onFilesDrop
  , module Halogen.HTML.Events.Forms.Extra
  ) where

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Foreign (unsafeReadTagged, toForeign)
import Data.Foreign.Index (prop)
import Prelude (map, (<<<), Unit(), pure, const, (<$>))

import Halogen.HTML.Events.Forms.Extra as E
import Halogen.HTML.Core (Prop(), eventName, handler')
import Halogen.HTML.Events.Handler (EventHandler(), preventDefault, stopPropagation)
import Halogen.HTML.Properties.Indexed (IProp, I)

import DOM.File.Types (FileList())
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace

onFilesChange
  :: ∀ r i.
     (FileList -> EventHandler i)
  -> IProp (onChange :: I | r) i
onFilesChange f = unsafeCoerce onFilesChange'
  where
  onFilesChange' = handler' (eventName "change") handle
    where
    handle { target } = either (const (pure Nothing)) (map Just <<< f) fileList
      where
      fileList = unsafeReadTagged "FileList" =<< prop "files" (toForeign target)

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

module Halogen.HTML.Properties.Indexed.Extra
  ( module Halogen.HTML.Properties.Indexed
  , multiple
  ) where

import Prelude (($))
import Data.Maybe (Maybe(..))

import Halogen (Prop)
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Core (prop, propName, attrName)

import Unsafe.Coerce (unsafeCoerce)

multiple :: ∀ r i. Boolean -> P.IProp (multiple :: P.I | r) i
multiple = unsafeCoerce multiple'
  where
    multiple' :: ∀ i. Boolean -> Prop i
    multiple' = prop (propName "multiple") (Just $ attrName "multiple")
