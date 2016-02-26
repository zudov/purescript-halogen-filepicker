module Halogen.FilePicker
  ( FilePicker()
  , FilePickerProps()
  , filePicker
  , openFilePicker
  , initFilePicker
  , onFilesChange
  ) where

import Prelude (Unit, void, (>>>), (<<<), map, pure, const, ($), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Bind ((=<<))
import Data.Foreign (unsafeReadTagged, toForeign)
import Data.Foreign.Index (prop) as Foreign
import Data.Either (either)
import Data.Maybe (Maybe(..))

import Halogen (Prop, HTML)
import Halogen.HTML.Core (handler', eventName, propName, attrName, prop)
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed (I)
import Halogen.HTML.Properties.Indexed as P

import DOM.File.Types (FileList)
import DOM.HTML.Types (HTMLElement, htmlElementToEventTarget)
import DOM.Event.EventTarget (dispatchEvent)
import DOM.Event.EventTypes (click) as EventTypes
import DOM.Event.Types (EventType, MouseEvent, mouseEventToEvent)
import DOM (DOM)

import Unsafe.Coerce (unsafeCoerce)

-- | Creates an `input` with `type=file`. You can pass it anything
-- | that you pass to normal `input` + a few additional properties that are
-- | defined in this module.
filePicker
  :: ∀ p f.
     Array (P.IProp FilePickerProps (f Unit))
  -> HTML p f
filePicker props =
  H.input
    ([ P.inputType P.InputFile ] <> unsafeCoerce props)

-- | Gives a `FileList` to the handler every time the user selects the files from
-- | the file picker.
onFilesChange
  :: ∀ r i.
     (FileList -> EventHandler i)
  -> P.IProp (onChange :: I, filePicker :: I | r) i
onFilesChange f = unsafeCoerce onFilesChange'
  where
  onFilesChange' = handler' (eventName "change") handle
    where
    handle { target } = either (const (pure Nothing)) (map Just <<< f) fileList
      where
      fileList = unsafeReadTagged "FileList" =<< Foreign.prop "files" (toForeign target)


-- | A reference to the `input` element which can be used to open file picker
-- | programatically.
newtype FilePicker
  = FilePicker HTMLElement


-- | An initializer which allows to receive a reference to the file picker.
initFilePicker
  :: ∀ r i.
     (FilePicker -> i)
  -> P.IProp (filePicker :: I, initializer :: I | r) i
initFilePicker f = P.initializer (f <<< FilePicker)

runFilePicker :: FilePicker -> HTMLElement
runFilePicker (FilePicker element) = element

-- | Opens a file picker by dispatching a 'click' event to a referenced input element.
openFilePicker :: ∀ eff. FilePicker -> Eff (dom :: DOM, err :: EXCEPTION | eff) Unit
openFilePicker = runFilePicker
   >>> htmlElementToEventTarget
   >>> dispatchEvent (mouseEventToEvent (newMouseEvent EventTypes.click))
   >>> void

foreign import newMouseEvent :: EventType -> MouseEvent


type FilePickerProps
  = ( --inputType :: I -- Not allowed. File picker is always `type=file`
      style :: I
    , onBlur :: I
    , onFocus :: I
    , onFocusIn :: I
    , onFocusOut :: I
    , onKeyDown :: I
    , onKeyUp :: I
    , onKeyPress :: I
    , onDoubleClick :: I
    , onClick :: I
    , onMouseDown :: I
    , onMouseEnter :: I
    , onMouseLeave :: I
    , onMouseMove :: I
    , onMouseOver :: I
    , onMouseOut :: I
    , onMouseUp :: I
    , id :: I
    , name :: I
    , title :: I
    , "class" :: I
    , spellcheck :: I
    , key :: I
    , initializer :: I
    , filePicker :: I
    , finalizer :: I
    , onContextMenu :: I
    , accept :: I
    , autocomplete :: I
    , autofocus :: I
    , checked :: I
    , disabled :: I
    , form :: I
    , formaction :: I
    , formenctype :: I
    , formmethod :: I
    , formnovalidate :: I
    , formtarget :: I
    , height :: I
    , list :: I
    , max :: I
    , min :: I
    , multiple :: I
    , onAbort :: I
    , onChange :: I
    , onError :: I
    , onInput :: I
    , onInvalid :: I
    , onLoad :: I
    , onSearch :: I
    , onSelect :: I
    , pattern :: I
    , placeholder :: I
    , readonly :: I
    , required :: I
    , size :: I
    , src :: I
    , step :: I
    , value :: I
    , width :: I
    )
