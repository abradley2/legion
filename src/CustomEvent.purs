module CustomEvent where

import Prelude
import Effect (Effect)
import Foreign.Object (Object)
import Web.DOM.Document (Document)
import Web.DOM.Document as Document
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.CustomEvent as CustomEvent
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

foreign import customEvent :: forall a. String -> Object a -> CustomEvent

getDocument :: Effect Document
getDocument = window >>= document >>= (toDocument >>> pure)

dispatchDocumentEvent :: CustomEvent -> Effect Unit
dispatchDocumentEvent e = do
  doc <- getDocument
  void $ dispatchEvent (CustomEvent.toEvent e) (Document.toEventTarget doc)
