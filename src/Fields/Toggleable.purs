module Fields.Toggleable where

import Prelude
import Data.Maybe (Maybe(..))

data Toggleable a
  = Enabled a
  | Disabled a

derive instance functorToggleable :: Functor Toggleable

isToggled :: forall a. Toggleable a -> Boolean
isToggled = case _ of
  Enabled _ -> true
  Disabled _ -> false

toggle :: forall a. Toggleable a -> Toggleable a
toggle = case _ of
  Enabled val -> Disabled val
  Disabled val -> Enabled val

unwrapToggleable :: forall a. Toggleable a -> a
unwrapToggleable = case _ of
  Enabled val -> val
  Disabled val -> val

getEnabled :: forall a. Toggleable a -> Maybe a
getEnabled = case _ of
  Enabled val -> Just val
  _ -> Nothing
