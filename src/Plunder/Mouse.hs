{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Plunder.Mouse
  ( leftClick
  , rightClick
  , mouseButtons
  , mousePositions
  , mouseMotion
  , _Pressed
  , calcMouseClickAxial
  , isClickInPanel
  ) where

import           Control.Lens
import           Data.Generics.Product
import           Data.Generics.Sum
import           Data.Int
import           Foreign.C.Types (CInt)
import           Plunder.Grid
import           Reflex.SDL2


leftClick :: Prism' MouseButton ()
leftClick = _Ctor @"ButtonLeft"

rightClick :: Prism' MouseButton ()
rightClick = _Ctor @"ButtonRight"

mouseButtons :: Lens' MouseButtonEventData MouseButton
mouseButtons = field @"mouseButtonEventButton"

mousePositions :: Lens' MouseButtonEventData (Point V2 Int32)
mousePositions = field @"mouseButtonEventPos"

mouseMotion :: Lens' MouseButtonEventData InputMotion
mouseMotion = field @"mouseButtonEventMotion"

_Pressed :: Prism' InputMotion ()
_Pressed = _Ctor @"Pressed"

calcMouseClickAxial :: MouseButtonEventData -> Axial
calcMouseClickAxial = pixelToAxial . fmap fromIntegral . view mousePositions

-- | Returns 'True' when the click lands in the bottom panel area
-- (Y coordinate >= windowHeight - panelH).
isClickInPanel :: CInt -> V2 CInt -> MouseButtonEventData -> Bool
isClickInPanel panelH (V2 _ winH) mbd =
  let P (V2 _ y) = mbd ^. mousePositions
  in fromIntegral y >= winH - panelH
