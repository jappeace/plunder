{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( libF
  )
where

import           Control.Lens
import           Control.Monad
import           Data.Foldable          (traverse_)
import           Data.Maybe
import qualified Data.Vector            as Vec
import           Foreign.C.Types
import           GHC.Int
import qualified SDL
import           SDL.Vect
import           Reflex.SDL2

data Assets = Assets
  { _assets_surfaces :: Vec.Vector SDL.Surface
  , _assets_screen   :: SDL.Surface
  , _assets_window   :: SDL.Window
  }
makeLenses ''Assets

data Entity = Entity
  { _ent_surface_id :: Int
  , _ent_pos        :: Point V2 Int32
  , _ent_speed      :: V2 Double
  }
makeLenses ''Entity

readInput :: SDL.EventPayload -> GameAction
readInput SDL.QuitEvent = QuitGame
readInput (SDL.KeyboardEvent evt) =
  if | keyCode == 119 -> MoveAct SpeedUp
     | keyCode == 115 -> MoveAct SpeedDown
     | keyCode == 97 -> MoveAct RoteLeft
     | keyCode == 100 -> MoveAct RoteRight
     | otherwise -> NoOp
  where
    keyCode = SDL.unwrapKeycode $ SDL.keysymKeycode $ SDL.keyboardEventKeysym evt
readInput _             = NoOp

data GameAction = MoveAct Movement
                | QuitGame
                | NoOp

data Movement
  = RoteLeft
  | RoteRight
  | SpeedUp
  | SpeedDown

data GameState = GameState
  { _state_is_exiting :: Bool
  , _state_main_char  :: Entity
   }
makeLenses ''GameState

handleInput :: GameAction -> GameState -> GameState
handleInput QuitGame state      = state & state_is_exiting .~ True
handleInput (MoveAct RoteLeft) state = state & state_main_char . ent_pos .~ (P $ V2 30 30)
handleInput (MoveAct RoteRight) state = state & state_main_char . ent_pos .~ (P $ V2 40 40)
handleInput (MoveAct _ ) state = state
handleInput NoOp state          = state

gameLoop :: Assets -> GameState -> IO ()
gameLoop assets curState = do
  events <- SDL.pollEvents
  let gameEvts = readInput . SDL.eventPayload <$> events
      gameEvts :: [GameAction]
      nextState = foldr handleInput curState gameEvts
  traverse_
    (\xOut -> SDL.surfaceBlit xOut Nothing (assets ^. assets_screen) Nothing) $
    assets ^. assets_surfaces

  SDL.surfaceBlit (fromMaybe (error "can't load character") $ (assets ^. assets_surfaces) Vec.!? (mainChar ^. ent_surface_id)) Nothing (assets ^. assets_screen) $ Just (mainChar ^. ent_pos . to (fmap CInt))

  SDL.updateWindowSurface $ assets ^. assets_window
  unless (nextState ^. state_is_exiting) $ gameLoop assets nextState
  where
    mainChar = curState ^. state_main_char

screenWidth :: CInt
screenWidth = 640

screenHeight :: CInt
screenHeight = 480

libF :: IO ()
libF = do
  SDL.initialize [SDL.InitVideo]
  window <-
    SDL.createWindow
      "SDL Tutorial"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window
  bg <- SDL.loadBMP "img/start.bmp"
  mainChar <- SDL.loadBMP "img/main-char.bmp"
  gameLoop
    (Assets
      { _assets_surfaces = Vec.fromList [bg, mainChar]
      , _assets_screen = screenSurface
      , _assets_window = window
      }) $
        GameState
        { _state_is_exiting = False
        , _state_main_char = Entity {
            _ent_surface_id = 1
            , _ent_pos        = P $ V2 20 20
            , _ent_speed      = V2 0 0
                                    }
        }
  SDL.destroyWindow window
  SDL.quit
