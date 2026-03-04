{-# LANGUAGE RankNTypes #-}

module Test.ReflexSpec(spec) where

import Control.Monad.IO.Class    (liftIO)
import Data.Dependent.Sum        (DSum((:=>)))
import Data.Functor.Identity     (Identity(..))
import Data.IORef                (readIORef)
import Plunder.Grid              (Axial(..))
import Plunder.Shop
import Plunder.State             (UpdateEvts(..))
import Reflex
import Reflex.Host.Class
import Reflex.Spider             (runSpiderHost)
import Test.Hspec

spec :: Spec
spec = describe "Event ordering" $ do

  it "UseItem wins over simultaneous LeftClick when listed first in leftmost" $ do
    result <- runSpiderHost $ do
      (useItemEvt,   useItemRef)   <- newEventWithTriggerRef
      (leftClickEvt, leftClickRef) <- newEventWithTriggerRef
      let item  = MkShopItem 4 ShopHealthPotion
          axial = MkAxial 2 3
          combined = leftmost [ UseItem   <$> useItemEvt    -- correct order
                              , LeftClick <$> leftClickEvt
                              ]
      handle <- subscribeEvent combined
      mt1 <- liftIO $ readIORef useItemRef
      mt2 <- liftIO $ readIORef leftClickRef
      case (mt1, mt2) of
        (Just t1, Just t2) ->
          fireEventsAndRead
            [t1 :=> Identity item, t2 :=> Identity axial]
            (readEvent handle >>= sequence)
        _ -> pure Nothing
    result `shouldBe` Just (UseItem (MkShopItem 4 ShopHealthPotion))

  it "LeftClick-first order swallows UseItem (documents the pre-fix bug)" $ do
    result <- runSpiderHost $ do
      (useItemEvt,   useItemRef)   <- newEventWithTriggerRef
      (leftClickEvt, leftClickRef) <- newEventWithTriggerRef
      let item  = MkShopItem 4 ShopHealthPotion
          axial = MkAxial 2 3
          combined = leftmost [ LeftClick <$> leftClickEvt  -- wrong order (the bug)
                              , UseItem   <$> useItemEvt
                              ]
      handle <- subscribeEvent combined
      mt1 <- liftIO $ readIORef useItemRef
      mt2 <- liftIO $ readIORef leftClickRef
      case (mt1, mt2) of
        (Just t1, Just t2) ->
          fireEventsAndRead
            [t1 :=> Identity item, t2 :=> Identity axial]
            (readEvent handle >>= sequence)
        _ -> pure Nothing
    -- With the wrong order, LeftClick beats UseItem every time
    result `shouldBe` Just (LeftClick (MkAxial 2 3))
