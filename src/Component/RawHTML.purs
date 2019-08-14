module App.Component.RawHTML where

import Prelude

import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

foreign import unsafeSetInnerHTMLImpl :: EffectFn3 Unit String HTMLElement Unit

unsafeSetInnerHTML :: String -> HTMLElement -> Effect Unit
unsafeSetInnerHTML = runEffectFn3 unsafeSetInnerHTMLImpl unit

type State =
    { elementRef :: H.RefLabel
    , html :: String
    }

type Input = String

data Action
    = SetInnerHTML
    | Receive Input

component :: ∀ m. MonadAff m => H.Component HH.HTML (Const Void) Input Void m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just SetInnerHTML
        }
    }
    where
        initialState :: Input -> State
        initialState html =
            { elementRef: H.RefLabel "html"
            , html
            }

        handleAction :: Action -> H.HalogenM State Action () Void m Unit
        handleAction = case _ of
            SetInnerHTML -> do
                { elementRef } <- H.get
                maybeElement <- H.getHTMLElementRef elementRef

                for_ maybeElement \element -> do
                    { html } <- H.get
                    H.liftEffect $ unsafeSetInnerHTML html element

            Receive html -> do
                H.modify_ _ { html = html }
                handleAction SetInnerHTML

        render :: State -> H.ComponentHTML Action () m
        render state = HH.div [ HP.ref state.elementRef ] []
