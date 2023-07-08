{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Elm where

import Data.Kind

data Effect model = forall screen. MkEffect (model screen)

newtype View message = MkView String

data App (model :: screenKind -> Type) (message :: screenKind -> Type) = forall startScreen . MkApp
  { appModel :: model startScreen
  , appUpdate :: forall screen. message screen -> model screen -> Effect model
  , appView :: forall screen. model screen -> View message
  }

data Screen = Screen1 | Screen2

data Model :: Screen -> Type where
  Screen1Model :: Int -> Model Screen1
  Screen2Model :: Char -> Model Screen2

data Message :: Screen -> Type where
  Plus :: Int -> Message Screen1
  ReplaceChar :: Char -> Message Screen2
  SwitchToScreen1 :: Message Screen2
  Reset :: Message a

update :: Message a -> Model a -> Effect Model
update message model =
  case message of
    Plus n1 -> case model of
      Screen1Model n2 -> MkEffect $ Screen1Model $ n1 + n2
    ReplaceChar c -> case model of
      Screen2Model _ -> MkEffect $ Screen2Model c
    SwitchToScreen1 -> case model of
      Screen2Model _ -> MkEffect $ Screen1Model 0
    Reset -> case model of
      Screen1Model _ -> MkEffect $ Screen1Model 0
      Screen2Model _ -> MkEffect $ Screen1Model 0

view :: Model a -> View Message
view model =
  case model of
    Screen1Model n ->
      MkView $
        unlines
          [ "Screen1 with a number: " ++ show n
          , "Add a number (TODO)"
          ]
    Screen2Model c -> MkView $ "Screen2 with a character: " ++ show c

myApp :: App Model Message
myApp = MkApp
    { appModel = initial
    , appUpdate = update
    , appView = view
    }

initial :: Model Screen1
initial = Screen1Model 5
