{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Elm where

import Data.Kind

f :: ()
f = ()

data Effect message model = MkEffect (forall screen. model screen)

newtype View message = MkView [message]

--data App model message = MkApp
--  { appModel :: model
--  , appUpdate :: message -> model -> Effect message model
--  , appView :: model -> View message
--  }

data Screen = Screen1 | Screen2

data Model :: Screen -> Type where
  Screen1Model :: Int -> Model Screen1
  Screen2Model :: Char -> Model Screen2

data Message :: Screen -> Type where
  Plus :: Int -> Message Screen1
  ReplaceChar :: Char -> Message Screen2

update :: Message a -> Model a -> Effect (Message b) (Model b)
update message model =
  case message of
    Plus n1 -> case model of
                Screen1Model n2 -> MkEffect $ Screen1Model $ n1 + n2
                Screen2Model _ -> undefined
    ReplaceChar c -> undefined
