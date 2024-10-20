{-# LANGUAGE TemplateHaskell #-}

module Mensam.Client.UI.Desks where

import Mensam.API.Aeson
import Mensam.API.Data.Desk
import Mensam.API.Data.Reservation
import Mensam.API.Data.Space
import Mensam.API.Route.Api.Reservation qualified as Route.Reservation
import Mensam.API.Route.Api.Space qualified as Route.Space
import Mensam.Client.Application
import Mensam.Client.Application.Event.Class
import Mensam.Client.UI.Brick.Draw
import Mensam.Client.UI.Brick.Events
import Mensam.Client.UI.Brick.Names

import Brick
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Kind
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time qualified as T
import Data.Time.Format.ISO8601 qualified as T
import Graphics.Vty.Input.Events
import Lens.Micro.Platform

desksListInitial :: GenericList ClientName Seq.Seq Route.Space.DeskWithInfo
desksListInitial =
  list
    ClientNameSpacesList
    mempty
    1

type NewDeskInfo :: Type
newtype NewDeskInfo = MkNewDeskInfo
  { _newDeskInfoName :: T.Text
  }
makeLenses ''NewDeskInfo

newDeskFormInitial :: Form NewDeskInfo e ClientName
newDeskFormInitial =
  newForm
    [ (str "Name: " <+>) @@= editTextField newDeskInfoName ClientNameDesksNewDeskName (Just 1)
    ]
    MkNewDeskInfo
      { _newDeskInfoName = ""
      }

type NewReservationInfo :: Type
data NewReservationInfo = MkNewReservationInfo
  { _newReservationInfoDesk :: T.Text
  , _newReservationInfoTimeBegin :: T.Text
  , _newReservationInfoTimeEnd :: T.Text
  }
makeLenses ''NewReservationInfo

newReservationFormInitial :: Desk -> Form NewReservationInfo e ClientName
newReservationFormInitial desk =
  newForm
    [ (str "Desk: " <+>) @@= editTextField newReservationInfoDesk ClientNameDesksNewReservationDesk (Just 1)
    , (str "Begin: " <+>)
        @@= editField
          newReservationInfoTimeBegin
          ClientNameDesksNewReservationTimeBegin
          (Just 1)
          id
          ( \case
              [line] -> (const line <$>) . T.iso8601ParseM @_ @T.TimeOfDay . T.unpack $ line
              _ -> Nothing
          )
          (txt . T.intercalate "\n")
          id
    , (str "End: " <+>)
        @@= editField
          newReservationInfoTimeEnd
          ClientNameDesksNewReservationTimeEnd
          (Just 1)
          id
          ( \case
              [line] -> (const line <$>) . T.iso8601ParseM @_ @T.TimeOfDay . T.unpack $ line
              _ -> Nothing
          )
          (txt . T.intercalate "\n")
          id
    ]
    MkNewReservationInfo
      { _newReservationInfoDesk = T.pack $ show $ deskId desk
      , _newReservationInfoTimeBegin = ""
      , _newReservationInfoTimeEnd = ""
      }

type ScreenDesksState :: Type
data ScreenDesksState = MkScreenDesksState
  { _screenStateDesksSpace :: Space
  , _screenStateDesksList :: GenericList ClientName Seq.Seq Route.Space.DeskWithInfo
  , _screenStateDesksShowHelp :: Bool
  , _screenStateDesksCreateReservation :: Maybe (Form NewReservationInfo ClientEvent ClientName)
  , _screenStateDesksPreviewDay :: T.Day
  , _screenStateDesksTimezone :: T.TimeZone
  , _screenStateDesksNewDeskForm :: Maybe (Form NewDeskInfo ClientEvent ClientName)
  }
makeLenses ''ScreenDesksState

desksDraw :: ScreenDesksState -> [Widget ClientName]
desksDraw = \case
  s@MkScreenDesksState {_screenStateDesksShowHelp = True} ->
    [ centerLayer $
        borderWithLabel (txt "Help") $
          cropRightTo 80 $
            txt
              "? - Toggle Help\n\
              \r - Refresh Desks\n\
              \c - Create new Desk\n\
              \Enter - Toggle reservations for a desk\n\
              \"
    ]
      <> desksDraw s {_screenStateDesksShowHelp = False}
  s@MkScreenDesksState {_screenStateDesksCreateReservation = Just form} ->
    [ centerLayer $ borderWithLabel (txt "New Reservation") $ cropRightTo 80 $ renderForm form
    ]
      <> desksDraw (s {_screenStateDesksCreateReservation = Nothing})
  s@MkScreenDesksState {_screenStateDesksNewDeskForm = Just form} ->
    [ centerLayer $ borderWithLabel (txt "New Desk") $ cropRightTo 80 $ renderForm form
    ]
      <> desksDraw (s {_screenStateDesksNewDeskForm = Nothing})
  MkScreenDesksState
    { _screenStateDesksSpace = space
    , _screenStateDesksList = desksWithInfo
    , _screenStateDesksPreviewDay = day
    , _screenStateDesksTimezone = tz
    } ->
      [ vBox
          [ joinBorders $
              borderWithLabel (txt $ "Desks (" <> unNameSpace (spaceName space) <> ")") $
                vBox
                  [ padBottom Max $
                      padRight Max $
                        renderList (\_focus (Route.Space.MkDeskWithInfo {Route.Space.deskWithInfoDesk, Route.Space.deskWithInfoReservations}) -> padRight Max $ txt $ T.pack ("#" <> show (unIdentifierDesk $ deskId deskWithInfoDesk) <> " ") <> unNameDesk (deskName deskWithInfoDesk) <> ": " <> T.pack (show deskWithInfoReservations)) True desksWithInfo
                  , hBorder
                  , let reservations =
                          case listSelectedElement desksWithInfo of
                            Nothing -> []
                            Just (_index, desk) -> Route.Space.deskWithInfoReservations desk
                     in vLimit 3 $ hCenter $ viewport ClientNameDesksReservationsViewport Horizontal $ visible $ txt $ prettyReservations tz day reservations
                  ]
          , padLeft Max $ txt footerMenuHelp
          ]
      ]

desksHandleEvent :: BrickEvent ClientName ClientEvent -> ApplicationT (EventM ClientName ScreenDesksState) ()
desksHandleEvent event = do
  s <- lift get
  case formState <$> _screenStateDesksNewDeskForm s of
    Nothing ->
      case formState <$> _screenStateDesksCreateReservation s of
        Nothing ->
          case event of
            VtyEvent (EvKey (KChar '?') []) -> lift $ put s {_screenStateDesksShowHelp = not $ _screenStateDesksShowHelp s}
            VtyEvent (EvKey (KChar 'r') []) -> sendEvent . ClientEventSwitchToScreenDesks . _screenStateDesksSpace =<< lift get
            VtyEvent (EvKey (KChar 'c') []) -> lift $ screenStateDesksNewDeskForm %= const (Just newDeskFormInitial)
            VtyEvent (EvKey KEnter []) -> lift $ put $ s {_screenStateDesksCreateReservation = newReservationFormInitial . Route.Space.deskWithInfoDesk . snd <$> listSelectedElement (_screenStateDesksList s)}
            VtyEvent e -> lift $ zoom screenStateDesksList $ handleListEvent e
            _ -> pure ()
        Just newReservationInfo ->
          case event of
            VtyEvent (EvKey KEnter []) ->
              sendEvent $
                ClientEventSendRequestCreateReservation
                  (_screenStateDesksSpace s)
                  Route.Reservation.MkRequestReservationCreate
                    { Route.Reservation.requestReservationCreateDesk = Identifier $ read $ T.unpack $ _newReservationInfoDesk newReservationInfo
                    , Route.Reservation.requestReservationCreateTimeWindow =
                        let toUTC text =
                              T.zonedTimeToUTC
                                T.ZonedTime
                                  { T.zonedTimeToLocalTime =
                                      T.LocalTime
                                        { T.localDay = _screenStateDesksPreviewDay s
                                        , T.localTimeOfDay = fromJust $ T.iso8601ParseM @_ @T.TimeOfDay $ T.unpack text
                                        }
                                  , T.zonedTimeZone = _screenStateDesksTimezone s
                                  }
                         in MkIntervalNonDegenerateUnsafe $
                              MkIntervalUnsafe
                                { intervalStart = toUTC $ _newReservationInfoTimeBegin newReservationInfo
                                , intervalEnd = toUTC $ _newReservationInfoTimeEnd newReservationInfo
                                }
                    }
            _ -> lift $ zoom (screenStateDesksCreateReservation . _Just) $ handleFormEvent event
    Just newDeskInfo ->
      case event of
        VtyEvent (EvKey KEnter []) ->
          sendEvent $
            ClientEventSendRequestCreateDesk
              (_screenStateDesksSpace s)
              Route.Space.MkRequestDeskCreate
                { Route.Space.requestDeskCreateName = MkNameDesk $ newDeskInfo ^. newDeskInfoName
                , Route.Space.requestDeskCreateSpace = Identifier $ spaceId $ _screenStateDesksSpace s
                , Route.Space.requestDeskCreateLocation = Nothing
                }
        _ -> lift $ zoom (screenStateDesksNewDeskForm . _Just) $ handleFormEvent event

prettyReservations :: T.TimeZone -> T.Day -> [Reservation] -> T.Text
prettyReservations tz day reservations = T.pack out
 where
  hours :: [Integer] = [0 .. 23]
  minutes :: [Integer] = [0, 15, 30, 45]
  diffTimes = T.secondsToDiffTime . (60 *) <$> concatMap (\hour -> (hour * 60 +) <$> minutes) hours
  diffTimeWindows = zip diffTimes ((+ T.secondsToDiffTime (15 * 60)) <$> diffTimes)
  toUTC diffTime =
    T.zonedTimeToUTC
      T.ZonedTime
        { T.zonedTimeToLocalTime =
            T.LocalTime
              { T.localDay = day
              , T.localTimeOfDay = T.timeToTimeOfDay diffTime
              }
        , T.zonedTimeZone = tz
        }
  utcTimeWindows = bimap toUTC toUTC <$> diffTimeWindows
  utcTimeWindowReservedByOne (begin, end) MkReservation {reservationTimeBegin, reservationTimeEnd} =
    reservationTimeBegin < end && reservationTimeEnd >= begin
  utcTimeWindowReservedByAny window = any (utcTimeWindowReservedByOne window) reservations
  reserved = utcTimeWindowReservedByAny <$> utcTimeWindows
  markers = (\x -> if x then '▀' else ' ') <$> reserved
  out =
    unlines
      [ "0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24"
      , "┠───┴───┴───┼───┴───┴───╂───┴───┴───┼───┴───┴───╂───┴───┴───┼───┴───┴───╂───┴───┴───┼───┴───┴───┨"
      , markers
      ]
