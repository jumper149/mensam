{ roots =
    [ "^Main.main$"
    , "^Mensam.Client.main$"
    , "^Mensam.Server.main$"
    -- TODO: Work in progress:
    , "^Mensam.Client.Application.Event.runAppEventT$"
    , "^Mensam.Client.Application.MensamClient.*$"
    , "^Mensam.Client.Debug.runF$"
    , "^Mensam.Server.Booking.spaceGet$"
    , "^Mensam.Server.Database.checkDatabase$"
    , "^Mensam.Server.User.userProfile$"
    ]
, type-class-roots = True
}
