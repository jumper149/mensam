{ roots =
    [ "^Main.main$"
    , "^Mensam.Client.main$"
    , "^Mensam.Server.main$"
    -- TODO: Work in progress:
    , "^Mensam.Client.Application.Event.runAppEventT$"
    , "^Mensam.Client.Application.MensamClient.*$"
    , "^Mensam.Client.Debug.runF$"
    , "^Mensam.Server.Database.checkDatabase$"
    , "^Mensam.Server.Booking.spaceGet$"
    , "^Mensam.Server.Booking.spaceUserPermissionRevoke$"
    , "^Mensam.Server.User.userProfile$"
    , "^Mensam.Server.User.userSessionGet$"
    ]
, type-class-roots = True
}
