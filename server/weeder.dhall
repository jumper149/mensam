{ roots =
    [ "^Main.main$"
    , "^Mensam.Client.main$"
    , "^Mensam.Server.main$"
    -- TODO: Work in progress:
    , "^Mensam.Client.Application.Event.runAppEventT$"
    , "^Mensam.Client.Application.MensamClient.*$"
    , "^Mensam.Client.Debug.runF$"
    , "^Mensam.Server.Database.Check.checkDatabase$"
    , "^Mensam.Server.Database.Space.spaceListDesks$"
    , "^Mensam.Server.User.userProfile$"
    , "^Mensam.Server.User.userSessionGet$"
    ]
, type-class-roots = True
}
