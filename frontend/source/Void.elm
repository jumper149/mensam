module Void exposing (Void, absurd)


type Void
    = MkVoidInternal Void


absurd : Void -> a
absurd (MkVoidInternal void) =
    absurd void
