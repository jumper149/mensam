module Mensam.Client.UI.Brick.AttrMap where

import Brick
import Brick.Forms
import Brick.Widgets.List
import Graphics.Vty

attrDefault :: Attr
attrDefault =
  Attr
    { attrStyle = Default
    , attrForeColor = SetTo brightWhite
    , attrBackColor = SetTo black
    , attrURL = Default
    }

attrsDefault :: AttrMap
attrsDefault =
  attrMap
    attrDefault
    [ (formAttr, attrDefault)
    , (focusedFormInputAttr, attrDefault {attrBackColor = SetTo brightBlack})
    , (invalidFormInputAttr, attrDefault {attrForeColor = SetTo brightRed})
    , (listAttr, attrDefault)
    , (listSelectedAttr, attrDefault `withStyle` standout)
    ]

attrBackground :: Attr
attrBackground =
  attrDefault
    { attrForeColor = SetTo white
    , attrBackColor = SetTo brightBlack
    }

attrsBackground :: AttrMap
attrsBackground =
  attrMap
    attrBackground
    [ (formAttr, attrBackground)
    , (focusedFormInputAttr, attrBackground {attrBackColor = SetTo brightBlack})
    , (invalidFormInputAttr, attrBackground {attrForeColor = SetTo brightRed})
    , (listAttr, attrBackground)
    , (listSelectedAttr, attrBackground `withStyle` standout)
    ]
