module Views.Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


size0 : Int
size0 =
    0


size1 : Int
size1 =
    1


size2 : Int
size2 =
    2


size3 : Int
size3 =
    4


size4 : Int
size4 =
    8


size5 : Int
size5 =
    6


length0 : Length
length0 =
    px 0


length1 : Length
length1 =
    px 1


length2 : Length
length2 =
    px 2


length3 : Length
length3 =
    px 4


length4 : Length
length4 =
    px 8


length5 : Length
length5 =
    px 6


colourPrimary : Color
colourPrimary =
    rgb255 20 30 40


colourSecondary : Color
colourSecondary =
    rgb255 80 90 100


colourSecondary2 : Color
colourSecondary2 =
    rgb255 90 100 110


colourSecondaryLight : Color
colourSecondaryLight =
    rgb255 160 170 180


colourWhite : Color
colourWhite =
    rgb255 240 240 240


colourAlt : Color
colourAlt =
    rgb255 50 60 20


colourAltSecondary : Color
colourAltSecondary =
    rgb255 80 90 50


colourCta : Color
colourCta =
    rgb255 20 80 80


colourError : Color
colourError =
    rgb255 150 60 50


textColourDark : Color
textColourDark =
    rgba255 10 10 10 0.8


textColourFeature : Color
textColourFeature =
    rgba255 20 30 40 0.8


textColourLight : Color
textColourLight =
    rgba255 250 250 250 0.8


buttonStyles : List (Attribute msg)
buttonStyles =
    [ padding size4
    , Background.color colourCta
    , Font.color textColourLight
    , Border.solid
    , Border.width size1
    , Border.color colourWhite
    , mouseOver
        [ Background.color colourAltSecondary ]
    , focused
        [ Background.color colourAlt ]
    ]
