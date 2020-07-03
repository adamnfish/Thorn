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
    12


size6 : Int
size6 =
    16


length0 : Length
length0 =
    px size0


length1 : Length
length1 =
    px size1


length2 : Length
length2 =
    px size2


length3 : Length
length3 =
    px size3


length4 : Length
length4 =
    px size4


length5 : Length
length5 =
    px size5


length6 : Length
length6 =
    px size6


colourPrimary : Color
colourPrimary =
    rgb255 222 184 65


colourSecondary : Color
colourSecondary =
    rgb255 55 50 62


colourSecondary2 : Color
colourSecondary2 =
    rgb255 65 60 72


colourSecondaryLight : Color
colourSecondaryLight =
    rgb255 191 189 193


colourWhite : Color
colourWhite =
    rgb255 240 240 240


colourBlack : Color
colourBlack =
    rgb255 10 10 10


colourBlack2 : Color
colourBlack2 =
    rgb255 25 25 25


colourAlt : Color
colourAlt =
    rgb255 222 184 65


colourAltSecondary : Color
colourAltSecondary =
    rgb255 222 184 65


colourCta : Color
colourCta =
    rgb255 15 139 141


colourHighlight : Color
colourHighlight =
    rgb255 200 10 60


colourHighlight2 : Color
colourHighlight2 =
    rgb255 210 20 70


colourError : Color
colourError =
    rgb255 150 60 50


textColourDark : Color
textColourDark =
    rgba255 10 10 10 0.8


textColourFeature : Color
textColourFeature =
    rgba255 55 50 62 0.8


textColourLight : Color
textColourLight =
    rgba255 250 250 250 0.8


buttonStyles : List (Attribute msg)
buttonStyles =
    [ padding size4
    , Background.color colourCta
    , Font.color textColourLight
    , Border.solid
    , Border.widthEach
        { each0 | bottom = size2 }
    , Border.color colourSecondary
    , mouseOver
        [ Background.color colourAltSecondary ]
    , focused
        [ Background.color colourAlt ]
    ]


featureButtonStyles : List (Attribute msg)
featureButtonStyles =
    List.append
        buttonStyles
        [ width fill
        , padding size5
        ]


textInputStyles : List (Attribute msg)
textInputStyles =
    [ width fill
    , padding size5
    , Background.color colourWhite
    , Font.color textColourDark
    , Font.alignLeft
    , Border.solid
    , Border.widthEach
        { each0 | bottom = size1 }
    , Border.color colourSecondary
    , Border.rounded 0
    ]


each0 :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
each0 =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


centerBlock : Element msg -> Element msg
centerBlock block =
    el
        [ width <| maximum 500 fill
        , centerX
        ]
        block


spacer : Int -> Element msg
spacer size =
    el
        [ width fill
        , height <| px <| size * size4
        ]
        Element.none
