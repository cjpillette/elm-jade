import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

init : (Model, Cmd Msg)
init = (Model emptyPlayerInfo emptyExAttributes) ! []


-- Model
type alias Model =
  { playerInformation : PlayerInformation
  , exAttributes : ExAttributes
  }

type alias PlayerInformation
  = Dict.Dict String (Maybe String)

type alias ExAttributes
  = Dict.Dict String Int

emptyPlayerInfo : PlayerInformation
emptyPlayerInfo =
  Dict.fromList
    [("Name", Nothing)
    ,("Player", Nothing)
    ,("Caste", Just "Zenith")
    ,("Concept", Nothing)
    ,("Anima", Nothing)
    ,("SupernalAbility", Just "Flying")
    ]

emptyExAttributes : ExAttributes
emptyExAttributes =
   Dict.fromList
    [("Strength", 1)
    , ("Dexterity", 1)
    , ("Stamina", 1)
    , ("Charisma", 1)
    , ("Intelligence", 1)
    , ("Wits", 1)
    ]

-- update

type Msg
  = EditPlayerInformation String String
  | EditExAttribute Operation String

type Operation
  = Increment
  | Decrement


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EditPlayerInformation section val ->
      let
        newPlayerInfo =
          Dict.insert
            section
              (if String.length val > 0 then
                (Just val)
                else
                  Nothing
                )
                model.playerInformation
      in
        { model | playerInformation = newPlayerInfo} ! []

    EditExAttribute operation exAttribute ->
      { model | exAttributes =
          updateExAttributes
            model.exAttributes
              operation
              exAttribute } ! []


updateExAttributes : ExAttributes -> Operation -> String -> ExAttributes
updateExAttributes attributes operation exAttribute =
  let
    oldAttribute =
      Dict.get exAttribute attributes
      |> Maybe.withDefault 0
  in
    case operation of
      Increment ->
        if oldAttribute < 5 then
          Dict.insert
            exAttribute
              (oldAttribute + 1)
              attributes
        else
            attributes
      Decrement ->
        if oldAttribute > 1 then
          Dict.insert
            exAttribute
              (oldAttribute - 1)
              attributes
        else
          attributes


-- view

view : Model -> Html Msg
view model =
  div []
    [playerInformationView model
    , allExAttributesView model
    ]

playerInformationView : Model -> Html Msg
playerInformationView model =
  div []
    [input
      [placeholder "Name"
      , onInput (EditPlayerInformation "Name")]
      []
    , input
      [placeholder "Player"
      , onInput (EditPlayerInformation "Player")]
      []
    , selectCaste
    , br [][]
    , input
      [placeholder "Concept"
      , onInput (EditPlayerInformation "Concept")]
      []
    , input
      [placeholder "Anima"
      , onInput (EditPlayerInformation "Anima")]
      []
    , selectSupernalAbility
    ]

simpleOption : String -> Html msg
simpleOption val =
  option [value val][text val]

castes : List String
castes =
  ["Zenith"
  , "Sunray"
  , "Eclipse"
  , "Stardust"
  ]

selectCaste : Html Msg
selectCaste =
  select [onInput (EditPlayerInformation "Caste")]
        (List.map simpleOption castes)

supernalAbilities : List String
supernalAbilities =
  ["Flying"
  , "Teletransportation"
  , "Lying without being caught"
  , "Making money"
  , "Live forever"
  , "Lay golden eggs"
  ]


selectSupernalAbility : Html Msg
selectSupernalAbility =
  select [onInput (EditPlayerInformation "SupernalAbility")]
        (List.map simpleOption supernalAbilities)


-- Attributes section

attributes : List String
attributes =
  ["Strength"
  , "Dexterity"
  , "Stamina"
  , "Charisma"
  , "Intelligence"
  , "Wits"
  ]

allExAttributesView : Model -> Html Msg
allExAttributesView model =
  div []
    (Dict.toList model.exAttributes
    |> List.map exAttributeView
    )

exAttributeView : (String, Int) -> Html Msg
exAttributeView ( exAttribute, exAttributeVal ) =
  div []
  [ text (exAttribute ++ " ")
  , text (toString exAttributeVal)
  , button
    [ onClick (EditExAttribute Decrement exAttribute)]
    [ text "-"]
  , button
    [ onClick (EditExAttribute Increment exAttribute)]
    [ text "+"]
  ]
