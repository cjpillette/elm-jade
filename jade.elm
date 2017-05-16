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
init = (Model emptyPlayerInfo) ! []


-- Model
type alias Model =
  { playerInformation : PlayerInformation
  }

type alias PlayerInformation
  = Dict.Dict String (Maybe String)

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

-- update

type Msg
  = EditPlayerInformation String String


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

-- view

view : Model -> Html Msg
view model =
  div []
    [playerInformationView model
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
