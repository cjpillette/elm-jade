import Dict
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String
import Svg
import Svg.Attributes as SvgAtt

main : Program Never Model Msg
main = program
  { init = init
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

init : (Model, Cmd Msg)
init = (Model emptyPlayerInfo emptyExAttributes emptyExAbilities) ! []


-- Model
type alias Model =
  { playerInformation : PlayerInformation
  , exAttributes : ExAttributes
  , exAbilities : ExAbilities
  }

type alias PlayerInformation
  = Dict.Dict String (Maybe String)

type alias ExAttributes
  = Dict.Dict String Int

type alias ExAbilities =
  Dict.Dict String ( Bool, Int )

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

emptyExAbilities : ExAbilities
emptyExAbilities =
  Dict.fromList
    [("Archery", (False, 0))
    , ("Athletics", (False, 0))
    , ("Awareness", (False, 0))
    , ("Brawl", (False, 0))
    , ("Bureaucracy", (False, 0))
    , ("Craft", (False, 0))
    , ("Dodge", (False, 0))
    , ("Integrity", (False, 0))
    , ("Investigation", (False, 0))
    , ("Larceny", (False, 0))
    , ("Linguistics", (False, 0))
    , ("Lore", (False, 0))
    , ("Martial Arts", (False, 0))
    , ("Medecine", (False, 0))
    , ("Melee", (False, 0))
    , ("Occult", (False, 0))
    , ("Performance", (False, 0))
    , ("Presence", (False, 0))
    , ("Ride", (False, 0))
    , ("Sail", (False, 0))
    , ("Socialize", (False, 0))
    , ("Stealth", (False, 0))
    , ("Survival", (False, 0))
    , ("Thrown", (False, 0))
    , ("War", (False, 0))
    ]

-- update

type Msg
  = EditPlayerInformation String String
  | EditExAttribute String Int
  | EditExAbility String Int
  | ToggleCasteOrFavoured String

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

    EditExAbility exAbility abilityValue ->
      { model
      | exAbilities =
        updateExAbilities
          model.exAbilities
          exAbility
          abilityValue
        } ! []

    ToggleCasteOrFavoured exAbility ->
            let
                exAbilities =
                    toggleCasteOrFavoured
                        exAbility
                        model.exAbilities
            in
                { model | exAbilities = exAbilities } ! []


updateExAttributes : ExAttributes -> String -> Int -> ExAttributes
updateExAttributes attributes exAttribute attributeValue =
  Dict.insert exAttribute attributeValue attributes

updateExAbilities :
    ExAbilities
    -> String
    -> Int
    -> ExAbilities
updateExAbilities exAbilities exAbility abilityValue =
    let
        ( favoured, _ ) =
            Dict.get exAbility exAbilities
                |> Maybe.withDefault ( False, 0 )
        updatedValue = ( favoured, abilityValue )
    in
        Dict.insert exAbility updatedValue exAbilities

toggleCasteOrFavoured : String -> ExAbilities -> ExAbilities
toggleCasteOrFavoured exAbility exAbilities =
  let
    ( favoured, abilityValue ) =
      Dict.get exAbility exAbilities
        |> Maybe.withDefault (False, 0)

    toggleFavoured =
      (not favoured, abilityValue)
  in
    Dict.insert exAbility toggleFavoured exAbilities

selectCaste : Html Msg
selectCaste =
  select [onInput (EditPlayerInformation "Caste")]
        (List.map simpleOption castes)

selectSupernalAbility : Html Msg
selectSupernalAbility =
  select [onInput (EditPlayerInformation "SupernalAbility")]
        (List.map simpleOption supernalAbilities)


-- my lists
attributes : List String
attributes =
  ["Strength"
  , "Dexterity"
  , "Stamina"
  , "Charisma"
  , "Manipulation"
  , "Appearance"
  , "Perception"
  , "Intelligence"
  , "Wits"
  ]

supernalAbilities : List String
supernalAbilities =
  ["Flying"
  , "Teletransportation"
  , "Lying without being caught"
  , "Making money"
  , "Live forever"
  , "Lay golden eggs"
  ]

abilities : List String
abilities =
  ["Archery"
  , "Athletics"
  , "Awareness"
  , "Brawl"
  , "Bureaucracy"
  , "Craft"
  , "Dodge"
  , "Integrity"
  , "Investigation"
  , "Larceny"
  , "Linguistics"
  , "Lore"
  , "Martial Arts"
  , "Medicine"
  , "Melee"
  , "Occult"
  , "Performance"
  , "Presence"
  , "Resistance"
  , "Ride"
  , "Sail"
  , "Socialize"
  , "Stealth"
  , "Survival"
  , "Thrown"
  , "War"
  ]

castes : List String
castes =
  ["Zenith"
  , "Sunray"
  , "Eclipse"
  , "Stardust"
  ]

-- view

view : Model -> Html Msg
view model =
  div []
    [playerInformationView model
    , allExAttributesView model
    , allAbilitiesView model.exAbilities
    ]

attributeDot : String -> Int -> Bool -> Html Msg
attributeDot exAttribute attributeValue filled =
  Svg.svg
    [ SvgAtt.width "20"
    , SvgAtt.height "20"
    , onClick (EditExAttribute exAttribute attributeValue)
    ]
    [ Svg.circle
       [ SvgAtt.cx "10"
       , SvgAtt.cy "10"
       , SvgAtt.r "8"
       , SvgAtt.stroke "black"
       , SvgAtt.strokeWidth "2"
       , if filled then
          SvgAtt.fill "black"
         else
           SvgAtt.fill "white"
       ]
       []
    ]

abilityDot : String -> Int -> Bool -> Html Msg
abilityDot exAbility abilityValue filled =
    Svg.svg
      [ SvgAtt.width "20"
      , SvgAtt.height "20"
      , onClick (EditExAbility exAbility abilityValue)
      ]
      [ Svg.circle
         [ SvgAtt.cx "10"
         , SvgAtt.cy "10"
         , SvgAtt.r "8"
         , SvgAtt.stroke "black"
         , SvgAtt.strokeWidth "2"
         , if filled then
            SvgAtt.fill "black"
           else
             SvgAtt.fill "white"
         ]
         []
      ]

casteOrFavouredBox : String -> Bool -> Html Msg
casteOrFavouredBox exAbility casteOrFavoured =
  Svg.svg
    [ SvgAtt.width "18"
    , SvgAtt.height "18"
    , onClick (ToggleCasteOrFavoured exAbility)
    ]
    [ Svg.rect
      [ SvgAtt.width "18"
      , SvgAtt.height "18"
      , SvgAtt.stroke "black"
      , SvgAtt.strokeWidth "4"
      , if casteOrFavoured then
        SvgAtt.fill "black"
        else
          SvgAtt.fill "white"
        ]
        []
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


allExAttributesView : Model -> Html Msg
allExAttributesView model =
  div []
    [physicalAttributes model.exAttributes
    , socialAttributes model.exAttributes
    , mentalAttributes model.exAttributes
    ]

physicalAttributes : ExAttributes -> Html Msg
physicalAttributes exAttributes =
    div []
      [ h3 [] [text "Social"]
      , exAttributeView exAttributes "Strength"
      , exAttributeView exAttributes "Dexterity"
      , exAttributeView exAttributes "Stamina"
      ]

socialAttributes : ExAttributes -> Html Msg
socialAttributes exAttributes =
  div []
    [ h3 [] [text "Social"]
    , exAttributeView exAttributes "Charisma"
    , exAttributeView exAttributes "Manipulation"
    , exAttributeView exAttributes "Appearance"
    ]

mentalAttributes : ExAttributes -> Html Msg
mentalAttributes exAttributes =
  div []
    [ h3 [] [text "Mental"]
    , exAttributeView exAttributes "Perception"
    , exAttributeView exAttributes "Intelligence"
    , exAttributeView exAttributes "Wits"
    ]

exAttributeView : ExAttributes -> String -> Html Msg
exAttributeView exAttributes exAttribute =
  let
    exAttributeVal
      = Dict.get exAttribute exAttributes
      |> Maybe.withDefault 1
    filledList =
      List.map2 (\ref val -> ref >= val)
        (List.repeat 5 exAttributeVal)
        (List.range 1 5)
  in
    div []
    [ text exAttribute
      , div
        []
        (List.map2 (attributeDot exAttribute)
          (List.range 1 5)
          filledList
          )
    ]

allAbilitiesView : ExAbilities -> Html Msg
allAbilitiesView exAbilities =
  div []
      ([ h2 [] [text "Abilities"]]
      ++ List.map (exAbilityView exAbilities) abilities
      )

exAbilityView : ExAbilities -> String -> Html Msg
exAbilityView exAbilities exAbility =
  let
    ( favoured, exAbilityVal ) =
      Dict.get exAbility exAbilities
        |> Maybe.withDefault ( False, 0)

    filledList =
      List.map2 (\ref val -> ref >= val)
      (List.repeat 5 exAbilityVal)
      (List.range 1 5)
  in
    div []
    [ casteOrFavouredBox exAbility favoured
    , text exAbility
    , div
        []
        (List.map2 (abilityDot exAbility)
          (List.range 1 5)
          filledList)
      ]
