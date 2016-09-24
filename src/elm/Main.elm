import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )
import String
import Html.Attributes exposing ( readonly )



-- APP
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model = { covered : List Int }

pass : String
pass = "a very long password"

model : Model
model = { covered = [] }


-- UPDATE
type Msg = Swap Int |
           Unselect |
           Select

update : Msg -> Model -> Model
update msg model = case msg of
  Swap fieldId  -> if List.member fieldId model.covered
    then { model | covered = List.filter (\x -> x /= fieldId) model.covered }
    else { model | covered = fieldId :: model.covered }
  Unselect -> { model | covered = [] }
  Select   -> { model | covered = [1..(String.length pass)] }

-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  let
      contains : Int -> Bool
      contains fieldId = List.member fieldId model.covered

      letter fieldId  = li
                    [ class "fields" ]
                    [ input
                        [class "field",
                         value (getNthPassLetter fieldId),
                         readonly True,
                         onClick (Swap fieldId)
                         ]
                        []
                    , div
                        [ style [("text-align", "center")] ]
                        [ text (toString fieldId) ]
                    ]

      getNthPassLetter : Int -> String
      getNthPassLetter n = pass |>
                           String.toList |>
                           List.drop (n-1) |>
                           List.head |>
                           Maybe.withDefault '?' |>
                           String.fromChar

      isDisabled : Int -> Bool
      isDisabled fieldId = List.member fieldId model.covered

  in div
        []
        [ List.map letter [1..(String.length pass)] |> ul [class "fields"]
        , div
          []
          [ button [ onClick Unselect, class "unselect" ] [ text "unselect all" ]
          , button [ onClick Select, class "select" ] [ text "select all" ]
          ]
        ]
