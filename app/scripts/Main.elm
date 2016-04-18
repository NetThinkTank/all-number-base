-- Thanks to Adam Marshall for the post at
-- http://stackoverflow.com/questions/34965852/elm-modal-dialog-box

module Main where

import Array exposing (Array, fromList, get)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Seed, initialSeed, generate, int)
import StartApp exposing (start)
import String exposing (join)
import Task exposing (Task)

port startTime: Float
port symbolList: List String
port maxNumber: Int
port maxGuesses: Int
port maxHints: Int

port alert: Signal String
port alert =
    Signal.map (\n -> n.alert) app.model

type alias Pair = (String, Int)

type alias Model = {
  base: Int,
  symbols: Array String,
  maxNumber: Int,
  puzzle: Pair,
  guess: String,
  guessesUsed: Int,
  guessesMax: Int,
  hints: List Pair,
  hintsMax: Int,
  state: State,
  winMessage: String,
  seed: Seed,
  alert: String
}

type Action = NewHint | SetGuess String | Check
type State = Ok | Win | Fail


randomInt: Int -> Int -> Seed -> (Int, Seed)
randomInt min max seed =
  generate (int min max) seed


largestPlace: Int -> Int -> Int
largestPlace n base =
  floor (logBase (toFloat base) (toFloat n)) + 1


pair: Int -> Array String -> Int -> Pair
pair n symbols base =
  let
    lp = largestPlace n base
    hc = pairConvert (buildPair n base [] lp) symbols
  in
    (hc, n)


buildPair: Int -> Int -> List Int -> Int -> List Int
buildPair n base partial place =
  let
    p = place - 1
    q = n // base^p
    
    remaining = n - q * base^p
  in
    if place <= 0 then
      partial
    else
      buildPair remaining base (List.append partial [q]) (place - 1)


pairConvert: List Int -> Array String -> String
pairConvert indices symbols =
  let
    m = List.filterMap (\i -> Array.get i symbols) indices
  in
    String.join "" m


init: (Model, Effects Action)
init =
  let 
    symbols = fromList symbolList
    base = Array.length symbols
    
    seed = initialSeed (round startTime)
    (r1, seed') = generate (int 1 maxNumber) seed
    (r2, seed'') = generate (int 1 maxNumber) seed'
    
    p = pair r1 symbols base
    h = pair r2 symbols base
    
    winMessage = "Base: " ++ (toString base) ++ "\n" ++ "Symbols: " ++ (toString symbolList)
    
    debug = ""
  in
    (
      { base = base, symbols = symbols, maxNumber = maxNumber,
        puzzle = p, guess = "", guessesUsed = 0, guessesMax = maxGuesses,
        hints = [h], hintsMax = maxHints, state = Ok,
        winMessage = winMessage, seed = seed'', alert = ""
      },
      Effects.none
    )


app: {
  html : Signal Html,
  model : Signal Model,
  tasks : Signal (Task Never ())
}

app = 
  StartApp.start
    { init = init,
      update = update,
      view = view,
      inputs = []
    }


main: Signal Html
main =
  app.html


renderGuesses: Model -> Signal.Address Action -> Html
renderGuesses model address =
  let
    p = fst model.puzzle
    
    gm = toString (model.guessesMax)
    gu = toString (model.guessesUsed)
    
    guessInput = input [
      id "guess", type' "text", value model.guess,
      style [("padding-left", "5px"), ("padding-right", "5px")],
      on "input" targetValue (\str -> Signal.message address (SetGuess str))
    ] []
    
    guessButton = if model.guessesUsed < model.guessesMax then
      button [ onClick address Check, style [("margin-top", "9px")] ] [ text "Check" ]
    else
      span [] []
  in
    div [class "container"] [
      h3 [] [text ( "TAKE A GUESS (" ++ gm ++ " max, " ++ gu ++ " used)" )],
      div [style [("margin-left", "25px"), ("font-size", "25px")]] [
        text (p ++ " = "),
        guessInput,
        text " ?"
      ],
      guessButton
    ]


renderHints: Model -> Signal.Address Action -> Html
renderHints model address =
  let
    hints = model.hints
    hm = toString (model.hintsMax)
    hu = toString (List.length model.hints)
    
    hintsButton = if List.length model.hints < model.hintsMax then
      button [ onClick address NewHint ] [ text "New Hint" ]
    else
      span [] []
      
    s = style [("font-size", "25px"), ("margin-left", "25px"), ("padding-left", "0px"), ("list-style-type", "none")]
  in
    div [class "container"] [
      h3 [] [text ( "HINTS (" ++ hm ++ " max, " ++ hu ++ " used)" )],
      ul
        [s]
        (List.map
          (\hint -> li [] [ text ((fst hint) ++ " = " ++ (toString (snd hint))) ])
          hints
        ),
      hintsButton
    ]


view: Signal.Address Action -> Model -> Html
view address model =
  div [] [
    nav [class "navbar navbar-inverse navbar-fixed-top"] [
      div [class "container"] [
        div [class "navbar-header"] [
          a [class "navbar-brand"] [
            i [class "fa fa-sort-numeric-asc", style [("padding-right", "15px")]] [],
            text "All Number Base"
          ]
        ]
      ]
    ],
    renderGuesses model address,
    br [] [],
    renderHints model address
  ]


update: Action -> Model -> (Model, Effects Action)
update action model =
  if model.state == Win then
    ( { model | alert = "You won!" }, Effects.none )

  else if model.state == Fail then
    ( { model | alert = "Sorry, you lost." }, Effects.none )

  else case action of
    NewHint ->
      let 
        (r, seed') = generate (int 1 model.maxNumber) model.seed
        hint = pair r model.symbols model.base
        hints' = model.hints ++ [hint]
      in
        if List.length hints' >= model.hintsMax then
          ( 
            { model | hints = hints', seed = seed', alert = "Last Hint!"},
            Effects.none
          )
        else
          (
            { model | hints = hints', seed = seed', alert = "" },
            Effects.none
          )
    
    SetGuess str ->
      ( {model | guess = str, alert = ""}, Effects.none )
          
    Check ->
      let
        p = snd model.puzzle
      
        gu = model.guessesUsed + 1
        g = String.toInt model.guess |> Result.toMaybe |> Maybe.withDefault -1
        
        h = toString (List.length model.hints)
        
        gText = if gu == 1 then
          "guess"
        else
          "guesses"
          
        hText = if List.length model.hints == 1 then
          "hint"
        else
          "hints"
        
        wmTemp = "Correct! You won in " ++ (toString gu) ++ " " ++ gText ++ " with " ++ h ++ " " ++ hText ++ ".\n\n"
        wm = wmTemp ++ model.winMessage
      in
        if g == p then
          ( {model | guessesUsed = gu, state = Win, alert = wm}, Effects.none )
        else if gu == model.guessesMax then
          ( {model | guessesUsed = gu, state = Fail, alert = "Incorrect :( Sorry, you lost."}, Effects.none )       
        else if gu + 1 == model.guessesMax then
          ( {model | guessesUsed = gu, state = Ok, alert = "Incorrect :(\nOne guess remaining!"}, Effects.none )
        else
          ( {model | guessesUsed = gu, state = Ok, alert = "Incorrect :("}, Effects.none )

