import Browser
import Browser.Events
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Json.Decode as Decode
import Random
import List.Extra exposing (..)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Coords =
    { x : Int
    , y : Int
    }

tickInterval = 50
playgroundSize = Coords 600 600
elementSize = Coords 10 10
initialHead = Coords (playgroundSize.x // 2) (playgroundSize.y // 2)
initialLength = 5
initialBody = List.indexedMap (\i coords -> Coords coords.x (coords.y + i)) (List.repeat initialLength initialHead)
initialSnake = Snake initialHead initialBody Down Down

type Status
    = Start
    | InGame
    | GameOver

type alias Snake =
    { head: Coords
    , body: List Coords
    , direction: Direction
    , requestedDirection: Direction
    }

type alias Model =
    { state : Status
    , snake : Snake
    , foodPosition : Coords
    , score : Int
    }

type Msg
    = Tick Time.Posix
    | KeyDown Direction
    | NewFoodPosition Coords

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Time.every tickInterval Tick
    ]

init : () -> ( Model, Cmd Msg )
init _ =
    ( Model InGame initialSnake (Coords 0 0 ) 0 , generateFoodPosition initialSnake )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Tick  _ ->
            case model.state of
                InGame ->
                    let
                        snake = resolveRequestedDirection model.snake 
                        nextPosition = getNextPosition snake
                        scoring = isScoring model nextPosition
                        collision = List.member nextPosition snake.body
                        newSnake = moveSnake nextPosition snake scoring
                    in
                    if collision then
                        ( { model | snake = newSnake, state = GameOver }, Cmd.none )
                    else
                        ( { model | snake = newSnake, score = if scoring then model.score + 1 else model.score },
                        if scoring then generateFoodPosition snake else Cmd.none )

                _ ->
                    ( model, Cmd.none )
            
        KeyDown direction ->
            let 
                snakeBefore = model.snake
            in
            ( { model | snake = { snakeBefore | requestedDirection = Debug.log "direction" direction } }, Cmd.none )

        NewFoodPosition position ->
            ( { model | foodPosition = position }, Cmd.none )

isScoring: Model -> Coords -> Bool
isScoring model nextPosition =
    if model.foodPosition == nextPosition then
        True
    else
        False

isDirectionValid: Snake -> Direction -> Bool
isDirectionValid snake direction =
    let
        invalidCombinations = [
            ( Up, Down ),
            ( Left, Right )
            ]
    in
    not <| List.member ( snake.direction, direction) invalidCombinations 
        || List.member ( direction, snake.direction) invalidCombinations

resolveRequestedDirection : Snake -> Snake
resolveRequestedDirection snake =
    let
        direction = if isDirectionValid snake snake.requestedDirection then snake.requestedDirection else snake.direction
    in
    { snake | direction = direction }
    

moveSnake : Coords -> Snake -> Bool -> Snake
moveSnake headPosition snake grow =
    let
        bodyLength = List.length snake.body
        newBody = snake.head :: List.take (bodyLength - if grow then 0 else 1) snake.body
    in
    { snake | head = headPosition, body = newBody }


snakeCoords : Snake -> List Coords
snakeCoords snake =
    snake.head :: snake.body

snakeLength : Snake -> Int
snakeLength snake =
    List.length ( snakeCoords snake )

popSnake : Snake -> Snake
popSnake snake =
    { snake | body = if List.length snake.body > 5 then List.take 5 snake.body else snake.body }

unshiftSnake : Coords -> Snake -> Snake
unshiftSnake coords snake = 
    { snake | body = coords :: snake.body }

view : Model -> Html Msg
view model =
    svg [ width ( getX playgroundSize ) , height ( getY playgroundSize )
        , viewBox ( "0 0 " ++ ( getX playgroundSize ) ++ ( getY playgroundSize ) ) ] 
    (
          rect [ x "0", y "0", width "100%", height "100%", fill "black" ] []
         ::(
         case model.state of
            Start ->
                [ text_ [ x "0", y "0", Svg.Attributes.style "fill: white" ] [ Svg.text "Press key to play" ] ]

            InGame ->
                text_ [ x "5", y "15", Svg.Attributes.style "fill: white" ] 
                      [ Svg.text ( "Score: " ++ ( String.fromInt model.score ) ) ]
                ::svgText "5" "30" ( "head: " ++ (String.fromInt model.snake.head.x) ++ ", " ++ (String.fromInt model.snake.head.y))
                ::svgText "5" "45" ( "food: " ++ (String.fromInt model.foodPosition.x) ++ ", " ++ (String.fromInt model.foodPosition.y))
                ::rect [ x (String.fromInt model.foodPosition.x), y (String.fromInt model.foodPosition.y), width "10px", height "10px", fill "red" ] []
                ::List.map snakeElement ( model.snake.head :: model.snake.body )
                

            GameOver ->
                text_ [ x "10", y "10", Svg.Attributes.style "fill: white" ] [ Svg.text "Game over" ]
                ::svgSnake model.snake
         )

    )

svgSnake snake =
    List.map snakeElement ( snake.head :: snake.body )


svgText x y text =
    text_ [ Svg.Attributes.x x, Svg.Attributes.y y, Svg.Attributes.style "fill: white" ] [ Svg.text text ]

getX : Coords -> String
getX coords = String.fromInt coords.x

getY : Coords -> String
getY coords = String.fromInt coords.y

-- playground : Model -> Html Msg
-- playground model =
--     List.map snakeElement model

snakeElement : Coords -> Svg Msg
snakeElement coords =
    rect [ x (String.fromInt coords.x), y (String.fromInt coords.y), width "10px", height "10px", fill "pink" ] []

getNextPosition : Snake -> Coords
getNextPosition snake =
    let
        head = snake.head
    in
    case snake.direction of
        Up ->
            Coords head.x ( modBy playgroundSize.y ( head.y - 1 * elementSize.y) )
        Right ->
            Coords ( modBy playgroundSize.x ( head.x + 1 * elementSize.x ) ) head.y
        Down ->
            Coords head.x ( modBy playgroundSize.y ( head.y + 1 * elementSize.y ) )
        Left ->
            Coords ( modBy playgroundSize.x ( head.x - 1 * elementSize.x ) ) head.y

allPlaygroundCoords : List Coords
allPlaygroundCoords =
    let
        xRange = List.range 0 (playgroundSize.x // elementSize.x - 1)
        xCoors = List.map (\x -> x * elementSize.x) xRange
        yRange = List.range 0 (playgroundSize.y // elementSize.y - 1)
        yCoors = List.map (\y -> y * elementSize.y) yRange
    in
        List.concatMap (\x -> List.map ( Coords x ) yCoors) xCoors 

generateFoodPosition : Snake -> Cmd Msg
generateFoodPosition snake =
    let
        allCoordsWithoutSnake = List.filter (\x -> not (List.member x (snake.head::snake.body)))  allPlaygroundCoords
        max = (List.length allPlaygroundCoords) - 1
        justOrOrigin = (\n -> case n of
            Just a -> a
            Nothing -> Coords 0 0)
        coordsGenerator = Random.map (\n -> justOrOrigin (getAt n allCoordsWithoutSnake)) (Random.int 0 max)
    in
        Random.generate NewFoodPosition coordsGenerator

    
-- Keyboard

type Direction
  = Up
  | Right
  | Down
  | Left

keyDecoder : Decode.Decoder Direction
keyDecoder =
  Decode.field "key" Decode.string
    |> Decode.andThen directionDecoder

directionDecoder : String -> Decode.Decoder Direction
directionDecoder string =
  case string of
    "ArrowUp" ->
        Decode.succeed Up
    "ArrowRight" ->
        Decode.succeed Right
    "ArrowDown" ->
        Decode.succeed Down
    "ArrowLeft" ->
        Decode.succeed Left
    _ ->
        Decode.fail "not an arrow key"