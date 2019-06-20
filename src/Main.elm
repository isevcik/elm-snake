import Browser
import Browser.Events
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Json.Decode as Decode
import Random
import List.Extra
import Maybe exposing (..)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

tickInterval = 50
playgroundSize = Coords 600 600
elementSize = 15
initialHead = Coords (playgroundSize.x // 2) (playgroundSize.y // 2)
initialLength = 5
initialBody = List.indexedMap (\i coords -> Coords coords.x (coords.y + i)) (List.repeat initialLength initialHead)
initialSnake = Snake initialHead initialBody Down Down
initialModel = Model PressKeyToStart initialSnake (Coords 0 0 ) 0

type alias Coords =
    { x : Int
    , y : Int
    }

type Status
    = PressKeyToStart
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
    | KeyPressed String
    | NewFoodPosition Coords

type Direction
  = Up
  | Right
  | Down
  | Left

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)
        , Time.every tickInterval Tick
    ]

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateFoodPosition initialSnake )

initInGame : () -> ( Model, Cmd Msg )
initInGame _ =
    ( { initialModel | state = InGame }, generateFoodPosition initialSnake )

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
        KeyPressed key ->
            case key of
                " " ->
                    case model.state of
                        PressKeyToStart ->
                            initInGame ()
                        GameOver ->
                            initInGame ()
                        _ ->
                            ( model, Cmd.none )
                _ ->
                    let
                        maybeDirection = keyToDirection key
                        snakeBefore = model.snake
                    in
                    ( { model | snake = { snakeBefore | requestedDirection = withDefault model.snake.requestedDirection maybeDirection } }, Cmd.none )
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
    svg [ width <| String.fromInt <| playgroundSize.x, height <| String.fromInt <| playgroundSize.y
        , viewBox <| String.join " " <| List.map String.fromInt [0, 0, playgroundSize.x, playgroundSize.y] ]
        <| List.concat [
            [ rect [ x "0", y "0", width "100%", height "100%", fill "black" ] [] ]
            ,
         case model.state of
            PressKeyToStart ->
                svgPressToStart
            InGame ->
                svgScore model.score

                ::rect [ x (String.fromInt model.foodPosition.x), y (String.fromInt model.foodPosition.y), width <| svgIntToPixels elementSize, height <| svgIntToPixels elementSize, rx "0.3%", fill "red" ] []
                ::svgSnake model.snake


            GameOver ->
                svgGameOver model.score
                ++ svgPressToStart
                ++ svgSnake model.snake
        ]

svgDebugInfo model =
    Svg.g [] [ svgText 5 30 ( "head: " ++ (String.fromInt model.snake.head.x) ++ ", " ++ (String.fromInt model.snake.head.y)) []
          ,svgText 5 45 ( "food: " ++ (String.fromInt model.foodPosition.x) ++ ", " ++ (String.fromInt model.foodPosition.y)) []
    ]

svgScore score =
    svgText 20 28 ("SCORE: " ++ String.fromInt score) [ Svg.Attributes.style "fill: white; font-family: monospace; font-size: 16px;" ]

svgPressToStart =
    [ svgText ( playgroundSize.x // 2 ) ( playgroundSize.y // 2 ) "Press SPACE to start" [ Svg.Attributes.style "fill: white; font-size: 20px; font-family: monospace; text-anchor: middle" ] ]

svgGameOver score =
    [ svgText ( playgroundSize.x // 2 ) ( playgroundSize.y // 2 - 30 ) ( "Game Over, Your Score: " ++ String.fromInt score ) [ Svg.Attributes.style "fill: white; font-size: 20px; font-family: monospace; text-anchor: middle" ] ]

svgSnake snake =
    List.map svgSnakeElement ( snake.head :: snake.body )

svgText : Int -> Int -> String -> List (Svg.Attribute msg) -> Svg msg
svgText x y text attrs =
    let
        attributes = [ Svg.Attributes.x <| String.fromInt <| x, Svg.Attributes.y <| String.fromInt <| y, Svg.Attributes.style "fill: white" ] ++ attrs
    in
    text_ attributes [ Svg.text text ]

svgSnakeElement : Coords -> Svg Msg
svgSnakeElement coords =
    rect [ x (String.fromInt coords.x)
         , y (String.fromInt coords.y)
         , width <| svgIntToPixels elementSize
         , height <| svgIntToPixels elementSize
         , fill "pink" ] []

svgIntToPixels : Int -> String
svgIntToPixels int =
    String.fromInt int ++ "px"

getNextPosition : Snake -> Coords
getNextPosition snake =
    let
        head = snake.head
    in
    case snake.direction of
        Up ->
            Coords head.x ( modBy playgroundSize.y ( head.y - 1 * elementSize) )
        Right ->
            Coords ( modBy playgroundSize.x ( head.x + 1 * elementSize ) ) head.y
        Down ->
            Coords head.x ( modBy playgroundSize.y ( head.y + 1 * elementSize ) )
        Left ->
            Coords ( modBy playgroundSize.x ( head.x - 1 * elementSize ) ) head.y

allPlaygroundCoords : List Coords
allPlaygroundCoords =
    let
        xRange = List.range 0 (playgroundSize.x // elementSize - 1)
        xCoors = List.map (\x -> x * elementSize) xRange
        yRange = List.range 0 (playgroundSize.y // elementSize - 1)
        yCoors = List.map (\y -> y * elementSize) yRange
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
        coordsGenerator = Random.map (\n -> justOrOrigin (List.Extra.getAt n allCoordsWithoutSnake)) (Random.int 0 max)
    in
        Random.generate NewFoodPosition coordsGenerator

keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

keyToDirection : String -> Maybe Direction
keyToDirection string =
  case string of
    "ArrowUp" ->
        Just Up
    "ArrowRight" ->
        Just Right
    "ArrowDown" ->
        Just Down
    "ArrowLeft" ->
        Just Left
    _ ->
        Nothing
