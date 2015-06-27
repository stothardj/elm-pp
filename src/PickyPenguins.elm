import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window
import Keyboard
import Signal exposing ((<~),(~))
import Json.Decode as Json exposing ((:=),Decoder)
import Http
import Task exposing (Task, andThen)

{-- Input Model --------------------------------------------------------------
------------------------------------------------------------------------------}

orInt : Int -> Int -> Int
orInt a b = if a /= 0 then a else b

orDirection : Direction -> Direction -> Direction
orDirection a b =
    { dx = orInt a.dx b.dx
    , dy = orInt a.dy b.dy }

type alias UserInput =
    { dx : Int
    , dy : Int}

keyboardInput : Signal Direction
keyboardInput = Direction <~ (.x <~ Keyboard.arrows) ~ (.y <~ Keyboard.arrows)

userInput : Signal UserInput
userInput = orDirection <~ keyboardInput ~ panDirection

type Input = TimeDelta Float | UserAction UserInput | LevelLoaded LevelId Level

{-- Game Model ---------------------------------------------------------------
------------------------------------------------------------------------------}

type GameColor = Red | Green | Blue

type BoxAction = BoxMove | BoxDisappear
type GoalAction = GoalDisappear

type alias Positioned a = { a | x : Int, y : Int}
type alias Colored a = { a | color : GameColor}

type alias Box = Positioned (Colored
    { boxActions : List BoxAction})

makeBox : Int -> Int -> GameColor -> Box
makeBox x y color =
    {x = x, y = y, color = color, boxActions = []}

type alias Goal = Positioned (Colored
    { goalActions : List GoalAction})

makeGoal : Int -> Int -> GameColor -> Goal
makeGoal x y color =
    {x = x, y = y, color = color, goalActions = []}

type alias Wall = Positioned
    {}

makeWall : Int -> Int -> Wall
makeWall x y = {x = x, y = y}

type alias Direction =
    { dx : Int
    , dy : Int}

type alias Dimensions =
    { dimx : Int
    , dimy : Int}

makeDimensions : Int -> Int -> Dimensions
makeDimensions dimx dimy = {dimx = dimx, dimy = dimy}

type alias Level =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , dimensions : Dimensions}

makeLevel : List Box -> List Goal -> List Wall -> Dimensions -> Level
makeLevel boxes goals walls dimensions =
    {boxes = boxes, goals = goals, walls = walls, dimensions = dimensions}

type alias LevelState =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , direction : Direction
    , dimensions : Dimensions
    , frame : Int
    , id : LevelId}

still : Direction
still = { dx = 0, dy = 0}

type alias LevelId = Int

type GameState = LoadLevel LevelId | PlayingLevel LevelState | GameError String

{-- JSON definition ----------------------------------------------------------
------------------------------------------------------------------------------}

stringToColor : String -> GameColor
stringToColor s =
    if | s == "red" -> Red
       | s == "green" -> Green
       | s == "blue" -> Blue

colorDecoder : Decoder GameColor
colorDecoder = Json.map stringToColor Json.string

boxDecoder : Decoder Box
boxDecoder = Json.object3 makeBox
             ("x" := Json.int)
             ("y" := Json.int)
             ("color" := colorDecoder)

goalDecoder : Decoder Goal
goalDecoder = Json.object3 makeGoal
              ("x" := Json.int)
              ("y" := Json.int)
              ("color" := colorDecoder)

wallDecoder : Decoder Wall
wallDecoder = Json.object2 makeWall
              ("x" := Json.int)
              ("y" := Json.int)

dimensionsDecoder : Decoder Dimensions
dimensionsDecoder = Json.object2 makeDimensions
                    ("width" := Json.int)
                    ("height" := Json.int)

levelDecoder : Decoder Level
levelDecoder = Json.object4 makeLevel
               ("boxes" := Json.list boxDecoder)
               ("goals" := Json.list goalDecoder)
               ("walls" := Json.list wallDecoder)
               ("dimensions" := dimensionsDecoder)

{-- Starting conditions ------------------------------------------------------
------------------------------------------------------------------------------}
        
defaultGame : GameState
defaultGame = LoadLevel 0

{-- Update the game ----------------------------------------------------------
------------------------------------------------------------------------------}

loadLevel : LevelId -> Level -> LevelState
loadLevel id l = { boxes = l.boxes
              , goals = l.goals
              , walls = l.walls
              , dimensions = l.dimensions
              , direction = still
              , frame = numFrames - 1
              , id = id}

numFrames = 8

occupiesSameSpot : Positioned a -> Positioned b -> Bool
occupiesSameSpot p1 p2 = p1.x == p2.x && p1.y == p2.y

inBounds : Dimensions -> Box -> Bool
inBounds {dimx,dimy} {x,y} =
    x >= 0 && x < dimx && y >= 0 && y < dimy

clearOfPositioned : List (Positioned a) -> Box -> Bool
clearOfPositioned xs box = xs
                 |> List.filter (occupiesSameSpot box)
                 |> List.isEmpty

-- Generic such that a list of goals can be passed for the box or a list of boxes can be found for the goal.
reachedGoal : List (Colored (Positioned a)) -> (Colored (Positioned b)) -> Bool
reachedGoal goals box = goals
                      |> List.filter (occupiesSameSpot box)
                      |> List.map .color
                      |> List.member box.color

determineBoxAction : LevelState -> Box -> List BoxAction
determineBoxAction levelState box =
    let movedBox = moveBox levelState.direction box
    in if | reachedGoal levelState.goals box -> [BoxDisappear]
          | not <| inBounds levelState.dimensions movedBox -> []
          | not <| clearOfPositioned levelState.walls movedBox -> []
          | not <| clearOfPositioned levelState.boxes movedBox -> []
          | otherwise -> [BoxMove]

tagBoxAction : LevelState -> Box -> Box
tagBoxAction levelState box = { box | boxActions <- determineBoxAction levelState box}

determineGoalAction : LevelState -> Goal -> List GoalAction
determineGoalAction levelState goal =
    if reachedGoal levelState.boxes goal then [GoalDisappear] else []

tagGoalAction : LevelState -> Goal -> Goal
tagGoalAction levelState goal = { goal | goalActions <- determineGoalAction levelState goal}
       
moveBox : Direction -> Box -> Box
moveBox {dx,dy} ({x,y} as box) =
    { box | x <- x + dx,
            y <- y + dy}

comp : (a -> Bool) -> a -> Bool
comp f x = not (f x)

determineActions : LevelState -> LevelState
determineActions levelState =
    { levelState | boxes <- List.map (tagBoxAction levelState) levelState.boxes,
                  goals <- List.map (tagGoalAction levelState) levelState.goals}

applyBoxActions : LevelState -> LevelState
applyBoxActions levelState =
    let remaining = List.filter (comp (List.member BoxDisappear) << .boxActions) levelState.boxes
        mobile = List.filter (List.member BoxMove << .boxActions) remaining
        immobile = List.filter ((List.member BoxMove << .boxActions) |> comp) remaining
    in {levelState | boxes <- immobile ++ List.map (moveBox levelState.direction) mobile,
                    direction <- if List.isEmpty mobile then still else levelState.direction}

applyGoalActions : LevelState -> LevelState
applyGoalActions levelState =
    {levelState | goals <- List.filter (comp (List.member GoalDisappear) << .goalActions) levelState.goals}

applyActions : LevelState -> LevelState
applyActions levelState = levelState |> applyBoxActions |> applyGoalActions

tryChangeDirection : UserInput -> LevelState -> LevelState
tryChangeDirection input levelState =
    if (levelState.direction == still)
      then { levelState | direction <- input, frame <- 0 }
      else levelState

stepFrame : LevelState -> LevelState
stepFrame levelState =
    if levelState.direction == still
    then levelState
    else { levelState | frame <- (levelState.frame + 1) % numFrames}

isGameOver : LevelState -> Bool
isGameOver = .goals >> List.isEmpty

timeUpdate : LevelState -> GameState
timeUpdate levelState =
    let newState = applyActions levelState
    in if isGameOver newState
       then LoadLevel (levelState.id + 1)
       else newState |> determineActions |> stepFrame |> PlayingLevel 

stepLevel : Input -> LevelState -> GameState
stepLevel input levelState =
    case input of
      TimeDelta _ -> if levelState.frame == numFrames - 1 then timeUpdate levelState else PlayingLevel <| stepFrame levelState
      UserAction userInput -> PlayingLevel (levelState |> tryChangeDirection userInput |> determineActions)
      LevelLoaded _ _ -> PlayingLevel levelState

checkLevelLoaded : Input -> GameState -> GameState
checkLevelLoaded input gameState =
    case input of
      TimeDelta _ -> gameState
      UserAction _ -> gameState
      LevelLoaded id lvl -> PlayingLevel <| loadLevel id lvl

stepGameState : Input -> GameState -> GameState
stepGameState input gameState = case gameState of
                                  LoadLevel x -> checkLevelLoaded input gameState
                                  PlayingLevel lvl -> stepLevel input lvl
                                  GameError msg -> GameError msg

{-- Display the game ---------------------------------------------------------
------------------------------------------------------------------------------}

type alias CellInfo =
    { screenDimX : Float
    , screenDimY : Float
    , cellDimX : Float
    , cellDimY : Float}

dispColor : GameColor -> Color
dispColor color = case color of
                    Red -> rgb 255 0 0
                    Green -> rgb 0 255 0
                    Blue -> rgb 0 0 255

backgroundColor : Color
backgroundColor = rgb 50 80 100

wallColor : Color
wallColor = rgb 255 255 255

cellDimensions : (Int,Int) -> Dimensions -> (Float,Float)
cellDimensions (w,h) {dimx,dimy} =
    let fw = toFloat w
        fh = toFloat h
        fDimX = toFloat dimx
        fDimY = toFloat dimy
    in (fw / fDimX, fh / fDimY)

cellPosition : CellInfo -> {a | x : Int, y : Int} -> (Float,Float)
cellPosition {cellDimX, cellDimY, screenDimX, screenDimY} {x,y} =
    let fx = toFloat x
        fy = toFloat y
    in (fx * cellDimX - screenDimX / 2 + cellDimX / 2, fy * cellDimY - screenDimY / 2 + cellDimY / 2)

displayBox : CellInfo -> Box -> Form
displayBox cell box = rect cell.cellDimX cell.cellDimY |> filled (dispColor box.color) |> move (cellPosition cell box)

tweenVec : Direction -> CellInfo -> Int -> (Float,Float)
tweenVec {dx,dy} cell frame =
    let fdx = toFloat dx
        fdy = toFloat dy
        fframe = toFloat frame
        fNumFrames = toFloat numFrames
    in (fdx * cell.cellDimX * fframe / fNumFrames, fdy * cell.cellDimY * fframe / fNumFrames)

tweenBox : CellInfo -> Box -> LevelState -> Form -> Form
tweenBox cell box levelState element =
    if List.member BoxMove box.boxActions
    then move (tweenVec levelState.direction cell levelState.frame) element
    else element

displayWall : CellInfo -> Wall -> Form
displayWall cell wall = rect cell.cellDimX cell.cellDimY |> filled wallColor |> move (cellPosition cell wall)

displayGoal : CellInfo -> Goal -> Form
displayGoal cell goal = oval cell.cellDimX cell.cellDimY |> filled (dispColor goal.color) |> move (cellPosition cell goal)

foreground : (Int,Int) -> LevelState -> Element
foreground (w,h) levelState =
    let (cellWidth,cellHeight) = cellDimensions (w,h) levelState.dimensions
        cell =
            { cellDimX = cellWidth
            , cellDimY = cellHeight
            , screenDimX = toFloat w
            , screenDimY = toFloat h}
    in collage w h <|
       List.concat [ List.map (\ b -> displayBox cell b |> tweenBox cell b levelState) levelState.boxes
                   , List.map (displayWall cell) levelState.walls
                   , List.map (displayGoal cell) levelState.goals]

displayGame : (Int,Int) -> LevelState -> Element
displayGame (w,h) levelState =
    collage w h
                [ rect (toFloat w) (toFloat h) |> filled backgroundColor
                , (foreground (w,h) levelState |> toForm)]

display : (Int,Int) -> GameState -> Element
display dim gameState =
    case gameState of
      LoadLevel id -> show <| "Should load level " ++ (toString id)
      PlayingLevel levelState -> displayGame dim levelState
      GameError msg -> show msg

-- display dim gameState =
--     show gameState

{-- Rest urls ----------------------------------------------------------------
------------------------------------------------------------------------------}

levelUrl : LevelId -> String
levelUrl id = Http.url "/levels/" [("id", toString id)]

{-- Javascript interop for gestures ------------------------------------------
------------------------------------------------------------------------------}

port panDirection : Signal Direction

{-- Wire up level querying ---------------------------------------------------
------------------------------------------------------------------------------}

currentLevel : Signal.Mailbox Level
currentLevel = Signal.mailbox {boxes = [], walls = [], goals = [], dimensions = {dimx = 0, dimy = 0}}

port queryLevel : Signal (Task Http.Error ())
port queryLevel =
    levelQuery.signal
        |> Signal.map getLevel
        |> Signal.map (\task -> task `andThen` Signal.send currentLevel.address)

levelQuery : Signal.Mailbox LevelId
levelQuery = Signal.mailbox 0

getLevel : LevelId -> Task Http.Error Level
getLevel id = Http.get levelDecoder (levelUrl id)

maybeLevelId : GameState -> Maybe LevelId
maybeLevelId gameState =
    case gameState of
      LoadLevel id -> Just id
      _ -> Nothing

gameNewLevel : Signal LevelId
gameNewLevel = levelState
             |> Signal.filterMap maybeLevelId 0
             |> Signal.dropRepeats

port sendToQuery : Signal (Task LevelId ())
port sendToQuery =
    Signal.send levelQuery.address <~ gameNewLevel

{-- Main signals -------------------------------------------------------------
------------------------------------------------------------------------------}

delta : Signal Float
delta = Time.fps 60

loadComplete : Signal Input
loadComplete =
    Signal.sampleOn currentLevel.signal
          <| LevelLoaded <~ levelQuery.signal ~ currentLevel.signal

input : Signal Input
input = Signal.mergeMany [ (TimeDelta <~ delta)
                         , (UserAction <~ userInput)
                         , loadComplete]

levelState : Signal GameState
levelState =
    Signal.foldp stepGameState defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ (Signal.dropRepeats levelState)
