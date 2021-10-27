module Main exposing (..)

import Browser
import Html
import Html.Attributes
import Html.Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Random
import Time

main = Browser.element {init=init, update=update, view=view, subscriptions=subscriptions}

type alias Dew = {x:Int, y:Int, size:Int}
type alias Splash = {x:Int, y:Int, vx:Int, vy:Int}
       
type alias Model = {dews: List Dew
                   ,splashes: List Splash
                   ,rest: Int}
    
type Msg = InitGenerated (List Int)
    | Dropped Int Int
    | Move Time.Posix
    | Solve
    | MonteCarlo
    | RandomGenerated (List Int)
bSize = 3
      
init: () -> (Model, Cmd Msg)
init _ = ({dews=[]
          ,splashes=[]
          ,rest = 10
          }
         ,Random.generate InitGenerated (Random.list (bSize*bSize) (Random.int 0 3)))

run: Model -> (Model, Cmd Msg)
run model = ({model|dews=[]
          ,splashes=[]
          }
            ,Random.generate RandomGenerated (Random.list (bSize*bSize) (Random.int 0 3)))

    
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InitGenerated rlist ->
            ({model|dews =
                  List.filter (\p -> p.size >0) (randomize 1 1 rlist)}
            , Cmd.none)
        RandomGenerated rlist ->
            if Debug.log "" model.rest == 0 then
                (model, Cmd.none)
            else
                ({model|splashes=[]
                 , rest = model.rest - 1
                 , dews =List.filter (\p -> p.size >0) (randomize 1 1 rlist)}
                ,Random.generate RandomGenerated
                    (Random.list (bSize*bSize) (Random.int 0 3)))
        Dropped x y ->
            (drop x y model |> phi , Cmd.none)
        Move _ ->
            (phi model, Cmd.none)
        Solve ->
            let
                sol = Debug.log ""  <|
                      solve (List.repeat (bSize*bSize*4) {x=0,y=0}) [] model
            in
                (model, Cmd.none)
        MonteCarlo ->
            run model
                --(model, Cmd.none)
                
phi: Model -> Model
phi model =
    let
        support dlist =
            List.map (\d -> (d.x,d.y)) dlist
        numsplash d = List.length (List.filter (\s -> s.x==d.x && s.y==d.y) model.splashes)
        newDews = List.map (\d -> {d | size = (numsplash d)+d.size}) model.dews |>
                  List.filter (\d -> d.size < 4)
        diff = List.filter (\p -> not(List.member p (support newDews)))
               (support model.dews)
        burstAt p =
            let
                x=Tuple.first p
                y=Tuple.second p
            in
                [{x=x+1,  y=y, vx=1, vy=0}
                ,{x=x-1,  y=y, vx=-1, vy=0}
                ,{x=x,  y=y+1, vx=0, vy=1}
                ,{x=x,  y=y-1, vx=0, vy=-1}]
        burstSplashes =
            List.foldr  (\p list -> List.concat [burstAt p, list]) [] diff
        remainingSplashes =
            List.map (\s -> {s|x=(s.x+s.vx), y=(s.y+s.vy)}) <|
            List.filter (\s -> not <| List.member (s.x,s.y) (support model.dews))
                model.splashes
    in
        {model | dews=newDews
        , splashes=List.filter (\s -> s.x > 0 && s.x <= bSize && s.y > 0 && s.y <= bSize) <|
            List.concat [burstSplashes, remainingSplashes]}

                
drop: Int -> Int -> Model -> Model
drop x y model =
    {model|splashes = List.concat [[{x=x,y=y,vx=0,vy=0}],model.splashes]}

randomize x y rlist =
    if y > bSize then
        []
    else if x> bSize then
             randomize 1 (y+1) rlist
         else
             {x=x
             , y=y
             , size = (Maybe.withDefault 0 (List.head rlist))
             }
         ::
             (randomize (x+1) y (List.drop 1 rlist))


sweep: Int -> Model -> Model
sweep depth model =
    let
        target =  Debug.log "at" <| List.head model.dews
    in
        case target of
            Nothing -> model
            Just d -> drop d.x d.y model |> mphi |> (sweep (depth+1))

mphi: Model -> Model
mphi model =
    if not (List.isEmpty model.splashes) then
        phi model |> mphi 
    else
        model

search: Int -> Dew -> Model -> List Int
search depth dew model =
    let
        dropped = drop dew.x dew.y model |> mphi
    in
        if List.isEmpty dropped.dews then
            [depth+1]
        else
            List.concat <|List.map (\d -> search (depth+1) d dropped)  dropped.dews

type alias Solution = List {x:Int, y:Int}

solve: Solution -> Solution  -> Model -> Solution
solve  minimum solution model =
    if List.isEmpty model.dews then
        solution
    else if (List.length solution) >= (List.length minimum) then
             minimum
         else 
             List.foldr (\d m ->
                             let
                                 sol = (drop d.x d.y model |> mphi |>
                                            solve m ({x=d.x,y=d.y}::solution))
                             in
                                 if (List.length sol) < (List.length minimum) then
                                     sol
                                 else
                                     m
                        ) minimum  model.dews

                
gen01Seq: Int -> String -> List String
gen01Seq length str =
    if String.length str == length then
        [str]
    else
        List.concat [
             gen01Seq length <| String.append str "0"                  
            ,gen01Seq length <| String.append str "1"
            ]
            
suiteki dew =
    circle [cx (String.fromInt (50*dew.x))
           ,cy (String.fromInt (50*dew.y))
           ,r (String.fromInt (8*dew.size))
           ,fill "#aaf"
           ,onClick (Dropped dew.x dew.y)][]

himatsu splash =
    g [transform (String.concat ["translate("
                                ,(String.fromInt (50*splash.x))
                                ,","
                                ,(String.fromInt (50*splash.y))
                                ,")"])]
        [
         circle [cx (String.fromInt (5*splash.vx))
                ,cy (String.fromInt (5*splash.vy))
                ,r (String.fromInt (6))
                ,fill "#33f"
                ][]
        ,circle [cx (String.fromInt (-1*splash.vx))
                ,cy (String.fromInt (-1*splash.vy))
                ,r (String.fromInt (3))
                ,fill "#33f"
                ][]
        ,circle [cx (String.fromInt (-8*splash.vx))
                ,cy (String.fromInt (-8*splash.vy))
                ,r (String.fromInt (2))
                ,fill "#33f"
                ][]
        ]
    
borderR ry =
    rect [x "25"
         ,y (String.fromInt (50*ry-25))
         ,width (String.fromInt (50*bSize))
         ,height "50"
         ,fill "none"
         ,stroke "black"]
    []

borderC cx =
    rect [x (String.fromInt (50*cx-25))
         ,y "25"
         ,width "50"
         ,height (String.fromInt (50*bSize))
         ,fill "none"
         ,stroke "black"]
    []
        
view model =
    Html.div [][
         Html.button[Html.Events.onClick Solve][Html.text "solve"]
        ,Html.button[Html.Events.onClick MonteCarlo][Html.text "Simulation"]
        ,Html.br[][]
        ,svg [width "500"
             ,height "500"]
             (List.concat [
                   (List.map suiteki model.dews)
                  ,(List.map himatsu model.splashes)
                  ,(List.map borderR (List.range 1 bSize))
                  ,(List.map borderC (List.range 1 bSize))
                  ]
             )
        ]


subscriptions model =
    Time.every 500 Move
