import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Random exposing (float, generate, initialSeed )

import Debug




-- MODEL

(simWidth,simHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)
----------------------------------------------------------------------
type State = Running | Paused
----------------------------------------------------------------------
type AntKind = Internal | External | Brood
----------------------------------------------------------------------
type alias Ant =
  { x:Float, y:Float,
    vx:Float, vy:Float,
    kind:AntKind,
    pheroAttraction:Float, --Attraction to pheromone
    pheroEmmit:Float --Pheromone emmitting capacity
  }
----------------------------------------------------------------------
type alias Pheromone =
  { x:Float, y:Float,
    strength:Float
  }
----------------------------------------------------------------------
--initAnt: AntKind -> (Float,Float) -> (Float,Float) -> Ant
initAnt k pa pe (xx,yy) (vxx,vyy)=
  {x=xx, y=yy, vx=vxx, vy=vyy,
    kind=k, pheroAttraction=pa, pheroEmmit=pe}
----------------------------------------------------------------------
computeVectorComponents d =
  let
    alpha = degrees d
    vx = cos alpha - sin alpha
    vy = sin alpha + cos alpha
  in
  (vx,vy)
----------------------------------------------------------------------
--initAnts: AntKind->Random.Seed->Float->(List Ant)
initAnts kind pa pe seed dispersion count=
  let
    (x, s1) = Random.generate (Random.list count
                                  (float -dispersion dispersion)) seed
    (y, s2) = Random.generate (Random.list count
                                  (float -dispersion dispersion)) s1
    (dir, s3) = Random.generate (Random.list count (float 0 360)) s2

    vel = List.map computeVectorComponents dir
    pos = List.map2 (,) x y
  in
    List.map2 (initAnt kind pa pe) pos vel
----------------------------------------------------------------------
type alias Simulation =
  { state: State,
    ants_I: List Ant,
    ants_E: List Ant,
    ants_B: List Ant,
    pheromones: List Pheromone,
    delta: Time
  }
----------------------------------------------------------------------
defaultSim: Simulation
defaultSim =
  let
    internal = initAnts Internal 0.5 0.2 (initialSeed 9) 50 10
    external = initAnts External 0.2 0.1 (initialSeed 10) 100 10
    brood = initAnts Brood 0.9 0.9 (initialSeed 11) 10 10
  in
  { state = Paused,
    ants_I = internal,
    ants_E = external,
    ants_B = brood,
    pheromones = [],
    delta = 0.0
  }
----------------------------------------------------------------------
type alias Input =
  { space : Bool,
    delta : Time
  }



--UPDATE
----------------------------------------------------------------------
--update : Input -> Simulation -> Simulation
update ({space, delta} as inp) ({state, ants_I, ants_E, ants_B, pheromones, delta} as sim)=
  let
    newState =
      if space && state == Paused then
        Running
      else if space && state == Running then
        Paused
      else
        state

    newAnts_I =
      if state == Paused then
        sim.ants_I
      else
        updateAnts inp.delta sim.ants_I

    newAnts_E =
      if state == Paused then
        sim.ants_E
      else
        updateAnts inp.delta sim.ants_E

    newAnts_B =
      if state == Paused then
        sim.ants_B
      else
        updateAnts inp.delta sim.ants_B

    newDelta = inp.delta -- + 1

    newPheromones = List.concat
        [ newAnts_I |> List.indexedMap (pheromoneUpdate (inp.delta*20)),
          newAnts_E |> List.indexedMap (pheromoneUpdate (inp.delta*30)),
          newAnts_B |> List.indexedMap (pheromoneUpdate (inp.delta*40)),
          sim.pheromones |> List.map (\p -> {p| strength = p.strength - 0.01})
        ]
        |> List.filter (\p -> p.strength > 0.01)
  in
    { sim |
      state = newState,
      ants_I = newAnts_I,
      ants_E = newAnts_E,
      ants_B = newAnts_B,
      pheromones = newPheromones,
      delta = newDelta
    }

----------------------------------------------------------------------
--update ants
--updateAnts : Random.Seed -> Time -> (List Ant) -> (List Ant)
updateAnts dt ants =
  ants
    |> List.indexedMap (physicsUpdate dt)
    -- |> List.indexedMap (pheromoneUpdate dt)
----------------------------------------------------------------------
pheromoneUpdate dt i ant =
  let
    (p1,s1) = generate (float 0 1) (initialSeed <| floor dt + i)
    (p2,s2) = generate (float 0 1) s1
    pm = if p1 < ant.pheroEmmit then 1.0 else 0.0
  in
    { x = ant.x, y = ant.y, strength = pm}
----------------------------------------------------------------------
--update physics
physicsUpdate dt i ant =
  let
    (p1,s1) = generate (float 0 1) (initialSeed <| floor dt*10 + i)
    (p2,s2) = generate (float 0 1) s1
    --rotate by a random an angle
    (alpha1,s3) = generate (float 140 180) s2 --ants moving randomly
    (alpha2,s4) = generate (float 0 45) s3 --random jitter

    alpha3 = if p1 < 0.2 then alpha1 else 0
    alpha4 = if p2 < 0.2 then alpha3 + alpha2 else alpha3

    angle = degrees (0.05 *alpha4)
    vx1 = ant.vx * (cos angle) - ant.vy * (sin angle)
    vy1 = ant.vx * (sin angle) + ant.vy * (cos angle)
  in
    { ant |
      vx = vx1,
      vy = vy1,
      x = ant.x + vx1, -- * (dvx) * 1.0,
      y = ant.y + vy1 -- * (dvy) * 1.0
    }
    |> bounceOffWalls

----------------------------------------------------------------------
bounceOffWalls ant =
  let
    vX = if (ant.x <= -halfWidth) || (ant.x >= halfWidth) then
            -ant.vx
         else
             ant.vx
    vY = if (ant.y <= -halfHeight) || (ant.y >= halfHeight) then
            -ant.vy
         else
             ant.vy
  in
    {  ant| x = ant.x + vX, y = ant.y + vY, vx = vX, vy = vY }
----------------------------------------------------------------------
near k c n =
  n >= k-c && n <= k+c
----------------------------------------------------------------------


--VIEW
msg = "SPACE to start"
txt f string =
  Text.fromString string
  |> Text.color white
  |> Text.monospace
  |> f
  |> leftAligned
----------------------------------------------------------------------
make shape ant =
  let
    angle = atan2 ant.vy ant.vx
  in
  shape
    |> move (ant.x, ant.y)
    |> rotate angle
----------------------------------------------------------------------
view: (Int,Int) -> Simulation -> Element
view (w,h) sim =
  let
    timestamp = txt (Text.height 10) (toString sim.delta)
    --timestamp = txt (Text.height 10) (toString (fst <| generate (float -1 1) sim.seed))
    ants_I = List.map (make ((rect 7 2)|> filled red)) sim.ants_I
    ants_E = List.map (make ((rect 7 2)|> filled white)) sim.ants_E
    ants_B = List.map (make ((rect 7 2)|> filled purple)) sim.ants_B
    pheros = List.map (\ p -> (circle 1) |> filled grey |> move (p.x,p.y)) sim.pheromones
    formItems =
      ( List.concat
        [[ rect simWidth simHeight |> filled green,
          toForm (if sim.state == Running then
            spacer 1 1 else
            txt identity msg)
         ],
         [toForm timestamp |> move (-simWidth/2 + 20, simHeight/2 - 10)],
         ants_I,
         ants_E,
         ants_B,
         pheros
        ]
      )
  in
    container w h middle <|
    collage simWidth simHeight
       formItems
----------------------------------------------------------------------


--SIGNAL

main = Signal.map2 view Window.dimensions simState
----------------------------------------------------------------------
simState: Signal Simulation
simState =
  Signal.foldp update defaultSim input
----------------------------------------------------------------------
--delta = Signal.map inSeconds (fps 20)
delta = Signal.map inSeconds (every 50)
--tt = Signal.map inSeconds (every
----------------------------------------------------------------------
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map2 Input
      Keyboard.space
      delta
----------------------------------------------------------------------
