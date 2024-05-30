module Main where

import Data.List
import Data.Maybe

newtype Stop = Stop {stopId :: String} deriving (Show)

instance Eq Stop where
  (==) s1 s2 = stopId s1 == stopId s2

type Time = Int

data StopTime = StopTime
  { stopTimeStop :: !Stop,
    arrivalTime :: !Time,
    departureTime :: !Time
  }
  deriving (Show)

instance Eq StopTime where
  (==) st1 st2 = stopTimeStop st1 == stopTimeStop st2

data Trip = Trip
  { tripId :: !String,
    stopTimes :: ![StopTime]
  }
  deriving (Show)

instance Eq Trip where
  (==) t1 t2 = stopTimes t1 == stopTimes t2

data Route = Route
  { routeId :: !String,
    trips :: ![Trip]
  }
  deriving (Show)

instance Eq Route where
  (==) r1 r2 = routeId r1 == routeId r2

stopsForTrip :: Trip -> [Stop]
stopsForTrip trip = map stopTimeStop $ stopTimes trip

splitTrips :: [Trip] -> [[Trip]]
splitTrips = foldr sortIntoTrips []

sortIntoTrips :: Trip -> [[Trip]] -> [[Trip]]
sortIntoTrips trip [] = [[trip]]
sortIntoTrips trip ([] : tgs) = sortIntoTrips trip tgs
sortIntoTrips trip (tg@(t : _) : tgs)
  | trip == t = (t : tg) : tgs
  | otherwise = tg : sortIntoTrips trip tgs

uniqueRoutes :: Route -> [Route]
uniqueRoutes route = case splitTrips (trips route) of
  [] -> []
  [_] -> [route]
  trips -> zipWith (curry f) [1 ..] trips
  where
    f :: (Integer, [Trip]) -> Route
    f (i, tg) = Route (routeId route ++ "_" ++ show i) tg

stopsForUniqueRoute :: Route -> [Stop]
stopsForUniqueRoute (Route _ []) = []
stopsForUniqueRoute (Route _ (t : _)) = stopsForTrip t

uniqueRoutesForStop :: Stop -> [Route] -> [Route]
uniqueRoutesForStop stop routes = [route | route <- routes, stop `elem` stopsForUniqueRoute route]

isPrecedingStop :: Stop -> Stop -> [Stop] -> Bool
isPrecedingStop stop referenceStop stops = fromJust (elemIndex stop stops) < fromJust (elemIndex referenceStop stops)

data QueueItem = QueueItem
  { queueStop :: !Stop,
    queueRoute :: !Route
  }

instance Eq QueueItem where
  (==) (QueueItem _ r1) (QueueItem _ r2) = r1 == r2

insertIntoQueue :: QueueItem -> [QueueItem] -> [QueueItem]
insertIntoQueue item [] = [item]
insertIntoQueue item@(QueueItem stop route) queue = case find (== item) queue of
  Just (QueueItem qStop _) -> if isPrecedingStop stop qStop (stopsForUniqueRoute route) then item : delete item queue else queue
  Nothing -> item : queue

queueMarkedStops :: [Stop] -> [Route] -> [QueueItem] -> [QueueItem]
queueMarkedStops [] _ queue = queue
queueMarkedStops (x : xs) routes queue = queueMarkedStops xs routes (foldr (insertIntoQueue . QueueItem x) queue (uniqueRoutesForStop x routes))

data Transfer = Transfer
  { from :: !Stop,
    to :: !Stop,
    transferTime :: !Time
  }
  deriving (Show)

type MarkedStops = [Stop]

isFinished :: MarkedStops -> Bool
isFinished = null

main :: IO ()
main = do
  let stops =
        [ Stop "Winkel",
          Stop "Hattenheim",
          Stop "Erbach",
          Stop "Eltville",
          Stop "Walluf",
          Stop "Wiesbaden-Schierstein",
          Stop "Wiesbaden-Biebrich",
          Stop "Wiesbaden Hauptbahnhof"
        ]
  let route_1 =
        Route
          { routeId = "RB10",
            trips =
              [ Trip
                  "morgens"
                  [ StopTime (stops !! 0) 1 2,
                    StopTime (stops !! 1) 2 3,
                    StopTime (stops !! 2) 3 4,
                    StopTime (stops !! 3) 4 5,
                    StopTime (stops !! 4) 5 6,
                    StopTime (stops !! 5) 6 7,
                    StopTime (stops !! 6) 7 8,
                    StopTime (stops !! 7) 8 9
                  ],
                Trip
                  "mittags"
                  [ StopTime (stops !! 0) 11 12,
                    StopTime (stops !! 1) 12 13,
                    StopTime (stops !! 2) 13 14,
                    StopTime (stops !! 3) 14 15,
                    StopTime (stops !! 4) 15 16,
                    StopTime (stops !! 5) 16 17,
                    StopTime (stops !! 6) 17 18,
                    StopTime (stops !! 7) 18 19
                  ],
                Trip
                  "abends"
                  [ StopTime (stops !! 0) 21 22,
                    StopTime (stops !! 1) 22 23,
                    StopTime (stops !! 2) 23 24,
                    StopTime (stops !! 3) 24 25,
                    StopTime (stops !! 4) 25 26,
                    StopTime (stops !! 5) 26 27,
                    StopTime (stops !! 6) 27 28,
                    StopTime (stops !! 7) 28 29
                  ]
              ]
          }
  let route_2 =
        Route
          { routeId = "RB11",
            trips =
              [ Trip
                  "morgens"
                  [ StopTime (stops !! 0) 1 2,
                    StopTime (stops !! 3) 2 3,
                    StopTime (stops !! 4) 3 4,
                    StopTime (stops !! 5) 4 5,
                    StopTime (stops !! 6) 5 6
                  ],
                Trip
                  "mittags"
                  [ StopTime (stops !! 3) 11 12,
                    StopTime (stops !! 4) 12 13,
                    StopTime (stops !! 5) 13 14,
                    StopTime (stops !! 6) 14 15
                  ],
                Trip
                  "abends"
                  [ StopTime (stops !! 3) 21 22,
                    StopTime (stops !! 4) 22 23,
                    StopTime (stops !! 5) 23 24,
                    StopTime (stops !! 6) 24 25
                  ]
              ]
          }
  let routes = concatMap uniqueRoutes [route_1, route_2]
  let connections = uniqueRoutesForStop (stops !! 3) routes
  let connectionRouteIds = map routeId connections
  print connectionRouteIds
