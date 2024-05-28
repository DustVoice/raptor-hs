module Main where

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

data Trip = Trip
  { tripId :: !String,
    stopTimes :: ![StopTime]
  }
  deriving (Show)

data Route = Route
  { routeId :: !String,
    trips :: ![Trip]
  }
  deriving (Show)

stopsForTrip :: Trip -> [Stop]
stopsForTrip trip = map stopTimeStop $ stopTimes trip

splitTrips :: [Trip] -> [[Trip]] -> [[Trip]]
splitTrips [] splits = splits
splitTrips trips splits = foldl (flip sortIntoTrips) splits trips

sortIntoTrips :: Trip -> [[Trip]] -> [[Trip]]
sortIntoTrips trip [] = [[t]]

uniqueRoutes :: Route -> [Route]
uniqueRoutes route = []

stopsForUniqueRoute :: Route -> [Stop]
stopsForUniqueRoute route = stopsForTrip $ head $ trips route

data Transfer = Transfer
  { from :: !Stop,
    to :: !Stop,
    transferTime :: !Time
  }
  deriving (Show)

type MarkedStops = [Stop]

isFinished :: MarkedStops -> Bool
isFinished = null

stopTimesForStop :: Stop -> Trip -> [StopTime]
stopTimesForStop stop trip = [stopTime | stopTime <- stopTimes trip, stopTimeStop stopTime == stop]

tripsForStop :: Stop -> Route -> [Trip]
tripsForStop stop route = [trip | trip <- trips route, not $ null $ stopTimesForStop stop trip]

routesForStop :: Stop -> [Route] -> [Route]
routesForStop stop routes =
  [route | route <- routes, not $ null $ tripsForStop stop route]

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
                  [ StopTime (stops !! 3) 1 2,
                    StopTime (stops !! 4) 2 3,
                    StopTime (stops !! 5) 3 4,
                    StopTime (stops !! 6) 4 5
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
  let routes = [route_1, route_2]
  let connections = routesForStop (stops !! 3) routes
  let connectionRouteIds = map routeId connections
  print connectionRouteIds
