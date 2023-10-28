module Week40Exercise1 where

numberOfMoons :: PlanetarySystem -> Integer
numberOfMoons s = sum [toInteger (length (moons planet)) | planet <- planets s]

atLeastOneMoon :: PlanetarySystem -> [Planet]
atLeastOneMoon s = filter (\planet -> not (null (moons planet))) (planets s)

data PlanetarySystem = PlanetarySystem
  { sunName :: String,
    planets :: [Planet]
  }
  deriving (Eq, Show)

data Planet = Planet
  { planetName :: String,
    moons :: [Moon]
  }
  deriving (Eq, Show)

newtype Moon = Moon {moonName :: String}
  deriving (Eq, Show)

solarSystem :: PlanetarySystem
solarSystem =
  PlanetarySystem
    { sunName = "The Sun",
      planets =
        [ Planet
            { planetName = "Mercury",
              moons = []
            },
          Planet
            { planetName = "Venus",
              moons = []
            },
          Planet
            { planetName = "Earth",
              moons =
                [ Moon {moonName = "The Moon"}
                ]
            },
          Planet
            { planetName = "Mars",
              moons =
                [ Moon {moonName = "Deimos"},
                  Moon {moonName = "Phobos"}
                ]
            },
          Planet
            { planetName = "Jupiter",
              moons =
                [ Moon {moonName = "Io"},
                  Moon {moonName = "Europa"},
                  Moon {moonName = "Ganymede"},
                  Moon {moonName = "Callisto"}
                ]
            },
          Planet
            { planetName = "Saturn",
              moons =
                [ Moon {moonName = "Mimas"},
                  Moon {moonName = "Enceladus"},
                  Moon {moonName = "Tethys"},
                  Moon {moonName = "Dione"},
                  Moon {moonName = "Rhea"},
                  Moon {moonName = "Titan"},
                  Moon {moonName = "Hyperion"},
                  Moon {moonName = "Iapetus"},
                  Moon {moonName = "Phoebe"}
                ]
            },
          Planet
            { planetName = "Uranus",
              moons =
                [ Moon {moonName = "Miranda"},
                  Moon {moonName = "Ariel"},
                  Moon {moonName = "Umbriel"},
                  Moon {moonName = "Titania"},
                  Moon {moonName = "Oberon"}
                ]
            },
          Planet
            { planetName = "Neptune",
              moons =
                [ Moon {moonName = "Proteus"},
                  Moon {moonName = "Triton"},
                  Moon {moonName = "Nereid"}
                ]
            }
        ]
    }
