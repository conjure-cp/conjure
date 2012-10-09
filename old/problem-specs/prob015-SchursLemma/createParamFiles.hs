
main = sequence_
    [ writeFile (filename bo ba) (filecontent bo ba)
    | bo <- [3..4]
    , ba <- [3..16]
    ]

filename bo ba = concat [shw bo, "boxes", shw ba, "balls.param"]
filecontent bo ba = unlines
    [ "language ESSENCE' 1.0"
    , ""
    , "letting n_boxes be " ++ show bo
    , "letting n_balls be " ++ show ba
    ]

shw i = padLeft 2 '0' (show i)

padLeft i ch str = replicate (i - length str) ch ++ str
