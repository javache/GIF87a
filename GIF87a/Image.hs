module GIF87a.Image where

data Image = Image { signature :: String
                   } deriving (Eq)
