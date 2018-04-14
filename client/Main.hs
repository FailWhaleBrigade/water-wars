-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import qualified WaterWars.Client.Render.Main

import Prelude

main :: IO ()
main = WaterWars.Client.Render.Main.main
