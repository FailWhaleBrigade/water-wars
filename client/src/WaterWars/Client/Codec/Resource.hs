module WaterWars.Client.Codec.Resource (loadPng, bulkLoad) where


import Control.Monad.IO.Class
import Miso
import WaterWars.Client.Resources.Image

loadPng :: (MonadIO m) => MisoString -> m GameImage
loadPng path = do
    img <- liftIO $ newImage path
    pure GameImage
        { image = img
        , imageSource = path
        }

bulkLoad :: (MonadIO m, Traversable t) => t MisoString -> m (t GameImage)
bulkLoad = traverse loadPng
