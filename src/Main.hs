{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

dt :: Double
dt = 20 * ms where ms = 1e-3

myimage :: Bitmap ()
myimage = bitmap "image.jpg"

main :: IO ()
main = start $ do
    f       <- frame [text := "Hokus Pokus"
                    ]
    bup     <- button f [text := "Up"]
    output  <- staticText f []
    t  <- timer f [ interval := ceiling (dt * 1e3) ]
    pp <- panel f [bgcolor := white
                 ]
 
    set f [layout := row 5 [minsize (sz 400 300) $ widget pp
                           ,column 5 $ [widget bup
                                       ,widget output
                                       ]
                           ]
          ]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
          etick <- event0 t command
          eup <- event0 bup command
        
          let counter :: Behavior t Int
              counter = accumB 0 $ ((+1) <$ etick) `union` ((+1000) <$ eup) 
    
          let drawSprite :: DC a -> b -> IO ()
              drawSprite dc _view = drawBitmap dc myimage (point 0 0) True []
          sink output [text :== show <$> counter] 
          sink pp [on paint :== pure drawSprite]
          reactimate $ repaint pp <$ etick

    network <- compile networkDescription    
    actuate network
