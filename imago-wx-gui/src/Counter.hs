{-# LANGUAGE ScopedTypeVariables #-} -- allows "forall t. Moment t"

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

dt :: Double
dt = 20 * ms 
  where 
    ms = 1e-3

main :: IO ()
main = start $ do
    f <- frame [ text := "Counter"
               , bgcolor := white
               ]
    bup <- button f [text := "Up 10000"]
    output <- staticText f []
    t  <- timer f [interval := ceiling (dt * 1e3)]
 
    set f [ layout := minsize (sz 50 50) $
              column 5 [ widget bup
                       , widget output
                       ]
          ]

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do
        
          etick <- event0 t command
          eup <- event0 bup command
        
          let counter :: Behavior t Int
              counter = accumB 0 $ ((+1) <$ etick) `union` ((+10000) <$ eup) 
    
          sink output [text :== show <$> counter] 

    network <- compile networkDescription    
    actuate network
