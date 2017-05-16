data Cmplx a = Cmplx { real :: a, img :: a } deriving (Eq, Show)

cEx1, cEx2 :: Cmplx Double
cEx1 = Cmplx { real = 1.0, img = 1.0 }
cEx2 = Cmplx { real = 0.0, img = 1.0 }

instance Num a => Num (Cmplx a) where
  c1 + c2 = Cmplx { real = real c1 + real c2,  img = img c1 + img c2 }
  (*) = undefined
  abs = undefined
  negate c = Cmplx { real = negate $ real c, img = negate $ img c } 
  fromInteger i = Cmplx { real = fromInteger i, img = fromInteger 0 }
