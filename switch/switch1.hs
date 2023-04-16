data A = A
data B = B

value :: Bool -> Either A B
value True = Left A
value False = Right B

func :: Bool -> Either (A -> String) (B -> String)
func True = Left $ \A -> "A"
func False = Right $ \B -> "B"

test :: Bool -> String
test useA = case (value useA, func useA) of
  (Left a, Left fa) -> fa a
  (Right b, Right fb) -> fb b
  _ -> error "Never happens"
