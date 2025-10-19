module Objects (exampleObjects) where

import HList
import Literal
import Object

exampleObjects ::
  HList
    [ Object (Literal "a"),
      Object (Literal "b"),
      Object (Literal "c")
    ]
exampleObjects =
  Object {name = makeLiteral "a"}
    `HCons` Object {name = makeLiteral "b"}
    `HCons` Object {name = makeLiteral "c"}
    `HCons` HNil
