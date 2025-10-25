module Objects
  ( exampleObjects,
    exampleObjects',
  )
where

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
  Object {name = Literal @"a"}
    `HCons` Object {name = Literal @"b"}
    `HCons` Object {name = Literal @"c"}
    `HCons` HNil

exampleObjects' ::
  HList
    [ Object' Int (Literal "x"),
      Object' Int (Literal "y"),
      Object' Int (Literal "z")
    ]
exampleObjects' =
  Object' {age = 20, title = Literal @"x"}
    `HCons` Object' {age = 30, title = Literal @"y"}
    `HCons` Object' {age = 40, title = Literal @"z"}
    `HCons` HNil
