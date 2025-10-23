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
    [ Object' Int (Literal "a"),
      Object' Int (Literal "b"),
      Object' Int (Literal "c")
    ]
exampleObjects' =
  Object' {age = 20, title = Literal @"a"}
    `HCons` Object' {age = 30, title = Literal @"b"}
    `HCons` Object' {age = 40, title = Literal @"c"}
    `HCons` HNil
