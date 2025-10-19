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
  Object {name = makeLiteral "a"}
    `HCons` Object {name = makeLiteral "b"}
    `HCons` Object {name = makeLiteral "c"}
    `HCons` HNil

exampleObjects' ::
  HList
    [ Object' Int (Literal "a"),
      Object' Int (Literal "b"),
      Object' Int (Literal "c")
    ]
exampleObjects' =
  Object' {age = 20, title = makeLiteral "a"}
    `HCons` Object' {age = 30, title = makeLiteral "b"}
    `HCons` Object' {age = 40, title = makeLiteral "c"}
    `HCons` HNil
