{-# OPTIONS_GHC -fglasgow-exts #-}

data Animal1 = Dog1 String
             | Cat1 String Int

call1 :: Animal1 -> String
call1 (Dog1 name) = name
call1 (Cat1 name whiskers) = name ++ " with " ++ show whiskers ++ " whiskers"

test1 = mapM_ (putStrLn . call1) [Dog1 "pochi", Cat1 "tama" 12]


data Dog2 = Dog2 String

callDog :: Dog2 -> String
callDog (Dog2 name) = name

data Cat2 = Cat2 String Int

callCat :: Cat2 -> String
callCat (Cat2 name whiskers) = name ++ " with " ++ show whiskers ++ " whiskers"

data Animal2 = Animal2 { call2 :: String }

dog2 :: Dog2 -> Animal2
dog2 dog = Animal2 { call2 = callDog dog }
cat2 :: Cat2 -> Animal2
cat2 cat = Animal2 { call2 = callCat cat }

test2 = mapM_ (putStrLn . call2) [dog2 (Dog2 "pochi"), cat2 (Cat2 "tama" 12)]


data Dog3 = Dog3 String
data Cat3 = Cat3 String Int

class Animal3Class a where
    call3' :: a -> String

instance Animal3Class Dog3 where
    call3' (Dog3 name) = name

instance Animal3Class Cat3 where
    call3' (Cat3 name whiskers) = name ++ " with " ++ show whiskers ++ " whiskers"

data Animal3 where
    Animal3 :: (Animal3Class a => a -> Animal3)
--data Animal3 = forall a. Animal3Class a => Animal3 a

call3 :: Animal3 -> String
call3 (Animal3 a) = call3' a

test3 = mapM_ (putStrLn . call3) [Animal3 (Dog3 "pochi"), Animal3 (Cat3 "tama" 12)]

{-
data Animal4 where
    Animal4 :: a -> Animal4

call4 :: forall a. Animal3Class a => Animal4 -> String
call4 (Animal4 a) = call3 a
-}


foo :: forall a. Read a => String -> a
foo s = read s :: a

class X a where
    bar :: a -> String

data X a => Y a = Y a

--baz :: Y a -> String
baz (Y x) = bar x
