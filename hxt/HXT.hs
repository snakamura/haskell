import Text.XML.HXT.Arrow

test1 = do xml <- runX $ xshow $ eelem "foo"
           putStrLn $ concat xml


testIO xml = runX (xshow xml) >>= putStrLn . concat

test2 = testIO $ aelem "foo" [sattr "abc" "def"]

test3 = testIO $ selem "foo" [eelem "bar"]

test4 = testIO $ mkelem "foo" [sattr "abc" "2"] [eelem "bar"]

test5 = testIO $ eelem "foo" += eelem "bar"
                             += sattr "baz" "bee"


testLA xml = concat $ runLA (xshow xml) []

test6 = testLA $ eelem "foo"
