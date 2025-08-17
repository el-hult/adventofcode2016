import Test.HUnit
import qualified Day7 as D7

testDay7 :: Test
testDay7 =
  TestList
    [ TestLabel "Windowing" $ TestCase $ assertEqual "case1" ["ab", "bc", "cd", "de"] $ D7.windows 2 "abcde",
      TestLabel "Parsing" $
        TestList
          [ TestCase $ assertEqual "Show" "abba[qwerty]bajs" $ show $ D7.IPv7 ["abba", "qwerty", "bajs"],
            TestCase $ assertEqual "Parsing" (D7.IPv7 ["abba", "qwerty", "bajs"]) (D7.parseIPv7 "abba[qwerty]bajs")
          ],
      TestLabel "ABBA detection" $
        TestList
          [ TestCase $ True @=? D7.strHasABBA "abba",
            TestCase $ False @=? D7.strHasABBA "qwery",
            TestCase $ False @=? D7.strHasABBA "xxxx",
            TestCase $ False @=? D7.strHasABBA "aaaab",
            TestCase $ True @=? D7.strHasABBA "aaaabba"
          ],
      TestLabel "TLS validation" $
        TestList
          [ TestCase $ assertBool "case1" $ D7.supportsTLS . D7.parseIPv7 $ "abba[mnop]qrst",
            TestCase $ assertBool "case2" $ not . D7.supportsTLS . D7.parseIPv7 $ "abcd[bddb]xyyx",
            TestCase $ assertBool "case3" $ not . D7.supportsTLS . D7.parseIPv7 $ "aaaa[qwer]tyui",
            TestCase $ assertBool "case4" $ D7.supportsTLS . D7.parseIPv7 $ "ioxxoj[asdfgh]zxcvbn",
            TestCase $ assertBool "case5" $ D7.supportsTLS . D7.parseIPv7 $ "ioxxoj[asdfgh]zxcvbn[qwertr]alll"
          ],
      TestLabel "ABA detection" $
        TestList
          [ TestCase $ assertEqual "case1" True $ D7.correspondingABABAB "aba" "bab",
            TestCase $ assertEqual "case2" True $ not $ D7.correspondingABABAB "aba" "qyq"
          ],
      TestLabel "SSL Validation" $
        TestList
          [ TestCase $ assertBool "case1" $ D7.supportsSSL . D7.parseIPv7 $ "aba[bab]xyz",
            TestCase $ assertBool "case2" $ not . D7.supportsSSL . D7.parseIPv7 $ "xyx[xyx]xyx",
            TestCase $ assertBool "case3" $ D7.supportsSSL . D7.parseIPv7 $ "aaa[kek]eke",
            TestCase $ assertBool "case4" $ D7.supportsSSL . D7.parseIPv7 $ "zazbz[bzb]cdb"
          ]
    ]

main :: IO ()
main = runTestTT testDay7 >>= print