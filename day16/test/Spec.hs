

testDay16 =
  TestList $
    map TestCase
      [ (@=?) "10000011110010000111" $ D16.toStr . take 20 . D16.dragonUntil 20 . D16.toBV $ "10000",
        (@=?) "100" $ D16.toStr . D16.getChecksum . D16.toBV $ "110010110100",
        (@=?) "01100" $ D16.toStr . D16.getChecksum . D16.toBV $ "10000011110010000111",
        (@=?) "01100" $ D16.solve 20 "10000"
      ]
