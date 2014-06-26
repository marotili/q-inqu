module Text where

import Test.HUnit
import Game.Render.Text
import Control.Lens

--tests :: Test
--tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

testTextAtlas :: Test
testTextAtlas = TestCase (do
        fm <- newFontManager
        font <- newFont fm "data/font.otf" 24
        newFontAtlas font
        return ()
    )

