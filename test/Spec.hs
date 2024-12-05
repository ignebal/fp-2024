{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [lib2Tests]

lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests"
  [ testParseQuery
  , testEmptyState
  , testStateTransition
  ]

testParseQuery :: TestTree
testParseQuery = testGroup "parseQuery tests"
  [ testCase "Parse LIKE command" $
      Lib2.parseQuery "LIKE \"Hello world\" \"Singer\" \"Genre\" 03:45" @?=
        Right (Lib2.LikeSong (Lib2.Song "Hello world" "Singer" "Genre" (3, 45)))

  , testCase "Parse UNLIKE command" $
      Lib2.parseQuery "UNLIKE \"Hello world\" \"Singer\" \"Genre\" 03:45" @?=
        Right (Lib2.UnlikeSong (Lib2.Song "Hello world" "Singer" "Genre" (3, 45)))

  , testCase "Parse OPEN command (empty list)" $
      Lib2.parseQuery "OPEN" @?= Right (Lib2.OpenSongList [])

  , testCase "Parse OPEN command with one song" $
      Lib2.parseQuery "OPEN \"Song 1\" \"Singer 1\" \"Genre 1\" 03:30" @?= Right (Lib2.OpenSongList [Lib2.Song "Song 1" "Singer 1" "Genre 1" (3,30)])

  , testCase "Parse OPEN command with just spaces after open" $
      Lib2.parseQuery "OPEN    " @?= Right (Lib2.OpenSongList [])

  , testCase "Parse LIKE command with just spaces after like" $
      Lib2.parseQuery "LIKE    \"Song 1\" \"Singer 1\" \"Genre 1\" 03:30" @?= Right (Lib2.LikeSong (Lib2.Song "Song 1" "Singer 1" "Genre 1" (3,30)))

  , testCase "Parse UNLIKE command with just spaces after unlike" $
      Lib2.parseQuery "UNLIKE    \"Song 1\" \"Singer 1\" \"Genre 1\" 03:30" @?= Right (Lib2.UnlikeSong (Lib2.Song "Song 1" "Singer 1" "Genre 1" (3,30)))

    , testCase "Parse song with just seconds" $
        Lib2.parseQuery "LIKE \"Song 1\" \"Singer 1\" \"Genre 1\" 30" @?= Right (Lib2.LikeSong (Lib2.Song "Song 1" "Singer 1" "Genre 1" (0,30)))

    , testCase "Parse song with just seconds in open" $
        Lib2.parseQuery "OPEN \"Song 1\" \"Singer 1\" \"Genre 1\" 30" @?= Right (Lib2.OpenSongList [Lib2.Song "Song 1" "Singer 1" "Genre 1" (0,30)])
        
    , testCase "Parse song with too many seconds" $
        Lib2.parseQuery "OPEN \"Song1\" \"Singer 1\" \"Genre 1\" 60" @?= Left "Seconds must be between 0 and 59"
      ]

testEmptyState :: TestTree
testEmptyState = testGroup "emptyState tests"
  [ testCase "Empty state has an empty song list" $
      Lib2.songs (Lib2.emptyState) @?= []
  ]

testStateTransition :: TestTree
testStateTransition = testGroup "stateTransition tests"
  [ testCase "Like a song" $
      let song = Lib2.Song "Hello world" "Singer" "Genre" (3, 45)
          expectedState = Lib2.State {Lib2.songs = [song]}
      in Lib2.stateTransition Lib2.emptyState (Lib2.LikeSong song) @?= Right (Just "Song liked!", expectedState)

  , testCase "Unlike a liked song" $
      let song = Lib2.Song "Hello world" "Singer" "Genre" (3, 45)
          initialState = Lib2.State [song]
          expectedState = Lib2.State []
      in Lib2.stateTransition initialState (Lib2.UnlikeSong song) @?= Right (Just "Song unliked!", expectedState)

  , testCase "Unlike a non-existent song" $
      let song = Lib2.Song "Hello world" "Singer" "Genre" (3, 55)
      in Lib2.stateTransition Lib2.emptyState (Lib2.UnlikeSong song) @?= Left "Song not found"

    , testCase "Open after liking a song" $
        let song = Lib2.Song "Hello world" "Singer" "Genre" (3, 45)
            initialState = Lib2.emptyState
            expectedOutput = show [song]
        in case Lib2.stateTransition initialState (Lib2.LikeSong song) of
            Right (_, stateAfterLike) -> case Lib2.stateTransition stateAfterLike (Lib2.OpenSongList []) of
                Right (Just message, _) -> message @?= expectedOutput
                Left err -> fail $ "Open command failed: " ++ err
                Right (Nothing,_) -> fail "Open command returned Nothing message"
            Left err -> fail $ "Like command failed: " ++ err
  ]