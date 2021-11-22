module Tests exposing (..)

import Test exposing (..)
import GameTests
import Expect


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        , skip <| test "This test should fail" <|
            \_ ->
                Expect.fail "failed as expected!"
        , GameTests.all
        ]
