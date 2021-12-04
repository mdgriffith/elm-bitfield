module Benchmarks exposing (main)

import Benchmark exposing (..)
import BitField
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Bitwise

main : BenchmarkProgram
main =
    program suite

myColor =
    BitField.init
        |> BitField.set red 255
        |> BitField.set green 255
        |> BitField.set blue 100
        |> BitField.setPercentage alpha 1

red =
    BitField.field
        { offset = 0
        , length = 8
        }


green =
    BitField.field
        { offset = 8
        , length = 8
        }


blue =
    BitField.field
        { offset = 16
        , length = 8
        }


alpha =
    BitField.field
        { offset = 24
        , length = 8
        }


myColorRecord =
    { red = 255
    , green = 255
    , blue = 100
    , alpha = 1
    }


suite : Benchmark
suite =
    describe "BitField"
        [  describe "Getting a value"
                [ benchmark "Using BitField" <|
                    \_ ->
                        let
                            myRed =
                                myColor
                                    |> BitField.get red
                        in
                        myRed * 20
                , benchmark "Using a normal record" <|
                    \_ ->
                        let
                            myRed =
                                myColorRecord.red
                        in
                        myRed * 20
                ]
        ,  describe "Setting a value"
                [ benchmark "Using BitField" <|
                    \_ ->
                        myColor
                            |> BitField.set red 34
               
                , benchmark "Using a normal record" <|
                    \_ ->
                        { myColorRecord | red = 34 }
                        
                ]

        ]
   
