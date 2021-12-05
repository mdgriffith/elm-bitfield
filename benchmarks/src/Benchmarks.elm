module Benchmarks exposing (main)

import Benchmark exposing (..)
import BitField
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Bitwise

main : BenchmarkProgram
main =
    program suite

type Rgba = Rgba

myColor =
    BitField.init
        |> BitField.set red 255
        |> BitField.set green 255
        |> BitField.set blue 100
        |> BitField.setPercentage alpha 1


red : BitField.BitField Rgba
red =
    BitField.first 8
        

green : BitField.BitField Rgba
green =
    red |> BitField.next 8 


blue : BitField.BitField Rgba
blue =
    green |> BitField.next 8 


alpha : BitField.BitField Rgba
alpha =
    blue |> BitField.next 8

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
                [ benchmark "Get: BitField" <|
                    \_ ->
                        let
                            myRed =
                                myColor
                                    |> BitField.get red
                        in
                        myRed * 20
                , benchmark "Get: normal record" <|
                    \_ ->
                        let
                            myRed =
                                myColorRecord.red
                        in
                        myRed * 20
                ]
        ,  describe "Setting a value"
                [ benchmark "Set: BitField" <|
                    \_ ->
                        myColor
                            |> BitField.set red 34
               
                , benchmark "Set: normal record" <|
                    \_ ->
                        { myColorRecord | red = 34 }
                
                , benchmark "Set: full copy, normal record" <|
                    \_ ->
                        { red = 34
                        , green = myColorRecord.green
                        , blue = myColorRecord.blue
                        , alpha = myColorRecord.alpha
                        }
                        
                ]

        ]
   
