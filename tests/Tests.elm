module Tests exposing (suite)

import BitField
import Bitwise
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


type Rgba
    = Rgba


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


suite : Test
suite =
    describe "BitField"
        [ fuzz (Fuzz.intRange 0 255) "Set value is returned" <|
            \i ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.set red i
                        |> BitField.get red
                    )
                    i
        , fuzz (Fuzz.intRange 0 255) "Setting values don't affect each other" <|
            \i ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.set green 200
                        |> BitField.set red i
                        |> BitField.get green
                    )
                    200
        , fuzz (Fuzz.floatRange 0 1) "Set red value is returned via percentage" <|
            \f ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.within (Expect.Absolute 0.002)
                    (color
                        |> BitField.setPercentage red f
                        |> BitField.getPercentage red
                    )
                    f
        , fuzz (Fuzz.floatRange 0 1) "Set blue value is returned via percentage" <|
            \f ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.within (Expect.Absolute 0.002)
                    (color
                        |> BitField.setPercentage blue f
                        |> BitField.getPercentage blue
                    )
                    f
        , test "Green oversetting percentage, comes back as 1.0" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.setPercentage green 2.5
                in
                Expect.within (Expect.Absolute 0.002)
                    (myColor
                        |> BitField.getPercentage green
                    )
                    1
        , test "Red comes back correctly" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 128
                            |> BitField.set green 255
                in
                Expect.equal
                    (myColor
                        |> BitField.get red
                    )
                    128
        , test "Over setting red doesnt bleed into green" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 20000
                            |> BitField.set green 100
                in
                Expect.equal
                    (myColor
                        |> BitField.get green
                    )
                    100
        , test "Green comes back correctly" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 182
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (myColor
                        |> BitField.get green
                    )
                    182
        , test "Oversetting green doesnt affect blue" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 238019283021
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (myColor
                        |> BitField.get blue
                    )
                    100
        , test "Blue comes back correctly" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (myColor
                        |> BitField.get blue
                    )
                    100
        , test "Alpha comes back correctly" <|
            \_ ->
                let
                    myColor =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (myColor
                        |> BitField.get alpha
                    )
                    255
        , test "Detect if a value is set" <|
            \_ ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.set red 300
                        |> BitField.has red
                    )
                    True
        , test "Detect that an empty value is `has false`" <|
            \_ ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                in
                Expect.equal
                    (color
                        |> BitField.has red
                    )
                    False
        , test "Clearing a field means it's no longer detected" <|
            \_ ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.clear red
                        |> BitField.has red
                    )
                    False
        , test "A value set out of range caps at the given max" <|
            \_ ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.set red 300
                        |> BitField.get red
                    )
                    255
        , test "A value set below range will be at 0" <|
            \_ ->
                let
                    color : BitField.Bits Rgba
                    color =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1
                in
                Expect.equal
                    (color
                        |> BitField.set red -1000
                        |> BitField.get red
                    )
                    0
        , test "Copying a field only copies that field" <|
            \_ ->
                let
                    source : BitField.Bits Rgba
                    source =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1

                    target =
                        BitField.init
                in
                Expect.equal
                    (target
                        |> BitField.copy red source
                        |> BitField.get red
                    )
                    255
        , test "Copying a field only copies that field, other fields remain blank" <|
            \_ ->
                let
                    source : BitField.Bits Rgba
                    source =
                        BitField.init
                            |> BitField.set red 255
                            |> BitField.set green 255
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1

                    target =
                        BitField.init
                in
                Expect.equal
                    (target
                        |> BitField.copy red source
                        |> BitField.get green
                    )
                    0
        , test "Multiple copies" <|
            \_ ->
                let
                    source : BitField.Bits Rgba
                    source =
                        BitField.init
                            |> BitField.set red 38
                            |> BitField.set green 136
                            |> BitField.set blue 100
                            |> BitField.setPercentage alpha 1

                    target =
                        BitField.init
                in
                Expect.equal
                    (target
                        |> BitField.copy red source
                        |> BitField.copy green source
                        |> BitField.get red
                    )
                    38
        , test "toString: 0 reports all zeros" <|
            \_ ->
                Expect.equal
                    (BitField.init
                        |> BitField.toString
                    )
                    "00000000000000000000000000000000"
        , test "toString: 1 reports a binary 1" <|
            \_ ->
                Expect.equal
                    (BitField.fromInt 1
                        |> BitField.toString
                    )
                    "00000000000000000000000000000001"
        , test "toString: 2 reports a binary 2" <|
            \_ ->
                Expect.equal
                    (BitField.fromInt 2
                        |> BitField.toString
                    )
                    "00000000000000000000000000000010"
        , test "shiftLeft actually moves left" <|
            \_ ->
                Expect.equal
                    (BitField.fromInt (1 |> Bitwise.shiftLeftBy 2)
                        |> BitField.toString
                    )
                    "00000000000000000000000000000100"
        ]


{-| -}
encode :
    { offset : Int
    , length : Int
    }
    -> Int
encode details =
    let
        offset =
            min (max 0 details.offset) 31

        length =
            if details.length + offset > 32 then
                32 - offset

            else
                details.length

        _ =
            Debug.log "ENCODE"
                { offset = offset
                , length = length
                , incoming = details
                }

        encodedOffset =
            offset

        encodedLength =
            Bitwise.shiftLeftBy 16 length

        -- top =
        --     Bitwise.shiftRightZfBy (32 - length) ones
        -- mask =
        --     top
        --         |> Bitwise.shiftLeftBy offset
    in
    Bitwise.or encodedOffset encodedLength
