module BitField exposing
    ( init, Bits, toInt, fromInt, toString
    , BitField, first, next, between
    , set, setPercentage, copy, clear
    , get, getFloat, getPercentage
    , has, equal
    )

{-|

    type Rgba = Rgba

    red : BitField Rgba
    red =
        BitField.first 8

    green : BitField Rgba
    green =
        red |> Bitfield.next 8

    blue : BitField Rgba
    blue =
        green |> Bitfield.next 8

    alpha : BitField Rgba
    alpha =
        blue |> Bitfield.next 8

    myColor : BitField.Bits Rgba
    myColor =
        BitField.init
            |> BitField.set red 255
            |> BitField.set green 255
            |> BitField.set blue 100
            |> BitField.setPercentage alpha 1

    myRed =
        myColor
            |> BitField.get red

@docs init, Bits, toInt, fromInt, toString

@docs BitField, first, next, between

@docs set, setPercentage, copy, clear

@docs get, getFloat, getPercentage

@docs has, equal

-}

import Bitwise


{-| -}
type BitField encoding
    = BitField
        -- We have a few useful values.
        -- Let's say we have a value that is offset 2, length 2
        --  - top, where the top `length` values are flipped and the rest are 0s
        --    11000000000000000000000000000000
        --  - mask, where the section is inverted
        --    00110000000000000000000000000000
        --    -invertedMask
        --    11001111111111111111111111111111
        { offset : Int
        , length : Int
        , top : Int
        , mask : Int
        , inverted : Int
        }


{-| -}
type Bits encoding
    = Bits Int


{-| -}
init : Bits encoding
init =
    Bits 0


{-| -}
toString : Bits encoding -> String
toString (Bits i) =
    viewBitsHelper i 0 ""


viewBitsHelper : Int -> Int -> String -> String
viewBitsHelper f slotIndex str =
    if slotIndex >= 32 then
        str

    else if Bitwise.and (slot slotIndex) f == 0 then
        viewBitsHelper f (slotIndex + 1) ("0" ++ str)

    else
        viewBitsHelper f (slotIndex + 1) ("1" ++ str)


{-| -}
slot : Int -> Int
slot slotIndex =
    1 |> Bitwise.shiftLeftBy slotIndex


{-| -}
zero : Int
zero =
    Bitwise.complement ones


{-| -}
ones : Int
ones =
    Bitwise.complement 0


{-| -}
custom :
    { offset : Int
    , length : Int
    }
    -> BitField encoding
custom details =
    let
        offset =
            min (max 0 details.offset) 31

        length =
            if details.length + offset > 32 then
                32 - offset
            else 
                details.length

        top =
            Bitwise.shiftRightZfBy (32 - length) ones

        mask =
            top
                |> Bitwise.shiftLeftBy offset
    in
    BitField
        { offset = offset
        , length = length
        , top = top
        , mask =
            mask
        , inverted =
            Bitwise.complement mask
        }


{-| Create a new bitfield that spans 2 existing bitfields.

It will start at the lowest offset and end at the farthest point described.

-}
between : BitField encoding -> BitField encoding -> BitField encoding
between (BitField one) (BitField two) =
    let
        lowest =
            min one.offset two.offset

        highest =
            max
                (one.offset + one.length)
                (two.offset + two.length)
    in
    custom
        { length =
            highest - lowest
        , offset = lowest
        }


{-| -}
first : Int -> BitField encoding
first len =
    custom
        { length = len
        , offset = 0
        }


{-| Create a new Bitfield that comes immediately after a given one.

This is really useful so you don't make a simple addition mistake!

-}
next : Int -> BitField encoding -> BitField encoding
next length (BitField f) =
    custom
        { length = length
        , offset = f.offset + f.length
        }


{-| -}
clear : BitField encoding -> Bits encoding -> Bits encoding
clear (BitField { offset, top, inverted }) (Bits bits) =
    bits
        -- clear the target section
        |> Bitwise.and inverted
        |> Bits


{-| Copy a specific bitfield from one set of bits to another.
-}
copy : BitField encoding -> Bits encoding -> Bits encoding -> Bits encoding
copy (BitField { offset, top, inverted, length, mask }) (Bits one) (Bits destination) =
    let
        newSection =
            one
                |> Bitwise.and mask
    in
    destination
        -- clear the target section
        |> Bitwise.and inverted
        -- Combine the two
        |> Bitwise.or newSection
        |> Bits


{-| -}
set : BitField encoding -> Int -> Bits encoding -> Bits encoding
set (BitField { offset, top, inverted, length }) unboundedVal (Bits bits) =
    let
        val =
            min ((2 ^ length) - 1) (max 0 unboundedVal)
    in
    bits
        -- clear the target section
        |> Bitwise.and inverted
        -- Add the new data
        |> Bitwise.or
            (Bitwise.shiftLeftBy offset
                (Bitwise.and top val)
            )
        |> Bits


{-| -}
setPercentage : BitField encoding -> Float -> Bits encoding -> Bits encoding
setPercentage (BitField { offset, top, inverted, length }) percentage (Bits bits) =
    let
        total =
            Bitwise.shiftLeftBy length 1 - 1

        val =
            round (percentage * toFloat total)
    in
    bits
        -- clear the target section
        |> Bitwise.and inverted
        -- Add the new data
        |> Bitwise.or
            (Bitwise.shiftLeftBy offset
                (Bitwise.and top val)
            )
        |> Bits


{-| -}
get : BitField encoding -> Bits encoding -> Int
get (BitField { offset, top }) (Bits bits) =
    bits
        |> Bitwise.shiftRightZfBy offset
        |> Bitwise.and top


{-| -}
getFloat : BitField encoding -> Bits encoding -> Float
getFloat (BitField { offset, top }) (Bits bits) =
    bits
        |> Bitwise.shiftRightZfBy offset
        |> Bitwise.and top
        |> toFloat


{-| -}
getPercentage : BitField encoding -> Bits encoding -> Float
getPercentage (BitField { offset, top, length }) (Bits bits) =
    let
        numerator =
            bits
                |> Bitwise.shiftRightZfBy offset
                |> Bitwise.and top
                |> toFloat
    in
    numerator / toFloat (Bitwise.shiftLeftBy length 1 - 1)


{-| -}
toInt : Bits encoding -> Int
toInt (Bits bits) =
    bits


{-| -}
fromInt : Int -> Bits encoding
fromInt =
    Bits


{-| -}
has : BitField encoding -> Bits encoding -> Bool
has (BitField bitField) (Bits base) =
    Bitwise.and bitField.mask base
        == bitField.mask


{-| -}
equal : Bits encoding -> Bits encoding -> Bool
equal (Bits one) (Bits two) =
    one - two == 0
