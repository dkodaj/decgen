module DecGen.AnonymousTypes exposing (grabAnonymousTypes)

import DecGen.Types exposing (..)

import List exposing (concat, filter, foldl, foldr, map, range, reverse)
import String exposing (contains, dropLeft, dropRight, fromChar, join, indices, left, length, right, split, repeat, toUpper, trim, words)


--== Find anonymous types ==--

-- An anonymous type is a record or tuple inside a type definition, e.g.:
--      type X = Y (String, Int) | Z {a: Bool, b: Float}

-- The module will generate decoders for anonymous types, but the result is ugly.
-- If you want it nice, use aliases instead:

--      type X = Y MyTuple | Z MyRecord
--      type alias MyTuple = (String, Int)
--      type alias MyRecord = {a: Bool, b: Float}

anonymous: TypeDef -> List Type
anonymous typeDef =
    anonymousHelp True typeDef.theType []

anonymousHelp: Bool -> Type -> List Type -> List Type
anonymousHelp topLevel a xs =
    case a of
        TypeAlias b->
            xs
        TypeArray b->
           anonymousHelp False b xs
        TypeBool->
            xs
        TypeDict (b,c)->
            let
                recurseOn = anonymousHelp False b << anonymousHelp False c
            in
                case topLevel of
                    True->
                        recurseOn xs
                    False->
                        recurseOn (a::xs)
        TypeFloat->
            xs
        TypeInt->
            xs
        TypeList b->
            anonymousHelp False b xs
        TypeMaybe b->
            anonymousHelp False b xs
        TypeRecord b->
            let
                recurseOn = foldr (<<) identity <| map (anonymousHelp False) <| map .fieldType b 
            in
                case topLevel of
                    True->
                        recurseOn xs
                    False->
                        recurseOn (a::xs)
        TypeString->
            xs
        TypeTuple (b,c)->
            let
                recurseOn = anonymousHelp False b << anonymousHelp False c
            in
                case topLevel of
                    True->
                        recurseOn xs
                    False->
                        recurseOn (a::xs)
        TypeUnion b->
            let
                typeList = concat <| map (\(x,y) -> y) b 
            in
                ( foldr (<<) identity <| map (anonymousHelp False) typeList ) xs

grabAnonymousTypes: List TypeDef -> List Type
grabAnonymousTypes typeDefs =
    unique <| concat <| map anonymous typeDefs

unique: List a -> List a
unique xs =
    uniqueHelp [] xs

uniqueHelp: List a -> List a -> List a
uniqueHelp checked remaining =
    case remaining of
        []->
            checked
        x::xs->
            case filter (\a->(a==x)) (checked++xs) of
                []->
                    uniqueHelp (x::checked) xs
                _->
                   uniqueHelp checked xs 

