module AnonymousTypes exposing (grabAnonymousTypes)

import List exposing (any, concat, filter, foldr, map)
import Types exposing (Type(..), TypeDef, coreType)



--== Find anonymous types ==--

-- An anonymous type is a record or tuple inside a type definition, e.g.:
--      type X = Y (String, Int) | Z {a: Bool, b: Float}

-- The module will generate decoders for anonymous types, but the result is ugly.
-- If you want nice results, use aliases instead:

--      type X = Y MyTuple | Z MyRecord
--      type alias MyTuple = (String, Int)
--      type alias MyRecord = {a: Bool, b: Float}


anonymous : TypeDef -> List Type
anonymous typeDef =
    anonymousHelp True typeDef.theType []


anonymousHelp : Bool -> Type -> List Type -> List Type
anonymousHelp topLevel a xs =
    case a of
        TypeArray b ->
            anonymousHelp False b xs

        TypeBool ->
            xs

        TypeDict ( b, c ) ->
            let
                recurseOn =
                    anonymousHelp False b << anonymousHelp False c
            in
            case topLevel of
                True ->
                    recurseOn xs

                False ->
                    recurseOn (a :: xs)

        TypeError b ->
            xs
            
        TypeExtendedRecord b -> --same as record
            let
                recurseOn =
                    foldr (<<) identity <| map (anonymousHelp False) <| map .theType b
            in
            case topLevel of
                True ->
                    recurseOn xs

                False ->
                    recurseOn (a :: xs)
            
        TypeExtensible _ -> --extensible records cannot be anonymous
            xs

        TypeFloat ->
            xs            
        
        TypeImported b ->
            xs

        TypeInt ->
            xs

        TypeList b ->
            anonymousHelp False b xs

        TypeMaybe b ->
            case topLevel of
                True ->
                    anonymousHelp False b xs

                False ->
                    anonymousHelp False b (TypeMaybe b :: xs)

        TypeProduct ( b, c ) ->
            case c of
                [] ->
                    xs

                _ ->
                    (foldr (<<) identity <| map (anonymousHelp False) c) xs

        TypeRecord b ->
            let
                recurseOn =
                    foldr (<<) identity <| map (anonymousHelp False) <| map .theType b
            in
            case topLevel of
                True ->
                    recurseOn xs

                False ->
                    recurseOn (a :: xs)

        TypeString ->
            xs

        TypeTuple bs ->
            let
                recurseOn =
                    foldr (<<) identity <| map (anonymousHelp False) bs
            in
            case topLevel of
                True ->
                    recurseOn xs

                False ->
                    recurseOn (a :: xs)

        TypeUnion b ->
            let
                typeList =
                    concat <| map (\( x, y ) -> y) b
            in
            (foldr (<<) identity <| map (anonymousHelp False) typeList) xs


grabAnonymousTypes : List TypeDef -> List Type
grabAnonymousTypes typeDefs =
    let
        notMaybe a =
            case a of
                TypeMaybe _ ->
                    False

                _ ->
                    True

        typeFilter =
            filter notMaybe << filter (not << coreType)
    in
    typeFilter <| unique <| concat <| map anonymous typeDefs


unique : List a -> List a
unique xs =
    uniqueHelp [] xs


uniqueHelp : List a -> List a -> List a
uniqueHelp checked remaining =
    case remaining of
        [] ->
            checked

        x :: xs ->
            case any (\a -> a == x) (checked ++ xs) of
                False ->
                    uniqueHelp (x :: checked) xs

                True ->
                    uniqueHelp checked xs
