module TypeExtract exposing
    ( extractAll
    , extractAllWithDefs
    , grabRawTypes
    , grabTypeDefs
    , typeNick
    )

import AnonymousTypes exposing (grabAnonymousTypes)
import Destructuring exposing (bracketIfSpaced, civilize, clean, debracket, decomment, derecord, detuple, deunion, dropWord, removeColons, removeStringLiterals, singleLine)
import List exposing (filter, map)
import Regex exposing (Match)
import String exposing (dropRight, join, trim, words)
import Types exposing (RawType, Type(..), TypeDef)


aliasDefs : List TypeDef -> List (List String)
aliasDefs types =
    let
        name a = 
            case a.theType of
                TypeExtendedRecord _ ->
                    a.name ++ "Extended"
                    
                _ ->
                    a.name
        
        def a =
            [ "type alias " ++ (name a) ++ " = " ++ typeDescr True a.theType ]
    in
    map def types


anonymousType : Type -> TypeDef
anonymousType a =
    { name = typeNick a, theType = a }


anonymousTypes : Bool -> List TypeDef -> List TypeDef
anonymousTypes encoding typeList =
    map anonymousType <| grabAnonymousTypes encoding typeList


extractAll : Bool -> String -> List TypeDef
extractAll encoding txt =
    let            
        (declared, anonymous) =
            extractHelp encoding txt
    in
    filter (not << Types.isExtensible) (declared ++ anonymous)


extractAllWithDefs : Bool -> String -> ( List TypeDef, List (List String) )
extractAllWithDefs encoding txt =
    let        
        (declared, anonymous) =
            extractHelp encoding txt
            
        nonEmptyRecord a =
            Types.isRecord a && not (Types.isEmptyRecord a)                               
            
        needToDefine =
            filter nonEmptyRecord anonymous
            ++ filter Types.isNonemptyExtended (declared ++ anonymous)
            
        filtered =
            filter (not << Types.isExtensible) (declared ++ anonymous)
    in
    ( filtered, aliasDefs needToDefine )


extractHelp : Bool -> String -> (List TypeDef, List TypeDef)
extractHelp encoding txt =
    let
        declared =
            grabTypeDefs txt
            
        scannedDeclared =
            map (detectExtendedRecord declared) declared
            
        anonymous =
            anonymousTypes encoding scannedDeclared
    in                    
      ( scannedDeclared, anonymous )


grabTypeDefs : String -> List TypeDef
grabTypeDefs txt =
    let
        toTypeDef a =
            { name = a.name, theType = typeOf a.extensible a.def }
    in
    map toTypeDef <| grabRawTypes txt


grabRawType : List (Maybe String) -> Maybe RawType
grabRawType submatches =
    case submatches of          
        Just a :: Just b :: _ ->
            case String.words (trim a) of
                x :: y :: _ -> -- means that the name is something like "LineSegment a", i.e. an extensible record
                    Just { name = x, def = trim <| singleLine b, extensible = True }
                
                x :: _ ->
                    Just { name = x, def = trim <| singleLine b, extensible = False }
                
                [] ->
                    Nothing                            

        _ ->
            Nothing


grabRawTypes : String -> List RawType
grabRawTypes txt =
    clean <| map grabRawType <| map .submatches <| regexIt <| decomment <| removeStringLiterals txt


regexIt : String -> List Match
regexIt txt =
    case Regex.fromString typeRegex of
        Nothing ->
            []

        Just regex ->
            Regex.find regex txt


typeRegex =
    "type\\s+(?:alias\\s+)([\\w_]+[\\w_\\s]*)=([\\w(){},|.:_ \\r\\n]+)(?=(?:\\r\\w|\\n\\w)|$)"



--== Recognize types ==--


typeOf : Bool -> String -> Type
typeOf extensible def =
    --  typeOf "List String" == TypeList TypeString
    --  typeOf "MyType | String" == TypeUnion [TypeOpaque "MyType", TypeString]
    --  typeOf "MyType" == TypeOpaque "MyType"
    let
        subType x =
            typeOf False x
    in
    case detuple def of
        a :: bs ->
            TypeTuple <| map subType (a :: bs)

        [] ->
            case derecord def of
                (a1,a2) :: bs ->
                    let
                        makeField ( x, y ) =
                            TypeDef x (subType y)
                            
                        fields =
                            case a1 == "" of
                                True ->
                                    []
                                
                                False ->
                                    map makeField ((a1,a2) :: bs)                                
                    in
                    case extensible of
                        True ->
                            TypeExtensible fields
                        
                        False ->
                            TypeRecord fields
                            
                [] ->
                    case words (debracket def) of
                        [] ->
                            TypeError "Type conversion error: empty string"

                        a :: bs ->
                            case a of
                                "Array" ->
                                    TypeArray (subType <| dropWord a <| debracket def)

                                "Bool" ->
                                    TypeBool

                                "Dict" ->
                                    case deunion (debracket def) of
                                        ( _, c :: d :: es ) :: fs ->
                                            TypeDict ( subType c, subType d )

                                        _ ->
                                            TypeError "Error parsing def as a Dict"

                                "Dict.Dict" ->
                                    case deunion (debracket def) of
                                        ( _, c :: d :: es ) :: fs ->
                                            TypeDict ( subType c, subType d )

                                        _ ->
                                            TypeError "Error parsing def as a Dict"

                                "Float" ->
                                    TypeFloat

                                "Int" ->
                                    TypeInt

                                "List" ->
                                    TypeList (subType <| dropWord a <| debracket def)

                                "Maybe" ->
                                    TypeMaybe (subType <| dropWord a <| debracket def)

                                "String" ->
                                    TypeString

                                _ ->
                                    let
                                        constructor ( x, y ) =
                                            case y of
                                                [ "" ] ->
                                                    ( x, [] )

                                                _ ->
                                                    ( x, map subType y )
                                    in
                                    case deunion def of
                                        (x, y) :: [] ->
                                            case y of
                                                [ "" ] ->
                                                    TypeOpaque x
                                                    
                                                _ ->
                                                    TypeProduct ( x, map subType y )

                                        c :: ds ->
                                            TypeUnion <| map constructor (c :: ds)

                                        [] ->
                                            TypeError "Union type conversion error: empty"


typeDescr : Bool -> Type -> String
typeDescr bracketIt a =
    --    typeDescr False (TypeList TypeInt) == "List Int"
    --    typeDescr True (TypeList TypeInt) == "(List Int)"
    let
        wrap x =
            if bracketIt then
                "(" ++ x ++ ")"

            else
                x
    in
    case a of
        TypeArray b ->
            wrap <| "Array " ++ typeDescr True b

        TypeBool ->
            "Bool"

        TypeDict ( b, c ) ->
            "Dict " ++ (bracketIfSpaced <| typeDescr False b) ++ " " ++ (bracketIfSpaced <| typeDescr False c)

        TypeError b ->
            b
        
        TypeExtendedRecord b -> --same as TypeRecord
            let
                fieldString x =
                    x.name ++ ": " ++ typeDescr False x.theType ++ ", "

                fields =
                    dropRight 2 <| String.concat <| map fieldString b
            in
            "{" ++ fields ++ "}"
        
        TypeExtensible b ->
            let
                fieldString x =
                    x.name ++ ": " ++ typeDescr False x.theType ++ ", "

                fields =
                    dropRight 2 <| String.concat <| map fieldString b
            in
            "{ a | " ++ fields ++ "}"

        TypeFloat ->
            "Float"

        TypeInt ->
            "Int"

        TypeList b ->
            wrap <| "List " ++ typeDescr True b

        TypeMaybe b ->
            wrap <| "Maybe " ++ typeDescr True b

        TypeOpaque b ->
            b

        TypeProduct ( b, c ) ->
            case c of
                [] ->
                    b

                _ ->
                    b ++ " " ++ (String.concat <| map (typeDescr True) c)

        TypeRecord b ->
            let
                fieldString x =
                    x.name ++ ": " ++ typeDescr False x.theType ++ ", "

                fields =
                    dropRight 2 <| String.concat <| map fieldString b
            in
            "{" ++ fields ++ "}"

        TypeString ->
            "String"

        TypeTuple bs ->
            "(" ++ (join ", " <| map (typeDescr False) bs) ++ ")"

        TypeUnion b ->
            let
                constructorString ( x, y ) =
                    case y of
                        [] ->
                            x ++ " | "

                        _ ->
                            x ++ " " ++ (String.concat <| map (typeDescr True) y) ++ " | "

                constructors =
                    dropRight 2 <| String.concat <| map constructorString b
            in
            constructors


typeNick : Type -> String
typeNick a =
    let
        tag prefix =
            prefix ++ (civilize <| typeDescr False a)
    in
    case a of
        TypeExtendedRecord _ ->
            tag "Record"
        
        TypeRecord _ ->
            tag "Record"

        TypeTuple _ ->
            tag "Tuple"

        _ ->
            tag ""

--== Extensible records ==--    

detectExtendedRecord : List TypeDef -> TypeDef -> TypeDef
detectExtendedRecord declaredTypes input =  
    let
        newType = detectExtendedRecordHelp declaredTypes [] input.theType
    in
    { input | theType = newType }


detectExtendedRecordHelp : List TypeDef -> List TypeDef -> Type -> Type
detectExtendedRecordHelp declaredTypes fieldsSoFar input =
    let
        extensiblesFor a =
            extensibleFields declaredTypes a
            
        recursion a b =
            detectExtendedRecordHelp declaredTypes a b
            
        lookAt a =
            detectExtendedRecordHelp declaredTypes [] a            
        
        lookInto a =
            detectExtendedRecord declaredTypes a        
    in
    case input of
        TypeArray ofType ->
            TypeArray (lookAt ofType)
            
        TypeDict ( key, val ) ->
            TypeDict ( key, lookAt val )
            
        TypeExtendedRecord fields ->
            TypeExtendedRecord (map lookInto fields)
            
        TypeExtensible fields ->
            TypeExtensible (map lookInto fields)
            
        TypeList ofType ->
            TypeList (lookAt ofType)
            
        TypeMaybe ofType ->
            TypeMaybe (lookAt ofType)
        
        TypeProduct (constructor, [subType]) ->
            case extensiblesFor constructor of
                Just extensibles ->
                    case subType of
                        TypeRecord newFields ->
                            TypeExtendedRecord (fieldsSoFar ++ map lookInto (extensibles ++ newFields))
                            
                        TypeProduct _ ->
                            recursion (fieldsSoFar ++ extensibles) subType
                            
                        _->
                            input
                
                Nothing->
                    input
        
        TypeRecord newFields ->
            case fieldsSoFar of
                [] ->
                    TypeRecord (map lookInto newFields)
                    
                _ ->
                    TypeExtendedRecord (fieldsSoFar ++ map lookInto newFields)
                    
        TypeTuple typeList ->
            TypeTuple (map lookAt typeList)
            
        TypeUnion list ->
            let
                mapper (constructor, subTypes) =
                    (constructor, map lookAt subTypes)
            in
            TypeUnion (map mapper list)
        
        _ ->
            input
            
                    

extensibleFields : List TypeDef -> String -> Maybe (List TypeDef)
extensibleFields allTypDefs name =
    let
        candidate x =
            x.name == name
    in
    case List.filter candidate allTypDefs of
        x :: _ ->
            case x.theType of
                TypeExtensible fields ->
                    Just fields
                
                _ ->
                    Nothing
            
        []->
            Nothing
