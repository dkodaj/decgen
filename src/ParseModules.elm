module ParseModules exposing (Module, parseAll)

import Char
import Destructuring exposing (regex, removeNothings)
import List.Extra as ListXtra
import ParseImports exposing (Expose(..), ExposeList(..), exposedTypes, Import, typeImports)
import ParseType
import Set
import Types exposing (Type(..),TypeDef)

type alias Module =
    { name : String
    , exposedTypes: List Expose
    , typeImports : List Import
    , typeDefs : List TypeDef
    }

moduleRegex =
    "\\s*(?:port\\s+)?module\\s+([\\w|_]+)\\s+exposing\\s+\\((.+)\\)\\s*(?:\\w|$)"


parseModule : Bool -> String -> Maybe Module
parseModule encoding source =
    let
        submatches = 
            List.map .submatches <| regex moduleRegex source            
    in
    case submatches of
        (Just name :: Just exposeStr :: _) :: _ ->
            Just
                { name = name
                , exposedTypes = ParseImports.exposedTypes exposeStr
                , typeImports = ParseImports.typeImports source
                , typeDefs = ParseType.extractBasic encoding source
                }
        _ ->
            Nothing


parseAll : Bool -> List String -> List TypeDef
parseAll encoding sources =
    let        
        unique =
            ListXtra.uniqueBy .name
    in
    case sources of
        x :: ys ->
            case parseModule encoding x of
                Just baseModule ->
                    let
                        baseDefs =
                            baseModule.typeDefs
                        
                        rest =
                            removeNothings (List.map (parseModule encoding) ys)
                    
                        puller a =
                            pullImported a baseModule rest []
                    in
                    unique <| baseDefs ++ ( List.concat (List.map puller baseDefs) )
                    
                Nothing ->
                    []
            
        [] ->
            []
    

pullImported : TypeDef -> Module -> List Module -> List TypeDef -> List TypeDef
pullImported typeDef homeModule modulePool pulled =
    pullImportedHelp typeDef.theType homeModule modulePool pulled


pullImportedHelp : Type -> Module -> List Module -> List TypeDef -> List TypeDef
pullImportedHelp thisType homeModule modulePool pulled =
    let
        recurseOn a =
            pullImported a homeModule modulePool pulled
        
        recurseOnHelp a =
            pullImportedHelp a homeModule modulePool pulled
    in
    case thisType of
        TypeImported name ->
            case define name homeModule modulePool of
                Just (newType, newHome) ->
                    pullImportedHelp 
                        newType 
                        newHome 
                        modulePool 
                        ( { name = name, theType = newType } :: pulled )
                    
                Nothing ->
                    pulled
                    
        TypeArray a ->
            (recurseOnHelp a) ++ pulled
            
        TypeBool ->
            pulled
            
        TypeDict ( a, b ) ->
            pulled ++ recurseOnHelp a ++ recurseOnHelp b
            
        TypeError _ ->
            pulled

        TypeExtendedRecord list ->
            pulled ++ List.concat (List.map recurseOn list)
            
        TypeExtensible list ->
            pulled ++ List.concat (List.map recurseOn list)
            
        TypeFloat ->
            pulled
            
        TypeInt ->
            pulled
            
        TypeList a ->
            pulled ++ recurseOnHelp a
            
        TypeMaybe a ->
            pulled ++ recurseOnHelp a
            
        TypeProduct ( a, list ) ->
            pulled ++ List.concat (List.map recurseOnHelp list)
            
        TypeRecord list ->
            pulled ++ List.concat (List.map recurseOn list)
            
        TypeString ->
            pulled
            
        TypeTuple list ->
            pulled ++ List.concat (List.map recurseOnHelp list)
            
        TypeUnion list ->
            let
                mapper (a, xs) =
                    List.map recurseOnHelp xs
            in
            pulled ++ (List.concat << List.concat) (List.map mapper list)
            
---== Helpers

define : String -> Module -> List Module -> Maybe (Type, Module)
define name homeModule modulePool =
    case List.reverse (String.split "." name) of
        x :: y :: zs ->
            let
                qualifier =
                    String.join "." <| List.reverse <| y::zs
                    
                imps =
                    homeModule.typeImports
                    
                mapper =
                    lookForType x qualifier modulePool
            in
            case removeNothings (List.map mapper imps) of
                result :: _ ->
                    Just result
                    
                [] ->
                    Nothing
                    
            
        x :: [] ->
            case List.filter (exposes name) homeModule.typeImports of
                mod :: _ ->
                    fetchType name mod.fullName modulePool
                    
                [] ->
                    Nothing
            
        [] ->
            Nothing

exposes : String -> Import -> Bool
exposes typeName imp =
    let
        mapper listItem =
            case listItem of
                Operator _ ->
                    False
                
                Simple name ->
                    name == typeName
                    
                Complex constructor _ ->
                    constructor == typeName
        
    in  
    case imp.exposes of
        Unqualified ->
            False
            
        Qualified list -> 
            List.foldl (||) False (List.map mapper list)


fetchType : String -> String -> List Module -> Maybe (Type, Module)
fetchType typeName moduleName modulePool =
    let
        targetDef a =
            a.name == typeName
            
        targetModule a =
            a.name == moduleName
    in
    case List.filter targetModule modulePool of
        mod :: _ ->
            case List.filter targetDef mod.typeDefs of
                x :: _ ->
                    Just (x.theType, mod)
                
                [] ->
                    Nothing
                    
        [] ->
            Nothing

     
lookForType : String -> String -> List Module -> Import -> Maybe (Type, Module)
lookForType typeName qualifier modulePool imp =
    let
        isThisTheModule =
            case imp.shortName of
                Just shorty ->
                    shorty == qualifier
                
                Nothing ->
                    imp.fullName == qualifier            
    in
    case isThisTheModule of
        True ->
            case imp.exposes of
                Qualified _ ->
                    case exposes typeName imp of
                        True ->
                            fetchType typeName imp.fullName modulePool
                            
                        False ->
                            Nothing        
                    
                Unqualified ->
                    fetchType typeName imp.fullName modulePool
            
        False ->
            Nothing
