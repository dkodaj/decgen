module DecGen.Destructuring exposing (..)

import List exposing (concat, map, map2)
import Regex exposing (HowMany(..), regex, replace)
import String exposing (dropLeft, dropRight, indices, join, left, length, repeat, right, split, toUpper, trim, words)

--== Destructuring records, tuples, union types ==--

-- derecord "{x: Int, y: String}" == [("x","Int"), ("y","String")]
-- detuple "(String, Int)" == Just ("String", "Int")
-- deunion "A String Int | B Int" == [("A", ["String", "Int"]), ("B", ["Int"])]

derecord: String -> List (String, String)
derecord txt =
    case inCurly txt of
        Nothing->
            []
        Just x->
            derecordHelp (indices "," x) x

derecordHelp: List Int -> String -> List (String, String)
derecordHelp idxs txt =
    case idxs of
        []->
            case toField txt of
                Just (a,b)->
                    [(a,b)]
                Nothing->
                    []
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    let
                        remains = dropLeft (x+1) txt
                        newIdxs = indices "," remains
                    in
                        case toField (left x txt) of
                            Just (a,b)->
                                (a,b) :: derecordHelp newIdxs remains
                            Nothing->
                                derecordHelp newIdxs remains
                False->
                    derecordHelp xs txt

toField: String -> Maybe (String, String)
toField def =
    case split ":" def of
        x::xs->
            Just (trim x, dropWord x def)
        []->
            Nothing

detuple: String -> List String
detuple txt =
    case inBrackets txt of
        Nothing->
            []
        Just x->
            detupleHelp (indices "," x) x []

detupleHelp: List Int -> String -> List String -> List String
detupleHelp idxs txt elements =
    case idxs of
        []->
            case elements of
                []->
                    []
                x::xs->
                    elements ++ [txt]
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    let
                        newTxt = dropLeft (x+1) txt
                    in
                        detupleHelp (indices "," newTxt) newTxt (elements ++ [left x txt]) 
                False->
                    detupleHelp xs txt elements

deunion: String -> List (String, List String)
deunion txt =
    clean <| map deunionHelp <| split "|" <| singleSpace txt

deunionHelp: String -> Maybe (String, List String)
deunionHelp txt =
    case words (trim txt) of
        x::ys->
            Just (x, components <| dropWord x (trim txt))
        []->
            Nothing


--== Parse/Write helpers ==--

bracket txt =
    "(" ++ txt ++ ")"


bracketCommas: List String -> List String
bracketCommas xs = 
--bracketCommas ("a","b") == "[ a, b"
    let
        glue a b = a ++ " " ++ b
        separators = "[" :: List.repeat (List.length xs - 1) ","
    in
        map2 glue separators xs

bracketIfSpaced txt =
    case indices " " txt of
        []->
            txt
        x::xs->
            bracket txt

capitalize txt =
    (toUpper <| left 1 txt) ++ (dropLeft 1 txt)

civilize txt =
    let
        to_ = replace All (regex "[(){}:,]") (\_ -> "_")
        deleteSpace = replace All (regex " ") (\_ -> "")
    in
        to_ <| deleteSpace txt

clean: List (Maybe a) -> List a
clean xs =
  --clean [Just a, Nothing, Just b] == [a, b]
    let
        dropJust a =
            case a of
                Nothing->
                    []
                Just b->
                    [b]
    in
        concat <| map dropJust xs

components: String -> List String 
components txt = --helper for Destructuring.deunion
    componentsHelp (indices " " txt) 0 txt

componentsHelp idxs start txt =
    case idxs of
        []->
            [dropLeft start txt]
        x::xs->
            case outsideBrackets x txt && outsideCurly x txt of
                True->
                    let
                        component = dropLeft start <| left x txt
                        newStart = x + 1
                    in
                        component :: componentsHelp xs newStart txt
                False->
                    componentsHelp xs start txt

debracket txt =
    let
        trimmed = trim txt
    in
        if left 1 trimmed == "(" && right 1 trimmed == ")"
            then
                trim <| dropLeft 1 <| dropRight 1 trimmed
            else
                trimmed

decomment txt = 
    let
        singleComment = regex "--.*"
        multiComment = regex "\\{\\-(.|\\n)*?\\-\\}"      
    in
        remove singleComment <| remove multiComment txt

dropWord word txt =
    trim <| dropLeft (length word + 1) txt

inCurly txt = inThese "{" "}" txt

inBrackets txt = inThese "(" ")" txt

inThese a b txt =
    let
        trimmed = trim txt
    in
        if left 1 trimmed == a && right 1 trimmed == b
            then Just (dropLeft 1 <| dropRight 1 trimmed)
            else Nothing

occurs a txt =
    List.length <| indices a txt

outside: String -> String-> Int -> String -> Bool
outside a b idx txt = 
    let
        chunk = left idx txt
    in
        occurs a chunk == occurs b chunk

outsideBrackets txt = outside "(" ")" txt

outsideCurly txt = outside "{" "}" txt

quote txt =
    "\""++txt++"\""

remove a b = replace All a (\_ -> "") b

removeColons txt = remove (regex "\\.") txt

removeStringLiterals txt = remove (regex "\".*?\"") <| remove (regex "\"\"\"(?:\\n|.)+?\"\"\"") txt 

singleLine txt =
    singleSpace <| replace All (regex "[\\r\\n]") (\a->" ") txt

singleSpace txt = 
    replace All (regex "[ ]+") (\_ -> " ") txt

tab n txt =
    (repeat (3*n) " ") ++ txt

tabLines n txt =
    join "\n" <| map (tab n) <| split "\n" txt