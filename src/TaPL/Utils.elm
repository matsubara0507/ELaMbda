module TaPL.Utils exposing (dropIfEndsWith, dropIfStartsWith)


dropIfStartsWith : String -> String -> String
dropIfStartsWith word s =
    if String.startsWith word s then
        String.dropLeft (String.length word) s

    else
        s


dropIfEndsWith : String -> String -> String
dropIfEndsWith word s =
    if String.endsWith word s then
        String.dropRight (String.length word) s

    else
        s
