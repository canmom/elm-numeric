module Matrix
    exposing
        ( fromList
        , from2DList
        , fromString
        , mat
        , mats
        , cvecFromList
        , rvecFromList
        , cvec
        , rvec
        , zeroes
        , ones
        , eye
        , upper
        , lower
        , strictUpper
        , strictLower
        , vec
        , dot
        , cross
        , sMul
        , sDiv
        , equivalent
        , add
        , eMul
        , get
        , map
        , map2
        , mul
        , vcat
        , transpose
        , determinant
        , prettyPrint
        , debugPrint
        , to2DList
        , toFlatList
        , getScaling
        , luDecomp
        , genIndices
        )

import Array


{-| A n x n matrix library.
This library aims to be a reasonably complete suite of linear algebra tools.

Some highlights are that this library has generic sized matrices,
transposes, multiplication, and inversion.

    import Matrix as Mt --and program away!


# The matrix type

@docs Matrix


# Creating matrices

@docs fromList, from2DList, fromString, mat, mats, zeroes, ones, eye, upper, lower
@docs strictLower, strictUpper


# Creating vectors

@docs cvec, rvec, vec


# Vector operations

@docs cross, dot


# Matrix element-wise operations

@docs add, equivalent, sMul, sDiv, map, map2, eMul


# Matrix operations

@docs mul, vcat, get, transpose, determinant


# Matrix display

@docs prettyPrint, debugPrint


# Interop

@docs to2DList, toFlatList

-}
type alias Matnxn =
    { dimensions : ( Int, Int )
    , elements : Array.Array Float
    }


type Matrix
    = Mat Matnxn
    | Err String



-- Matrix Creation


{-| Create a (n rows x m columns) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.

    matrix =
        Matrix.fromList ( 2, 3 )
            [ [ 2, 2, 2 ]
            , [ 3, 3, 3 ]
            ]

-}
fromList : ( Int, Int ) -> List Float -> Matrix
fromList dimensions elements =
    fromArray dimensions <| Array.fromList elements


{-| Create a (n rows x m columns) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.
-}
fromArray : ( Int, Int ) -> Array.Array Float -> Matrix
fromArray ( rows, columns ) elements =
    if rows * columns == Array.length elements then
        Mat { dimensions = ( rows, columns ), elements = elements }
    else
        let
            dimensions =
                toString (rows * columns)

            numelements =
                toString <| Array.length elements
        in
            Err <| "The dimensions, row * columns: " ++ dimensions ++ ", do not match the number of elements: " ++ numelements


{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:

    matrix =
        Matrix.from2DList
            [ [ 2, 2, 2 ]
            , [ 3, 3, 3 ]
            ]

-}
from2DList : List (List Float) -> Matrix
from2DList a =
    case List.head a of
        Just data ->
            let
                columns =
                    List.length <| data

                rows =
                    List.length a

                -- check if all sublists are same length
                is_correct =
                    List.all ((==) columns) <| List.map List.length a
            in
                if is_correct then
                    fromList ( rows, columns ) <| List.concat a
                else
                    Err <| "One or more of the sublist rows are malformed"

        Nothing ->
            fromList ( 0, 0 ) []


{-| Create a (n x m) matrix with inner lists being rows.
The following is a 2 x 3 matrix:

    matrix =
        Matrix.from2DList
            [ [ 2, 2, 2 ]
            , [ 3, 3, 3 ]
            ]

-}
mat : List (List Float) -> Matrix
mat =
    from2DList


{-| Create a (n x m) matrix with inner lists being rows.
In string format you can use Matlab/Julia-esque syntax.
Spaces denote elements in a row.
Semicolons denote elements in a column.
The string must begin with [ and end with ].

The following is a 2 x 3 matrix:

    matrix =
        Matrix.fromString "[ 2 2 2; 3 3 3]"

Any alpha/garbage characters will be set to zero.

-}
fromString : String -> Matrix
fromString string =
    from2DList <| matParser string


mats : String -> Matrix
mats =
    fromString


{-| Create a column vector from a list
-}
cvecFromList : List Float -> Matrix
cvecFromList a =
    fromList ( List.length a, 1 ) a


{-| Create a column vector from a list
-}
cvec : List Float -> Matrix
cvec =
    cvecFromList


{-| Create a column vector from a list
-}
vec : List Float -> Matrix
vec =
    cvec


{-| Create a row vector from a list
-}
rvecFromList : List Float -> Matrix
rvecFromList a =
    fromList ( 1, List.length a ) a


{-| Create a row vector from a list
-}
rvec : List Float -> Matrix
rvec =
    rvecFromList


{-| Generate a random matrix
-}
rand : ( Int, Int ) -> Matrix
rand a =
    Err "Not Implemented"


{-| Generate a matrix of ones
-}
ones : ( Int, Int ) -> Matrix
ones ( rows, columns ) =
    fromArray ( rows, columns ) <| Array.repeat (rows * columns) 1.0


{-| Generate a matrix of zeroes
-}
zeroes : ( Int, Int ) -> Matrix
zeroes ( rows, columns ) =
    fromArray ( rows, columns ) <| Array.repeat (rows * columns) 0.0


{-| Create an nxn identity matrix
-}
eye : Int -> Matrix
eye diagonal =
    let
        gen x =
            if x % (diagonal + 1) == 0 then
                1
            else
                0
    in
        Array.initialize (diagonal * diagonal) gen
            |> fromArray ( diagonal, diagonal )


{-| Create an nxn upper triangular matrix
-}
upper : Int -> Matrix
upper diagonal =
    let
        range =
            List.range 0 (diagonal - 1)

        f i =
            List.append (List.repeat i 0.0) (List.repeat (diagonal - i) 1.0)

        list =
            List.map f range
    in
        from2DList list


{-| Create an nxn strict upper triangular matrix
This means that elements along the diagonal are zero
-}
strictUpper : Int -> Matrix
strictUpper diagonal =
    map2 (-) (upper diagonal) (eye diagonal)


{-| Create an nxn lower triangular matrix
-}
lower : Int -> Matrix
lower diagonal =
    transpose <| upper diagonal


{-| Create an nxn strict lower triangular matrix
This means that elements along the diagonal are zero
-}
strictLower : Int -> Matrix
strictLower diagonal =
    map2 (-) (transpose <| upper diagonal) (eye diagonal)



-- Operations


{-| Perform matrix multiplication

    A * B

-}
mul : Matrix -> Matrix -> Matrix
mul a b =
    forwardError "[in mul]" mulList a b


mulArray : Matnxn -> Matnxn -> Matrix
mulArray a_ b_ =
    Err "Not Implemented"



{--
    if numColumns a_ /= numRows b_ then
        let
            acolumns =
                toString <| numColumns a_

            brows =
                toString <| numRows b_
        in
            Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ " b.rows = " ++ brows ++ "."
    else
    let
        a_arr = a_.elements
            let
                array =
                    case transposeBase b_ of
                        Mat t_ ->
                            t_.elements
                        _ ->
                            Array.fromList []
            in
        b_arr = array
        collapse x y =
            Array.foldr (+) 0  <| arraymap2 (*) x y --element level

        constructList x y =
            case x of
                [] ->
                    []
                m :: ms ->
                    List.map (collapse m) y :: constructList (List.drop 1 x) y

    in
       from2DList <| constructList a_list b_list
       --}


{-| Implementation of multiplication using lists.
Probably slow.
-}
mulList : Matnxn -> Matnxn -> Matrix
mulList a_ b_ =
    if numColumns a_ /= numRows b_ then
        let
            acolumns =
                toString <| numColumns a_

            brows =
                toString <| numRows b_
        in
            Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ " b.rows = " ++ brows ++ "."
    else
        let
            a_list =
                to2DListBase a_

            b_list =
                to2DList <| transposeBase b_

            collapse x y =
                List.sum <| List.map2 (*) x y

            --element level
            constructList x y =
                case x of
                    [] ->
                        []

                    m :: ms ->
                        List.map (collapse m) y :: constructList (List.drop 1 x) y
        in
            from2DList <| constructList a_list b_list


{-| Get the lu decomposition of a matrix
Since pivoting isn't implemented, watch out for numerical instability
-}
luDecomp : Matrix -> ( Matrix, Matrix )
luDecomp a =
    case a of
        Mat a_ ->
            if numRows a_ == numColumns a_ then
                luSplit a_
            else
                ( Err "Must be a square matrix", Err "" )

        Err string ->
            ( Err string, Err "" )


{-| Splits the lu factorized single matrix into two
-}
luSplit : Matnxn -> ( Matrix, Matrix )
luSplit a =
    let
        single =
            luNoPivotSingle a

        dim =
            numColumns a

        l =
            eMul single (strictLower dim)
                |> add (eye dim)

        u =
            eMul single (upper dim)
    in
        ( l, u )


{-| Performs lu factorization
-}
luNoPivotSingle : Matnxn -> Matrix
luNoPivotSingle a =
    let
        lu =
            zeroes a.dimensions

        --i is inner loop j outer loop
        -- bind computelem to a
        luCompute index dest =
            luComputeElem index a dest

        indices =
            genIndices a.dimensions
    in
        case lu of
            Mat lu_ ->
                Mat <| List.foldl luCompute lu_ indices

            _ ->
                lu


genIndices : ( Int, Int ) -> List ( Int, Int )
genIndices ( i_range, j_range ) =
    let
        listj =
            List.concat <| List.map (List.repeat i_range) <| List.range 1 j_range

        listi =
            List.concat <| List.repeat j_range (List.range 1 i_range)
    in
        List.map2 (,) listi listj


luComputeElem : ( Int, Int ) -> Matnxn -> Matnxn -> Matnxn
luComputeElem ( i, j ) original lu =
    let
        tiny =
            10 ^ (-40)

        getWithDefault index mat =
            Maybe.withDefault 0.0 (getBase index mat)

        aij =
            getWithDefault ( i, j ) original

        bjj =
            Maybe.withDefault tiny <| getBase ( j, j ) lu

        compute index =
            getWithDefault ( i, index ) lu * getWithDefault ( index, j ) lu
    in
        if i > j then
            let
                k =
                    List.range 1 (j - 1)
            in
                setBase ( i, j ) ((aij - (List.sum <| List.map compute k)) / bjj) lu
        else
            let
                k =
                    List.range 1 (i - 1)
            in
                setBase ( i, j ) (aij - (List.sum <| List.map compute k)) lu


getScaling : List (List Float) -> List Float
getScaling mat =
    List.map (\x -> List.map abs x) mat
        |> List.map (List.maximum)
        |> List.map (Maybe.withDefault 0)
        |> List.map (\x -> 1 / x)


{-| Get an item at index (row, column). Indices are 1-indexed.
-}
get : ( Int, Int ) -> Matrix -> Maybe Float
get indices a =
    case a of
        Mat a_ ->
            getBase indices a_

        Err _ ->
            Nothing


getBase : ( Int, Int ) -> Matnxn -> Maybe Float
getBase ( r_index, c_index ) a_ =
    let
        check_r_bounds =
            0 < r_index && r_index <= numRows a_

        check_c_bounds =
            0 < c_index && c_index <= numColumns a_
    in
        if check_r_bounds && check_c_bounds then
            Array.get ((r_index - 1) * (numColumns a_) + c_index - 1)
                a_.elements
        else
            Nothing


{-| Get an item at index (row, column). Indices are 1-indexed.
-}
set : ( Int, Int ) -> Float -> Matrix -> Matrix
set indices data a =
    case a of
        Mat a_ ->
            Mat <| setBase indices data a_

        Err string ->
            Err string


setBase : ( Int, Int ) -> Float -> Matnxn -> Matnxn
setBase ( r_index, c_index ) data a_ =
    let
        check_r_bounds =
            0 < r_index && r_index <= numRows a_

        check_c_bounds =
            0 < c_index && c_index <= numColumns a_
    in
        if check_r_bounds && check_c_bounds then
            { a_
                | elements =
                    Array.set ((r_index - 1) * (numColumns a_) + c_index - 1)
                        data
                        a_.elements
            }
        else
            a_


{-| Add two matrices of identical dimensions together
-}
add : Matrix -> Matrix -> Matrix
add a b =
    forwardError "[in add]" addBase a b


addBase : Matnxn -> Matnxn -> Matrix
addBase a_ b_ =
    if equalSize a_ b_ then
        fromArray ( numRows a_, numColumns a_ ) <| arraymap2 (+) a_.elements b_.elements
    else
        let
            adims =
                dimToString a_

            bdims =
                dimToString b_
        in
            Err <| "Matrices not equal size. a: " ++ adims ++ ", b: " ++ bdims


{-| Map a function over all elements individually
-}
map : (Float -> Float) -> Matrix -> Matrix
map f a =
    case a of
        Mat a_ ->
            fromArray ( numRows a_, numColumns a_ ) <| Array.map f a_.elements

        Err string ->
            Err string


{-| Map a function over elements of same index between matrices
-}
map2 : (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix
map2 f a b =
    forwardError "[in map2]" (map2Base f) a b


map2Base : (Float -> Float -> Float) -> Matnxn -> Matnxn -> Matrix
map2Base f a b =
    if equalSize a b then
        fromArray a.dimensions <| arraymap2 f a.elements b.elements
    else
        Err "Unequal Matrix sizes"


{-| Perform scalar multiplication on a matrix
-}
sMul : Float -> Matrix -> Matrix
sMul a b =
    map ((*) a) b


{-| Perform element by element multiplication on a matrix
-}
eMul : Matrix -> Matrix -> Matrix
eMul a b =
    map2 (*) a b


{-| Perform scalar division on a matrix
-}
sDiv : Float -> Matrix -> Matrix
sDiv a b =
    sMul (1 / a) b


{-| Invert a square matrix
-}
invert : Matrix -> Matrix
invert a =
    case a of
        Mat a_ ->
            if numRows a_ == numColumns a_ then
                Err "Not Implemented"
            else
                Err "Matrix is not square"

        Err string ->
            Err string


{-| Transpose a matrix
-}
transpose : Matrix -> Matrix
transpose a =
    case a of
        Mat a_ ->
            transposeBase a_

        Err string ->
            Err string


transposeBase : Matnxn -> Matrix
transposeBase a_ =
    let
        f index =
            let
                mappedindex =
                    (index % numRows a_)
                        * (numColumns a_)
                        + (index // numRows a_)
            in
                Maybe.withDefault 0.0 (Array.get mappedindex a_.elements)

    in
        Array.initialize (numColumns a_ * numRows a_) f
            |> fromArray ( numColumns a_, numRows a_ )


{-| Get the determinant of a square matrix
-}
determinant : Matrix -> Maybe Float
determinant a =
    case a of
        Mat a_ ->
            if numRows a_ == numColumns a_ then
                let
                    single =
                        luNoPivotSingle a_
                in
                    List.range 1 (numRows a_)
                        |> List.map (\x -> ( x, x ))
                        |> List.map (\x -> Maybe.withDefault 0.0 (get x single))
                        |> List.product
                        |> Just
            else
                Nothing

        Err _ ->
            Nothing


{-| Performs the dot product of two nxn vectors
-}
dot : Matrix -> Matrix -> Maybe Float
dot a b =
    case ( a, b ) of
        ( Mat a_, Mat b_ ) ->
            let
                a_is_vector =
                    numColumns a_ == 1

                b_is_vector =
                    numColumns b_ == 1

                same_length =
                    (numRows a_) == (numRows b_)
            in
                if a_is_vector && b_is_vector && same_length then
                    arraymap2 (*) a_.elements b_.elements
                        |> Array.foldr (+) 0
                        |> Just
                else
                    Nothing

        ( _, _ ) ->
            Nothing



-- is there a way to do good scalar error handling?


{-| Get the cross product of two 3d vectors
a cross b
-}
cross : Matrix -> Matrix -> Matrix
cross a b =
    forwardError "[in cross]" crossBase a b


crossBase : Matnxn -> Matnxn -> Matrix
crossBase a_ b_ =
    if check3dVec a_ && check3dVec b_ then
        let
            ax =
                getBase ( 1, 1 ) a_

            ay =
                getBase ( 2, 1 ) a_

            az =
                getBase ( 3, 1 ) a_

            bx =
                getBase ( 1, 1 ) b_

            by =
                getBase ( 2, 1 ) b_

            bz =
                getBase ( 3, 1 ) b_
        in
            case ( ax, ay, az, bx, by, bz ) of
                ( Just ax_, Just ay_, Just az_, Just bx_, Just by_, Just bz_ ) ->
                    let
                        i =
                            (ay_ * bz_) - (by_ * az_)

                        j =
                            -1 * ((ax_ * bz_) - (bx_ * az_))

                        k =
                            (ax_ * by_) - (bx_ * ay_)
                    in
                        vec <| [ i, j, k ]

                ( _, _, _, _, _, _ ) ->
                    Err "One or more elements are missing, possible malformed vector"
    else
        Err "One or both vectors are malformed."


{-| Checks if two matrices are of equal size
-}
equalSize : Matnxn -> Matnxn -> Bool
equalSize a b =
    (numRows a == numRows b) && (numColumns a == numColumns b)


{-| Checks if two matrices are equivalent within some epsilon
-}
equivalent : Matrix -> Matrix -> Float -> Bool
equivalent a b epsilon =
    case ( a, b ) of
        ( Mat a_, Mat b_ ) ->
            let
                equal_size =
                    equalSize a_ b_

                equal_members =
                    arraymap2 (-) a_.elements b_.elements
                        |> Array.map (\x -> abs x < epsilon)
                        |> Array.toList
                        |> List.all ((==) True)
            in
                if equal_size && equal_members then
                    True
                else
                    False

        _ ->
            False


{-| Concatenate two matrices vertically.
-}
vcat : Matrix -> Matrix -> Matrix
vcat a b =
    forwardError "[in vcat]" vcatBase a b


vcatBase : Matnxn -> Matnxn -> Matrix
vcatBase a_ b_ =
    let
        acols =
            numColumns a_

        bcols =
            numColumns b_

        arows =
            numRows a_

        brows =
            numRows b_
    in
        if acols == bcols then
            fromArray ( arows + brows, acols ) (Array.append a_.elements b_.elements)
        else
            Err <|
                "Number of columns are not equal: a: "
                    ++ toString acols
                    ++ " b: "
                    ++ toString bcols


{-| Returns matrix as flat list
-}
toFlatList : Matrix -> List Float
toFlatList n =
    case n of
        Mat n_ ->
            Array.toList n_.elements

        _ ->
            []


{-| Returns matrix as 2d list.
Returns empty list if Matrix is in error
-}
to2DList : Matrix -> List (List Float)
to2DList n =
    case n of
        Mat n_ ->
            to2DListBase n_

        _ ->
            [ [] ]


to2DListBase : Matnxn -> List (List Float)
to2DListBase z =
    make2D (numColumns z) (Array.toList z.elements)


{-| Returns size of matrix
-}
size : Matrix -> ( Int, Int )
size n =
    case n of
        Mat n_ ->
            n_.dimensions

        _ ->
            ( 0, 0 )



-- Auxiliary Functions


{-| Helper to catch errors in functions of two variables (Matrix -> Matrix) ->
Matrix
-}
forwardError : String -> (Matnxn -> Matnxn -> Matrix) -> Matrix -> Matrix -> Matrix
forwardError error f a b =
    case ( a, b ) of
        ( Err string, Mat _ ) ->
            Err <| "\n" ++ error ++ " Matrix a: " ++ string

        ( Mat _, Err string ) ->
            Err <| "\n" ++ error ++ " Matrix b: " ++ string

        ( Err string, Err string2 ) ->
            Err <| "\n" ++ error ++ " Matrix a: " ++ string ++ "\n Matrix b: " ++ string2

        ( Mat a_, Mat b_ ) ->
            f a_ b_


{-| Helper to take a Matrix and stringify its dimensions
-}
dimToString : Matnxn -> String
dimToString a =
    let
        arows =
            toString <| numRows a

        acols =
            toString <| numColumns a
    in
        "(" ++ arows ++ "," ++ acols ++ ")"


{-| Change matrix into string form
-}
prettyPrint : Matrix -> String
prettyPrint a =
    case a of
        Mat mat ->
            prettyPrintBasic mat

        Err string ->
            string


{-| Change correctly formed matrix into string form
-}
prettyPrintBasic : Matnxn -> String
prettyPrintBasic a =
    let
        strings =
            a.elements
                |> Array.toList
                |> List.map toString
                |> List.map ((++) " ")

        structured_strings =
            make2D (numColumns a) strings
    in
        List.intersperse [ "\n" ] structured_strings
            |> List.concat
            |> List.foldr (++) ""


{-| Helper to re-2dify a flat matrix
-}
make2D : Int -> List a -> List (List a)
make2D num_row_elem list =
    case list of
        [] ->
            []

        items ->
            if List.length list < num_row_elem then
                [ list ]
            else
                List.take num_row_elem list :: make2D num_row_elem (List.drop num_row_elem list)


{-| Helper to debug print
-}
debugPrint : Matrix -> String
debugPrint a =
    Debug.log (prettyPrint a) ""


{-| Get the number of rows in a matrix
-}
numRows : Matnxn -> Int
numRows a =
    Tuple.first a.dimensions


{-| Get the number of columns in a matrix
-}
numColumns : Matnxn -> Int
numColumns a =
    Tuple.second a.dimensions


{-| Check if the matrix is a 3d vector
-}
check3dVec : Matnxn -> Bool
check3dVec a =
    if numRows a == 3 && numColumns a == 1 then
        True
    else
        False


{-| Map2 for arrays
-}
arraymap2 : (a -> b -> c) -> Array.Array a -> Array.Array b -> Array.Array c
arraymap2 f a b =
    if Array.isEmpty a || Array.isEmpty b then
        Array.fromList []
    else
        let
            dropArr a =
                Array.slice 1 (Array.length a) a

            result =
                case ( Array.get 0 a, Array.get 0 b ) of
                    ( Just aval, Just bval ) ->
                        Array.fromList [ f aval bval ]

                    _ ->
                        Array.fromList []
        in
            Array.append result (arraymap2 f (dropArr a) (dropArr b))



--string must be enclosed by brackets


matParser : String -> List (List Float)
matParser string =
    if String.startsWith "[" string && String.endsWith "]" string then
        String.dropLeft 1 string
            |> String.dropRight 1
            |> String.split ";"
            |> List.map (String.split " ")
            |> List.map (List.filter (\x -> not <| String.isEmpty x))
            |> List.map (List.map (\x -> Result.withDefault 0 (String.toFloat x)))
    else
        []



-- Operators for convenience


{-| Matrix multiply
-}
(**) : Matrix -> Matrix -> Matrix
(**) =
    mul


{-| alias for add function
-}
(.+) : Matrix -> Matrix -> Matrix
(.+) =
    add
