module Matrix exposing
    ( Matrix
    , fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper
    , cvecFromList, rvecFromList, cvec, rvec, vec
    , cross, dot
    , add, equivalent, sMul, sDiv, map, map2, eMul
    , mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size
    , toString, debugPrint
    , to2DList, toFlatList
    , luPivotWrapper
    )

{-| A matrix library written completely in Elm.
This library aims to be a reasonably complete suite of linear algebra tools.

Some highlights are that this library has generic sized matrices,
transposes, multiplication, and inversion.

    import Matrix as Mt


    --and program away!


# The Matrix Type

@docs Matrix


# Creating Matrices

@docs fromList, from2DList, mat, fromString, mats, zeroes, ones, eye, upper, lower, strictLower, strictUpper


# Creating Vectors

@docs cvecFromList, rvecFromList, cvec, rvec, vec


# Vector Operations

@docs cross, dot


# Matrix Element-wise Operations

@docs add, equivalent, sMul, sDiv, map, map2, eMul


# Matrix Operations

@docs mul, vcat, hcat, get, set, transpose, determinant, det, solveV, solve, invert, inv, luDecomp, getRows, getColumns, size


# Matrix Display

@docs toString, debugPrint


# Interop

@docs to2DList, toFlatList

-}

import Array

--import Debug


type alias Matnxn =
    { dimensions : ( Int, Int )
    , elements : Array.Array Float
    }


{-| The Matrix type. It can either be an actual matrix or an error string.
-}
type Matrix
    = Mat Matnxn
    | Err String



-- Matrix Creation


{-| Create a (n rows x m columns) matrix with the list as the elements.
Fails if dimension mismatch. Elements need to be specified in row-major order.

    matrix =
        Matrix.fromList ( 2, 3 ) [ 2, 2, 2, 3, 3, 3 ]

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
                String.fromInt (rows * columns)

            numelements =
                String.fromInt <| Array.length elements
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
        Matrix.mat
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


{-| Shorthand for fromString
-}
mats : String -> Matrix
mats =
    fromString


{-| Create a column vector from a list.

    column =
        Matrix.cvecFromList [ 1, 2, 3, 4 ]

-}
cvecFromList : List Float -> Matrix
cvecFromList a =
    fromList ( List.length a, 1 ) a


{-| Create a column vector from a list.
-}
cvec : List Float -> Matrix
cvec =
    cvecFromList


{-| Create a column vector from a list.
-}
vec : List Float -> Matrix
vec =
    cvec


{-| Create a row vector from a list.
-}
rvecFromList : List Float -> Matrix
rvecFromList a =
    fromList ( 1, List.length a ) a


{-| Create a row vector from a list.
-}
rvec : List Float -> Matrix
rvec =
    rvecFromList


{-| Generate a random matrix
-}
rand : ( Int, Int ) -> Matrix
rand a =
    Err "Not Implemented"


{-| Generate a matrix of ones.

    lots_of_ones =
        Matrix.ones ( 4, 3 )

-}
ones : ( Int, Int ) -> Matrix
ones ( rows, columns ) =
    fromArray ( rows, columns ) <| Array.repeat (rows * columns) 1.0


{-| Generate a matrix of zeroes.

    lots_of_zeroes =
        Matrix.zeroes ( 3, 4 )

-}
zeroes : ( Int, Int ) -> Matrix
zeroes ( rows, columns ) =
    fromArray ( rows, columns ) <| Array.repeat (rows * columns) 0.0


{-| Create an nxn identity matrix.

    identity =
        Matrix.eye 3

-}
eye : Int -> Matrix
eye diagonal =
    let
        gen x =
            if modBy (diagonal + 1) x == 0 then
                1

            else
                0
    in
    Array.initialize (diagonal * diagonal) gen
        |> fromArray ( diagonal, diagonal )


{-| Create an nxn upper triangular matrix.

    triangle =
        Matrix.upper 4

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


{-| Create an nxn strict upper triangular matrix.
This means that elements along the diagonal are zero.

    striangle =
        Matrix.strictUpper 4

-}
strictUpper : Int -> Matrix
strictUpper diagonal =
    map2 (-) (upper diagonal) (eye diagonal)


{-| Create an nxn lower triangular matrix.

    ltriangle =
        Matrix.lower 4

-}
lower : Int -> Matrix
lower diagonal =
    transpose <| upper diagonal


{-| Create an nxn strict lower triangular matrix.
This means that elements along the diagonal are zero.

    sltriangle =
        Matrix.strictLower 4

-}
strictLower : Int -> Matrix
strictLower diagonal =
    map2 (-) (transpose <| upper diagonal) (eye diagonal)



-- Operations


{-| Perform matrix multiplication

    A * B

    a =
        Matrix.eye (2, 2)

    b =
        Matrix.mats "[2, 3; 4 5]"

    c =
        mul a b

-}
mul : Matrix -> Matrix -> Matrix
mul a b =
    forwardError2 "[in mul]" mulList a b


mulArray : Matnxn -> Matnxn -> Matrix
mulArray a_ b_ =
    Err "Not Implemented"


{-| Implementation of multiplication using lists.
Probably slow.
-}
mulList : Matnxn -> Matnxn -> Matrix
mulList a_ b_ =
    if numColumns a_ /= numRows b_ then
        let
            acolumns =
                String.fromInt <| numColumns a_

            brows =
                String.fromInt <| numRows b_
        in
        Err <| "Dimension mismatch in a*b: a.columns = " ++ acolumns ++ " b.rows = " ++ brows ++ "."

    else
        let
            a_list =
                to2DListBase a_

            b_list =
                to2DList <| transposeBase b_

            --collapse a row from the left and column from the right into scalar
            collapse x y =
                List.sum <| List.map2 (*) x y

            --element level
            constructList x y =
                case x of
                    [] ->
                        []

                    m :: ms ->
                        List.map (collapse m) y :: constructList ms y
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


{-| Splits the LU factorized single matrix into two
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

type alias Permutation = Array.Array Int

type alias LUColumn =
    { pivotRow : Int
    , uColumn : Array.Array Float
    , lColumn : Array.Array Float
    }

type alias LUElements =
    { permutation : Permutation
    , lRows : List (Array.Array Float)
    , uColumns : List (Array.Array Float)
    }

luPivotWrapper : Matrix -> LUElements
luPivotWrapper m =
    case m of
        Mat m_ ->
            luPivot m_
        _ ->
            { permutation = Array.empty
            , lRows = []
            , uColumns = []
            }

{- Performs LU factorization with a pivot.

Algorithm based on Crout's method, as described in _Numerical Recipes in C_ section 2.3, page 46.-}
luPivot : Matnxn -> LUElements
luPivot a = 
    let
        -- first find the scaling factors for every row
        rows =
            getRowArrays a

        n = List.length rows --assume square matrix check already done

        scalingFactors =
            getScaling rows

        -- now process each column by Crout's method, while searching for the best pivot.
        columns = transposeVectors rows n
            |> List.map Array.toList

    in
    luOuterLoop scalingFactors columns

{-| Compute scaling factors as the first stage of LU factorisation, to ensure proper implicit pivoting.
-}
getScaling : List (Array.Array Float) -> List Float
getScaling m =
    let
        processRow : Array.Array Float -> Float
        processRow r =
            r
                |> Array.map abs
                |> Array.foldl max 0.0
                |> (/) 1
    in
    List.map processRow m

{-| First inner loop for Crout's method. Computes the elements of the U matrix (above-diagonal), which do not depend on the pivot.

- aColumn should be a column of the original matrix to be factorised as a list.
- lRowsTruncated should be a list of partial rows of the work-in-progress L matrix as lists of floats
- uColumnSoFar should be given an empty array for the initial call, and will be grown through recursive tail calls.-}
computeLUElements : List Float -> List (List Float) -> List Float -> List Float
computeLUElements aColumn lRowsTruncated uColumnSoFar =
    case (aColumn, lRowsTruncated) of
        ([],_) ->
            -- we have reached the end of this portion of the column, meaning we should have i=j-1, so return
            List.reverse uColumnSoFar

        (_, []) ->
            List.reverse uColumnSoFar -- required for compiler safety but should not be called

        (aij :: nextAij, lRow :: nextLRow) ->
            let
                -- this works because uColumnSoFar only has i-1 elements, or because lRows is truncated to only j-1 elements
                luSum = List.reverse uColumnSoFar
                    |> List.map2 (*) lRow
                    |> List.sum

                thisElement = aij - luSum

            in --should be amenable to tail call optimisations
            computeLUElements nextAij nextLRow (thisElement :: uColumnSoFar)

{-| Search a column of undivided elements in the L matrix for the highest scaled element, i.e. the best implicit pivot.

- scalingFactors is the inverse of the largest element in each row.
- lColumn is an array of work in progress elements of the L matrix, which have NOT been divided by the pivot.
-}
findLUPivot : List Float -> List Float -> (Int, Float)
findLUPivot scalingFactors lColumn =
    let
        indexedMax : (Int, Float, Float) -> (Int, Float, Float) -> (Int, Float, Float)
        indexedMax ((_, scaled1, _) as indexedNumber1) ((_, scaled2, _) as indexedNumber2) =
            if scaled1 >= scaled2 then
                indexedNumber1
            else
                indexedNumber2
    in 
    List.map2 (\s l -> (s*l, l)) scalingFactors lColumn
        --|> Debug.log "scaled"
        |> List.indexedMap (\i (s, l) -> (i, s, l))
        |> List.foldl indexedMax (0, 0.0, 0.0)
        |> (\(i, s, l) -> (i, l))

{- Inner loops wrapper for Crout's method. Computes a single column of the L and U matrices and determines which row is best for pivoting
-}
computeLUColumn : List Float -> Int -> List Float -> List (List Float) -> LUColumn
computeLUColumn scalingFactors j aColumn lRowsTruncated =
    let
        length = List.length aColumn

        -- get a column of the LU matrix without doing any pivot related shenanigans
        luColumnPrePivot = computeLUElements aColumn lRowsTruncated []
            |> Array.fromList

        --subdiagonal = Array.slice j length luColumnPrePivot
        --    |> Debug.log "subdiagonal"

        ----now figure out our best pivot
        --pivot = findLUPivot scalingFactors (Array.toList subdiagonal)
        --    |> Debug.log "pivot"

        ----the index of the row with the best pivot in the subdiagonal list
        --pivotRow = (Tuple.first pivot)

        --luColumnPivoted =
        --    if pivotRow /= 0 then
        --        interchange j (pivotRow+j) luColumnPrePivot
        --    else
        --        luColumnPrePivot

        --uColumn = Array.slice 0 (j+1) luColumnPivoted

        uColumn = Array.slice 0 (j+1) luColumnPrePivot

        pivot = Array.get j luColumnPrePivot
            |> Maybe.withDefault 0.0

        --pivotReciprocal = 1.0/(Tuple.second pivot)

        pivotReciprocal = 1.0/pivot

        --lColumn = Array.slice (j+1) length luColumnPivoted
        lColumn = Array.slice (j+1) length luColumnPrePivot
            |> Array.map ((*) pivotReciprocal)
    in
    -- we will save switching rows in the rest of the matrix for the wrapper function
    { pivotRow = 0
    , uColumn = uColumn
    , lColumn = lColumn
    }    

{-| Interchange two elements in an array. Do nothing if either index out of bounds. -}
interchange : Int -> Int -> Array.Array a -> Array.Array a
interchange firstIndex secondIndex column =
    let
        first = Array.get firstIndex column

        second = Array.get secondIndex column
    in
        case (first, second) of
            (Just first_, Just second_) ->
                column
                    |> Array.set firstIndex second_
                    |> Array.set secondIndex first_

            _ ->
                column

luOuterLoop : List Float -> List (List Float) -> LUElements
luOuterLoop scalingFactors aColumns =
    let
        length = List.length aColumns

        unfinishedLRows = List.repeat length Array.empty

        basePermutation = List.range 0 (length - 1)
            |> Array.fromList
    in
    luOuterLoopKernel scalingFactors aColumns 0 [] unfinishedLRows [] basePermutation

{-| Compute the pivot permutation and U and L matrices by iterating through columns of the a matrix.

- scalingFactors - inverses of largest element in each row of a
- aColumns - the matrix to be factorised
- finishedLRows - builds up L matrix as it's calculated. initially, pass an empty list
- unfinishedLRows -  -}
luOuterLoopKernel : List Float -> List (List Float) -> Int -> List (Array.Array Float) -> List (Array.Array Float) -> List (Array.Array Float) -> Array.Array Int -> LUElements
luOuterLoopKernel scalingFactors aColumns j finishedLRows unfinishedLRows uColumns permutation =
    case aColumns of
        [] ->
            { permutation = permutation
            , lRows = List.reverse finishedLRows
            , uColumns = uColumns
            }
        aColumn :: nextAColumn ->
            let
                length = List.length aColumn

                lRows = List.append (List.reverse finishedLRows) unfinishedLRows -- gross gross gross, find a way to avoid this!!
                    |> List.map Array.toList

                { pivotRow, uColumn, lColumn } = computeLUColumn scalingFactors j aColumn lRows

                newUColumns = uColumn :: uColumns -- note that this counter-intuitively builds to the left! but it's efficient

                -- if we need to swap rows for the pivot, do so
                ( newPermutation, switchedLRows) =
                    --if pivotRow /= 0 then
                    --    ( interchange j (pivotRow + j) permutation, Array.toList (interchange 0 pivotRow (Array.fromList unfinishedLRows)) )
                    --else
                        (permutation, unfinishedLRows)

                (newFinishedLRows, newUnfinishedLRows) =
                    case switchedLRows of
                        h :: t ->
                            ( h :: finishedLRows
                            , List.map2 Array.push (Array.toList lColumn) t
                            )
                        [] -> --should not reach this point in correct use
                            ( [] , [] )

            in
                luOuterLoopKernel scalingFactors nextAColumn (j+1) newFinishedLRows newUnfinishedLRows newUColumns newPermutation


{-| Performs LU factorization without a pivot.

The result is a single matrix that contains all elements of both L and U.
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

{-| Generates a list of pairs of indices, going first through rows, then through columns

i.e. (1, 1), (2, 1), (3, 1) ... (i_range, 1), (2, 1), (2, 2) ... (i_range, j_range)
-}
genIndices : ( Int, Int ) -> List ( Int, Int )
genIndices ( i_range, j_range ) =
    let
        listj =
            List.range 1 j_range
            |> List.map (List.repeat i_range)
            |> List.concat

        listi =
            List.range 1 i_range
            |> List.repeat j_range 
            |> List.concat 
    in
    List.map2 (\a b -> ( a, b )) listi listj

{-| Update one element of the LU decomposition using Crout's algorithm, as described in _Numerical Recipes in C_ section 2.3.

-}
luComputeElem : ( Int, Int ) -> Matnxn -> Matnxn -> Matnxn
luComputeElem ( i, j ) original lu =
    let
        tiny =
            10 ^ -40

        getWithDefault index m =
            Maybe.withDefault 0.0 (getBase index m)

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
        -- NRIC 2.3.13
        setBase ( i, j ) ((aij - (List.sum <| List.map compute k)) / bjj) lu

    else
        let
            k =
                List.range 1 (i - 1)
        in
        -- NRIC 2.3.12
        setBase ( i, j ) (aij - (List.sum <| List.map compute k)) lu




{-| Get an item at index (row, column). Indices are 1-indexed.

    a =
        Matrix.mats "[3 4; 5 6]"

    b =
        -- equals 4
        get ( 1, 2 ) a

-}
get : ( Int, Int ) -> Matrix -> Maybe Float
get indices a =
    forwardErrorF "[in get]" (getBase indices) a


getBase : ( Int, Int ) -> Matnxn -> Maybe Float
getBase ( r_index, c_index ) a_ =
    let
        check_r_bounds =
            0 < r_index && r_index <= numRows a_

        check_c_bounds =
            0 < c_index && c_index <= numColumns a_
    in
    if check_r_bounds && check_c_bounds then
        Array.get ((r_index - 1) * numColumns a_ + c_index - 1)
            a_.elements

    else
        Nothing


{-| Get an item at index (row, column). Indices are 1-indexed.
-}
set : ( Int, Int ) -> Float -> Matrix -> Matrix
set indices data a =
    forwardError "[in set]" (\x -> Mat <| setBase indices data x) a


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
                Array.set ((r_index - 1) * numColumns a_ + c_index - 1)
                    data
                    a_.elements
        }

    else
        a_


{-| Add two matrices of identical dimensions together

    a =
        Matrix.add (Matrix.zeroes ( 2, 2 )) (Matrix.ones ( 2, 2 ))

-}
add : Matrix -> Matrix -> Matrix
add a b =
    forwardError2 "[in add]" addBase a b


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
    forwardError "[in map]"
        (\a_ -> fromArray ( numRows a_, numColumns a_ ) <| Array.map f a_.elements)
        a


{-| Map a function over elements of same index between matrices
-}
map2 : (Float -> Float -> Float) -> Matrix -> Matrix -> Matrix
map2 f a b =
    forwardError2 "[in map2]" (map2Base f) a b


map2Base : (Float -> Float -> Float) -> Matnxn -> Matnxn -> Matrix
map2Base f a b =
    if equalSize a b then
        fromArray a.dimensions <| arraymap2 f a.elements b.elements

    else
        Err "Unequal Matrix sizes"


{-| Perform scalar multiplication on a matrix.
-}
sMul : Float -> Matrix -> Matrix
sMul a b =
    map ((*) a) b


{-| Perform element by element multiplication on a matrix.
-}
eMul : Matrix -> Matrix -> Matrix
eMul a b =
    map2 (*) a b


{-| Perform scalar division on a matrix.
-}
sDiv : Float -> Matrix -> Matrix
sDiv a b =
    sMul (1 / a) b


{-| Invert a square matrix.

    a =
        "[ 2 5; 6 7]"

    inva =
        invert a

    identity =
        mul a inva

-}
invert : Matrix -> Matrix
invert a =
    case a of
        Mat a_ ->
            solve a (eye (numRows a_))

        Err string ->
            Err string


{-| Shorthand for invert.
-}
inv : Matrix -> Matrix
inv =
    invert


{-| Solve a system of equations of the form
AX = B. You provide A and b and get back x
Where A, B, X are matrices
B is a matrix of solution vectors horizontally concatenated.

    a =
        Matrix.eye 3

    b =
        Matrix.hcat (Matrix.vec [ 3, 2, 1 ]) (Matrix.vec [ 1, 2, 3 ])

    x =
        Matrix.solve a b

-}
solve : Matrix -> Matrix -> Matrix
solve a b =
    case ( a, b ) of
        ( Mat a_, Mat b_ ) ->
            if numRows a_ == numRows b_ then
                let
                    bs =
                        getColumns b

                    single =
                        luNoPivotSingle a_

                    boundApply v =
                        case v of
                            Mat vec_ ->
                                applySubstitution single vec_

                            _ ->
                                v
                in
                List.foldr hcat (fromList ( numRows a_, 0 ) []) <| List.map boundApply bs

            else
                Err "Dimension mismatch"

        ( _, _ ) ->
            Err "One of the inputs was malformed"


{-| Solve a system of equations of the form
Ax = b. You provide A and b and get back x
Where A is a matrix, and b and x are vectors

    a =
        Matrix.eye 3

    b =
        Matrix.vec [ 3, 2, 1 ]

    x =
        Matrix.solve a b

-}
solveV : Matrix -> Matrix -> Matrix
solveV a b =
    forwardError2 "[in solveV]" solveVBase a b


solveVBase : Matnxn -> Matnxn -> Matrix
solveVBase a b =
    let
        b_is_vector =
            numColumns b == 1

        dimensions_match =
            numRows b == numColumns a

        a_is_square =
            numColumns a == numRows a
    in
    case ( b_is_vector, dimensions_match, a_is_square ) of
        ( True, True, True ) ->
            applySubstitution (luNoPivotSingle a) b

        ( False, _, _ ) ->
            Err "b is not a vector."

        ( _, False, _ ) ->
            Err "Dimensions of A and b do not match"

        ( _, _, False ) ->
            Err "A is not square"



{-| Applies forward and backward substitution, decoupling substitution from
   computing lu decomp
-}
applySubstitution : Matrix -> Matnxn -> Matrix
applySubstitution single b =
    let
        brows =
            numRows b

        z =
            zeroes ( brows, 1 )
    in
    case ( z, single ) of
        ( Mat z_, Mat single_ ) ->
            let
                is =
                    List.range 1 (numRows b)

                fsBound i y_ =
                    forwardSubstitution i single_ b y_

                y =
                    List.foldl fsBound z_ is

                bsBound i x_ =
                    backSubstitution i single_ y x_

                x =
                    List.foldr bsBound z_ is
            in
            Mat x

        _ ->
            z


{-| Perform forward substitution remembering that the diagonal of the lower lu
matrix is all ones.
l is technically the lower lu matrix
b is the original passed in vector of b's
y is the solution, updated piecewise, of L \* y = b

Numerical Recipes in C 2.3.6

-}
forwardSubstitution : Int -> Matnxn -> Matnxn -> Matnxn -> Matnxn
forwardSubstitution i l b y =
    let
        getWithDefault index m =
            Maybe.withDefault 0.0 (getBase index m)

        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        yi =
            case i of
                1 ->
                    getVecDefault i b

                _ ->
                    let
                        bi =
                            getVecDefault i b

                        j =
                            List.range 1 (i - 1)

                        lijs =
                            List.map (\curr_j -> getWithDefault ( i, curr_j ) l) j

                        yjs =
                            List.map (\curr_j -> getVecDefault curr_j y) j

                        sum =
                            List.sum <| List.map2 (*) lijs yjs
                    in
                    bi - sum
    in
    setBase ( i, 1 ) yi y


{-| Perform forward substitution remembering that the diagonal of the lower lu
matrix is all ones.
u is technically the upper lu matrix
y is the original passed in vector of b's
x is the solution, updated piecewise of BOTH Ux = y and the original Ax = b

Numerical Recipes in C 2.3.7

-}
backSubstitution : Int -> Matnxn -> Matnxn -> Matnxn -> Matnxn
backSubstitution i u y x =
    let
        getWithDefault index m =
            Maybe.withDefault 0.0 (getBase index m)

        getVecDefault index v =
            getWithDefault ( index, 1 ) v

        uii =
            getWithDefault ( i, i ) u

        xrows =
            numRows x

        xi =
            if i == xrows then
                getVecDefault i y / uii

            else
                let
                    --aij = getWithDefault ( i, j ) original
                    yi =
                        getVecDefault i y

                    j =
                        List.range (i + 1) xrows

                    uijs =
                        List.map (\curr_j -> getWithDefault ( i, curr_j ) u) j

                    xjs =
                        List.map (\curr_j -> getVecDefault curr_j x) j

                    sum =
                        List.sum <| List.map2 (*) uijs xjs
                in
                (yi - sum) / uii
    in
    setBase ( i, 1 ) xi x


{-| Transpose a matrix.
-}
transpose : Matrix -> Matrix
transpose a =
    forwardError "[in transpose]" transposeBase a


transposeBase : Matnxn -> Matrix
transposeBase a_ =
    let
        f index =
            let
                mappedindex =
                    modBy (numRows a_) index
                        * numColumns a_
                        + (index // numRows a_)
            in
            Maybe.withDefault 0.0 (Array.get mappedindex a_.elements)
    in
    Array.initialize (numColumns a_ * numRows a_) f
        |> fromArray ( numColumns a_, numRows a_ )


{-| Get the determinant of a square matrix.

    a =
        Matrix.mats "[1 2 3; 4 5 6; 7 8 9]"

    is_singular =
        if determinant a == 0 then
            "Matrix is singular"

        else
            "Matrix is not singular"

-}
determinant : Matrix -> Maybe Float
determinant a =
    let
        detBase a_ =
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
    in
    forwardErrorF "[in determinant]" detBase a


{-| Shorthand for determinant.
-}
det : Matrix -> Maybe Float
det =
    determinant


{-| Performs the dot product of two nxn vectors

    x =
        Matrix.vec [ 1, 0, 0 ]

    y =
        Matrix.vec [ 0, 1, 0 ]

    zero =
        Matrix.dot x y

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
                    numRows a_ == numRows b_
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


{-| Get the cross product of two 3d vectors. a >< b

    x =
        Matrix.vec [ 1, 0, 0 ]

    y =
        Matrix.vec [ 0, 1, 0 ]

    z =
        Matrix.cross x y

-}
cross : Matrix -> Matrix -> Matrix
cross a b =
    forwardError2 "[in cross]" crossBase a b


crossBase : Matnxn -> Matnxn -> Matrix
crossBase a_ b_ =
    if check3dVec a_ && check3dVec b_ then
        let
            getDefault x v =
                Maybe.withDefault 0.0 <| getBase x v

            ax =
                getDefault ( 1, 1 ) a_

            ay =
                getDefault ( 2, 1 ) a_

            az =
                getDefault ( 3, 1 ) a_

            bx =
                getDefault ( 1, 1 ) b_

            by =
                getDefault ( 2, 1 ) b_

            bz =
                getDefault ( 3, 1 ) b_
        in
        let
            i =
                (ay * bz) - (by * az)

            j =
                -1 * ((ax * bz) - (bx * az))

            k =
                (ax * by) - (bx * ay)
        in
        vec <| [ i, j, k ]

    else
        Err "One or both vectors are malformed."


{-| Checks if two matrices are of equal size
-}
equalSize : Matnxn -> Matnxn -> Bool
equalSize a b =
    (numRows a == numRows b) && (numColumns a == numColumns b)


{-| Checks if two matrices are equivalent within some epsilon.

    epsilon =
        10 ^ -4

    is_equivalent =
        equivalent epsilon a b

-}
equivalent : Float -> Matrix -> Matrix -> Bool
equivalent epsilon a b =
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
    forwardError2 "[in vcat]" vcatBase a b


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
                ++ String.fromInt acols
                ++ " b: "
                ++ String.fromInt bcols


{-| Concatenate two matrices horizontally.
-}
hcat : Matrix -> Matrix -> Matrix
hcat a b =
    forwardError2 "[in hcat]" hcatBase a b


hcatBase : Matnxn -> Matnxn -> Matrix
hcatBase a b =
    let
        arows =
            numRows a

        brows =
            numRows b
    in
    if arows == brows then
        vcat (transposeBase a) (transposeBase b)
            |> transpose

    else
        Err <|
            "Number of rows are not equal: a: "
                ++ String.fromInt arows
                ++ " b: "
                ++ String.fromInt brows


{-| Returns matrix as flat list

    Matrix.toFlatList (Matrix.eye 2) == [ 1, 0, 0, 1 ]

-}
toFlatList : Matrix -> List Float
toFlatList n =
    case n of
        Mat n_ ->
            Array.toList n_.elements

        _ ->
            []


{-| Returns matrix as 2d list.
Returns empty list if Matrix is in error.

    Matrix.to2DList (Matrix.eye 2) == [ [ 1, 0 ], [ 0, 1 ] ]

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


{-| Returns size of matrix, (rows, columns)
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
forwardError2 : String -> (Matnxn -> Matnxn -> Matrix) -> Matrix -> Matrix -> Matrix
forwardError2 error f a b =
    case ( a, b ) of
        ( Err string, Mat _ ) ->
            Err <| "\n" ++ error ++ " Matrix a: " ++ string

        ( Mat _, Err string ) ->
            Err <| "\n" ++ error ++ " Matrix b: " ++ string

        ( Err string, Err string2 ) ->
            Err <| "\n" ++ error ++ " Matrix a: " ++ string ++ "\n Matrix b: " ++ string2

        ( Mat a_, Mat b_ ) ->
            f a_ b_


forwardError : String -> (Matnxn -> Matrix) -> Matrix -> Matrix
forwardError error f a =
    case a of
        Mat a_ ->
            f a_

        Err string ->
            Err <| "\n" ++ error ++ string


forwardErrorF : String -> (Matnxn -> Maybe Float) -> Matrix -> Maybe Float
forwardErrorF error f a =
    case a of
        Mat a_ ->
            f a_

        Err string ->
            Nothing


{-| Helper to take a Matrix and stringify its dimensions
-}
dimToString : Matnxn -> String
dimToString a =
    let
        arows =
            String.fromInt <| numRows a

        acols =
            String.fromInt <| numColumns a
    in
    "(" ++ arows ++ "," ++ acols ++ ")"


{-| Change matrix into string form, such as what would be displayed in the terminal.

    Matrix.toString (Matrix.eye 3) == " 1 0 0\n 0 1 0\n 0 0 1"

-}
toString : Matrix -> String
toString a =
    case a of
        Mat m ->
            toStringBasic m

        Err string ->
            string


{-| Change correctly formed matrix into string form
-}
toStringBasic : Matnxn -> String
toStringBasic a =
    let
        strings =
            a.elements
                |> Array.toList
                |> List.map String.fromFloat
                |> List.map ((++) " ")

        structured_strings =
            make2D (numColumns a) strings

        matrix_string =
            structured_strings
                |> List.intersperse [ "\n" ]
                |> List.concat
                |> List.foldr (++) ""
    in
    matrix_string


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


{-| Returns the rows of a matrix as bare arrays in a list.
-}
getRowArrays : Matnxn-> List (Array.Array Float)
getRowArrays m =
    let
        nRows = numRows m

        nColumns = numColumns m
        
        fetchRow j =
            let
                offset = j * nColumns
            in
                Array.slice offset (offset + nColumns) m.elements

    in
        List.range 0 (nRows - 1)
            |> List.map fetchRow

{-| Take a list of arrays representing rows (or columns) of a matrix, and transpose them.

Private function with no safety checks for maximum speed. Be sure all arrays have the same length! -}
transposeVectors : List (Array.Array Float) -> Int -> List (Array.Array Float)
transposeVectors rowList nElements =
    let
        columnIndices = List.range 0 (nElements-1)

        getElementFromEach : List (Array.Array Float) -> Int -> Array.Array Float
        getElementFromEach rows colIndex =
            rows
                |> List.map (Array.get colIndex >> Maybe.withDefault 0.0)
                |> Array.fromList
    in
    List.map (getElementFromEach rowList) columnIndices

{-| Returns the rows of a matrix in a list.
-}
getRows : Matrix -> List Matrix
getRows a =
    case a of
        Mat m ->
            getRowArrays m
                |> List.map (fromArray (1, Tuple.second m.dimensions))
        _ ->
            []

{-| Returns the columns of a matrix in a list.
-}
getColumns : Matrix -> List Matrix
getColumns a =
    case a of
        Mat anxn ->
            let
                rows = getRowArrays anxn

                nColumns = numColumns anxn

                nRows = numRows anxn

                columns = transposeVectors rows nColumns
            in
            List.map (fromArray (nRows, 1)) columns
        _ ->
            []

{-| Helper to debug print. Most useful in repl.

    > Matrix.debugPrint (Matrix.eye 3)
    (3, 3) Matrix
     1 0 0
     0 1 0
     0 0 1
     : ""
     ""

-}
debugPrint : Matrix -> String
debugPrint a =
    let
        dimensions = size a
        num_rows = String.fromInt <| Tuple.first dimensions
        num_cols = String.fromInt <| Tuple.second dimensions
        description_string =
            "(" ++ num_rows ++ "," ++ num_cols ++ ")" ++ " Matrix\n"

        final_string =
            description_string ++ toString a ++ "\n"
    in
    final_string


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
    numRows a == 3 && numColumns a == 1


{-| Map2 for arrays
-}
arraymap2 : (a -> b -> c) -> Array.Array a -> Array.Array b -> Array.Array c
arraymap2 f a b =
    if Array.isEmpty a || Array.isEmpty b then
        Array.fromList []

    else
        let
            dropArr aa =
                Array.slice 1 (Array.length aa) aa

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
            |> List.map (List.map (\x -> Maybe.withDefault 0 (String.toFloat x)))

    else
        []
