module Utils exposing (interchangeArray, interchangeList, reversePrepend, splitReverse, mapReverse, map2foldl)

import Array

{-| Prepend one list onto another while reversing it.

Equivalent to
```
List.append (List.reverse pre) post
```
but theoretically faster. -}
reversePrepend : List a -> List a -> List a
reversePrepend pre post =
    case pre of
        [] ->
            post
        head :: tail ->
            reversePrepend tail (head :: post)

{-| Interchange two elements in an array. Do nothing if either index out of bounds. -}
interchangeArray : Int -> Int -> Array.Array a -> Array.Array a
interchangeArray firstIndex secondIndex column =
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

{-| Swap two elements in a list using the fastest available method.

TODO: Improve the logic for determining the fastest method.
-}
interchangeList : Int -> Int -> List a -> List a
interchangeList index1 index2 list =
    if index1 == index2 then
        list
    else if (max index1 index2) > 400 then --roughly the break even point in terms of performance - can get a more specific condition with better benchmarking
        interchangeListViaArray index1 index2 list
    else
        interchangeListRecursively index1 index2 list


{-| Convert a an array to a list and interchange two elements, then convert back.

This is very slow when swapping near the start of a list, but outperforms the tail recursion when we're near the end of a long list! -}
interchangeListViaArray : Int -> Int -> List a -> List a
interchangeListViaArray firstIndex secondIndex aList =
    aList
        |> Array.fromList
        |> interchangeArray  firstIndex secondIndex
        |> Array.toList

{-| Interchange two elements in a list using a tail recursion scheme.

Much faster in the best case than converting to an array, slightly slower when we're near the end of a long list.
-}
interchangeListRecursively : Int -> Int -> List a -> List a
interchangeListRecursively index1 index2 list =
    if index1 < index2 then
        interchangeListRecursivelyKernel index1 index2 list list [] []
    else
        interchangeListRecursivelyKernel index2 index1 list list [] []

interchangeListRecursivelyKernel : Int -> Int -> List a -> List a -> List a -> List a -> List a
interchangeListRecursivelyKernel index1 index2 scan1 scan2 initial middle =
    case (index1, index2) of
        (0, 0) ->
            --we have now found both elements. initial and middle are in reverse order.
            case scan2 of
                [] -> --uh oh, no second element to swap
                    []
                head2 :: tail2 -> --we need to swap head with the head of scan1
                    case scan1 of
                        [] -> -- should never be reached
                            []
                        head1 :: _ ->
                            (head1 :: tail2)
                                |> reversePrepend middle
                                |> List.tail --the first element of middle is head1, which we don't want
                                |> Maybe.withDefault []
                                |> (::) head2
                                |> reversePrepend initial
        (0, _) -> 
            case scan2 of
                --we've found the first element to be swapped. hold onto it and start building out the middle section
                [] -> -- list ran out early!
                    [] --empty list means 'discard the result'
                head :: tail ->
                    interchangeListRecursivelyKernel index1 (index2 - 1) scan1 tail initial (head :: middle)
        (_, _) -> --in this case, scan1 and scan2 should be identical!
            -- walk through the list until we reach the first element to be swapped
            case scan1 of
                [] -> -- list ran out early!
                    [] --empty list means 'discard the result'
                head :: tail ->
                    interchangeListRecursivelyKernel (index1 - 1) (index2 - 1) tail tail (head :: initial) middle

{-| Split a list in two, with the first n elements being returned in reverse order. Faster in the specific situation where you want to reverse the list. -}
splitReverse : Int -> List a -> (List a, List a)
splitReverse n list =
    splitKernel n [] list

splitKernel : Int -> List a -> List a -> (List a, List a)
splitKernel n front back =
    case n of
        0 ->
            (front, back)
        _ ->
            case back of
                [] ->
                    (List.reverse front, [])
                head :: tail ->
                    splitKernel (n-1) (head :: front) tail

{-| Iterate over a list, returning the results in reverse order for maximum speed.
-}
mapReverse : (a -> b) -> List a -> List b
mapReverse f l =
    mapReverseKernel f l []

mapReverseKernel : (a -> b) -> List a -> List b -> List b
mapReverseKernel f l acc =
    case l of
        [] ->
            acc
        head :: tail ->
            mapReverseKernel f tail ((f head) :: acc)

{-| Simultaneously map2 two lists together with one function and foldl them with a second, avoiding unnecessary intermediate storage -}
map2foldl : (a -> b -> c) -> (c -> c -> c) -> List a -> List b -> c -> c
map2foldl combineFunc reduceFunc list1 list2 acc =
    case (list1, list2) of
        (head1 :: tail1, head2 :: tail2) ->
            let
                newAcc = combineFunc head1 head2
                    |> reduceFunc acc
            in
                map2foldl combineFunc reduceFunc tail1 tail2 newAcc
        _ ->
            acc