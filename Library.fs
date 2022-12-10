namespace ListZipperVsRbTree

(* Implementation guided by following paper: https://arxiv.org/pdf/1412.4882.pdf *)

module AATree =
    type AaTree<'T> = 
        | E
        | T of int * AaTree<'T> * 'T * AaTree<'T>

    let sngl = function
        | E -> false
        | T(_, _, _, E) -> true
        | T(lvx, _, _, T(lvy, _, _, _)) -> lvx > lvy

    let empty = E

    let lvl = function
        | E -> 0
        | T(lvt, _, _, _) -> lvt

    let nlvl = function
        | T(lvt, _, _, _) as t -> 
            if sngl t
            then (lvt - 1)
            else lvt
        | _ -> failwith "unexpected nlvl case"

    let skew = function
        | T(lvx, T(lvy, a, ky, b), kx, c) when lvx = lvy
            -> T(lvx, a, ky, T(lvx, b, kx, c))
        | t -> t

    let split = function
        | T(lvx, a, kx, T(lvy, b, ky, T(lvz, c, kz, d))) 
            when lvx = lvy && lvy = lvz
              -> T(lvx + 1, T(lvx, a, kx, b), ky, T(lvx, c, kz, d))
        | t -> t

    let rec insert item = function
        | E -> T(1, E, item, E)
        | T(h, l, v, r) as node ->
            if item < v
            then split <| (skew <| T(h, insert item l, v, r)
            elif item > v
            then split <| (skew <| T(h, l, v, insert item r))
            else node

    let adjust = function
        | T(lvt, lt, kt, rt) as t when lvl lt >= lvt - 1 && lvl rt >= (lvt - 1) 
            -> t
        | T(lvt, lt, kt, rt) when lvl rt < lvt - 1 && sngl lt-> 
            skew <| T(lvt - 1, lt, kt, rt)
        | T(lvt, T(lv1, a, kl, T(lvb, lb, kb, rb)), kt, rt) when lvl rt < lvt - 1 
            -> T(lvb + 1, T(lv1, a, kl, lb), kb, T(lvt - 1, rb, kt, rt))
        | T(lvt, lt, kt, rt) when lvl rt < lvt 
            -> split <| T(lvt - 1, lt, kt, rt)
        | T(lvt, lt, kt, T(lvr, T(lva, c, ka, d), kr, b)) -> 
            let a = T(lva, c, ka, d)
            T(lva + 1, T(lvt - 1, lt, kt, c), ka, (split (T(nlvl a, d, kr, b))))
        | _ -> failwith "unexpected adjust case"

    let rec dellrg = function
        | T(_, l, v, E) -> (l, v)
        | T(h, l, v, r) ->
            let (newLeft, newVal) = dellrg l
            T(h, newLeft, v, r), newVal
        | _ -> failwith "unexpected dellrg case"

    let rec delete item = function
        | E -> E
        | T(_, E, v, rt) when item = v -> rt
        | T(_, lt, v, E) when item = v -> lt
        | T(h, l, v, r) as node ->
            if item < v
            then adjust <| T(h, delete item l, v, r)
            elif item > v
            then T(h, l, v, delete item r)
            else 
                let (newLeft, newVal) = dellrg l
                T(h, newLeft, newVal, r)
