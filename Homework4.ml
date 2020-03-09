let pow a b =
    let rec helper acc n =
        match n with
        | 0 -> 1
        | 1 -> acc
        | x -> helper (acc * a) (x - 1)
    in helper a b;;


let float_pow a b =
    let rec helper acc n =
        match n with
        | 0 -> 1.0
        | 1 -> acc
        | (-1) -> (1.0 /. acc)
        | x -> if x < 0 then helper (acc *. a) (x + 1) else helper (acc *. a) (x - 1)
    in helper a b;;


let reverse lst =
    let rec helper final = function
      | [] -> final
      | h::t -> helper (h::final) t in
    helper [] lst;;


let rec compress lst = 
    match lst with
    | h :: (b :: _ as t) -> if h = b then compress t else h :: compress t
    | other -> other;;


let cluster lst =
    let rec helper sublist final = function
      | [] -> []
      | [x] -> (x :: sublist) :: final
      | h :: (b :: _ as t) ->
         if h = b then helper (h :: sublist) final t
         else helper [] ((h :: sublist) :: final) t  
      in reverse (helper [] [] lst);;


let slice lst i j =
    let rec sublist n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: sublist (n-1) t
    in
    let rec drop n = function
      | [] -> []
      | h :: t as l -> if n = 0 then l else drop (n-1) t
    in
    sublist (j - i) (drop i lst);;


let composition f g = fun x -> f (g x);;


let rec equiv_on f g = function
  | [] -> true
  | h::t -> if (f h) = (g h) then equiv_on f g t else false;;
  

let rec pairwisefilter cmp lst = 
    match lst with
    | h :: (b :: t) -> (cmp h b) :: pairwisefilter cmp t
    | other -> other;;


let polynomial lst = 
  fun x ->
  let rec helper = function
  | [] -> 0
  | (a, b) :: t -> a * (pow x b) + helper t
in helper lst;;


type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

let truth_table a b expression =
  let rec evalulate x val_a y val_b = function
    | Lit letter -> if letter = a then val_a
                    else if letter = b then val_b
                    else failwith "Literals in expression must match literals given in arguments"
    | Not e -> not(evalulate x val_a y val_b e)
    | And(e1, e2) -> evalulate x val_a y val_b e1 && evalulate x val_a y val_b e2
    | Or(e1, e2) -> evalulate x val_a y val_b e1 || evalulate x val_a y val_b e2
  in
  [(true, true, evalulate a true b true expression);
  (true, false, evalulate a true b false expression);
  (false, true, evalulate a false b true expression);
  (false, false, evalulate a false b false expression) ];;


type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let tree2str binary_tree = 
  let rec helper = function
  | Node (n, Empty, Empty) -> string_of_int n
  | Node (n, Empty, c2) -> string_of_int n ^ "(," ^ helper c2 ^ ")"
  | Node (n, c1, Empty) -> string_of_int n ^ "(" ^ helper c1 ^ ",)"
  | Node (n, c1, c2) -> string_of_int n ^ "(" ^ helper c1 ^ "," ^ helper c2 ^ ")"
  | Empty -> ""
in helper binary_tree;; 