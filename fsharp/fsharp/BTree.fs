// (c) MP-I (1998/9-2006/7) and CP (2005/6-2020/21)

module BTree

open Cp

// (1) Datatype definition -----------------------------------------------------

type BTree<'a> = Empty | Node of 'a * (BTree<'a> * BTree<'a>)

let inBTree x = either (konst Empty) Node x

let outBTree x =
    match x with
    | Empty -> i1()
    | Node (a,(t1,t2)) -> i2(a,(t1,t2))
    
// (2) Ana + cata + hylo -------------------------------------------------------
    
let baseBTree f g = id -|- (f >< (g >< g))

let recBTree g = baseBTree id g

let rec cataBTree a x = (a << (recBTree (cataBTree a)) << outBTree) x 

let rec anaBTree f x = (inBTree << (recBTree (anaBTree f)) << f) x

let hyloBTree a c = cataBTree a << anaBTree c

// (3) Map ---------------------------------------------------------------------

// instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )

let fmap f = cataBTree ( inBTree << baseBTree f id )


// instance Functor LTree
   //      where fmap f = cataLTree ( inLTree . baseLTree f id )
// let fmap f = cataLTree ( inLTree << baseLTree f id )

// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inBTree << (id -|- (id >< swap))) x

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (konst 0) (succ << (uncurry(+)) << p2)) x

// (4.3) Serialization ---------------------------------------------------------
let join(x,(l,r)) = l @ [x] @ r

let inord x = either nil join x

let auxpre(x,(l,r)) = x::l @ r

let preord l = (either nil auxpre) l

let auxpos (x,(l,r)) = l @ r @ [x]

let postordt l = cataBTree (either nil auxpos) l

// (4.4) Quicksort ---------------------------------------------------------

let rec part p l =
    match l with
    | [] -> ([],[])
    | h::t when p h ->
        let s,l = part p t
        h::s,l
    | h::t ->
        let s,l = part p t
        s,h::l
   
let qsep l =
    match l with
    | [] -> i1()
    | h::t -> let (s,l) = part ((>) h) t in i2(h,(s,l))
    
let qSort' l =
              match l with
              | [] -> []
              | otherwise -> hyloBTree inord qsep l

// pointwise version:

let rec qSort xs =
  match xs with
  | [] -> []
  | x :: xs ->
      let smaller = qSort (xs |> List.filter(fun e -> e <= x))
      let larger  = qSort (xs |> List.filter(fun e -> e > x))
      smaller @ [x] @ larger
                   
// (4.5) Traces ---------------------------------------------------------

let union l1 l2 = l1 @ List.filter (fun x -> not (List.contains x l1)) l2
               
let tunion(a,(l,r)) = union [for b in l -> a :: b] [for b in r -> a :: b]

let traces x = cataBTree (either (konst [[]]) tunion) x


// (4.6) Towers of Hanoi --------------------------------------------------

let present l = inord l

let strategy (d,n) =
    match n with
    | 0 -> i1()
    | otherwise -> i2 ((n-1,d),((not d,n-1),(not d,n-1)))
    
let hanoi x = (hyloBTree present strategy) x

(*
The Towers of Hanoi problem comes from a puzzle marketed in 1883
    by the French mathematician Édouard Lucas, under the pseudonym
    Claus. The puzzle is based on a legend according to which
    there is a temple, apparently in Bramah rather than in Hanoi as
    one might expect, where there are three giant poles fixed in the
    ground. On the first of these poles, at the time of the world's
    creation, God placed sixty four golden disks, each of different
    size, in decreasing order of size. The Bramin monks were given
    the task of moving the disks, one per day, from one pole to another
    subject to the rule that no disk may ever be above a smaller disk.
    The monks' task would be complete when they had succeeded in moving
    all the disks from the first of the poles to the second and, on
    the day that they completed their task the world would come to
    an end!
    
    There is a well known inductive solution to the problem given
    by the pseudocode below. In this solution we make use of the fact
    that the given problem is symmetrical with respect to all three
    poles. Thus it is undesirable to name the individual poles. Instead
    we visualize the poles as being arranged in a circle; the problem
    is to move the tower of disks from one pole to the next pole in
    a specified direction around the circle. The code defines H n d
    to be a sequence of pairs (k,d') where n is the number of disks,
    k is a disk number and d and d' are directions. Disks are numbered
    from 0 onwards, disk 0 being the smallest. (Assigning number 0
    to the smallest rather than the largest disk has the advantage
    that the number of the disk that is moved on any day is independent
    of the total number of disks to be moved.) Directions are boolean
    values, true representing a clockwise movement and false an anticlockwise
    movement. The pair (k,d') means move the disk numbered k from
    its current position in the direction d'. The semicolon operator
    concatenates sequences together, [] denotes an empty sequence
    and [x] is a sequence with exactly one element x. Taking the pairs
    in order from left to right, the complete sequence H n d prescribes
    how to move the n smallest disks one by one from one pole to the
    next pole in the direction d following the rule of never placing
    a larger disk on top of a smaller disk.
    
    H 0     d = [],
    H (n+1) d = H n d ; [ (n, d) ] ; H n d.
    
    (excerpt from R. Backhouse, M. Fokkinga / Information Processing
    Letters 77 (2001) 71--76)
*)

// (5) Depth and balancing (using mutual recursion) --------------------------

let auxbal1 ((b1,d1),(b2,d2)) = ((b1,b2),(d1,d2))

let auxbal2 (_,((b1,b2),(d1,d2))) = (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)

let auxbal3 b = either (konst(true,1)) (auxbal2 << (id >< auxbal1)) b

let baldepth b = cataBTree auxbal3 b

let balBTree b = (p1 << baldepth) b

let depthBTree b = (p2 << baldepth) b

(*
// (6) Going polytipic -------------------------------------------------------

-- natural transformation from base functor to monoid
tnat :: Monoid c => (a -> c) -> Either () (a,(c, c)) -> c
tnat f = either (const mempty) (theta . (f >< theta))
         where theta = uncurry mappend

-- monoid reduction 

monBTree f = cataBTree (tnat f)

-- alternative to (4.2) serialization ----------------------------------------

preordt' = monBTree singl

-- alternative to (4.1) counting ---------------------------------------------

countBTree' = monBTree (const (Sum 1))

-- (7) Zipper ----------------------------------------------------------------

data Deriv a = Dr Bool a (BTree a)

type Zipper a = [ Deriv a ]

plug :: Zipper a -> BTree a -> BTree a
plug [] t = t
plug ((Dr False a l):z) t = Node (a,(plug z t,l))
plug ((Dr True  a r):z) t = Node (a,(r,plug z t))

---------------------------- end of library ----------------------------------
*)