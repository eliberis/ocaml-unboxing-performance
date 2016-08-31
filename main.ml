open Core.Std
open Core_bench.Std

type point3d = { x : int; y : int; z : int }
type triangle = { a : point3d; b : point3d; c : point3d; }
type triangle_ubx = { m : point3d [@unboxed];
                      n : point3d [@unboxed];
                      o : point3d [@unboxed];
                    }
type colourful_triangles = { purple : triangle;
                             yellow : triangle;
                             orange : triangle;
                             salmon : triangle;
                           }
type colourful_triangles_ubx = { green : triangle_ubx [@unboxed];
                                 white : triangle_ubx [@unboxed];
                                 black : triangle_ubx [@unboxed];
                                 lilac : triangle_ubx [@unboxed];
                               }

(* The following integers are not constants to prevent propagating
   and inlining. *)
let random_int1 = Random.int 5000
let random_int2 = Random.int 5000

let random_point random_int = {
  x = random_int;
  y = random_int;
  z = random_int;
}

let random_triangle r = {
  a = random_point r;
  b = random_point r;
  c = random_point r;
}

let random_triangle_ubx r = {
  m = random_point r;
  n = random_point r;
  o = random_point r;
}

let random_coltri r = {
  purple = random_triangle r;
  yellow = random_triangle r;
  orange = random_triangle r;
  salmon = random_triangle r;
}

let random_coltri_ubx r = {
  green = random_triangle_ubx r;
  white = random_triangle_ubx r;
  black = random_triangle_ubx r;
  lilac = random_triangle_ubx r;
}

let consume_point a =
  ignore(a.x > 0 && a.y > 0 && a.z > 0)

let consume_triangle t =
  consume_point t.a;
  consume_point t.b;
  consume_point t.c

let consume_triangle_ubx t =
  consume_point t.m; (* If Flambda is not enabled, this will
                        result in an allocation *)
  consume_point t.n;
  consume_point t.o

let t1 =
  Bench.Test.create
    ~name:"Access boxed (depth = 1)"
    (fun () ->
       let t1 = random_triangle random_int1 in
       let t2 = random_triangle random_int2 in
       let b = random_int1 > 5 in
       (* Undecidable branch (at compile time) to prevent inlining
          the triangle *)
       let t = if b then t1 else t2 in
       consume_triangle t
    )

let t2 =
  Bench.Test.create
    ~name:"Access unboxed (depth = 1)"
    (fun () ->
       let t1 = random_triangle_ubx random_int1 in
       let t2 = random_triangle_ubx random_int2 in
       let b = random_int1 > 5 in
       let t = if b then t1 else t2 in
       consume_triangle_ubx t
    )

let t3 =
  Bench.Test.create
    ~name:"Access boxed (depth = 2)"
    (fun () ->
       let t1 = random_coltri random_int1 in
       let t2 = random_coltri random_int2 in
       let b = random_int1 > 5 in
       let t = if b then t1 else t2 in
       consume_triangle t.purple;
       consume_triangle t.yellow;
       consume_triangle t.orange;
       consume_triangle t.salmon
    )

let t4 =
  Bench.Test.create
    ~name:"Access unboxed (depth = 2)"
    (fun () ->
       let t1 = random_coltri_ubx random_int1 in
       let t2 = random_coltri_ubx random_int2 in
       let b = random_int1 > 5 in
       let t = if b then t1 else t2 in
       consume_triangle_ubx t.green;
       consume_triangle_ubx t.white;
       consume_triangle_ubx t.black;
       consume_triangle_ubx t.lilac
    )

let tests = [ t1; t2; t3; t4 ]

let command = Bench.make_command tests
let () =
  Random.init(0);
  Command.run(command)

(* Benchmark results: *)
(*
   ┌────────────────────────────┬──────────┬─────────┬────────────┐
   │ Name                       │ Time/Run │ mWd/Run │ Percentage │
   ├────────────────────────────┼──────────┼─────────┼────────────┤
   │ Access boxed (depth = 1)   │  20.89ns │  32.00w │     23.46% │
   │ Access unboxed (depth = 1) │   1.78ns │         │      2.00% │
   │ Access boxed (depth = 2)   │  89.03ns │ 138.00w │    100.00% │
   │ Access unboxed (depth = 2) │  39.22ns │  74.01w │     44.05% │
   └────────────────────────────┴──────────┴─────────┴────────────┘

   Explanation:
   1. (Access boxed, depth = 1)
     Program allocates:
       * 2 triangles --- (1 header word and 3 fields
                          plus the following each)
         * 3 points  --- (1 header word and 3 immediates each)
       Total: 2 x (1 + 3 + 3 x (1 + 3)) = 32 (words)

   2. (Access unboxed, depth = 1)
     Program allocates:
       * 2 triangles --- (1 header word plus the following (inlined) each)
         * 3 points  --- 3 words (3 immediates, no header)
     Total: 2 x (1 + 3 x 3) = 20 (words)
     The compiler is able to inline [consume_triangle] and [random_triangle],
     and match up field declaration and projection to avoid allocations
     altogether.

   3. (Access boxed, depth = 2)
     Program allocates:
       * 2 coloured triangles --- (1 header word and 4 fields
                                   plus the following each)
        * 4 triangles --- (1 header word and 3 fields
                           plus the following each)
          * 3 points  --- (1 header word and 3 immediates each)
       Total: 2 x (1 + 4 + 4 x (1 + 3 + 3 x (1 + 3)) = 138 (words)

   4. (Access unboxed, depth = 2)
     Program allocates:
       * 2 coloured triangles --- (1 header word plus
                                   the following (inlined) each)
         * 4 triangles --- (no header word plus the following (inlined) each)
           * 3 points  --- 3 words (3 immediates, no header)
       Total: 2 x (1 + 4 x 3 x 3) = 74 (words)
*)
