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
  assert(a.x > 0 && a.y > 0 && a.z > 0)

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
       let b = random_int1 > 2500 in
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
       let b = random_int1 > 2500 in
       let t = if b then t1 else t2 in
       consume_triangle_ubx t
    )

let t3 =
  Bench.Test.create
    ~name:"Access boxed (depth = 2)"
    (fun () ->
       let t1 = random_coltri random_int1 in
       let t2 = random_coltri random_int2 in
       let b = random_int1 > 2500 in
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
       let b = random_int1 > 2500 in
       let t = if b then t1 else t2 in
       consume_triangle_ubx t.green;
       consume_triangle_ubx t.white;
       consume_triangle_ubx t.black;
       consume_triangle_ubx t.lilac
    )

let t5 =
  Bench.Test.create
    ~name:"Access boxed (depth = 2), with-style record creation"
    (fun () ->
       let t1 = random_coltri random_int1 in
       let t2 = { t1 with purple = random_triangle random_int2 } in
       let b = random_int1 > 2500 in
       let t = if b then t1 else t2 in
       consume_triangle t.purple;
       consume_triangle t.yellow;
       consume_triangle t.orange;
       consume_triangle t.salmon
    )

let t6 =
  Bench.Test.create
    ~name:"Access unboxed (depth = 2), with-style record creation"
    (fun () ->
       let t1 = random_coltri_ubx random_int1 in
       let tri2 = { t1 with green = random_triangle_ubx random_int2 } in
       let b = random_int1 > 25 in
       let t = if b then t1 else tri2 in
       consume_triangle_ubx t.green;
       consume_triangle_ubx t.white;
       consume_triangle_ubx t.black;
       consume_triangle_ubx t.lilac
    )

let tests = [ t1; t2; t3; t4; t5; t6 ]

let command = Bench.make_command tests
let () =
  Random.init(0);
  Command.run(command)

(* Benchmark results (with Flambda): *)
(*

┌────────────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                                                   │ Time/Run │ mWd/Run │ Percentage │
├────────────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
│ Access boxed (depth = 1)                               │  20.16ns │  32.00w │     20.87% │
│ Access unboxed (depth = 1)                             │  11.50ns │  20.00w │     11.91% │
│ Access boxed (depth = 2)                               │  96.62ns │ 138.00w │    100.00% │
│ Access unboxed (depth = 2)                             │  39.84ns │  74.01w │     41.23% │
│ Access boxed (depth = 2), with-style record creation   │  64.89ns │  90.00w │     67.16% │
│ Access unboxed (depth = 2), with-style record creation │  44.44ns │  74.01w │     45.99% │
└────────────────────────────────────────────────────────┴──────────┴─────────┴────────────┘

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

   5. (Access boxed (depth = 2), with-style record creation)
     The situation is similar to 3, but the compiler only allocates [t1]
      -- 69 words (138/2).
     In addition to that, another block is allocated for the [purple] field
     of the triangle -- 16 words.
     A block for [t2] --- 5 words (1 header + 4 fields)
     Total: 90 words.

   5. (Access unboxed (depth = 2), with-style record creation)
     Exactly as in 4.
*)

(* Benchmark results (without Flambda): *)
(*
┌────────────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                                                   │ Time/Run │ mWd/Run │ Percentage │
├────────────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
│ Access boxed (depth = 1)                               │  21.67ns │  32.00w │      7.24% │
│ Access unboxed (depth = 1)                             │  44.03ns │  56.00w │     14.71% │
│ Access boxed (depth = 2)                               │ 120.43ns │ 138.04w │     40.25% │
│ Access unboxed (depth = 2)                             │ 299.23ns │ 338.04w │    100.00% │
│ Access boxed (depth = 2), with-style record creation   │  51.93ns │  90.02w │     17.35% │
│ Access unboxed (depth = 2), with-style record creation │ 157.13ns │ 302.06w │     52.51% │
└────────────────────────────────────────────────────────┴──────────┴─────────┴────────────┘
*)
