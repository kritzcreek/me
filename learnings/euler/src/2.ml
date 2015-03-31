(*
 * Project Euler #2
 *
 * By considering the terms in the Fibonacci sequence whose values doe not
 * exceed four million, find the sum of the even-valued terms.
 *)

let rec fib =
  match n with
  | (0 | 1) -> 1
  | x when x > 0 -> (fib (x-2) + fib (x-1))
  | _ -> raise (Invalid_argument "Negative value supplied to fib")
