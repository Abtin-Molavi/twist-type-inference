fun deutsch (uf : (qubit & qubit)<P> -> (qubit & qubit)<P>) =
    let input = (H (qinit ()), H (X (qinit ()))) in
    let (x, _) = split<P>(uf (entangle<P>(input))) in
    input

fun main () = deutsch
