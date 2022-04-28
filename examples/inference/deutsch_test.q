fun deutsch (uf : (qubit & qubit)<P> -> (qubit & qubit)<P>) =
    let input = (H (qinit ()), H (X (qinit ()))) in
    let (x, _) = split<P>(uf (input)) in
    measure (H (x))


fun cnot (xy : (qubit & qubit)<P>) = CNOT (xy)

fun main () = CNOT (qinit (), qinit ())