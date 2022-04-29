fun deutsch (uf : (qubit & qubit)<P> -> (qubit & qubit)<P>) =
    let input = (H (qinit ()), H (X (qinit ()))) in
    let (x, _) = split<P>(uf (entangle<P>(input))) in
    measure (H (x))

fun cnot (xy : (qubit & qubit)<P>) = CNOT (xy)

fun always_true (xy : (qubit & qubit)<P>)  =
    let (x : qubit<M>, y : qubit<M>) = split<M>(cast<M>(xy)) in
    cast<P>(entangle<M>(x, X (y)))

fun always_false (xy : (qubit & qubit)<P>) = xy

fun main () = ((deutsch (always_false), deutsch (always_true)), deutsch (cnot))
