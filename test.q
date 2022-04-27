fun bell_pair () =
    CNOT (H (qinit ()), qinit ())

fun teleport (q1 : qubit<P>) =
    let (q2, q3) = split<M>(cast<M>(bell_pair ())) in
    let (q1, q2) = split<M>(CNOT (cast<M>(q1), q2)) in
    let q1 = H (q1) in
    let (q2, q3) = split<M>(CNOT (q2, q3)) in
    let (q1, q3) = split<M>(CZ (q1, q3)) in
    let all  = cast<P>(((q1, q2), q3)) in
    let (_, q3) = split<P>(all) in
    q3

fun main () = teleport (H (qinit ()))
