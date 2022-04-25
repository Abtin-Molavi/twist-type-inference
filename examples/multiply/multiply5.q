type qubits_m = ((((qubit & qubit) & qubit) & qubit) & qubit)<M>
type qubits_p = ((((qubit & qubit) & qubit) & qubit) & qubit)<P>

type all = (qubit & ((((qubit & qubit) & qubit) & qubit) & qubit))<P>

type qp = qubit<P>
type qm = qubit<M>

fun mult (cqs : all) : all =
    let (c : qm, qs : qubits_m) = cqs in
    let ((((q1 : qm, q2 : qm), q3 : qm), q4 : qm), q5 : qm) = qs in
    let (c : qm, q1 : qm) = CNOT (c, q1) in
    let (c : qm, q2 : qm) = CNOT (c, q2) in
    let (c : qm, q3 : qm) = CNOT (c, q3) in
    let (c : qm, q4 : qm) = CNOT (c, q4) in
    let (c : qm, q5 : qm) = CNOT (c, q5) in
    let (c : qm, (q3 : qm, q4 : qm)) = FRED (c, (q3, q4)) in
    let (c : qm, (q2 : qm, q3 : qm)) = FRED (c, (q2, q3)) in
    let (c : qm, (q1 : qm, q2 : qm)) = FRED (c, (q1, q2)) in
    let (c : qm, (q1 : qm, q5 : qm)) = FRED (c, (q1, q5)) in
    let res : all = (c, ((((q1, q2), q3), q4), q5)) in
    res

fun invert (cqs : all) : all =
    let (c : qm, qs : qubits_m) = cqs in
    let ((((q1 : qm, q2 : qm), q3 : qm), q4 : qm), q5 : qm) = qs in
    let (c : qm, q1 : qm) = CNOT (c, q1) in
    let (c : qm, q2 : qm) = CNOT (c, q2) in
    let (c : qm, q3 : qm) = CNOT (c, q3) in
    let (c : qm, q4 : qm) = CNOT (c, q4) in
    let (c : qm, q5 : qm) = CNOT (c, q5) in
    let (c : qm, (q1 : qm, q5 : qm)) = FRED (c, (q1, q5)) in
    let (c : qm, (q1 : qm, q2 : qm)) = FRED (c, (q1, q2)) in
    let (c : qm, (q2 : qm, q3 : qm)) = FRED (c, (q2, q3)) in
    let (c : qm, (q3 : qm, q4 : qm)) = FRED (c, (q3, q4)) in
    let res : all = (c, ((((q1, q2), q3), q4), q5)) in
    res

fun z () : qp = qinit ()

fun o () : qp = H (qinit ())

fun main () : (qp * qubits_p) =
  let c = o () in
  let num : qubits_p = (((((o ()), z ()), z ()), o ()), o ()) in
  let (c : qp, rest : qubits_p) = invert (mult (entangle<P>(c, num))) in
  (c, rest)
