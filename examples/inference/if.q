fun f () =
  let x = qinit () in
  if measure (qinit ()) then x else X (x)

fun main ()  = f ()
