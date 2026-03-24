let counter = ref 0

let gen () =
  incr counter;
  !counter
