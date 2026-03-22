type t = { start : int; finish : int }

let make start finish = { start; finish }
let length s = s.finish - s.start
