
let current_space = ref 0
let max_space = ref 0

let space_increment (delta: int): unit =
  current_space := !current_space + delta;
  max_space := max !max_space !current_space

let space_decrement (delta: int): unit =
  current_space := !current_space - delta;
  if (!current_space < 0) then failwith "deleted space that does not exist"