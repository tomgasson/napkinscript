let name = if true {
  user.name
}

let name = if true {
  user.name
} else {
  "steve"
}

let name = if true {
  user.name
} else if false {
  user.lastName 
} else {
  defaultName
}
