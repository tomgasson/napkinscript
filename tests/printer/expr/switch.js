switch x {
| A => ()
| B => ()
}

switch /a, b/ {
| /Some(a), Some(b)/ => 42
| _ => 3
}
