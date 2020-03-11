// Basic
if let Some(x) = foo() {
    doSomethingWithX(x)
}

// Else branch
if let Some(x) = foo(){
    doSomethingWithX(x)
} else {
    doSomethingElse()
}

// Else-if support
if let Some(x) = foo(){
    doSomethingWithX(x)
} else if let Some(y) = bar(){
    doSomethingWithY(y)
} else {
    doSomethingElse()
}

// Mixed conditions, pattern start
if let Some(x) = foo(){
    doSomethingWithX(x)
} else if n > 10 {
    doSomethingWithForN()
} else {
    doSomethingElse()
}

// Mixed conditions, condition start
if n > 10 {
    doSomethingWithForN()
} else if let Some(x) = foo() {
    doSomethingWithY(x)
} else {
    doSomethingElse()
}

// Maintains attrs correctly
if n > 10 {
    @aa
    doSomethingWithForN()
} else if let @bb Some(@cc x) = @dd foo() {
    @ee
    doSomethingWithY(x)
} else {
    @ff
    doSomethingElse()
}

// pattern guards are allowed
if let Some(x) = foo() when x > 10 {
    doSomethingWithX(x)
}

if let Some(x) = foo() when x > 10 {
    doSomethingWithX(x)
} else if let Some(y) = bar() when y > 10 {
    doSomethingWithY(y)
} else if let Some(z) = baz() when z > 10 {
    doSomethingWithZ(z)
}


// full destructioning
if let Some(Thing(With({
  many: Internal([Components, q]),
}))) as p = foo() when isValid(q) {
  doSomethingWithE(e)
}


let a = if let Some(x) = foo() when x > 10 {
    x
} else {
    123
}
