guard let Some(x) = foo() else {
    "return"
}
doSomethingWithX(x)


guard let Some(x) = foo() else {
    raise(FailedToUnwrap)
}

guard let Some(x) = foo() when x > 10 else {
    raise(FailedToUnwrap)
}
doSomethingWithX(x)
