// if let Some(x) = foo() {
//     doSomethingWith(x)
// }

// if let Some(x) = foo() {
//     doSomethingWith(x)
// } else {
//     defaultBehavior()
// }

// if cond() {
//     doSomething()
// } else if let Some(x) = foo() {
//     doSomethingWith(x)
// } else {
//     defaultBehavior()
// }

// if let Some(x) = foo() {
//     doSomethingWith(x)
// } else if cond() {
//     doSomething()
// } else if other_cond() {
//     doSomethingElse()
// }

// if let Some(x) = foo() when x > 1 {
//     doSomethingWith(x)
// }

// if let Some(x) = foo() when x > 1 {
//     doSomethingWith(x)
// } else if let Some(y) = bar() when y > 1 {
//     doSomethingWith(y)
// }
// if let Some(x) = foo() when x > 1 {
//     doSomethingWith(x)
// } else if let Some(y) = bar() when y > 1 {
//     doSomethingWith(y)
// } else {
//     doSomethingElse()
// }

// switch foo() {
//     | Some(x) when x > 1 => doSomethingWith(x)
//     | _ => switch bar() {
//         | Some(y) when y > 1 => doSomethingWith(y)
//         | _ => doSomethingElse()
//     }
// }
