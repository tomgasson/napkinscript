type user = {"age": int}
type user = {"age": int, "name": string}
type user = {"age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks}


type magic = {..}
type t = {.. "age": int}
type magicallyLong = {.."age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks}

external test: (foo, bar, baz) => {.."age": int, "name": string, "moreFields": veryVeryVeryLongtype, "superLongNameThat": whyDoesATypeWithThisAmountOfCharactersMakeItIntoThisFile, "because": testingLineBreaks} = "primitive"
