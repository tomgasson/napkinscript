user.name = "steve"
config.shouldSkip = !filePath.includes(allMlSuffixesCategory) &&
  !filePath.endsWith(allScriptDirectoriesCategory)

let () =
  @attr user.name = "steve"

let () =
  @attr user.name = "steve" |> @attr user.name = "steve"
