module type Signature = {
  module type Belt

  @onModTypeDecl
  module type Belt

  module type Belt = {
    module type Array
    module type List
  }

  @onModTypeDecl
  module type Belt = {
    module type Array
    module type List
  }
}
