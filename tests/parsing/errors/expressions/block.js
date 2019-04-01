let findThreadByIdLinearScan = (~threads, ~id) => {
  Js.Array2.findi(ThreadsModel.threads, (thread, i) => {
    let thisId = switch (thread) {
    | ServerData.OneToOne({otherPersonIDWhichIsAlsoThreadID}) =>
      otherPersonIDWhichIsAlsoThreadID
    | Group({id}) =>
      id
    | Unknown({id}) =>
      // TODO: this is stupidly dangerous
      unknown.id->Js.String.make->FBID.ofStringUnsafe
    }

    thisId === id
  }
}
