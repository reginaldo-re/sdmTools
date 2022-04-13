describe("Given the function to check a string", {
  it("When the string to be check is a valid string, then I expect the verification returns a true result.", {
    result <- .check(fun = "string", "A valid string", msg = "Some message.")
    expect_true(result)
  })

  it("When the string to be check is not a valid string, then I expect the verification returns a specific error message.", {
    result <- .check(fun = "string", 1)
    expect_true(result == "Must be of type 'string', not 'double'")
  })

})

describe("Given the function to check a string and a message error", {
  it("When the string to be check is a valid string, then I expect the verification returns a true result.", {
    result <- .check(fun = "string", "A valid string", msg = "Some message.")
    expect_true(result)
  })

  it("When the string to be check is not a valid string, then I expect the verification returns a specific error message.", {
    result <- .check(fun = "string", 1, msg = "Some message.")
    expect_true(result == "Some message.")
  })
})


describe("Given the function to abort", {
  it("When the string to be check is a valid string, then I expect the verification returns a true result.", {
    result <- .result_or_abort(fun = "string", "A valid string", msg = "Some message.")
  })

  it("When the string to be check is not a valid string, then I expect an error.", {
    expect_error(
      .result_or_abort(fun = "string", 1),
      "Must be of type 'string', not 'double'"
    )
  })
})

describe("Given the function to abort and a message error", {
  it("When the string to be check is not a valid string, then I expect an error with a specific message.", {
    expect_error(
      .result_or_abort(fun = "string", 1, msg = "Some message."),
      "Some message."
    )
  })
})

describe("Given the function to nested checks", {
  it("When the string to be check is not a valid string, then I expect an error with a specific message.", {
    result <- nested_check(

    )

  })
})
