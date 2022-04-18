describe("Given the function to check a string", {
  it("When the string to be check is a valid string, then I expect the verification returns a true result.", {
    result <- .check(fun = "string", "A valid string", msg = "Some message.")
    expect_true(result)
  })

  it("When the string to be check is not a valid string, then I expect the verification returns an generate error message.", {
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
    expect_true(result)
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
  it("When use a 'or' to combine checks of a valid string and a invalid integer, then I expect a true result", {
    result <- nested_check(
      check_string("a"),
      check_int("1"),
      combine = "or"
    )
    expect_true(result)
  })

  it("When use a 'or' to combine checks of a invalid string and a invalid integer, then I expect a true result", {
    result <- nested_check(
      check_string(1),
      check_int("1"),
      combine = "or"
    )
    expect_true(result[[1]] == "Must be of type 'string', not 'double'")
    expect_true(result[[2]] == "Must be of type 'single integerish value', not 'character'")
  })

  it("When use a 'and' to combine checks of a valid string and a invalid integer, then I expect a generated error message.", {
    result <- nested_check(
      check_string("a"),
      check_int("1"),
      combine = "and"
    )
    expect_true(result == "Must be of type 'single integerish value', not 'character'")
  })

  it("When use a 'and' to combine checks of a valid string and a invalid integer, then I expect a specific message.", {
    result <- nested_check(
      check_string("a", msg = "Not showed message."),
      check_int("1", msg = "Some message."),
      combine = "and"
    )
    expect_true(result == "Some message.")
  })
  it("When use a 'and' to combine checks of a valid string and a invalid integer, then I expect a specific message of the nested_check call.", {
    result <- nested_check(
      check_string("a", msg = "Not showed message."),
      check_int("1", msg = "Some message."),
      combine = "and",
      msg = "Nested check message."
    )
    expect_true(result == "Nested check message.")
  })

  it("When use a 'and' to combine checks of a valid string and a invalid integer, then I expect a specific message of the nested_check call.", {
    result <- nested_check(
      check_string("a", msg = "Not showed message."),
      check_int("1", msg = "Some message."),
      combine = "and",
      msg = "Nested check message."
    )
    expect_true(result == "Nested check message.")
  })

  it("When use a 'and' to combine each nested check, then I expect some specifics messages of eache nested_check call.", {
    result <- nested_check(
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int("1", msg = "First nested message."),
        combine = "and"
      ),
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int("1", msg = "Second nested message."),
        combine = "and"
      )
    )
    expect_true(result[[1]] == "First nested message.")
    expect_true(result[[2]] == "Second nested message.")
  })

  it("When use a 'and' to combine all nested checks, then I expect some specifics messages of eache nested_check call.", {
    result <- nested_check(
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int(1, msg = "First nested message."),
        combine = "and"
      ),
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int("1", msg = "Second nested message."),
        combine = "and"
      ),
      combine = "and",
      msg ="Combined message."
    )
    expect_true(result == "Combined message.")
  })

  it("When use a 'or' to combine each nested check, then I expect a true result.", {
    result <- nested_check(
      nested_check(
        check_string(1, msg = "Not showed message."),
        check_int("a", msg = "First nested message."),
        combine = "or"
      ),
      nested_check(
        check_string(1, msg = "Not showed message."),
        check_int(1, msg = "Second nested message."),
        combine = "or"
      )
    )
    expect_true(result)
  })


  it("When use a 'or' and an 'and' to combine each nested check with at least one valid check, then I expect then I expect a true result.", {
    result <- nested_check(
      nested_check(
        check_string(1, msg = "Not showed message."),
        check_int("a", msg = "First nested message."),
        combine = "and"
      ),
      nested_check(
        check_string(1, msg = "Not showed message."),
        check_int(1, msg = "Second nested message."),
        combine = "or"
      )
    )
    expect_true(result)
  })

  it("When use a deep combination of 'or' and 'and' on a valid nested check, then I expect then I expect a true result.", {
    result <- nested_check(
      nested_check(
        check_string(1, msg = "Not showed message."),
        check_int("a", msg = "First nested message."),
        nested_check(
          check_string(1, msg = "Not showed message."),
          check_int("a", msg = "First nested message."),
        )
      ),
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int(1, msg = "Second nested message."),
        nested_check(
          check_string("a", msg = "Not showed message."),
          check_int(1, msg = "Second nested message."),
          combine = "and"
        ),
        combine = "and"
      ),
      combine = "or"
    )
    expect_true(result)
  })
})


describe("Given the function to make a assertion", {
  it("When the assertion is inside another assertion, then I expect an error.", {
    expect_error(
      assert(
        assert(
          check_string("a", msg = "Not showed message."),
        )
      ),
    "There exists at least one invalid call to assert function or assert_that function. You must use only nested_check calls."
    )
  })

  it("When use a 'or' to combine a nested check of a valid string and a invalid integer, then I expect a true result", {
    result <- assert(
      nested_check(
        check_string("a"),
        check_int("1"),
        combine = "or"
      )
    )

    expect_true(result)
  })

  it("When use a 'or' to combine a nested check of a valid string and a invalid integer and an invalid argument, then I expect a true result", {
    result <- assert(
      nested_check(
        check_string("a"),
        check_int("1"),
        combine = "or"
      ),
      an_invalid_arg = "aaa"
    )

    expect_true(result)
  })


  it("When use an 'and' to combine a nested check of a valid string and a invalid integer, then I expect an generic error message.", {
    expect_error(
      assert(
        nested_check(
          check_string("a"),
          check_int("1"),
          combine = "and"
        )
      ),
      "Must be of type 'single integerish value', not 'character'"
    )
  })

  it("When use a deep combination of 'or' and 'and' on a invalid nested check, then I expect then I expect a combined error message.", {
    expect_error(
      assert(
        nested_check(
          check_string("a", msg = "Not showed message."),
          check_int("a", msg = "First nested message."),
          nested_check(
            check_string("a", msg = "Not showed message."),
            check_int("a", msg = "Second nested message."),
          )
        ),
        nested_check(
          check_string("a", msg = "Not showed message."),
          check_int(1, msg = "Third nested message."),
          nested_check(
            check_string("a", msg = "Not showed message."),
            check_int("1", msg = "Fourth nested message."),
            combine = "and"
          ),
          combine = "and"
        ),
        combine = "and",
        msg = "Assertion error."
      ),
      "Assertion error.\nFourth nested message."
    )
  })

  it("When use a deep combination of 'or' and 'and' on a valid nested check, then I expect then I expect a combined error message.", {
    result <- assert(
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int("a", msg = "First nested message."),
        nested_check(
          check_string("a", msg = "Not showed message."),
          check_int("a", msg = "Second nested message."),
        )
      ),
      nested_check(
        check_string("a", msg = "Not showed message."),
        check_int(1, msg = "Third nested message."),
        nested_check(
          check_string("a", msg = "Not showed message."),
          check_int("1", msg = "Fourth nested message."),
          combine = "and"
        ),
        combine = "and"
      ),
      combine = "or",
      msg = "Assertion error."
    )
    expect_true(result)
  })
})
