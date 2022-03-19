describe("Given the variables is into a list", {
  it("When detecting none variable names, then I expect an empty list.", {
    expect_list(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list()
        ),
      len = 0
    )
  })

  it("When detecting all variable names using NULL option, then I expect that all variables are matched.", {
    expect_true(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        NULL
      ) %>%
        identical(list("bio_5m_01","bio_5m_02"))

    )
  })


  it("When study area variable names is empty, then I expect an error.", {
    expect_error(
      .detect_vars(
        list(),
        list("bio_5m_01","bio_5m_02")
      )
    )
  })

  it("When detecting ambiguous variable names, then I expect an error.", {
    expect_error(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("bio_5m", "bio_5")
      )
    )
  })

  it("When detecting case insensitive variable names, then I expect that all variables are matched.", {
    expect_true(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5M_02")
      ) %>%
      identical(c("5m_01", "5M_02"))
    )
  })

  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5m_02")
      ) %>%
      identical(c("5m_01", "5m_02"))
    )
  })

  it("When detecting variable names into a vector instead of into a list, then I expect that all variables are matched.", {
    expect_true(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        c("5m_01", "5m_02")
      ) %>%
        identical(c("5m_01", "5m_02"))
    )
  })

  it("When detecting case insensitive and invalid variable names, then I expect that invalid variables are not selected.", {
    expect_true(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5M_02", "invalid_name", "another invalid variable")
      ) %>%
        identical(c("5m_01", "5M_02"))
    )
  })

  it("When detecting variable names with special characters, then I expect that all variables are matched.", {
    expect_true(
      .detect_vars(
        list("bio_5m_ 01","bio_5m_ 02"),
        list("5m_ 01", "5M_ 02")
      ) %>%
        identical(c("5m_ 01", "5M_ 02"))
    )
  })

  it("When detecting only invalid variable names, then I expect that none variable are matched.", {
    expect_list(
      .detect_vars(
        list("bio_5m_ 01","bio_5m_ 02"),
        list("invalid1", "invalid2")
      ),
      len = 0
    )
  })
})

describe("Given the variables is into a Spatial*DataFrame object", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        SPDF,
        list("sigla", "geocodigo")
      ) %>%
        identical(c("sigla", "geocodigo"))
    )
  })
})

describe("Given the variables is into a Spatial object", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        SP,
        list("sigla", "geocodigo")
      ) %>%
        identical(list())
    )
  })

  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        SL,
        list("sigla", "geocodigo")
      ) %>%
        identical(list())
    )
  })
})

describe("Given the variables is into a SDM area", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        a_sdm_area,
        list("sigla", "geocodigo")
      )  %>%
        identical(c("sigla", "geocodigo"))
    )
  })
})

describe("Given the variables is into a directory containing raster files", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        system.file("rast_files", package="sdmTools"),
        list("bio_5m_01","bio_5m_02")
      )  %>%
        identical(c("bio_5m_01","bio_5m_02"))
    )
  })
})


describe("Given only one raster filename", {
  it("When detecting only one variable name, then I expect that it is matched.", {
    expect_true(
      detect_vars(
        system.file("rast_files/wc2.0_bio_5m_01.tif", package="sdmTools"),
        list("bio_5m_01")
      )  %>%
        identical(c("bio_5m_01"))
    )
  })
})


describe("Given the variables is into a directory containing vect files", {
  it("When detecting variable names inside a directory instead of a vect file, then I expect an error.", {
    expect_error(
      detect_vars(
        system.file("vect_files", package="sdmTools"),
        list("sigla", "geocodigo")
      )
    )
  })

  it("When detecting variable names inside a vect file, I expect that all variables are matched.", {
    expect_true(
      detect_vars(
        system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools"),
        list("length", "dist_dn")
      ) %>%
        identical(c("length", "dist_dn"))
    )
  })
})


