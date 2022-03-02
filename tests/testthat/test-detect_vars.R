describe("Given the variables is into a list", {
  it("When detecting none variable names, then I expect an error.", {
    expect_error(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list()
        )
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
    checkmate::expect_subset(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5M_02")
      ) %>% unlist(),
      c("5m_01", "5M_02"),
      empty.ok = F
    )
  })

  it("When detecting variable names, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5m_02")
      ) %>% unlist(),
      c("5m_01", "5m_02"),
      empty.ok = F
    )
  })

  it("When detecting variable names into a vector instead of into a list, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        c("5m_01", "5m_02")
      ) %>% unlist(),
      c("5m_01", "5m_02"),
      empty.ok = F
    )
  })

  it("When detecting case insensitive and invalid variable names, then I expect that invalid variables are not selected.", {
    checkmate::expect_subset(
      .detect_vars(
        list("bio_5m_01","bio_5m_02"),
        list("5m_01", "5M_02", "invalid_name", "another invalid variable")
      ) %>% unlist(),
      c("5m_01", "5M_02"),
      empty.ok = F
    )
  })

  it("When detecting variable names with special characters, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      .detect_vars(
        list("bio_5m_ 01","bio_5m_ 02"),
        list("5m_ 01", "5M_ 02")
      ) %>% unlist(),
      c("5m_ 01", "5M_ 02"),
      empty.ok = F
    )
  })

  it("When detecting only invalid variable names, then I expect that none variable are matched.", {
    checkmate::expect_list(
      .detect_vars(
        list("bio_5m_ 01","bio_5m_ 02"),
        list("invalid1", "invalid2")
      ),
      len = 0
    )
  })
})

describe("Given the variables is into a Spatial object", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      detect_vars(
        SPDF,
        list("sigla", "geocodigo")
      )  %>% unlist(),
      c("sigla", "geocodigo"),
      empty.ok = F
    )
  })
})

describe("Given the variables is into a SDM area", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      detect_vars(
        a_sdm_area,
        list("sigla", "geocodigo")
      )  %>% unlist(),
      c("sigla", "geocodigo"),
      empty.ok = F
    )
  })
})

describe("Given the variables is into a directory containing raster files", {
  it("When detecting variable names, then I expect that all variables are matched.", {
    checkmate::expect_subset(
      detect_vars(
        system.file("rast_files", package="sdmTools"),
        list("bio_5m_01","bio_5m_02")
      )  %>% unlist(),
      c("bio_5m_01","bio_5m_02"),
      empty.ok = F
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
    checkmate::expect_subset(
      detect_vars(
        system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools"),
        list("length", "dist_dn")
      ) %>% unlist(),
      c("length", "dist_dn"),
      empty.ok = F
    )
  })
})
