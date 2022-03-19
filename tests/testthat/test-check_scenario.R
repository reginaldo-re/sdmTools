describe("Given the folder containing inner files and a inner folder in same hierarchy level", {
  it("When cheking the validity of that scenario folder, then I expect an error.", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir, overwrite = T)

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_folder"), overwrite = T)

        expect_error(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})

describe("Given the folder containing a mix of raster an vect files", {
  it("When cheking the validity of that scenario folder, then I expect an error.", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir, overwrite = T)

        system.file("vect_files", package="sdmTools") %>%
           fs::dir_copy(a_dir, overwrite = T)

        expect_error(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})

describe("Given the empty folder", {
  it("When cheking the validity of that scenario folder, then I expect an error.", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        expect_error(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})

describe("Given the raster files containing incosistent layers", {
  it("When cheking the validity of that scenario folder, then I expect an error.", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        a_dir %>%
          fs::path("inner_raster1") %>%
          fs::dir_create()

        system.file("rast_files", package="sdmTools") %>%
          fs::path("wc2.0_bio_5m_01.tif") %>%
          fs::file_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_raster3") %>% fs::path("inner_inner_raster1"), overwrite = T)


        expect_error(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})


describe("Given the correct hierarchy of raster files", {
  it("When cheking the validity of that scenario folder, then I expect a valid scenario (TRUE).", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

        system.file("rast_files", package="sdmTools") %>%
          fs::dir_copy(a_dir %>% fs::path("inner_raster3") %>% fs::path("inner_inner_raster1"), overwrite = T)


        expect_true(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})

describe("Given the correct hierarchy of vect files", {
  it("When cheking the validity of that scenario folder, then I expect a valid scenario (TRUE).", {
    a_dir <- tempdir()
    withr::with_tempdir(
      tmpdir = a_dir,
      clean = T,
      code = {
        a_dir <- a_dir %>% fs::path("scenarios_folder")
        if (a_dir %>% fs::is_dir()){
          a_dir %>%
            fs::dir_delete()
          a_dir %>%
            fs::dir_create()
        }

        a_dir %>%
          fs::path("inner_vect1") %>%
          fs::dir_create()
        system.file("vect_files", package="sdmTools") %>%
          fs::path("brasil_uf.gpkg") %>%
          fs::file_copy(a_dir %>% fs::path("inner_vect1"), overwrite = T)
        system.file("vect_files", package="sdmTools") %>%
          fs::path("brasil_uf.gpkg") %>%
          fs::file_copy(a_dir %>% fs::path("inner_vect1") %>% fs::path("brasil_uf2.gpkg"), overwrite = T)


        a_dir %>%
          fs::path("inner_vect2") %>%
          fs::dir_create()
        system.file("vect_files", package="sdmTools") %>%
          fs::path("brasil_uf.gpkg") %>%
          fs::file_copy(a_dir %>% fs::path("inner_vect2"), overwrite = T)


        a_dir %>%
          fs::path("inner_vect3") %>%
          fs::path("inner_inner_vect1") %>%
          fs::dir_create()
        system.file("vect_files", package="sdmTools") %>%
          fs::path("brasil_uf.gpkg") %>%
          fs::file_copy(a_dir %>% fs::path("inner_vect3") %>% fs::path("inner_inner_vect1"), overwrite = T)

        expect_true(
          a_dir %>%
            check_scenario()
        )
      }
    )
  })
})







