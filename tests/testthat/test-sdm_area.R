describe("Given a invalid vect format file name", {
  it("When creating a SDM area, then I expect an error.", {
    expect_error("zzzzz" %>% sdm_area("Invalid Area", "EPSG:6933", 50000))
  })
})

describe("Given a SpatialLines object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SL %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })
})


describe("Given a SpatialPolygons object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SP %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })
})



describe("Given a SpatialLinesDataFrame object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SLDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(SLDF %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })

  it("When creating a SDM area changing the CRS, then I expect a new SDM area object.", {
    new_area <- SLDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:5880",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(SLDF %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })
})


describe("Given a SpatialPolygonsDataFrame object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SPDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(SPDF %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })

  it("When creating a SDM area selecting some variables, then I expect a new SDM area object with some selected variables.", {
    new_area <- SPDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        var_names = c("nome", "sigla", "geom"),
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(c("nome", "sigla", "geom")))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })

  it("When creating a SDM area selecting none variables, then I expect a new SDM area object without any variables.", {
    new_area <- SPDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        var_names = list(),
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_equal(new_area$study_area %>% names() %>% length(), 0)
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })


  it("When creating a SDM area changing the CRS, then I expect a new SDM area object.", {
    new_area <- SPDF %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:5880",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(SPDF %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })

  it("When creating a SDM area from a SpatialPolygonsDataFrame object without a CRS, then I expect that SpatialPolygonsDataFrame object has been converted to EPSG:4326.", {
    SPDF_tmp <- SPDF %>%
      spTransform("EPSG:4326")
    crs(SPDF_tmp) <- NA

    new_area <- SPDF_tmp %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:5880",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(SPDF %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })

  it("When creating a SDM area from a SpatialPolygonsDataFrame object with an invalid CRS, then I expect an error.", {
    SPDF_tmp <- SPDF
    crs(SPDF_tmp) <- NA

    expect_error(
      SPDF_tmp %>%
        sdm_area(
          sdm_area_name = "Test area",
          epsg_code = "EPSG:5880",
          resolution =  50000,
          dir_path = "/tmp/testing"
        )
    )
  })



  it("When creating a SDM area with a blank name, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = "",
          epsg_code = "EPSG:6933",
          resolution =  50000,
          dir_path = "/tmp/testing"
        )
    )
  })

  it("When creating a SDM area with a NULL name, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = NULL,
          epsg_code = "EPSG:6933",
          resolution =  50000,
          dir_path = "/tmp/testing"
        )
    )
  })

  it("When creating a SDM area with a invalid EPSG code, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = "Test area",
          epsg_code = "6933",
          resolution =  50000,
          dir_path = "/tmp/testing"
        )
    )
  })

  it("When creating a SDM area with a NULL EPSG code, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = "Test area",
          epsg_code = NULL,
          resolution =  50000,
          dir_path = "/tmp/testing"
        )
    )
  })

  it("When creating a SDM area with a invalid resolution, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = "Test area",
          epsg_code = "EPSG:6933",
          resolution =  0,
          dir_path = "/tmp/testing"
        )
    )
  })
  it("When creating a SDM area with a NULL dir_path, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(
          sdm_area_name = "Test area",
          epsg_code = "EPSG:6933",
          resolution =  50000
        )
    )
  })
})

describe("Given a gridded SpatialPolygonsDataFrame object", {
  it("When creating a SDM area with unequal resolution, then I expect an error.", {
   expect_error(a_sdm_area_gridded_area$study_area %>%
                  sdm_area(
                    sdm_area_name = "Test area",
                    epsg_code = "EPSG:5880",
                    resolution =  10000,
                    dir_path = "/tmp/testing"
                  )
   )
  })


  it("When get resolution, then I expect a NULL result.", {
    expect_null(
      SPDF %>%
        .get_resolution()
    )
  })

})

describe("Given a non gridded SpatialPolygonsDataFrame object", {
  it("When get resolution, then I expect a NULL result.", {
    expect_null(
      SPDF %>%
        .get_resolution()
    )
  })
})


describe("Given a valid filename", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    vect_file <- system.file("vect_files/brasil_uf.gpkg", package="sdmTools")

    new_area <- vect_file %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution =  50000,
        dir_path = "/tmp/testing"
      )

    vect_file <- vect_file %>%
      rgdal::readOGR(verbose = F)

    expect_equal(new_area$sdm_area_name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_true(new_area$study_area %>% names() %>% identical(vect_file %>% names()))
    expect_equal(new_area$dir_path, "/tmp/testing")
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/Test area.gpkg")
  })
})






