describe("Given a invalid vect format file name", {
  it("When creating a SDM area, then I expect an error.", {
    expect_error("zzzzz" %>% sdm_area("Invalid Area", "EPSG:6933", 50000))
  })
})

describe("Given a SpatialLines object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SL %>%
      sdm_area("Test area", "EPSG:6933", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })
})


describe("Given a SpatialPolygons object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SP %>%
      sdm_area("Test area", "EPSG:6933", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })
})



describe("Given a SpatialLinesDataFrame object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SLDF %>%
      sdm_area("Test area", "EPSG:6933", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })
  it("When creating a SDM area changing the CRS, then I expect a new SDM area object.", {
    new_area <- SLDF %>%
      sdm_area("Test area", "EPSG:5880", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })
})


describe("Given a SpatialPolygonsDataFrame object", {
  it("When creating a SDM area, then I expect a new SDM area object.", {
    new_area <- SPDF %>%
      sdm_area("Test area", "EPSG:6933", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })


  it("When creating a SDM area changing the CRS, then I expect a new SDM area object.", {
    new_area <- SPDF %>%
      sdm_area("Test area", "EPSG:5880", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })

  it("When creating a SDM area from a SpatialPolygonsDataFrame object without a CRS, then I expect that SpatialPolygonsDataFrame object has been converted to EPSG:4326.", {
    SPDF_tmp <- SPDF %>%
      spTransform("EPSG:4326")
    crs(SPDF_tmp) <- NA

    new_area <- SPDF_tmp %>%
      sdm_area("Test area", "EPSG:5880", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:5880")
    expect_equal(new_area$crs, raster::crs("EPSG:5880"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })

  it("When creating a SDM area from a SpatialPolygonsDataFrame object with an invalid CRS, then I expect an error.", {
    SPDF_tmp <- SPDF
    crs(SPDF_tmp) <- NA

    expect_error(
      SPDF_tmp %>%
        sdm_area("Test area", "EPSG:5880", 50000, "/tmp/testing")
    )
  })



  it("When creating a SDM area with a blank name, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = "", "EPSG:6933", 50000, "/tmp/testing")
    )
  })

  it("When creating a SDM area with a NULL name, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = NULL, "EPSG:6933", 50000, "/tmp/testing")
    )
  })

  it("When creating a SDM area with a invalid EPSG code, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = "Test area", "6933", 50000, "/tmp/testing")
    )
  })

  it("When creating a SDM area with a NULL EPSG code, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = "Test area", epsg_code = NULL, 50000, "/tmp/testing")
    )
  })

  it("When creating a SDM area with a invalid resolution, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = "Test area", epsg_code = "EPSG:6933", 0, "/tmp/testing")
    )
  })
  it("When creating a SDM area with a NULL dir_path, then I expect an error.", {
    expect_error(
      SPDF %>%
        sdm_area(name = "Test area", epsg_code = "EPSG:6933", 50000)
    )
  })
})

describe("Given a gridded SpatialPolygonsDataFrame object", {
  it("When creating a SDM area with unequal resolution, then I expect an error.", {
   expect_error(a_sdm_area_gridded_area$study_area %>%
      sdm_area("Test area", "EPSG:5880", 10000, "/tmp/testing"))
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
    new_area <- system.file("vect_files/brasil_uf.gpkg", package="sdmTools") %>%
      sdm_area("Test area", "EPSG:6933", 50000, "/tmp/testing")

    expect_equal(new_area$name, "Test area")
    expect_equal(new_area$epsg, "EPSG:6933")
    expect_equal(new_area$crs, raster::crs("EPSG:6933"))
    expect_equal(new_area$resolution, 50000)
    expect_false(new_area$gridded)
    expect_s3_class(new_area, "SDM_area")
    checkmate::expect_file_exists("/tmp/testing/test_area.gpkg")
  })
})






