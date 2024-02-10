
test_that(
  desc = "Test ncRead",
  code = {

    testFolder <- test_path("testdata")

    cat(testFolder)

    ids = c(296, 263, 264, 265, 295, 297, 327, 328, 329)

    result <- ncRead(testFolder, ids)
    result <- rbindlist(l = result)

    expect_true(!is.null(result))

    expect_true(as.POSIXct(result$time[1], tz="GMT") == as.POSIXct("1951-01-01 00:30:00", tz="GMT"))

    expect_true(sprintf("%.35f", result$V1[1]) == "0.00007953369640745222568511962890625")
  }
)

test_that(
  desc = "Test calculateIDF",
  code = {

    testFolder <- test_path("testdata")
    ids = c(296, 263, 264, 265, 295, 297, 327, 328, 329)

    result <- ncRead(testFolder, ids)
    result <- rbindlist(l = result)

    result_m <- melt(data = result,
                      id.vars = "time",
                      variable.name = "cell_id")

    result_m$time <- as.POSIXct(result_m$time)

    result_spl <- split(x = result_m,
                     f = result_m$cell_id)

    result_idf <- lapply(X = result_spl,
                      FUN = calculateIdf)


    result_idf <- rbindlist(l = result_idf, idcol = "cell_id")


    expect_true(!is.null(result_idf))

    expect_true(result_idf$dur[1] == 1)
    expect_true(result_idf$dur[2] == 2)
    expect_true(result_idf$dur[3] == 5)

    expect_true(result_idf$rp[1] == 2)
    expect_true(result_idf$rp[2] == 2)
    expect_true(result_idf$rp[3] == 2)

    expect_true(sprintf("%.50f", result_idf$value[1]) == "0.00145529097671182050532934226794168353080749511719")
    expect_true(sprintf("%.50f", result_idf$value[2]) == "0.00142120661376177181978164298925548791885375976562")
    expect_true(sprintf("%.50f", result_idf$value[3]) == "0.00121959833974527587230340941459871828556060791016")
  }
)

