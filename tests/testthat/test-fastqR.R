library(fastqR)
library(testthat)

test_that("Reading FastQ files", {

  expect_output(
    str(
      suppressWarnings(
        read_fastq(system.file("good.fq", package = "fastqR"))
      )),
    "tibble"
  )
  expect_warning(read_fastq(system.file("good.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("broken_format.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("duplicate_ids.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("mismatched_lengths.fq", package = "fastqR")))
  expect_error(read_fastq(system.file("wrong_extension.fa", package = "fastqR")))
})


test_that("Check GC content", {
  expect_equal(gc_content("GGATCTTAGG"), 50)
  expect_warning(gc_content("GGATCNTTAGG"))
})
