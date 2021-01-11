
#' Read in a FastQ file
#'
#' @param file an uncompressed file (.fq extension) in fastq format
#'
#' @return A tibble with ID, Bases, Qualities and GC columns
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' read_fastq(system.file("good.fq", package = "fastqR"))
read_fastq <- function(file) {
  assertthat::assert_that(assertthat::is.readable(file))
  assertthat::assert_that(assertthat::has_extension(file,"fq"))

  scan(file, character()) -> file.lines
  file.lines[c(T,F,F,F)] -> ids
  file.lines[c(F,T,F,F)] -> sequences
  file.lines[c(F,F,F,T)] -> qualities

  if (!all(startsWith(ids,"@"))) {
    stop("Some ID lines didn't start with @")
  }

  stringr::str_sub(ids,2) -> ids

  if (!all(nchar(sequences)==nchar(qualities))) {
    stop("Some sequences were a different length to the qualities")
  }

  if (any(duplicated(ids))) {
    stop("Some IDs are duplicated")
  }

  tibble::tibble(ID = ids, Bases=sequences, Qualities=qualities, GC=gc_content(sequences)) %>%
    return()

}


#' Calculate DNA GC content
#'
#' @param seq a character vector of DNA sequences
#'
#' @return a vector of percentage GC values can be upper or lower case. Any non GATC characters will be treated as non-GC
#' @export
#'
#' @examples
#' gc_content(c("GGATCG","gtattagc"))
gc_content <- function(seq) {

  assertthat::assert_that(is.character(seq))

  if (any(stringr::str_detect(seq,"[^GATC]"))) {
    warning("Non GATC characters found in sequences")
  }

  seq <- toupper(seq)

  stringr::str_replace_all(seq,"[^GC]","") -> just_gc

  return(100*(nchar(just_gc)/nchar(seq)))

}
