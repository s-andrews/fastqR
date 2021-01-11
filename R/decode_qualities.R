decode_qualities <- function(qualities, offset=33) {

  assert_that(is.scalar(offset))
  assert_that(is.number(offset))

  if (!(offset==33 | offset==64)) {
    stop("Offset can only be 33 or 64")
  }

  as.integer(charToRaw(qualities)) - offset -> phred_scores

  if (any(phred_scores < 1)) {
    stop("Negative phred scores produced - check offset")
  }

  return(phred_scores)
}
