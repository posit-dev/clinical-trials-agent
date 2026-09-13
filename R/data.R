# ADaM sample data from random.cdisc.data.

adam_data <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- list(
        adsl = random.cdisc.data::cadsl,
        adae = random.cdisc.data::cadae,
        adex = random.cdisc.data::cadex,
        adlb = random.cdisc.data::cadlb,
        adaette = random.cdisc.data::cadaette,
        adtte = random.cdisc.data::cadtte
      )
    }
    cache
  }
})
