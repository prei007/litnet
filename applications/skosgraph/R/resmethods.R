resmethods <<- list(
  quantitative = list(
    name = "Quantitative",
    items = list(
      survey = list(
        name = "Survey",
        items = list(
          s1 = list(name = "S1"),
          s2 = list(name = "S2"),
          'meth:Survey' = list(name = "Survey")
        )
      ),
      experiment = list(
        name = "Experiment",
        items = list(
          quasi = list(name = "Quasiexperiment"),
          scdr = list(name = "SCDR")
        )
      )
    )
  ),
  qualitative = list(
    name = "Qualitative",
    items = list(
      interview = list(
        name = "Interview",
        items = list(
          focusgroup = list(
            name = "Focusgroup"
          ))),
      observation = list(
        name = "Observation",
        items = list(
          field = list(name = "FieldObseration"),
          video = list(name = "VideoObservation")
        )
      )
    )
  )
)