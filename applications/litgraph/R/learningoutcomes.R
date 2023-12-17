
CodeLabels  <- list(
  LearningOutcomes = list(
    name = "LearningOutomes", 
    items = list(
      "lo:LO1" = list(name = "LO1"),
      "lo:LO2" = list(name = "LO2")
    )
  ),
  Pedagogies = list(
    name = "Pedagogies",
    items = list(
      "envped:Ped1" = list(name = "Ped1"),
      "envped:Ped2" = list(name = "Ped2")
    )
  )
)


list1 = list("A", "B", "C")

list2 = list(first = "A", second = "B", third = "C")

knowledge_outcomes = list("lo:SystemsThinking" = "SystemsThinking", 
                          "lo:ComplexityThinking" = "ComplexityThinking")

list3 = list(first = "A", second = knowledge_outcomes)





learning_outcomes <- list(
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