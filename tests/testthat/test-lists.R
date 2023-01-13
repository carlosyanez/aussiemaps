test_that("lists", {
  list_attributes()
  list_structure("2021")
  list_structure("2021", list("CED_NAME_2021"=c("Wills","Melbourne")))
})
