library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import NCA config 1", {
  
  # # Import the 1-cpt PK model from JSON
  # model <- loadFromJSON(CampsisModel(), file.path(testFolder, "json_examples", "1cpt_fo_model.json"))
  # 
  # expect_equal(expectedModel, model)
})
