# The checks and drawing are tested in test-common-annotations.R.

expmon <- load_dashboard("experience-monitoring")

test_that("annotations.yml has no problems", {
  expect_no_annotation_problems(
    file.path(repo_root, "dashboards", "experience-monitoring",
              "annotations.yml"),
    "funnel", expmon$required_funnels
  )
})
