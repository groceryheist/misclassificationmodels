test_that("Defensive; fail fast", {
    expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, proxy_family = gaussian()))
    expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, truth_family = gaussian()))
    expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data, truth_family = gaussian(), proxy_family = gaussian()))
})

test_that("Base case IV Gaussian", {
    expect_error(glm_fixit(y ~ x || w + z, data = research_data, data2 = val_data), NA)
})

test_that("Base case IV Binomial", {
    val_data3 <- val_data
    research_data3 <- research_data
    research_data3$y <- research_data$y >= median(research_data$y)
    val_data3$y <- val_data$y >= median(val_data$y)
    expect_error(glm_fixit(y ~ x || w + z, data = research_data3, data2 = val_data3, family = binomial()), NA)
})
