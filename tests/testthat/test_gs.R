library("photobiology")

context("stomatal_conductance")

test_that("molar gs for water from pore size is correct", {

  expect_equal(signif(gs_w_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1, temperature = 21), 7), 6.029968e-10)
  expect_equal(signif(gs_w_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1e8, temperature = 21), 7), 6.029968e-2)
  expect_message(gs <- signif(gs_w_from_size(length = 30, width = 20, depth = 10,
                                             num = 1, temperature = 21), 7))
  expect_equal(gs, 6.029968e-10)
  expect_message(gs <- signif(gs_w_from_size(length = 30, width = 20, depth = 10,
                                             num = 1e8, temperature = 21), 7))
  expect_equal(gs, 6.029968e-2)
  expect
})

test_that("molar gs for CO2 from pore size is correct", {

  expect_equal(signif(gs_c_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1, temperature = 21), 7), 3.662174e-10)
  expect_equal(signif(gs_c_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1e8, temperature = 21), 7), 3.662174e-2)
  expect_message(gs <- signif(gs_c_from_size(length = 30, width = 20, depth = 10,
                                             num = 1, temperature = 21), 7))
  expect_equal(gs, 3.662174e-10)
  expect_message(gs <- signif(gs_c_from_size(length = 30, width = 20, depth = 10,
                                             num = 1e8, temperature = 21), 7))
  expect_equal(gs, 3.662174e-2)
  expect
})

test_that("gs from pore size and D is correct", {

  expect_equal(gs_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1, D = D_water(21)),
               gs_w_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                                     num = 1, temperature = 21))
  expect_equal(gs_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                            num = 1, D = D_CO2(21)),
               gs_c_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                              num = 1, temperature = 21))
})

test_that("1 / rs == gs", {

  expect_equal(1 / rs_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                            num = 1, D = D_water(21)),
               gs_w_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                              num = 1, temperature = 21))
  expect_equal(1 / rs_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                            num = 1, D = D_CO2(21)),
               gs_c_from_size(length = 30e-6, width = 20e-6, depth = 10e-6,
                              num = 1, temperature = 21))
})

test_that("water diffusivity is correct", {

  expect_equal(D_water(c(5, 20, 40)),
               c(2.20e-05, 2.42e-05, 2.72e-05))
  expect_equal(D_CO2(c(5, 20, 40)),
               c(1.33e-05, 1.47e-05, 1.65e-05))
  expect_true(is.na(D_water(-7.1)))
  expect_true(is.na(D_CO2(-7.1)))
  expect_true(is.na(D_water(47.1)))
  expect_true(is.na(D_CO2(47.1)))

})

