library( climex )
library( climexUI )
library( xts )
library( sp )

context( "Testing the conversion and checking of the input" )

## Generate some dummy data
data( temp.potsdam, package = "climex" )
data( data.frame.positions.fallback, package = "climexUI" )
data( list.data.sources.fallback, package = "climexUI" )

data.list.unnamed <- list( data = list( temp.potsdam ) )
data.list.named <- list( data = list( Potsdam = temp.potsdam ) )
data.unnamed.list <- list( list( Potsdam = temp.potsdam ) )
data.list.data.frame <-
  list( data = list( Potsdam = data.frame(
                       value = as.numeric( temp.potsdam ),
                       date = index( temp.potsdam ) ) ) )
data.list.data.frame.contaminated <-
  list( data = list( Potsdam = data.frame(
                       value = as.numeric( temp.potsdam ),
                       date = index( temp.potsdam ) ),
                    station2 = data.frame(
                       value = as.numeric( temp.potsdam ),
                      date = as.character( temp.potsdam ) ) ) )
data.list.numeric <-
  list( data = list( Potsdam = as.numeric( temp.potsdam ) ) )
data.positions.spatial <- data.frame.positions.fallback
sp::coordinates( data.positions.spatial ) <-
  c( "longitude", "latitude" )

test_that( "check.input converts spatial data", {
  expect_true(
    all( names( climexUI:::check.input(
                             data.list.named, data.positions.spatial,
                             silent = TRUE )$data.frame.positions ) %in%
         names( data.frame.positions.fallback ) ) )
})

test_that( "check.input converts the data", {
  expect_equal(
    climexUI:::check.input(
                 data.list.named, data.frame.positions.fallback
               )$list.data.sources, data.list.named )
  expect_equal(
    climexUI:::check.input( data.list.data.frame,
                           data.frame.positions.fallback,
                           silent = TRUE )$list.data.sources,
    data.list.named )
})

test_that( "check.input discards corrupted data.frames", {
  expect_equal(
    climexUI:::check.input( data.list.data.frame.contaminated,
                           data.frame.positions.fallback,
                           silent = TRUE )$list.data.sources,
    data.list.named )
})

test_that( "check.input detects flaws in the input data", {
  expect_true(
    !climexUI:::check.input( data.list.unnamed,
                            data.frame.positions.fallback,
                            silent = TRUE )$check.result )
  expect_true(
    climexUI:::check.input( data.list.named,
                           data.frame.positions.fallback,
                           silent = TRUE )$check.result )
  expect_true(
    !climexUI:::check.input( data.unnamed.list,
                            data.frame.positions.fallback,
                            silent = TRUE )$check.result )
  expect_true(
    climexUI:::check.input( data.list.data.frame,
                           data.frame.positions.fallback,
                           silent = TRUE )$check.result )
  expect_true(
    climexUI:::check.input( data.list.data.frame.contaminated,
                           data.frame.positions.fallback,
                           silent = TRUE )$check.result )
  expect_true(
    !climexUI:::check.input( data.list.numeric,
                            data.frame.positions.fallback,
                            silent = TRUE )$check.result )
  expect_true(
    !climexUI:::check.input( list( station = temp.potsdam ),
                            data.frame.positions.fallback,
                            silent = TRUE )$check.result )
})


