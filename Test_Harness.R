# Test harness for LetterInventory.R

library(testthat)
source("LetterInventory.R")


test_convert_letter_to_number <- function(){
    test_that("a", expect_equal(convert_letter_to_number("a"), 1))
    test_that("b", expect_equal(convert_letter_to_number("b"), 2))
    test_that("e", expect_equal(convert_letter_to_number("e"), 5))
    test_that("x", expect_equal(convert_letter_to_number("x"), 24))
    test_that("z", expect_equal(convert_letter_to_number("z"), 26))
}


test_calculate_inventory <- function(string){
    test_that("Counts in inventory equal length of string",
                                    expect_equal(sum(calculate_inventory(string)), nchar(string)))
    
}


test_convert_letter_to_number()
