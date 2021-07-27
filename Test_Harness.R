# Test harness for LetterInventory.R

library(testthat)
source("LetterInventory.R")


test_find_number_of_letters <- function(){
    test_that("0", expect_equal(find_number_of_letters("\\\\    .,.33321,.,>?"), 0))
    test_that("1", expect_equal(find_number_of_letters(" b "), 1))
    test_that("5", expect_equal(find_number_of_letters("hello"), 5))
    test_that("10", expect_equal(find_number_of_letters("12abcdefghij12?/;;;"), 10))
    test_that("15", expect_equal(find_number_of_letters("k l m n o p q rstuvwxy"), 15))
}


test_convert_letter_to_number <- function(){
    test_that("a", expect_equal(convert_letter_to_number("a"), 1))
    test_that("b", expect_equal(convert_letter_to_number("b"), 2))
    test_that("e", expect_equal(convert_letter_to_number("e"), 5))
    test_that("x", expect_equal(convert_letter_to_number("x"), 24))
    test_that("z", expect_equal(convert_letter_to_number("z"), 26))
}


test_George_W_Bush_inventory <- function(string = "George W. Bush"){
    inventory <- calculate_inventory(string)
    test_that("Counts in inventory equal number of letters in string", expect_equal(sum(inventory),
                                                                    find_number_of_letters(string)))
    test_that("b", expect_equal(inventory[convert_letter_to_number("b")], 1))
    test_that("g", expect_equal(inventory[convert_letter_to_number("g")], 2))
    test_that("r", expect_equal(inventory[convert_letter_to_number("r")], 1))
    test_that("w", expect_equal(inventory[convert_letter_to_number("w")], 1))
}


test_Hillary_Clinton_inventory <- function(string = "Hillary Clinton"){
    inventory <- calculate_inventory(string)
    test_that("Counts in inventory equal number of letters in string", expect_equal(sum(inventory),
                                                                    find_number_of_letters(string)))
    test_that("h", expect_equal(inventory[convert_letter_to_number("h")], 1))
    test_that("l", expect_equal(inventory[convert_letter_to_number("l")], 3))
    test_that("a", expect_equal(inventory[convert_letter_to_number("a")], 1))
    test_that("i", expect_equal(inventory[convert_letter_to_number("i")], 2))
}


string1 <- "hello!"
inventory1 <- LetterInventory(string1)
test_inventory_constructor <- function(inventory, string){
    test_that("inventory has correct size", expect_equal(inventory@size,
                                                         find_number_of_letters(string)))
    test_that("h", expect_equal(get(inventory, "h"), 1))
    test_that("e", expect_equal(get(inventory, "e"), 1))
    test_that("l", expect_equal(get(inventory, "l"), 2))
    test_that("o", expect_equal(get(inventory, "o"), 1))
}


test_get <- function(){
    string1 <- "Barack Obama"
    inventory1 <- LetterInventory(string1)

    test_that("b", expect_equal(get(inventory1, "b"), 2))
    test_that("a", expect_equal(get(inventory1, "a"), 4))
    # get was already tested in test_inventory_constructor, so not many additinoal tests
    # are necessary
    
    # Testing if exceptions are thrown. Uncomment the following lines to test.
    # Input is multiple characters
    get(inventory1, "abcd")

    # Input is not a letter in the alphabet
    get(inventory, "$")
}


test_find_number_of_letters()
test_convert_letter_to_number()
test_George_W_Bush_inventory()
test_Hillary_Clinton_inventory()
test_inventory_constructor(inventory1, string1)
test_get()


cat('\014')  # Clear console
rm(list = ls())  # Clear variables