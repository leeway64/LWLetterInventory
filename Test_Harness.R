# Test harness for LetterInventory.R

library(testthat)
source("LetterInventory.R")


# Finds the number of letters in a string
find_number_of_letters <- function(str){
    count = 0
    # Split str into vector composed of individual letters
    str_characters <- strsplit(tolower(str), "")[[1]]
    for (character in str_characters){
        if (grepl('[A-Za-z]+', character)){  # Increment count if character is in the alphabet
            count = count + 1
        }
    }
    return(count)
}


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



test_find_number_of_letters()
test_convert_letter_to_number()
test_George_W_Bush_inventory()
