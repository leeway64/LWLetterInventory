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


test_inventory_constructor <- function(){
    inventory0 <- LetterInventory("")
    test_that("Size of inventory0", expect_equal(size(inventory0), 0))
    test_that("isEmpty", expect_equal(isEmpty(inventory0), TRUE))
    
    string1 <- "Hello!"
    inventory1 <- LetterInventory(string1)
    test_that("inventory has correct size", expect_equal(inventory1@size,
                                                         find_number_of_letters(string1)))
    test_that("h", expect_equal(get(inventory1, "h"), 1))
    test_that("e", expect_equal(get(inventory1, "E"), 1))
    test_that("l", expect_equal(get(inventory1, "l"), 2))
    test_that("o", expect_equal(get(inventory1, "O"), 1))
    test_that("o", expect_equal(get(inventory1, "o"), 1))
    test_that("Size of inventory1", expect_equal(size(inventory1), 5))
    
}


test_get_size_and_isEmpty <- function(){
    string1 <- "Barack Obama"
    inventory1 <- LetterInventory(string1)
    test_that("isEmpty", expect_equal(isEmpty(inventory1), FALSE))
    
    test_that("b", expect_equal(get(inventory1, "B"), 2))
    test_that("a", expect_equal(get(inventory1, "a"), 4))
    test_that("size", expect_equal(size(inventory1), 11))

    # get was already tested in test_inventory_constructor, so not many additional tests
    # are necessary
    
    # Testing if exceptions are thrown. Uncomment the following lines to test.
    # Input is multiple characters
    # get(inventory1, "abcd")

    # Input is not a letter in the alphabet
    # get(inventory1, "$")
}


test_toString <- function(){
    string1 <- "Hillary Clinton"
    inventory1 <- LetterInventory(string1)
    test_that("Hillary Clinton", expect_equal(toString(inventory1), "[achiilllnnorty]"))
    string2 <- "George W. Bush"
    inventory2 <- LetterInventory(string2)
    test_that("George W. Bush", expect_equal(toString(inventory2), "[beegghorsuw]"))
}


test_set <- function(){
    string1 <- "Eddard Stark"
    inventory1 <- LetterInventory(string1)
    
    test_that("d", expect_equal(get(inventory1, "d"), 3))
    set(inventory1, "D", 5)
    test_that("Set 'd' to 5", expect_equal(get(inventory1, "d"), 5))
    
    test_that("a", expect_equal(get(inventory1, "a"), 2))
    set(inventory1, "a", 99)
    test_that("Set 'a' to 99", expect_equal(get(inventory1, "a"), 99))
    
    # Testing if exceptions are thrown. Uncomment the following lines to test.
    # Input is not a letter in the alphabet
    # set(inventory1, "%", 0)
    
    # value is not positive
    # set(inventory1, "z", -1)
}


# Test each of the inventory's member functions
test_inventory <- function(letter_inventory){
    
}


test_add <- function(){
    string0 = "George W. Bush"
    inventory0 <- LetterInventory(string0)
    string1 <- "Hillary Clinton"
    inventory1 <- LetterInventory(string1)
    
    inventory2 <- add(inventory0, inventory1)
    test_that("[abceegghhiilllnnoorrstuwy]", expect_equal(toString(inventory2),
                                                                "[abceegghhiilllnnoorrstuwy]"))
    test_that("size", expect_equal(size(inventory2), inventory0@size + inventory1@size))
    
}


test_subtract <- function(){
    string0 = "vwxyz"
    inventory0 <- LetterInventory(string0)
    string1 = "abcd"
    inventory1 <- LetterInventory(string1)
    
    inventoryA <- subtract(inventory0, inventory1)
    test_that("size", expect_equal(size(inventoryA), 5))
    
    string2 = "abcd"
    inventory2 <- LetterInventory(string2)
    string3 = "abc"
    inventory3 <- LetterInventory(string3)
    
    inventoryB <- subtract(inventory2, inventory3)
    test_that("size", expect_equal(size(inventoryB), 1))
    
}
source("LetterInventory.R")


test_find_number_of_letters()
test_convert_letter_to_number()
test_George_W_Bush_inventory()
test_Hillary_Clinton_inventory()
test_inventory_constructor()
test_get_size_and_isEmpty()
test_toString()
test_set()
test_add()
test_subtract()


cat('\014')  # Clear console
rm(list = ls())  # Clear variables
