# This class keeps track of the count of each letter in a string


# Converts "a" to 1, "b" to 2, "c" to 3, and so on.
convert_letter_to_number <- function(letter){
  alphabet <- letters[1:26]
  letter_number <- match(letter, alphabet)
}


# 
calculate_inventory <- function(str){
  # Create vector of 26 0's. This is a vector of 26 counters (one per each letter)
  inventory <- rep(0, 26)
  # Split str into vector composed of individual letters
  str_characters <- strsplit(tolower(str), "")[[1]]
  for (character in str_characters){
    if (grepl('[A-Za-z]+', character)){  # Returns TRUE if character is in the alphabet
      # If character is 'a', letter_number is 1. If it's b', then letter_number is 2, etc.
      letter_number <- convert_letter_to_number(character)
      # Increment the counter for that letter
      inventory[letter_number] = 1 + inventory[letter_number]      
    }
  }  
  return(inventory)
}


LetterInventory <- setClass("LetterInventory", slots = list(inventory = "vector",
                                                            size = "numeric"))


setGeneric("get", function(object, letter) {
  standardGeneric("get")
})


# Returns the count of the letter
setMethod("get", signature(object = "LetterInventory"), 
          function(object, letter){
            if (nchar(letter) != 1){
              stop("Illegal argument exception: letter must be a single character")
            } else if(grepl('[A-Za-z]+', letter) == FALSE){
              stop("Illegal argument exception: letter must be a character in the alphabet")
            }else{
              return(object@inventory[convert_letter_to_number(letter)])
            }
          })


setGeneric("size", function(object) {
  standardGeneric("size")
})


# Returns the object's size (how many letters it holds)
setMethod("size", signature(object = "LetterInventory"), 
          function(object){
            return(object@size)
          })


setGeneric("isEmpty", function(object) {
  standardGeneric("isEmpty")
})


# Returns if the object contains no letters
setMethod("isEmpty", signature(object = "LetterInventory"), 
          function(object){
            return(object@size == 0)
          })


setGeneric("toString", function(object) {
  standardGeneric("toString")
})


# Converts the contents of the inventory into a readable format.
setMethod("toString", signature(object = "LetterInventory"), 
          function(object){
            string_representation = "["
            for (index in 1:length(object@inventory)){
              for (i in 1:object@inventory[index]){
                string_representation <- paste(string_representation, object@inventory[index])
              }
            }
            string_representation <- paste(string_representation, "]")
            return(string_representation)
          })


setGeneric("set", function(object, letter, value) {
  standardGeneric("set")
})


# Sets a letter in the object's inventory to a certain value
setMethod("set", signature(object = "LetterInventory"),
          function(object, letter, value){
            letter_number <- convert_letter_to_number(letter)
            object@inventory[letter_number] <- value
          })


setGeneric("add", function(object, other) {
  standardGeneric("add")
})


# Adds 2 LetterInventory objects together. Adds their sizes and counts.
setMethod("add", signature(object = "LetterInventory"),
          function(object, other){
            inventory <- object@inventory + other@inventory
            size = object@size + other@size
            new_LetterInventory <- LetterInventory(inventory = inventory, size = size)
            return(new_LetterInventory)
          })


setGeneric("subtract", function(object, other) {
  standardGeneric("subtract")
})


# Subtracts LetterInventory other from LetterInventory object. Subtracts the size of other from
# object. Subtracts each count of other from each count of object. If any value would be negative,
# then it is turned to 0. 
setMethod("subtract", signature(object = "LetterInventory"),
          function(object, other){
            size = object@size - other@size
            if (size < 0){
              size = 0
            }
            inventory <- object@inventory - other@inventory
            for (i in length(inventory)){
              if (inventory[i] < 0){
                inventory[i] = 0
              }
            }
            new_LetterInventory <- LetterInventory(inventory = inventory, size = size)
          })
