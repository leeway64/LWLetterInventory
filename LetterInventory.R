# This class keeps track of the count of each letter in a string

alphabet_size = 26

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


# Converts "a" to 1, "b" to 2, "c" to 3, and so on.
convert_letter_to_number <- function(letter){
  alphabet <- letters[1:alphabet_size]
  letter_number <- match(letter, alphabet)
}


# Finds the number of each letter in str and increments the appropriate counter. If there is an "a"
# in str, the first counter would be incremented by 1. If there is a "z" in str, the 26th counter
# would be incremented.
calculate_inventory <- function(str){
  # Create vector of 26 0's. This is a vector of 26 counters (one per each letter)
  inventory <- rep(0, alphabet_size)
  
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


# LetterInventory generator function
setClass("LetterInventory", slots = list(inventory = "vector", size = "numeric"))


# LetterInventory constructor
LetterInventory <- function(string_data){
  return(new("LetterInventory", inventory = calculate_inventory(tolower(string_data)), size =
               find_number_of_letters(string_data)))
}


setGeneric("get", function(object, letter) {
  standardGeneric("get")
})


# Returns the count of the letter
# If more than 1 character is passed into letter or if letter is not alphabetic, an illegal
# argument exception is thrown.
setMethod("get", signature(object = "LetterInventory"), 
          function(object, letter){
            letter = tolower(letter)
            if (nchar(letter) != 1){
              stop("Illegal argument exception: letter must be a single character")
            } else if(!grepl('[A-Za-z]+', letter)){
              stop("Illegal argument exception: letter must be a character in the alphabet")
            } else{
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


# Returns a string representation of the inventory, converting the contents of the inventory into
# a readable format.
setMethod("toString", signature(object = "LetterInventory"), 
          function(object){
            string_representation = "["
            for (index in 1:length(object@inventory)){
              letter_count = object@inventory[index]
              if (letter_count != 0){  # 1:0 creates a vector of 1, 0
                for (i in 1:letter_count){
                  letter <- letters[1:alphabet_size][index]
                  string_representation <- paste(string_representation, letter, sep = "")
                }
              }
            }
            string_representation <- paste(string_representation, "]", sep = "")
            return(string_representation)
          })


setGeneric("set", function(object, letter, value) {
  standardGeneric("set")
})


# Sets a letter in the object's inventory to a certain value
# letter must be a letter in the alphabet, an illegal argument exception will be thrown if not
# value must be positive, an illegal argument exception will be thrown if not
setMethod("set", signature(object = "LetterInventory"),
          function(object, letter, value){
            if (!grepl('[A-Za-z]+', letter))
            {
              stop("Illegal argument exception: letter must be a character in the alphabet")
            }
            else if (value < 0)
            {
              stop("Illegal argument exception: value must be positive")
            }
            else
            {
              letter <- tolower(letter)
              letter_number <- convert_letter_to_number(letter)
              # Modify the object in-place
              eval.parent(substitute(object@inventory[letter_number] <- value))
            }
          })


setGeneric("add", function(object, other) {
  standardGeneric("add")
})


# Adds 2 LetterInventory objects together. Adds their inventories and sizes. Returns a new
# LetterInventory with the sum of their inventories and sizes.
setMethod("add", signature(object = "LetterInventory"),
          function(object, other){
            inventory <- object@inventory + other@inventory
            size = object@size + other@size
            return(new("LetterInventory", inventory = inventory, size = size))
          })


setGeneric("subtract", function(object, other) {
  standardGeneric("subtract")
})


# Subtracts LetterInventory other from LetterInventory object. Subtracts the size of other from
# object. Subtracts each count of other from each count of object. If any value would be negative,
# then it is turned to 0.
# Returns a new letterInventory with the difference of the inventories and sizes.
setMethod("subtract", signature(object = "LetterInventory"),
          function(object, other){
            inventory <- object@inventory - other@inventory
            inventory <- replace(inventory, inventory < 0, 0)
            size = sum(inventory)
            return(new("LetterInventory", inventory = inventory, size = size))
          })
