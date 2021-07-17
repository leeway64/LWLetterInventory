# 


convert_letter_to_number <- function(letter){
  alphabet <- letters[1:26]
  letter_number <- match(letter, alphabet)
}


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


setMethod("size", signature(object = "LetterInventory"), 
          function(object){
            return(object@size)
          })


setGeneric("isEmpty", function(object) {
  standardGeneric("isEmpty")
})


setMethod("isEmpty", signature(object = "LetterInventory"), 
          function(object){
            return(object@size == 0)
          })


setGeneric("toString", function(object) {
  standardGeneric("toString")
})


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


setMethod("set", signature(object = "LetterInventory"),
          function(object, letter, value){
            letter_number <- convert_letter_to_number(letter)
            object@inventory[letter_number] <- value
          })


setGeneric("add", function(object, other) {
  standardGeneric("add")
})


setMethod("add", signature(object = "LetterInventory"),
          function(object, other){
            inventory <- object@inventory
            
          })


setGeneric("subtract", function(object, other) {
  standardGeneric("subtract")
})


setMethod("subtract", signature(object = "LetterInventory"),
          function(object, other){
            
          })
