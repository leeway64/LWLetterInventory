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


LetterInventory <- setClass("LetterInventory", slots = list(inventory = calculate_inventory(data),
                                                            size = nchar(data)))


setGeneric("get", function(LetterInventory) {
  standardGeneric("get")
})


setMethod("get", signature(object = "LetterInventory"), 
          function(LetterInventory, letter){
            if (nchar(letter) != 1){
              stop("Illegal argument exception: letter must be a single character")
            } else if(grepl('[A-Za-z]+', letter) == FALSE){
              stop("Illegal argument exception: letter must be a character in the alphabet")
            }else{
              return(LetterInventory@inventory[convert_letter_to_number(letter)])
            }
          })


setGeneric("size", function(LetterInventory) {
  standardGeneric("size")
})


setMethod("size", signature(object = "LetterInventory"), 
          function(LetterInventory){
            return(LetterInventory@size)
          })


setGeneric("isEmpty", function(LetterInventory) {
  standardGeneric("isEmpty")
})


setMethod("isEmpty", signature(object = "LetterInventory"), 
          function(LetterInventory){
            return(size == 0)
          })


setGeneric("toString", function(LetterInventory) {
  standardGeneric("toString")
})


setMethod("toString", signature(object = "LetterInventory"), 
          function(LetterInventory){
            string_representation = "["
            for (index in 1:length(LetterInventory@inventory)){
              for (i in 1:LetterInventory@inventory[index]){
                string_representation <- paste(string_representation,
                                                                  LetterInventory@inventory[index])
              }
            }
            string_representation <- paste(string_representation, "]")
            return(string_representation)
          })


setGeneric("set", function(LetterInventory) {
  standardGeneric("set")
})


setGeneric("add", function(LetterInventory) {
  standardGeneric("add")
})


setGeneric("subtract", function(LetterInventory) {
  standardGeneric("subtract")
})