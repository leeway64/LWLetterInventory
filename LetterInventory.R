# 


calculate_inventory <- function(str){
  inventory <- rep(0, 26)  # Create vector of 26 0's
  return(inventory)
}


LetterInventory <- setClass("LetterInventory", slots = list(
                                                            inventory = calculate_inventory(data),
                                                            size = nchar(data)))


setGeneric("get", function(LetterInventory) {
  standardGeneric("get")
})


setMethod("get", signature(object = "LetterInventory"), 
          function(LetterInventory, letter){
            if (nchar(letter) != 1){
              stop("Illegal argument exception: letter must be a single character")
            } else if(grepl('[A-Za-z]+') == FALSE){
              stop("Illegal argument exception: letter must be a character in the alphabet")
            }else{
              return(LetterInventory$letter)
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
          })
