# 


calculate_inventory <- function(str){
  inventory <- c()
  return(inventory)
}


LetterInventory <- setClass("LetterInventory", slots = list(
                                                            inventory = calculate_inventory,
                                                            size = length(str)))


setGeneric("get", function(LetterInventory) {
  standardGeneric("get")
})


setMethod("get", signature(object = "LetterInventory"), 
          function(LetterInventory){
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
