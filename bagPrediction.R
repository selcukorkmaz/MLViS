bag.prediction <- function(bagmodel, newdat){
    bagPreds = NULL
    for (i in 1:length(bagmodel)){
        pred.i = predict.train(bagmodel[[i]], newdata=newdat)
        bagPreds = cbind(bagPreds, pred.i)
    }
	return(bagPreds)
}	