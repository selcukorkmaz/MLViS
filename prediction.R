prediction <- function(model, newdat){
{if (model$method != "nb"){
  tmp = predict.train(model, newdata=newdat)
  pred = ifelse(tmp == 0, "Nondrug-like", "Drug-like")
}

else if (model$method == "nb"){
  if (nrow(newdat) == 1){
    tmp = predict.train(model, newdata=rbind(newdat, newdat))
    pred = ifelse(tmp[1] == 0, "Nondrug-like", "Drug-like")
  }
  if (nrow(newdat) != 1){
    tmp = predict.train(model, newdata=newdat)
    pred = ifelse(tmp == 0, "Nondrug-like", "Drug-like")
  } 
}}
return(pred)
}