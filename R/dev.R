# Private function, used only by findsplit()
dev=function(y){
  dev=sum((y-mean(y))^2)
  return(dev)
}