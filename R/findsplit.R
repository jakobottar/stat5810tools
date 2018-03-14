#' Find Split Function
#'
#' Desc
#' @keywords test
#' @export
#' @examples
#' findsplit()
findsplit=function(z){
  zsort=z[order(z[,1]),]
  n=nrow(z)
  w=data.frame(matrix(0,ncol=7,nrow=n-1))
  names(w)=c("Splitpoint","RSSleft","RSSright","RSStotal","MSEleft","MSEright","MSEtotal")
  for(j in 1:n-1){
    w[j,1] = (zsort[j,1] + zsort[j+1,1])/2
    w[j,2] = dev(zsort[1:j,2])
    w[j,5] = w[j,2]/j
    w[j,3] = dev(zsort[(j+1):n,2])
    w[j,6] = w[j,3]/(n-j)
    w[j,4] = w[j,2] + w[j,3]
    w[j,7] = w[j,4]/n
  }
  return(list(zsort,w))
}