dataset1=RS7_2F31_Now_2_[RS7_2F31_Now_2_$flag_no_3D==0,]
dataset2=RS7_2F31_Now_2_[RS7_2F31_Now_2_$flag_no_3D==1,]

x1=dataset1$`Login24%`
x2=dataset2$`Login24%`
n1=dataset1$M0_Raw
n2=dataset2$M0_Raw

z.prop(x1,x2,n1,n2)


x1=args[[1]];x2=args[[2]];n1=args[[3]];n2=args[[4]];
z.prop = function(x1,x2,n1,n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1+x2) / (n1+n2)
  denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
  z.prop.stat = numerator / denominator
  result=rep(0,length(z.prop.stat))
  for (i in 1:length(z.prop.stat)){
    if(is.nan(z.prop.stat[i])){
      next
    }
    if(z.prop.stat[i] > 0){
      result[i]=(1-pnorm(z.prop.stat[i],mean=0,sd=1))
    }
    else{
      result[i]=(pnorm(z.prop.stat[i],mean=0,sd=1))
    }
  }
  return(result)
};

z.prop(x1,x2,n1,n2)
