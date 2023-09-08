tier1_blanket = function(n){
  if(n>=1&n<=10){return(250000)}
  if(n>=11&n<=100){return(500000)}
  if(n>=101&n<=500){return(750000)}
  if(n>=501&n<=1000){return(1000000)}
  if(n>=1001&n<=1300){return(1250000)}
  if(n>=1301){return(1500000)}
}

tier1_marginalbond = function(d){
  if(d<=500){return(2500)}
  if(d>=501&d<=1000){return(5000)}
  if(d>=1001&d<=3000){return(10000)}
  if(d>=3001&d<=6000){return(20000)}
  if(d>=6001&d<=9000){return(32500)}
  if(d>=9001&d<=12000){return(42500)}
  if(d>=12000){return(55000)}
}

tier2_blanket = function(n){
  if(n>=1&n<=10){return(375000)}
  if(n>=11&n<=100){return(750000)}
  if(n>=101&n<=500){return(1125000)}
  if(n>=501&n<=1000){return(1500000)}
  if(n>=1001&n<=1300){return(1875000)}
  if(n>=1301){return(2250000)}
}

tier2_marginalbond = function(d){
  if(d<=500){return(3750)}
  if(d>=501&d<=1000){return(7500)}
  if(d>=1001&d<=3000){return(15000)}
  if(d>=3001&d<=6000){return(30000)}
  if(d>=6001&d<=9000){return(48750)}
  if(d>=9001&d<=12000){return(63750)}
  if(d>=12000){return(82500)}
}

tier3_blanket = function(n){
  if(n>=1&n<=10){return(500000)}
  if(n>=11&n<=100){return(1000000)}
  if(n>=101&n<=500){return(1500000)}
  if(n>=501&n<=1000){return(2000000)}
  if(n>=1001&n<=1300){return(2500000)}
  if(n>=1301){return(3000000)}
}

tier3_marginalbond = function(d){
  if(d<=500){return(5000)}
  if(d>=501&d<=1000){return(10000)}
  if(d>=1001&d<=3000){return(20000)}
  if(d>=3001&d<=6000){return(40000)}
  if(d>=6001&d<=9000){return(65000)}
  if(d>=9001&d<=12000){return(85000)}
  if(d>=12000){return(110000)}
}
