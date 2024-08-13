# CG update 2024-08-06
# Adjusting blanket bond well # bins to reflect new proposal + amounts:
#  1 - 10
# 11 - 25
# 26 - 50
# 51 - 100
# 101 - 250 
# 251 - 500
# 501 - 750
# 751 - 1000
# 1001 - 1500
# 1501 - 2000
# 2001 - 2500

tier1_blanket = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(200000)}
  if(n>=11&n<=25){return(300000)}
  if(n>=26&n<=50){return(400000)}
  if(n>=51&n<=100){return(500000)}
  if(n>=101&n<=250){return(650000)}
  if(n>=251&n<=500){return(800000)}
  if(n>=501&n<=750){return(1000000)}
  if(n>=751&n<=1000){return(1250000)}
  if(n>=1001&n<=1500){return(1500000)}
  if(n>=1501&n<=2000){return(2000000)}
  if(n>=2001&n<=2500){return(2500000)}
  if(n>=2501){return(2500000)}
}

tier1_marginalbond = function(d){
  if(is.na(d)){return(NA)}
  if(d<=500){return(2500)}
  if(d>=501&d<=1000){return(5000)}
  if(d>=1001&d<=3000){return(10000)}
  if(d>=3001&d<=6000){return(20000)}
  if(d>=6001&d<=9000){return(32500)}
  if(d>=9001&d<=12000){return(42500)}
  if(d>=12000){return(55000)}
}

tier2_blanket = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(300000)}
  if(n>=11&n<=25){return(450000)}
  if(n>=26&n<=50){return(600000)}
  if(n>=51&n<=100){return(750000)}
  if(n>=101&n<=250){return(975000)}
  if(n>=251&n<=500){return(1200000)}
  if(n>=501&n<=750){return(1500000)}
  if(n>=751&n<=1000){return(1875000)}
  if(n>=1001&n<=1500){return(2250000)}
  if(n>=1501&n<=2000){return(3000000)}
  if(n>=2001&n<=2500){return(3750000)}
  if(n>=2501){return(3750000)}
}

tier2_marginalbond = function(d){
  if(is.na(d)){return(NA)}
  if(d<=500){return(3750)}
  if(d>=501&d<=1000){return(7500)}
  if(d>=1001&d<=3000){return(15000)}
  if(d>=3001&d<=6000){return(30000)}
  if(d>=6001&d<=9000){return(48750)}
  if(d>=9001&d<=12000){return(63750)}
  if(d>=12000){return(82500)}
}

tier3_blanket = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(400000)}
  if(n>=11&n<=25){return(600000)}
  if(n>=26&n<=50){return(800000)}
  if(n>=51&n<=100){return(1000000)}
  if(n>=101&n<=250){return(1300000)}
  if(n>=251&n<=500){return(1600000)}
  if(n>=501&n<=750){return(2000000)}
  if(n>=751&n<=1000){return(2500000)}
  if(n>=1001&n<=1500){return(3000000)}
  if(n>=1501&n<=2000){return(4000000)}
  if(n>=2001&n<=2500){return(5000000)}
  if(n>=2501){return(5000000)}
}

tier3_marginalbond = function(d){
  if(is.na(d)){return(NA)}
  if(d<=500){return(5000)}
  if(d>=501&d<=1000){return(10000)}
  if(d>=1001&d<=3000){return(20000)}
  if(d>=3001&d<=6000){return(40000)}
  if(d>=6001&d<=9000){return(65000)}
  if(d>=9001&d<=12000){return(85000)}
  if(d>=12000){return(110000)}
}

######################
## UPA Schedule
#####################

tier1_blanket_UPA = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(300000)}
  if(n>=11&n<=100){return(600000)}
  if(n>=101&n<=500){return(900000)}
  if(n>=501&n<=1000){return(1200000)}
  if(n>=1001&n<=1300){return(1500000)}
  if(n>=1301){return(1800000)}
}


tier2_blanket_UPA = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(450000)}
  if(n>=11&n<=100){return(900000)}
  if(n>=101&n<=500){return(1350000)}
  if(n>=501&n<=1000){return(1800000)}
  if(n>=1001&n<=1300){return(2250000)}
  if(n>=1301){return(2700000)}
}


tier3_blanket_UPA = function(n){
  if(n==0){return(0)}
  if(n>=1&n<=10){return(600000)}
  if(n>=11&n<=100){return(1200000)}
  if(n>=101&n<=500){return(1800000)}
  if(n>=501&n<=1000){return(2400000)}
  if(n>=1001&n<=1300){return(3000000)}
  if(n>=1301){return(3600000)}
}

