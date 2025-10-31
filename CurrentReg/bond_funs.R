individual_well_bond = function(d){
  if(d<1000){return(1500)}
  if(d>=1000&d<3000){return(15000)}
  if(d>-3000&d<10000){return(30000)}
  if(d>=10000){return(60000)}
}
individual_well_bond <- Vectorize(individual_well_bond)


blanket_bond = function(d){
  if(d<1000){return(15000)}
  if(d>=1000){return(120000)}
}