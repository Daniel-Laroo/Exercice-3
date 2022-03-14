#Exo_2

# 1- Creer fonction

# *Moyenne
Moyenne <- function(vecteur) {
  moy = sum(vecteur)/length(vecteur)
  print(paste0("La moyenne est: ", moy))
}

# *Variance
variance <- function(vecteur){
  vari = sum( (vecteur - Moyenne(vecteur) )^2 )/length(vecteur)
  print(paste0("La variance est: ", vari))
}

# *Ecart-Type
Ecart_type <- function(vecteur){
  ET = variance(vecteur)^(1/2)
  print(paste0("L'écart-type est: ", ET))
}

# *Mediane
Médiane <- function(vecteur){
  if (length(vecteur)%%2 == 0)
  {
    print(paste0("La mediane est: ", vecteur(length(vecteur)/2)))
  }
  else{
    print(paste0("La mediane est: ", vecteur((1+length(vecteur))/2)))
  }
}
  
# *Covariance
Covariance <- function(vect1,vect2){
  Covar = sum((vect1-Moyenne(vect1))*(vect2-Moyenne(vect2)))/length(vect1)
  print(paste0("La covariance est: ", Covar))
}


# 2- Resolution d'equation du 2nd degres

Equation_du_second_degres <- function(a,b,c){#equation aX^2+bX+c
  d = b^2-4*a*c
  if (d<0)
  {
    print("Cette equation a coeficients reels n'admet pas de solutions reelles")
  }
  else if (d == 0)
  {
    x0 = b/(2*a)
    print(paste0("Cette equation a coeficients reels a une unique solution : ", x0) )
  }
  else{
    x1 = (-b-d^(1/2))/(2*a)
    x2 = (-b+d^(1/2))/(2*a)
    print(paste0("Cette equation a coeficients reels admet 2 solutions reelles : ",x1, "et", x2)) 
  }
}


# 3- fonction factoriel
factoriel <- function(a){
  if (is.integer(a)==FALSE)
  {
    print("veuillez entrer un entier")
  }
  else{
    c <- 1:a
    print(paste0("le factoriel de a (a!) est:", prod(c)))
  }
}


# EXO_1




