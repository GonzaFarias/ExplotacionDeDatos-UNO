## Practica Clase 1

# Con los siguientes números: 7.3, 6.8, 0.005, 9, 12, 2.4, 18.9, .9
vectorNum = c(7.3, 6.8, 0.005, 9, 12, 2.4, 18.9, 0.9)

# a) Calcula la media.
media = mean(vectorNum)

# b) Calcula la raíz cuadrada de los números.
sqrt(vectorNum)

# c) Cuantos valores son mayores que 1?
contador = 0
for(i in vectorNum){
  if(i > 1){
    contador = contador + 1
  }
}
print(contador)
#Con funcion
sum(vectorNum > 1)

# d) Obtén los números que son mayores que su raíz cuadrada.
vectorNum[vectorNum>sqrt(vectorNum)]

