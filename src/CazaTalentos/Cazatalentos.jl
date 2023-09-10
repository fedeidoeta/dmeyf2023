############## Cazatalentos 1 #####################
# Adolescentes: 100
# Tiros: 100

"""
Las 100 adolescentes son los cambios que hacemos en nuestros hiperparametros y dataset, 
probar 100 veces cada uno le daria consistencia a los resultados
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
mejor = [0.8]
peloton = Vector((31:79) / 100)
peor = [0.3]
jugadoras = append!(mejor, peloton,peloton,peor)

# veo que tiene el vector
jugadoras

sort!(jugadoras, rev=true)
jugadoras

# hago que las 100 jugadoras tiren 10 veces cada una
ftirar.(jugadoras, 100)

global primera_ganadora = 0
global suma_aciertos = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end  
  global suma_aciertos += vaciertos[1]
end

print(primera_ganadora)
println("Probabilidad de enceste: ", suma_aciertos/(10000*100))

# Probabilidad de ser la mejor con 100 tiros

for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end

# ¿Cuantos tiros necesito para tener un 99.9% de prob de que sea la mejor?

for tiros_libres in [ 100, 30000]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end




################### Cazatalentos 2 #####################
# Adolescentes: 200
# Tiros: 100

"""
Las 200 adolescentes son los cambios que hacemos en nuestros hiperparametros y dataset, 
probar 100 veces cada uno le daria consistencia a los resultados
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
mejor = [0.8]
peloton_1= repeat([0.79], 6)
peloton_2= repeat([0.78], 5)
peloton_3= repeat([0.77], 4)
peloton_4= repeat([0.76], 3)
peloton_5= repeat([0.75], 2)
peloton_6 = Vector((11:74) / 100)
peloton_7= repeat([0.10], 115)

jugadoras = []

jugadoras = append!(mejor, peloton_1, peloton_2, peloton_3, peloton_4, peloton_5, peloton_6, peloton_7)

# veo que tiene el vector
jugadoras
sort!(jugadoras, rev=true)
jugadoras

# hago que las 100 jugadoras tiren 10 veces cada una
res = ftirar.(jugadoras, 10)

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end

for tiros_libres in [100, 10000, 20000]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end


################### Cazatalentos 3 #####################
# Adolescentes: 2
# Tiros: 100

"""
Lo entiendo como pocos cambios en el local, pero muchas pruebas en Kaggle corriendo el riesgo de overfittear el pulico.
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
jugadora_1 = [0.8]
jugadora_2 = [0.75]

jugadoras = []

jugadoras = append!(jugadora_1, jugadora_2)

# veo que tiene el vector
jugadoras

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 600)  # 100 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end


################### Cazatalentos 4 #####################
# Adolescentes: 100
# Tiros: 10


"""
Este somos nosotros en Kaggle, hacemos varios cambios muchos cambios en el local pero pocas subidas 
y nos terminamos quedando con el mejor en el publico, es un criterio "debil".
"""

using Random

Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
mejor = [0.9]
peloton = Float64[]

# Generamos 99 números aleatorios entre 0.1 y 0.8 con un decimal
for i in 1:99
    numero_aleatorio = round(rand(0.1:0.1:0.8), digits=1)
    push!(peloton, numero_aleatorio)
end

jugadoras = []

jugadoras = append!(mejor, peloton)
jugadoras = sort(jugadoras, rev=true)

# veo que tiene el vector
jugadoras

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 10)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end


################### Cazatalentos 5 #####################
# Adolescentes: 100
# Tiros: 100
# Rondas: 3

"""
Lo entiendo como quedarme con los 5 mejores modelos que funcionan en el local e ir haciendo algunos cambios subiendolos a Kaggle
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

using Statistics

# defino las jugadoras
jugadora_a = [mean([0.85, 0.69, 0.7])]
jugadora_b = [mean([0.84, 0.74, 0.76])]
jugadora_c = [mean([0.84, 0.74, 0.75])]
jugadora_d = [mean([0.82, 0.70, 0.73])]
jugadora_e = [mean([0.81, 0.75, 0.74])]

jugadoras = []

jugadoras = append!(jugadora_a,jugadora_b, jugadora_c, jugadora_d, jugadora_e)

# veo que tiene el vector
jugadoras

global primera_ganadora = 0
global suma_diferencias = 0
global suma_diferencias_2 = 0
global prom_aciertos = 0

global suma_diferencias_elegida = 0
global suma_diferencias_2_elegida = 0
global prom_aciertos_elegida = 0

for i = 1:10000  # diez mil experimentos
    vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres cada jugadora
    mejor_ronda = findmax(vaciertos)[2]
    mejor_ronda_1 = findmax(vaciertos)
    aciertos_torneo = vaciertos[mejor_ronda]
    aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 100)
    aciertos_tercera = ftirar.(jugadoras[mejor_ronda], 100)
    global suma_diferencias += (aciertos_torneo - aciertos_segunda)
    global suma_diferencias_2 += (aciertos_torneo - aciertos_tercera)
    global prom_aciertos += mean([aciertos_torneo, aciertos_segunda, aciertos_tercera])
    #println(aciertos_torneo, "\t", aciertos_segunda, "\t", aciertos_tercera)
    
    #me quedo con los valores de la jugadora A para conocer sus estadisticas.
  if mejor_ronda_1[2] == 1
    global primera_ganadora += 1
    global suma_diferencias_elegida += (aciertos_torneo - aciertos_segunda)
    global suma_diferencias_2_elegida += (aciertos_torneo - aciertos_tercera)
    global prom_aciertos_elegida += mean([aciertos_torneo, aciertos_segunda, aciertos_tercera])
  end
end
print("Diferencia general: ", suma_diferencias / 10000)
print("Promedio aciertos general: ", prom_aciertos / 10000)
print("Cantidad de veces que la elegida quedo primera ", primera_ganadora)

print("Diferencia de la elegida: ", suma_diferencias_elegida/primera_ganadora)
print("Promedio aciertos de la elegida: ", prom_aciertos_elegida / primera_ganadora)
print("Cantidad de veces que la elegida quedo primera ", primera_ganadora)

################### Cazatalentos 5 V2 #####################
# Adolescentes: 100
# Tiros: 100
# se eligen 5 de la primer ronda
# 2 rondas de 100 tiros


using Random


Random.seed!(270001)


# calcula cuántos encestes logra una jugadora con índice de enceste prob
# haciendo qty tiros libres
function ftirar(prob, qty)
    return sum(rand() < prob for i in 1:qty)
end


# Defino las jugadoras
mejores = [0.85, 0.84, 0.84, 0.82, 0.81]
peloton = Float64[]


# Generamos 95 números aleatorios entre 0.1 y 0.8 con un decimal
for i in 1:95
    numero_aleatorio = round(rand(0.10:0.01:0.80), digits=1)
    push!(peloton, numero_aleatorio)
end


jugadoras = append!(mejores, peloton)
jugadoras = sort(jugadoras, rev=true)


# Veo qué tiene el vector
jugadoras


jugadoras_ronda2 = [0.69, 0.74, 0.74, 0.70, 0.75]
jugadoras_ronda3 = [0.70, 0.76, 0.75, 0.73, 0.74]


global primera_ganadora = 0


for i = 1:10000
    # Realizamos 100 tiros libres para todas las jugadoras en la primera ronda
    vaciertos = ftirar.(jugadoras, 100)
   
    # Selecciono las 5 mejores jugadoras de la primera ronda
    mejores_rondas = sortperm(vaciertos, rev=true)[1:5]

    # Verificamos si la jugadora con valor 0.85 está en primer lugar (puntaje máximo)
    if findfirst(jugadoras[mejores_rondas] .== 0.85) == 1
        aciertos_segunda = ftirar(jugadoras_ronda2[1], 100)  # cuento cantidad de aciertos
        aciertos_tercera = ftirar(jugadoras_ronda3[1], 100)  # cuento cantidad de aciertos
       
        if aciertos_segunda >= 69 && aciertos_tercera >= 69
            global primera_ganadora += 1
        end
    end
end


println(primera_ganadora)


for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end



################### Cazatalentos 6 #####################

"""
Esto seria como entrenar el modelo en local, ver su performance y cuando estoy confiado subo el resultado a Kaggle. Muy arriesgado
"""

#Adolescentes = 100
# Solo 1 prueba


using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

jugadoras = [0.8]

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  if vaciertos[1] > 80
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end


################### Cazatalentos 7 #####################

# Adolescentes: 100
# Tiros: 100
# Rondas: 3

"""
Probar 100 veces a cada una, le da certidumbre de la eleccion de las mejores 5 y probar nuevamente 100 veces a cada una, reafirma la eleccion.
El problema que veo es que no sabemos como dieron en la primera vuelta.
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
jugadora_a = [0.8]
jugadora_b = [0.79]
jugadora_c = [0.78]
jugadora_d = [0.77]
jugadora_e = [0.72]
jugadoras = append!(jugadora_a,jugadora_b, jugadora_c, jugadora_d, jugadora_e)


# veo que tiene el vector
jugadoras

global primera_ganadora = 0
global primera_ganadora_2 = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100) 
  mejor_ronda = findmax(vaciertos)[2]
  mejor_ronda_1 = findmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 100)
  mejor_ronda_2 = findmax(aciertos_segunda)
  #println(aciertos_torneo, "\t", aciertos_segunda)
  if mejor_ronda_1[2] == 1
    global primera_ganadora += 1
  end
  if mejor_ronda_2[1] >= 80
    global primera_ganadora_2 += 1
  end
end

print(primera_ganadora)
print(primera_ganadora_2)



############################# Cazatalentos 7 V2 ##########################
using Random


Random.seed!(207001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

#defino las jugadoras de la segunda ronda
jugadoras_ronda2 = [0.80, 0.79, 0.78, 0.77, 0.72]
jugadoras_ronda2

global primera_ganadora = 0

# Iterar sobre las simulaciones
for i = 1:10000
    vaciertos = ftirar.(jugadoras_ronda2, 100)
    mejor_ronda_1 = findmax(vaciertos)
    #aciertos_segunda = ftirar.(jugadoras_ronda2[mejor_ronda], 100) #Se estan ingresando indices del vector por fuera de los 5 definidos
    if mejor_ronda_1[2] == 1
        global primera_ganadora += 1
    end
end

println(primera_ganadora)


for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras_ronda2, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end



################### Cazatalentos 8 #####################
# Adolescentes: 100
# Tiros: 100
"""
Aca pensaria: cual es la probabilidad de que haga 85 encestes dado que la semana pasada hizo 79.
"""

using Random
Random.seed!(270001)
# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres


function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end


# defino las jugadoras

peloton_1 = []

for i in 1:12
    append!(peloton_1, repeat([0.85 - 0.01*i],i+1))
end  

peloton_2= repeat([0.72], 9)

jugadoras = []

mejor = [0.85]
jugadoras = append!(mejor, peloton_1)
jugadoras = append!(jugadoras, peloton_2)
jugadoras

global primera_ganadora = 0


for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres de cada jugadora
  mejor_ronda = findmax(vaciertos)
 if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)


for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end


################### Cazatalentos 9 #####################
# Adolescentes: 1
# Tiros: 100
# Rondas: 10
#Lo pienso como un vector de 10 jugadoras (en primera instancia)

"""
Cito el texto: esta deshonestidad de la Cazatalentos 9 no debería parecerle extraña, es exactamente lo mismo
que elegir cómo submit final el que le fue mejor en el Public Leaderboard, algo muy común entre
los alumnos ...
"""

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres


function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end


# defino el vector de las 10 tiradas de la jugadora
jugadora = [0.68,0.74,0.78,0.70,0.68,0.63,0.80,0.68,0.67,0.65]
jugadora = sort(jugadora, rev=true)


# veo que tiene el vector
jugadora


global primera_ganadora = 0


for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadora, 100)  # 100 tiros libres de la jugadora en cada ronda
  mejor_ronda = findmax(vaciertos)

  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)



################### Cazatalentos 9 #####################
# Adolescentes: 1
# Tiros: 100
# Rondas: 10
#Lo pienso como un vector de 10 jugadoras (en primera instancia)




"""
Cito el texto: esta deshonestidad de la Cazatalentos 9 no debería parecerle extraña, es exactamente lo mismo
que elegir cómo submit final el que le fue mejor en el Public Leaderboard, algo muy común entre
los alumnos ...
"""

using Statistics

using Random
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino el vector de las 10 tiradas de la jugadora
jugadora = [0.68,0.74,0.78,0.70,0.68,0.63,0.80,0.68,0.67,0.65]

jugadora_2 = mean(jugadora)

sort(jugadora, rev=true)

# veo que tiene el vector
jugadora

ftirar.(jugadora_2, 100)

global primera_ganadora = 0
global suma_aciertos = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadora_2, 100)  # 100 tiros libres de la jugadora en cada ronda
  if vaciertos >= 80
    global primera_ganadora += 1
    end
global suma_aciertos += vaciertos
end

println("Número de rondas con 80 o más aciertos:", primera_ganadora)

println("Probabilidad de enceste:", suma_aciertos/(10000*100))


for tiros_libres in [ 10, 20, 50, 100]

  primera_ganadora = 0

  for  i in  1:10000
    vaciertos = ftirar.(jugadoras, tiros_libres)
    mejor_ronda = findmax( vaciertos )[2]

    if mejor_ronda == 1   primera_ganadora += 1  end
  end

  println( tiros_libres,  "\t", primera_ganadora/10000 )
end



#########################################################################


# intencionalmente la mejor jugadora va al final de la lista de jugadoras
# porque la funcion findmax() de Julia hace trampa
# si hay un empate ( dos máximos) se queda con el que esta primero en el vector
using Random

Random.seed!(102191)

function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end


# defino las jugadoras
mejor   = [0.7]
peloton = Vector((501:599) / 1000)
jugadoras = append!(peloton, mejor) # intencionalmente el mejor esta al final


function  explorar()

  for tiros_libres in [100]

    primera_ganadora = 0

    for  i in  1:10000
      vaciertos = ftirar.(jugadoras, tiros_libres)
      mejor_ronda = findmax( vaciertos )[2]

      if mejor_ronda == 100   primera_ganadora += 1  end
    end

    println( tiros_libres,  "\t", primera_ganadora/10000 )
  end
end


@time  explorar()

