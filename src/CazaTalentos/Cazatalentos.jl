############## Cazatalentos 1 #####################
# Adolescentes: 100
# Tiros: 100


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
res = ftirar.(jugadoras, 10)

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)


  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)




################### Cazatalentos 2 #####################
# Adolescentes: 200
# Tiros: 100

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

################### Cazatalentos 3 #####################
# Adolescentes: 2
# Tiros: 100

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
  vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)

  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)


################### Cazatalentos 4 #####################
# Adolescentes: 100
# Tiros: 10

using Random

Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

# defino las jugadoras
mejor = [0.9]
"""jugadoras_08 = fill(0.8, 50) #REVISAR CODIGO
jugadoras_07 = fill(0.7, 49) #REVISAR CODIGO

jugadoras = append!(mejor, jugadoras_07, jugadoras_08)

"""
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

################### Cazatalentos 5 #####################
############ REVISAR ####################
# Adolescentes: 100
# Tiros: 100
# Rondas: 3

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

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)[2]
  mejor_ronda_1 = findmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 100)
  global suma_diferencias += (aciertos_torneo - aciertos_segunda)
  println(aciertos_torneo, "\t", aciertos_segunda)
  if mejor_ronda_1[2] == 1
    global primera_ganadora += 1
  end
end
print(suma_diferencias / 10000)
print(primera_ganadora)

################### Cazatalentos 6 #####################
################### REVISAR ##########################
#Adolescentes = 100
# Solo 1 prueba


using Random

Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end

jugadora = [0.8]

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadora, 100)  # 10 tiros libres cada jugadora
  if vaciertos[1] > 80
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

################### Cazatalentos 7 #####################
############ REVISAR ####################
# Adolescentes: 100
# Tiros: 100
# Rondas: 3

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
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)[2]
  mejor_ronda_1 = findmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 100)
  mejor_ronda_2 = findmax(aciertos_segunda)
  println(aciertos_torneo, "\t", aciertos_segunda)
  if mejor_ronda_1[2] == 1
    global primera_ganadora += 1
  end
  if mejor_ronda_2[2] == 1
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

# defino las jugadoras
jugadoras_ronda1 = Float64[]

# Generamos 99 números aleatorios entre 0.10 y 0.85 con un decimal
for i in 1:100
    numero_aleatorio = round(rand(0.01:0.10:0.85), digits=12)
    push!(jugadoras_ronda1, numero_aleatorio)
end

jugadoras_ronda1 = sort(jugadoras_ronda1, rev=true)
jugadoras_ronda1

#defino las jugadoras de la segunda ronda
jugadoras_ronda2 = [0.80, 0.79, 0.78, 0.77, 0.72]
jugadoras_ronda2

contador = 0

# Iterar sobre las simulaciones
for i = 1:10000
    vaciertos = ftirar.(jugadoras_ronda1, 100)
    mejor_ronda = sortperm(vaciertos, rev=true)[1:5]
    #aciertos_segunda = ftirar.(jugadoras_ronda2[mejor_ronda], 100) #Se estan ingresando indices del vector por fuera de los 5 definidos
    if mejor_ronda[1] <= 5
        contador += 1
    end
end


println(contador)


################### Cazatalentos 8 #####################
# Adolescentes: 100
# Tiros: 100


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


################### Cazatalentos 9 #####################
# Adolescentes: 1
# Tiros: 100
# Rondas: 10
#Lo pienso como un vector de 10 jugadoras (en primera instancia)


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