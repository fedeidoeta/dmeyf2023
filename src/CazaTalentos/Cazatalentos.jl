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
peloton_2 = Vector((31:79) / 100)
peor = [0.3]
jugadoras = append!(mejor, peloton,peloton_2,peor)


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
peloton = [0.79, 0.79, 0.79, 0.79, 0.79, 0.79, 0.78, 0.78,
0.78, 0.78,0.78, 0.77, 0.77, 0.77,0.77, 0.76,
0.76, 0.76,0.75, 0.75,0.74, 0.73,0.72, 0.71,
0.7, 0.69,0.68, 0.67,0.66, 0.65,0.64, 0.63,
0.62, 0.61,0.6, 0.59,0.58, 0.57,0.56, 0.55,
0.54, 0.53,0.52, 0.51,0.5, 0.49,0.48, 0.47,
0.46, 0.45,0.44, 0.43,0.42, 0.41,0.40, 0.39,
0.38, 0.37,0.36, 0.35,0.34, 0.33,0.32, 0.31,
0.30, 0.29,0.28, 0.27,0.26, 0.25,0.24, 0.23,
0.22, 0.21,0.20, 0.19,0.18, 0.17,0.16, 0.15,
0.14, 0.13,0.12, 0.11,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1, 0.1,0.1, 0.1,0.1, 0.1,
0.1, 0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,
0.1,0.1,0.1,0.1,0.1,0.1,0.1]
jugadoras = append!(mejor, peloton)


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
jugadoras_08 = fill(0.8, 50) #REVISAR CODIGO
jugadoras_07 = fill(0.7, 49) #REVISAR CODIGO

jugadoras = append!(mejor, jugadoras_07, jugadoras_08)


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
jugadoras = append!(jugadora_a,jugadora_b, jugadora_c, jugadora_d, jugadora_e)


# veo que tiene el vector
jugadoras

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)[2]
  mejor_ronda_1 = findmax(vaciertos)
  aciertos_torneo = vaciertos[mejor_ronda]
  aciertos_segunda = ftirar.(jugadoras[mejor_ronda], 100)
  println(aciertos_torneo, "\t", aciertos_segunda)
  if mejor_ronda_1[2] == 1
    global primera_ganadora += 1
  end
end

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
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
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
