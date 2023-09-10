using Random
using Statistics
Random.seed!(270001)

# calcula cuantos encestes logra una jugadora con indice de enceste prob

function ftirar(prob, qty)
  return sum(rand() < prob for i in 1:qty)
end


############## Cazatalentos 1 #####################
# Adolescentes: 100
# Tiros: 100

cantidad_adol = 100

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

desvio = sqrt(mejor[1]*(1-mejor[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", mejor[1]-1.96*desvio)
print("Desvio std: ", desvio)

################### Cazatalentos 2 #####################
# Adolescentes: 200
# Tiros: 100

cantidad_adol = 200

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

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 100 tiros libres cada jugadora
  mejor_ronda = findmax(vaciertos)
  if mejor_ronda[2] == 1
    global primera_ganadora += 1
  end
end

print(primera_ganadora)

desvio = sqrt(mejor[1]*(1-mejor[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", mejor[1]-1.96*desvio)
print("Desvio std: ", desvio)


################### Cazatalentos 3 #####################
# Adolescentes: 2
# Tiros: 100

cantidad_adol = 2

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

desvio = sqrt(jugadora_1[1]*(1-jugadora_1[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", jugadora_1[1]-1.96*desvio)
print("Desvio std: ", desvio)

################### Cazatalentos 4 #####################
# Adolescentes: 100
# Tiros: 10

cantidad_adol = 100

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

desvio = sqrt(mejor[1]*(1-mejor[1])/10)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", mejor[1]-1.96*desvio)
print("Desvio std: ", desvio)


################### Cazatalentos 5 #####################
# Adolescentes: 100
# Tiros: 100
# se eligen 5 de la primer ronda
# 2 rondas de 100 tiros

cantidad_adol = 100

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

mejor = mean([0.85, 0.69, 0.7])

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
       
        if aciertos_segunda >= 69 && aciertos_tercera >= 70
            global primera_ganadora += 1
        end
    end
end


desvio = sqrt(mejor*(1-mejor)/300)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", mejor-1.96*desvio)
print("Desvio std: ", desvio)


################### Cazatalentos 6 #####################
# Adolescentes: 100
# Tiros: 100
# se eligen 5 de la primer ronda
# 2 rondas de 100 tiros

cantidad_adol = 100

jugadoras = [0.8]

global primera_ganadora = 0

for i = 1:10000  # diez mil experimentos
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  if vaciertos[1] >= 80
    global primera_ganadora += 1
  end
end


desvio = sqrt(jugadoras[1]*(1-jugadoras[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(length(cantidad_adol/100))/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", jugadoras[1]-1.96*desvio)
print("Desvio std: ", desvio)

############################# Cazatalentos 7 ##########################
#Adolescentes = 100
#Solo 1 prueba

cantidad_adol = 100

#defino las jugadoras de la segunda ronda
jugadoras = [0.80, 0.79, 0.78, 0.77, 0.72]

global primera_ganadora = 0

# Iterar sobre las simulaciones
for i = 1:10000
    vaciertos = ftirar.(jugadoras, 100)
    mejor_ronda_1 = findmax(vaciertos)
    if mejor_ronda_1[2] == 1
        global primera_ganadora += 1
    end
end

desvio = sqrt(jugadoras[1]*(1-jugadoras[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", mejor[1]-1.96*desvio)
print("Desvio std: ", desvio)

################### Cazatalentos 8 #####################
# Adolescentes: 100
# Tiros: 100

cantidad_adol = 100
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

prob_enceste_con_hist = (85+790)/1100

desvio = sqrt(prob_enceste_con_hist*(1-prob_enceste_con_hist)/1100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", prob_enceste_con_hist-1.96*desvio)
print("Desvio std: ", desvio)

################### Cazatalentos 9 #####################
# Adolescentes: 1
# Tiros: 100
# Rondas: 10

cantidad_adol = 1

# defino el vector de las 10 tiradas de la jugadora
jugadora = [0.68,0.74,0.78,0.70,0.68,0.63,0.80,0.68,0.67,0.65]

jugadora_2 = mean(jugadora)

sort(jugadora, rev=true)

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

desvio = sqrt(jugadora_2[1]*(1-jugadora_2[1])/100)

print("Probabilidad de ser la primera de su grupo: ",primera_ganadora/10000)
print("Estandarizada a 100 jugadoras: ",round(primera_ganadora*sqrt(cantidad_adol/100)/10000, digits = 4))
print("Probilidad de enceste de la jugadora elegida (CI): ", jugadora_2[1]-1.96*desvio)
print("Desvio std: ", desvio)