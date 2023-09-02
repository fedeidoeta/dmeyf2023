############## Cazatalentos 1 #####################

using Random

Random.seed!(102191)

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
#############################################

