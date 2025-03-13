# Instalar pacotes necessários (se ainda não instalados)
#install.packages("PNADcIBGE")
#install.packages("survey")

# Carregar pacotes
library(PNADcIBGE)
library(survey)


# Definir anos e trimestres
anos <- 2012:2013
trimestres <- 1:4

# Criar uma lista para armazenar os resultados
resultados <- list()
###Criar dataframe

# Loop alinhado: para cada ano, rodar todos os trimestres
for (ano in anos) {
  for (trimestre in trimestres) {
dadospnad <- get_pnadc(year=ano, quarter=trimestre, labels = TRUE, design = TRUE, vars=c("UF","VD4001","V1028","VD4002","VD4009","V4019","V4012")) 


##TotalEmpregados no setor privado sem carteira
total_privado_semcarteira<- svytotal(x=~VD4009=="Empregado no setor privado sem carteira de trabalho assinada", design=dadospnad, na.rm=TRUE)
total_privado_semcarteira
total_privado_semcarteira_true <- total_privado_semcarteira[
  names(total_privado_semcarteira) == "VD4009 == \"Empregado no setor privado sem carteira de trabalho assinada\"TRUE"
]
total_privado_semcarteira_true

##Total Trabalhador doméstico sem carteira de trabalho assinada
total_domestico_semcarteira <- svytotal (x=~VD4009=="Trabalhador doméstico sem carteira de trabalho assinada", design=dadospnad, na.rm=TRUE)
total_domestico_semcarteira
total_domestico_semcarteira_true <- total_domestico_semcarteira[
  names(total_domestico_semcarteira) == "VD4009 == \"Trabalhador doméstico sem carteira de trabalho assinada\"TRUE"
]
total_domestico_semcarteira_true

##Total Trabalhador familiar auxiliar
total_trab_familiar <- svytotal (x=~VD4009=="Trabalhador familiar auxiliar", design=dadospnad, na.rm=TRUE)
total_trab_familiar
total_trab_familiar_true <- total_trab_familiar[
  names(total_trab_familiar) == "VD4009 == \"Trabalhador familiar auxiliar\"TRUE"
]
total_trab_familiar_true

####Intersecção -> empregadores e conta própria sem CNPJ
###Empregadores s/CNPJ
totalintersecempreg <- svytotal(x=~interaction(V4019 == "Não", V4012 == "Empregador"), design=dadospnad, na.rm=TRUE)
totalintersecempreg
totalintersecempreg_true <- totalintersecempreg[
  names(totalintersecempreg) == "interaction(V4019 == \"Não\", V4012 == \"Empregador\")TRUE.TRUE"
]
totalintersecempreg_true

###Conta Prop s/CNPJ
totalinterseccontaprop <- svytotal(x=~interaction(V4019 == "Não", V4012 == "Conta própria"), design=dadospnad, na.rm=TRUE)
totalinterseccontaprop
totalinterseccontaprop_true <- totalinterseccontaprop[
  names(totalinterseccontaprop) == "interaction(V4019 == \"Não\", V4012 == \"Conta própria\")TRUE.TRUE"
]
totalinterseccontaprop_true

###############################################################################################################################################################################################

#######CÁLCULO DA TAXA DE INFORMALIDADE
###TOTAL INFORMAL
totalinformais = total_privado_semcarteira_true + total_domestico_semcarteira_true + total_trab_familiar_true + totalintersecempreg_true + totalinterseccontaprop_true
totalinformais

# Denominador: total de pessoas ocupadas
ocupados <- svytotal (x=~VD4002 == "Pessoas ocupadas", design=dadospnad, na.rm=TRUE)
ocupados
ocupados_true <- ocupados[
  names(ocupados) == "VD4002 == \"Pessoas ocupadas\"TRUE"
]
ocupados_true


##Calculando a Taxa de Informalidade na mão
taxainformalidade <- (totalinformais/ocupados_true)*100

# Salvar os resultados em uma data frame com metadados (ano e trimestre)
resultados[[paste(ano, trimestre, sep = "_")]] <- data.frame(
  Ano = ano,
  Trimestre = trimestre,
  Total = as.numeric(taxainformalidade)
)
# Consolidar os resultados em uma tabela final
tabela_taxainformalidade <- do.call(rbind, resultados)

# Exibir os resultados organizados
print(tabela_taxainformalidade)

# Exportar Tabelas
write.csv(tabela_taxainformalidade, "tabela_taxainformalidade.csv", row.names = FALSE)
  }
}
