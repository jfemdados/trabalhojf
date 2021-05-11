#biblio
library(tidyverse)
library(basedosdados)
library(rio)


#pegando a base

# sexo e idade e remuneracao
trabalho_jf <- read_sql('SELECT faixa_etaria, sexo_trabalhador, avg(valor_remun_media_nominal) FROM `basedosdados.br_me_rais.microdados_vinculos`
                       where sigla_uf = "MG" and ano = 2019 and id_municipio = 3136702
                       group by faixa_etaria, sexo_trabalhador',
                       "double-voice-305816")
trabalho_jf2 <- trabalho_jf%>%
  rename(salario = f0_)%>%
  pivot_wider(id_cols = faixa_etaria, values_from = salario, names_from = sexo_trabalhador)



#historico de remuneracao  
hist_trab_jf <- read_sql('SELECT ano, raca_cor, sexo_trabalhador, avg(valor_remun_media_sm) FROM `basedosdados.br_me_rais.microdados_vinculos`
where sigla_uf = "MG" and id_municipio = 3136702 and grau_instrucao_apos_2005 = 7 and ano > 2005
group by raca_cor, sexo_trabalhador, ano',
                        "double-voice-305816")



pretopardoebranco <- hist_trab_jf%>%
  filter(raca_cor == 2 | raca_cor == 4 | raca_cor == 8 | raca_cor == 6)%>%
  pivot_wider(id_cols = ano, values_from = f0_, names_from = c(raca_cor,sexo_trabalhador))%>%
  arrange(ano)


#historico da razao salario mulher preta/ homem branco  
razao_trab_jf <- read_sql('SELECT ano, raca_cor, sexo_trabalhador, avg(valor_remun_media_nominal) FROM `basedosdados.br_me_rais.microdados_vinculos`
where sigla_uf = "MG" and id_municipio = 3136702 and grau_instrucao_apos_2005 IN(8,9) and ano > 2005 and raca_cor IN (2,8)
group by raca_cor, sexo_trabalhador, ano',
                         "double-voice-305816")
  
razao_final <- razao_trab_jf%>%
  filter(raca_cor == 2 & sexo_trabalhador == 1 | raca_cor == 8 & sexo_trabalhador == 2)%>%
  pivot_wider(id_cols = ano, names_from = c(raca_cor,sexo_trabalhador), values_from = f0_)%>%
  mutate(razao_homem_mulher = `2_1`/`8_2`)%>%
  arrange(ano)


export(razao_final, "razaoextremos.csv")





export(pretopardoebranco, "pretopardobranco.csv")






#exportando 
setwd("~/Documentos/jfemdados")
export(trabalho_jf2, "sexoidaderem.csv")
