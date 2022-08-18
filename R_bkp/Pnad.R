library('PNADcIBGE')
library('tidyverse')
library('ggplot2')
library('grid')
library('shadowtext')
library('plotly')


#### EXTRAINDO DADOS DA PNAD ----
variaveisselec <- c("VD2002","VD2003","VD2004","V1022","V4039","V4039C","V2007","V2010","V2009","V1023","V1016","RM_RIDE","Capital","VD4001", 
                    "VD4002","VD4003","V40081","V4032","V4019","V4040","V4022","VD4032","VD4016","V1028","V2005","V3009",
                    "VD3005","V3009A","V4012","V4029","V4063A","V4071","V4073","V4074A","V4082","VD2002","VD2003",
                    "V1014","V1008","V1016",'V40761',"V40762","V405812","V405112","V4074","V403322","V403312")

### BASE DE 2015 -----
Pnad15 <- rbind(get_pnadc(year = 2015, quarter = 1, vars = variaveisselec, design = FALSE),
                    get_pnadc(year = 2015, quarter = 2, vars = variaveisselec, design = FALSE),
                    get_pnadc(year = 2015, quarter = 3, vars = variaveisselec, design = FALSE),
                    get_pnadc(year = 2015, quarter = 4, vars = variaveisselec, design = FALSE))

Pnad15_2s <- rbind(get_pnadc(year = 2015, quarter = 3, vars = variaveisselec, design = FALSE),
                   get_pnadc(year = 2015, quarter = 4, vars = variaveisselec, design = FALSE))
                  
 
##### BASE DE 2021 ----
BasePnad21 <- rbind(get_pnadc(year = 2021, quarter = 1, vars = variaveisselec, 
    design = FALSE),get_pnadc(year = 2021, quarter = 2, vars = variaveisselec, 
    design = FALSE),get_pnadc(year = 2021, quarter = 3, vars = variaveisselec,
    design = FALSE),get_pnadc(year = 2021, quarter = 4, vars = variaveisselec, 
    design = FALSE))

#### FILTRANDO GEOGRAFICAMENTE PARA AS CIDADES DO AOP ----

ufsaop <- c("São Paulo","Rio de Janeiro","Bahia","Rio Grande do Sul",
                "Minas Gerais","Alagoas","Paraná","Goiás",
                "Pará","Amazonas","Maranhão","Ceará","Pernambuco",
                "Rio Grande do Norte","Mato Grosso do Sul")

Municipios <- c("Manaus","Belém","São Luís","Fortaleza","Natal","Recife",
                "Maceió","Salvador","Belo Horizonte","Rio de Janeiro",
                "São Paulo","Curitiba","Porto Alegre","Campo Grande","Goiania")

Aop_Ed <- Pnad15_2s %>% filter(UF==ufsaop & V1023=="Capital")


#### DADOS DA INTRODUCAO ----

BasePnad21_Adulta <- BasePnad21 %>% filter(V2009>=18) %>% drop_na(V3009A) %>% 
  mutate(Anos_ensino=as.numeric(VD3005))
Pnad40porc_pobres <- filter(BasePnad21_Adulta,V403312<=1100)

#Pnad_21_cid_AOP <- filter(BasePnad21, UF==ufsaop & V1023=="Capital")

quantile(BasePnad21$V403312, na.rm=T, probs = seq(0,1,1/10))

mean(Pnad40porc_pobres$Anos_ensino,na.rm=T)

Tabela_ensino <- BasePnad21 %>% filter(V2009>= 18 & V403312<=1100) %>% 
  drop_na(V3009A) %>% group_by(V3009A) %>% summarise(total=as.numeric(n())) %>% 
  mutate(percentual=(total/sum(total)*100))


mean(BasePnad21$V403312,na.rm=T)


###### APLICANDO FILTROS GEOGRAFICOS

Base_AopEd <- Aop_Ed %>% mutate(esteve_empregado=(if_else(
  VD4001=="Pessoas na força de trabalho",1,0)),procurouemprego=(if_else(
    V4071=="SIM",1,0))) %>% mutate(desempregado=if_else(
      procurouemprego==0 & esteve_empregado==0,1,0)) %>% 
  drop_na(desempregado)

#nao_procurouemprego <- summary(Base_AopEd$V4074A)

motivos <- names(nao_procurouemprego)
valores <- c(nao_procurouemprego[[1]],nao_procurouemprego[[2]],
             nao_procurouemprego[[3]],nao_procurouemprego[[4]],
nao_procurouemprego[[5]],nao_procurouemprego[[6]],nao_procurouemprego[[7]],
nao_procurouemprego[[8]],nao_procurouemprego[[9]],nao_procurouemprego[[10]])

motivosgerais <- cbind(motivos,valores,percentual=(valores/sum(valores))*100)

##### A nível de Cidade ----
motivo <- "Tinha que cuidar de filho(s), de outro(s) dependente(s) ou dos afazeres domésticos"
motivo2 <- "Tinha que cuidar dos afazeres domésticos, do(s) filho(s) ou de outro(s) parente(s)"

Base_AopEd <- Pnad15_2s %>% filter(Trimestre==4 & UF==ufsaop) %>% 
  mutate(nao_proc_por_filhos = if_else(
    V4074A==motivo2,1,0)) %>% drop_na(
    nao_proc_por_filhos)

Base_AopEd <- Base_AopEd %>% mutate(sexo = if_else(V2007=="Mulher",1,0))

tabmotivo <- Base_AopEd %>% group_by(UF) %>% summarise(
  Percentual = weighted.mean(nao_proc_por_filhos,V1028),
  percentual_sexo_feminino = weighted.mean(sexo)) %>% mutate(
  Percentual = round(Percentual, digits = 4),
  percentual_sexo_feminino = round(percentual_sexo_feminino, digits = 4),
  Percentual_de_mulheres = (Percentual*percentual_sexo_feminino)*100) %>% 
  mutate(Percentual_de_homens = ((1-percentual_sexo_feminino)*Percentual)*100)

tabmotivo <- cbind(Municipios,tabmotivo)




##### GRAFICOS DO ANEXO ----

percentual <- round(motivosgerais_df$percentual, digits = 2)

# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

plt <- ggplot(motivosgerais_df) + 
  geom_col(aes(percentual,motivos),fill = BLUE, witdth = 0.6)

plt <- plt + scale_x_continuous(
  limits = c(0,30),
  breaks = seq(0,30, by = 1),
  expand = c(0,0),
  position = "top") + 
  scale_y_discrete(expand = expansion(add = c(0,1))) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.ticks.length = unit(0, "mm"),
    panel.grid.major = element_line(),
    axis.text.y = element_blank(),
    axis.line.y.left = element_line(color = "BLACK"),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "Econ Sans Cnd",size = 14)
  ) 
  
# plt <- plt + 
  shadowtext::geom_shadowtext(
    motivosgerais_df = subset(motivosgerais_df,percentual < 15),
    mapping =  aes(percentual, y = motivos, label = motivos),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLACK,
    bg.colour = "white",
    bg.r = 0.3,
    family = "Econ Sans Cnd",
    size = 5
  )
  
 plt <- plt +
    geom_text(motivosgerais_df = subset(motivosgerais_df,percentual > 15),
        aes(0, y = motivos, label = motivos),
        hjust = 0,
        nudge_x = 0.3,
        colour = "black",
        family = "Econ Sans Cnd",
        size = 5
    )

 #### LABELS OUTSITE GRAPHIC
 plt <- plt  +
   labs(
     title = "Motivos para não ter procurado emprego na semana de referência",
     subtitle = "Percentual dentro da população desempregada em 2015"
   ) + 
   theme(
     plot.title = element_text(
       family = "Econ Sans Cnd", 
       face = "bold",
       size = 22
     ),
     plot.subtitle = element_text(
       family = "Econ Sans Cnd",
       size = 20
     )
   )
 plt
 
 
###### GRÁFICO PARA CIDADES ----

plt_cid <- ggplot(tabmotivo) + scale_x_continuous(
  limits = c(0,38),
  breaks = seq(0,38, by = 5),
  expand = c(0,0),
  position = "top") + 
   theme(panel.background = element_rect(fill = "white")) +
   geom_col(aes(Percentual,Municipios,fill = sexo_feminino,
            position = "fill",stat = "identity")) + 
   labs(
     title = "Desempregados que não procuraram emprego para cuidar do(s) filho(s)",
     subtitle = "Percentual para 16 cidades presentes no Projeto Acesso a Oportunidades, em 2015"
   ) + 
   theme(
     plot.title = element_text(
       family = "Econ Sans Cnd", 
       face = "bold",
       size = 22
     ),
     plot.subtitle = element_text(
       family = "Econ Sans Cnd",
       size = 18))
 
 plt_cid 
 
 
 
 plt_cid <- ggplot(tabmotivo,aes(
   Percentual,Municipios,fill = percentual_sexo_feminino)) +
   scale_x_continuous(
     limits = c(0,38),
     breaks = seq(0,38, by = 5),
     expand = c(0,0),
     position = "top") + 
   theme(panel.background = element_rect(fill = "white")) + 
   geom_bar(position = "stack",stat = "identity") + 
   scale_fill_viridis_c(direction = -1) + 
   labs(
     title = "Desempregados que não procuraram emprego para cuidar do(s) filho(s)",
     subtitle = "Percentual para 16 cidades presentes no Projeto Acesso a Oportunidades, em 2015"
   ) + 
   theme(
     plot.title = element_text(
       family = "Econ Sans Cnd", 
       face = "bold",
       size = 22
     ),
     plot.subtitle = element_text(
       family = "Econ Sans Cnd",
       size = 18))
 
 plt_cid 

 
###### plotly ----
 
fig <- plot_ly(tabmotivo,x = ~Percentual_de_mulheres,y =~Municipios, type = 'bar',
               orientation = 'h', name = 'Mulheres',
               marker = list(color = 'rgba(246, 78, 139, 0.6)',
                             line = list(color = 'rgba(246, 78, 139, 1.0)',
                                         width = 3)))
                
fig <- fig %>% add_trace(x = ~Percentual_de_homens, name = "Homens", 
                         marker = list(color = 'rgba(58,71,80,0.6)',
                              line = list(color = 'rgba(58,71,80,1.0)',
                                          width = 3)))

fig <- fig %>% layout(barmode = "stack",
  title = "Desempregados que não procuraram emprego para cuidar do(s) filho(s) em 2015",
  xaxis = list(title ="Valor percentual desses desempregados"), 
  yaxis = list(title = ""))

fig


  