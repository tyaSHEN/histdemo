library(tidyverse)
library(HMDHFDplus)

### parameters ###
country<-"DNK"
year = 2018
yeard = 10
age=100

# Enter your HFD username and password
username.hfd = ""
password.hfd = ""
# Enter your HMD username and password
username.hmd = ""
password.hmd = ""
### 

#### data ####
## HFD data
Birth <- readHFDweb(CNTRY = country, item = "birthsTR", username =
                            username.hfd, password = password.hfd)
Birth$Age[Birth$Age=="12-"]<-12
Birth$Age[Birth$Age=="55+"]<-55
Birth$Cohort[is.na(Birth$Cohort)]<-0

## HMD data
Pop = readHMDweb(CNTRY = country, item = "Population", username =
                   username.hmd, password = password.hmd)
Pop = Pop[,c(1,2,4,5,6)]
colnames(Pop) <- c("Year","Age","Female","Male","Total")
Pop$Age[Pop$Age=="110+"]<-110
Pop$Year = gsub("[+]", "", Pop$Year)

Tot.Birth = readHMDweb(CNTRY = country, item = "Births", username =
                   username.hmd, password = password.hmd)
Tot.Birth.F<- Tot.Birth[,c(1,2)]
Tot.Birth.M<- Tot.Birth[,c(1,3)]

Death = readHMDweb(CNTRY = country, item = "Deaths_lexis", username =
                         username.hmd, password = password.hmd)
Death$Cohort[Death$Cohort=="."]<-0
Death$Cohort<-as.numeric(Death$Cohort)
Death$Age[Death$Age=="110+"]<-110
Death$Age<-as.numeric(Death$Age)

#### R ####
pop1 <- Pop %>% filter(Year == year) %>% summarise(sum(Female)) %>% as.numeric()
pop2 <- Pop %>% filter(Year == year-yeard) %>% summarise(sum(Female)) %>% as.numeric()
R = log(pop1/pop2)/yeard
####

rB = c()
rx = c()
dS = c()

for (a in 0:age) {
  #### rx ####
  pop1 <- Pop %>% filter(Year == year,Age == a) %>% pull(Female)
  pop2 <- Pop %>% filter(Year == year-yeard,Age == a) %>% pull(Female)
  temp <- log(pop1/pop2)/yeard
  rx = append(rx,temp)
  
  #### rB ####
  temp<-log(Tot.Birth.F$Female[which(Tot.Birth.F$Year==year-a-1)]/Tot.Birth.F$Female[which(Tot.Birth.F$Year==year-yeard-a-1)])/yeard
  rB = append(rB,temp)
  
  #### dS ####
  ## qx is calculated by eq.81 in HMD Methods protocol v6
  qx1 = Death %>% filter(Cohort == year-a-1,Age <= a) %>% select(1:4)
  qx1$Female[which(qx1$Year == year)] = 0
  qx1 = qx1 %>% group_by(Age,Cohort) %>%
    summarise(D = sum(Female),.groups = 'drop') 
  qx1$P <- NA
  
  for (c in a:0) {
    qx1$P[which(qx1$Age==c)] <- Pop %>% filter(Year == year+c-a, Age == c) %>% pull(Female) +
      Death %>% filter(Year == year+c-a-1, Age == c) %>% slice_max(Cohort) %>% pull(Female)
  }
  
  qx1$qx <- qx1$D/qx1$P 
  ## lx1 is the later cohort and lx2 is the earlier cohort 
  lx1 = tail(cumprod(1-qx1$qx),1)
  
  qx2 = Death %>% filter(Cohort == year-yeard-a-1,Age <= a) %>% select(1:4)
  qx2$Female[which(qx2$Year == year-yeard)] = 0
  qx2 = qx2 %>% group_by(Age,Cohort) %>%
    summarise(D = sum(Female),.groups = 'drop') 
  qx2$P <- NA
  
  for (c in a:0) {
    qx2$P[which(qx2$Age==c)] <- Pop %>% filter(Year == year-yeard+c-a, Age == c) %>% pull(Female) +
      Death %>% filter(Year == year-yeard+c-a-1, Age == c) %>% slice(which.max(Cohort)) %>% pull(Female)
  }
  
  qx2$qx <- qx2$D/qx2$P 
  lx2 = tail(cumprod(1-qx2$qx),1)
  
  temp <- log(lx1/lx2)/yeard
  dS = append(dS,temp)
  
}

rx[is.na(rx)] <- 0
rx[is.nan(rx)] <- 0
rx[is.infinite(rx)] <- 0

dS[is.na(dS)] <- 0
dS[is.nan(dS)] <- 0
dS[is.infinite(dS)] <- 0

#### mi ####
mi<-rx-rB-dS

#### Cx ####
pop <- Pop %>% filter(Year == year-yeard) %>% filter(Age %in% c(0:age)) %>% pull(Female)
pop2 <- pop * exp(rx*0.5*yeard)
Cx = pop2/sum(pop2)
####

#### For male results, change all `Female` in `Male` above this line. No changes needed beyone #### 

R - sum(Cx * rx) # error is very small

# Figure 1
Age = 0:age
deco1 = data.frame(Age,rx,dS,mi,rB,Cx)
deco1 = pivot_longer(deco1,2:5)
deco1$name = factor(deco1$name,levels = unique(deco1$name))

ggplot(deco1)+
  geom_line(aes(x= Age, y = value*Cx*100,color= name)) +
  scale_x_continuous(breaks = seq(0,120,20)) +
  ggtitle(country)+
  scale_y_continuous("Growth rate & its components",breaks = seq(-1,1,0.025)) +
  theme(legend.position = "bottom",legend.title = element_blank(),panel.background = element_blank(),axis.line = element_line())+
  geom_hline(yintercept = 0,linetype=2,color = "grey50") +
  scale_color_manual(values=c("#0077ff","#16cc10","#ed0909","#cf05ed"),label=c(expression('Growth rate (r'[x]*')'),expression('Mortality ('*Delta*'S)'),expression('Migration ('*Delta*'M)'),expression('Growth rate at birth (r'[B]*')')))

t1= deco1 %>% mutate(x= Cx*value) %>% group_by(name) %>% 
  summarise(Sum = sum(x)) %>% filter(name %in% c("rx","dS","mi","rB"))
t1$name <- recode(t1$name,rx="Growth Rate (rx)",dS = "Mortality (dS)",mi="Migration (dM)",rB="Growth rate at birth (rB)")
t1$name <- factor(t1$name,levels = c("Growth Rate (rx)","Mortality (dS)","Migration (dM)","Growth rate at birth (rB)"))
t1$Sum <- round(t1$Sum ,5)* 100
t1


#### rxf ####
### rB = sum(rf*CB) + sum(rx2*CB)
### rxf = sum(rx2*CB)
B <- Birth %>% group_by(Year,Age) %>%
  summarise(SUMM = sum(Total),.groups = 'drop')
CB <- c()
rx2 <- c()
rxf <- c()
for (a in 0:age) {
  B1 <- B %>% filter(Year == (year-a-1)) %>% pull(SUMM)
  B2 <- B %>% filter(Year == (year-yeard-a-1)) %>% pull(SUMM)
  if(length(B2)!=0){
    r.tem <- log(B1/B2)/yeard
    
    r.tem[is.na(r.tem)] <- 0
    r.tem[is.nan(r.tem)] <- 0
    r.tem[is.infinite(r.tem)] <- 0
    
    B3 <- B2/2.05 * exp(r.tem*0.5*yeard) 
    
    temp.CB = B3/sum(B3)
  }
  CB = cbind(CB,temp.CB)
  
  ## change in mother population
  pop1 <- (Pop %>% filter(Year == (year-a-1),Age %in% c(12:55)) %>% pull(Female) + Pop %>% filter(Year == (year-a),Age %in% c(12:55)) %>% pull(Female))/2
  pop2 <- (Pop %>% filter(Year == (year-yeard-a-1),Age %in% c(12:55)) %>% pull(Female) + Pop %>% filter(Year == (year-yeard-a),Age %in% c(12:55)) %>% pull(Female))/2
  temp.rx <- log(pop1/pop2)/yeard
  rx2 <- cbind(rx2,temp.rx)
  
  temp = sum(temp.CB*temp.rx)
  rxf = append(rxf,temp)
}

#### rf ####
### rf = rB - sum(rx2*CB)
rf = rB-rxf 
####

# Figure 2
Age = 0:age
deco2 = data.frame(Age,rx,rB,dS,mi,rf,rxf,Cx)
deco2 = pivot_longer(deco2,2:7)
deco2$name <- factor(deco2$name, levels = unique(deco2$name))

ggplot(deco2 %>% filter(name %in% c("rx","rB","rf","rxf")))+
  geom_line(aes(x= Age, y = value*Cx*100,color= name)) +
  scale_x_continuous(breaks = seq(0,120,20)) +
  ggtitle(country)+
  scale_y_continuous("Growth rate & its components",breaks = seq(-1,1,0.025)) +
  theme(legend.position = "bottom",legend.title = element_blank(),panel.background = element_blank(),axis.line = element_line())+
  geom_hline(yintercept = 0,linetype=2,color = "grey50") +
  scale_color_manual(values=c("#0077ff","#cf05ed","#ff80c2","#ff7f00"),label=c(expression('Growth rate (r'[x]*')'),expression('Growth rate at birth (r'[B]*')'),expression('Fertility ('*Delta*'f)'),expression('Growth rate of mothers (r'[xf]*')')))

t2=deco2 %>% mutate(x= Cx*value) %>% group_by(name) %>% 
  summarise(Sum = sum(x)) %>% filter(name %in% c("rx","rB","rf","rxf"))
t2$name <- recode(t2$name,rx="Growth Rate (rx)",rB="Growth rate at birth (rB)",rf="Fertility (df)",rxf="Residual (rxf)")
t2$name <- factor(t2$name,levels = c("Growth Rate (rx)","Growth rate at birth (rB)","Fertility (df)","Residual (rxf)"))
t2$Sum <- round(t2$Sum ,5)* 100
t2
### For more iterations please email tianyu.shen@anu.edu.au

