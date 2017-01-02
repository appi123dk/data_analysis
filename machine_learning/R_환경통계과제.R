library(foreign)
data = read.dta("kgss2010/kgss2010.dta", convert.factors = FALSE)

head(data)

#### 인구 사회학적 변수
female = ifelse(data$sex == 1, 0, 1)

data$age[data$age == 888] = NA
age = data$age

data$incom0[data$incom0 == 7777 | data$incom0 == 9999] = NA
income = data$incom0

data$rincom0[data$rincom0 == 7777 | data$rincom0 == 9999] = 0
rincom0 = data$rincom0

data$rxtinc0[data$rxtinc0 == 7777 | data$rxtinc0 == 9999] = 0
rxtinc0 = data$rxtinc0

data$uxtinc0[data$uxtinc0 == 7777 | data$uxtinc0 == 9999] = 0
uxtinc0 = data$uxtinc0

#### 사회경제적 변수
lnincome = log(income)

data$educ[data$educ == 88] = NA
data$educ[data$educ == 8] = 0
educ = data$educ

partylr = 6 - data$partylr 
partylr[partylr == -2] = NA

data$marital[data$marital == 8] = NA
marital = data$marital

data$rank[data$rank == 88] = NA
rank = data$rank

data$urbrural[data$urbrural == 8] = NA
urbrural = data$urbrural

marital[marital == 4] = 2
marital[marital == 5 | marital == 6] = 3

marital2 = data$marital
marital2[marital2 == 2] = 1
marital2[marital2 == 3 | marital2 == 4] = 2
marital2[marital2 == 5 | marital2 == 6] = 3

#### 거주지역 집합적 효능 6개 변수
good2wlk = 6 - data$good2wlk 
good2wlk[good2wlk == -2] = NA

freshveg = 6 - data$freshveg
freshveg[freshveg == -2] = NA

facility = 6 - data$faclity
facility[facility == -2] = NA

safe = 6 - data$safe
safe[safe == -2] = NA

cares = 6 - data$cares
cares[cares == -2] = NA

wilnghlp = 6 - data$wilnghlp
wilnghlp[wilnghlp == -2] = NA

#### 거주지역 환경오염도
airpol = 5 - data$airpol
airpol[airpol == -3] = NA

waterpol = 5 - data$waterpol
waterpol[waterpol == -3] = NA

noise = 5 - data$noise
noise[noise == -3] = NA

#### 환경행동변수
recycle = 5 - data$recycle
recycle[recycle == -3] = NA
REavg = mean(recycle, na.rm = TRUE)

chemfree = 5 - data$chemfree
chemfree[chemfree == -3] = NA
CHEMavg = mean(chemfree, na.rm = TRUE)

redcheme = 5 - data$redcehme
redcheme[redcheme == -3] = NA
REDavg = mean(redcheme, na.rm = TRUE)

h2oless = 5 - data$h2oless
h2oless[h2oless == -3] = NA
H2Oavg = mean(h2oless, na.rm = TRUE)

nobuygrn = 5 - data$nobuygrn
nobuygrn[nobuygrn == -3] = NA
NOavg = mean(nobuygrn, na.rm = TRUE)

### 연령 카테고리화
agectg[data$age <= 29] = 20
agectg[data$age >= 30 & data$age < 40] = 30
agectg[data$age >= 40 & data$age < 50] = 40
agectg[data$age >= 50 & data$age < 60] = 50
agectg[data$age >= 60 & data$age < 70] = 60
agectg[data$age >= 70 & data$age < 80] = 70
agectg[data$age >= 80 & data$age < 100] = 80


## 환경운동
data$grnsign[data$grnsign == 8] = NA
grnsign = data$grnsign

data$grnmoney[data$grnmoney == 8] = NA
grnmoney = data$grnmoney

data$grndemo[data$grndemo == 8] = NA
grndemo = data$grndemo

data$grngroup[data$grngroup == 8] = NA
grngroup = data$grngroup

data$grncon[data$grncon == 8] = NA
grncon = data$grncon

carsgen = 6 - data$carsgen
carsgen[carsgen == -2] = NA

indusgen = 6 - data$indusgen
indusgen[indusgen == -2] = NA

chemgen = 6 - data$chemgen
chemgen[chemgen == -2] = NA

watergen = 6 - data$watergen
watergen[watergen == -2] = NA

tempgen = 6 - data$tempgen
tempgen[tempgen == -2] = NA

genegen = 6 - data$genegen
genegen[genegen == -2] = NA

nukegen = 6 - data$nukegen
nukegen[nukegen == -2] = NA

encon1 = (carsgen+ indusgen +watergen +tempgen +grncon)/6
encon2 = (carsgen+ indusgen+ chemgen +watergen +tempgen+ genegen + nukegen +grncon)/8
encon3 = (carsgen+ indusgen+ chemgen +watergen +tempgen)/6

# 환경변수 생성
CE = (good2wlk+ facility+ safe+ cares+ wilnghlp)/5
envrnact = ( recycle+ chemfree+ redcheme+ h2oless+ nobuygrn)/5
pollution =( airpol + waterpol + noise)/3
envact = (grnsign + grnmoney + grndemo + grngroup)/4

F1 = (good2wlk+ facility+ safe)/3
F2 = (cares+ wilnghlp)/2

# 추가변수
emply = data$emply
emply[data$emply == 1] = 0
emply[data$emply == 2] = 1
emply[data$emply == 8] = NA

healthy = data$healthy
healthy[healthy == 8] = NA

chronic = data$chronic
chronic = 2 - chronic 
chronic[data$chronic == -6] = NA


# 모델 만들기
sdt = data.frame(female, age, marital2, urbrural, income, lnincome, educ, partylr,
                 good2wlk, facility, safe, cares, wilnghlp,
                 airpol, waterpol, noise,
                 carsgen, indusgen, chemgen, watergen, tempgen, genegen, nukegen, grncon,
                 recycle, chemfree, redcheme, h2oless, nobuygrn,
                 encon2, pollution, CE, envrnact)

ldt = sdt[complete.cases(sdt) == TRUE & sdt$lnincome != -Inf , ]
summary(ldt)
summary(sdt)
sapply(sdt, length)
sapply(ldt, length)

m1 = lm(data = ldt, envrnact ~ factor(female) + age + factor(marital2) + urbrural + 
          lnincome + educ + partylr +
          encon2 + pollution)

m2 = lm(data = ldt, envrnact ~ factor(female) + age + factor(marital2) + urbrural + 
        lnincome + educ + partylr +
        encon2 + pollution + CE)

m3 = lm(data = ldt, envrnact ~ factor(female) + age + factor(marital2) + urbrural + 
        lnincome + educ + partylr +
        encon2 + pollution + CE + pollution*CE)

library(stargazer)
stargazer(m1, m2, m3, title="Model Results", 
          align=TRUE, no.space = FALSE,
          #dep.var.labels = c("Self-Reported Health"),
          #column.labels = c("beetween","pooled","fixed effect","random effect"),
          #covariate.labels = c("","") 
          type="text")









