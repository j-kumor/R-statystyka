p_fail = 0.05

x <- 0:1

pmf <- dbinom(x, size = 1, prob = p_fail)
cdf <- pbinom(x, 1, p_fail) 
df_bernoulli <- data.frame(status = x, PMF = pmf, CDF = cdf)

library(ggplot2)

ggplot(df_bernoulli, aes(x=status,y=PMF)) + 
  geom_col(fill = "lightpink") +
  geom_text(aes(label = PMF), vjust = -0.5)

ggplot(df_bernoulli, aes(x=status,y=CDF)) + 
  geom_col(fill = "lightpink") +
  geom_text(aes(label = CDF), vjust = -0.5)

E_bernoulli = p_fail
V_bernoulli = p_fail*(1-p_fail)

### rozklad dwumianowy
n = 20
p_val = 0.4

x = 0:n

pmf_binom = dbinom(x, size = n, prob = p_val)
cdf_binom = pbinom(x, size = n, prob = p_val)

df_binom = data.frame(sukcesy = x, PMF = pmf_binom, CDF = cdf_binom)

ggplot(df_binom, aes(x=sukcesy,y=PMF)) + 
  geom_col(fill = "lightpink") +
  geom_text(aes(label = PMF), vjust = -0.5)

ggplot(df_binom, aes(x=sukcesy,y=CDF)) + 
  geom_col(fill = "lightpink") ### bez geom text nie ma liczb

E_binom = n*p_val
V_binom = n*p_val*(1-p_val)
sqrt(V_binom)

pbinom(10, size = 20, prob = p_val)
###zad na EGZAMINIE jakie prawdopodobienstwo ze mniej niz 10 produktow przejdzie pozytywnie test
1 - pbinom(10, size = 20, prob = p_val)

### rozklad Poisson

lambda = 3

x = 0:15

pmf_pois = dpois(x, lambda)
cdf_pois = ppois(x, lambda)
df_pois = data.frame(bledy = x, PMF = pmf_pois, CDF = cdf_pois)

ggplot(df_pois, aes(x=bledy,y=PMF)) + 
  geom_col(fill = "lightpink")

ggplot(df_pois, aes(x=bledy,y=CDF)) + 
  geom_col(fill = "lightpink")

E_pois = lambda
V_pois = lambda

### jakie praw. ze srednia liczba bledow na godzine nie przekroczy 4 
ppois(4,lambda)

### rozklad jednostajny
a=0
b=1
x=seq(a,b,length.out = 1000)

pdf_unit = dunif(x,min = a, max = b)
cdf_unit = punif(x,min = a, max = b)

df_unit = data.frame(x=x,PDF = pdf_unit, CDF = cdf_unit)
head(df_unit, n = 10) ### n=10 wyswietla 10 wartosci zamiast 6
tail(df_unit)

ggplot(df_unit, aes(x = x, y = PDF)) +
  geom_line()

ggplot(df_unit, aes(x = x, y = CDF)) +
  geom_line() 

E_unif = (a+b) / 2 #nie wyswietla
V_unif = (b-a)^2 /12 #nie wyswietla

punif(0.75,a,b)

### rozklad normalny
mu = 20
sigma = 2

x = seq(mu - 3 * sigma, mu+3*sigma, length.out = 1000)

pdf_norm = dnorm(x, mean = mu, sd = sigma)
cdf_norm = pnorm(x, mean = mu, sd = sigma)

df_norm = data.frame(x, PDF = pdf_norm, CDF = cdf_norm)

ggplot(df_norm, aes(x,y = PDF)) +
  geom_line()


ggplot(df_norm, aes(x,y = CDF)) +
  geom_line()


###SEGMENT Z CHATU
# P(x < 22)

p <- pnorm(22, mean = mu, sd = sigma)
print(p)

# P(x > 22)

p <- 1 - pnorm(22, mean = mu, sd = sigma)
print(p)

# P( X > 22) i P(x <24)
pnorm(24, mean = mu, sd = sigma) - pnorm(22, mean = mu, sd = sigma)

# P(X < 16) i P(x>24)
pnorm(16, mu, sigma) + (1 - pnorm(24, mu, sigma))

### rozklad t-studenta

df_val = 10
x = seq(-4,4,length.out = 1000)

pdf_t = dt(x, df = df_val)
cdf_t = pt(x, df = df_val)

df_t = data.frame(x=x, PDF=pdf_t, CDF=cdf_t)

ggplot(df_t, aes(x,PDF)) +
  geom_line(size=2)

ggplot(df_t, aes(x,CDF)) +
  geom_line(size=2)

E_t = 0
V_t = df_val / (df_val - 2)
print(V_t)

# P(x > 1)
1 - pt(1, df = df_val)

# P(x < 1)
pt(1, df = df_val)

#P(x > -1) i P(x <1)
pt(1, df = df_val) - pt(-1, df = df_val)

#P(x < -2) i P(x> 2)
pt(-2, df = df_val) + (1 - pt(2, df = df_val))

### rozklad chi^2

k = 5
x = seq(0,20,length.out = 1000)

pdf_chi2 = dchisq(x, df=k)
cdf_chi2 = pchisq(x, df=k)

df_chi2 = data.frame(x=x, PDF=pdf_chi2,CDF=cdf_chi2)

ggplot(df_chi2, aes(x,PDF)) +
  geom_line(size = 2)

ggplot(df_chi2, aes(x,CDF)) +
  geom_line(size = 2)

E_chisq = k
V_chisq = 2*k

#P(x<7)
pchisq(7, df = k)

#P(x>7)
1 - pchisq(7, df = k)


