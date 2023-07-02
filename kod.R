install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
example(stan_model, package = "rstan", run.dontrun = TRUE)

library(gridExtra)
library(ggplot2)
library(rstan)
library(dplyr)
library(plotly)

dane <- read.csv("Housing.csv", header = TRUE, sep = ",")
dane <- cbind(dane["CRIM"], dane["RM"], dane["LSTAT"], dane["PTRATIO"], dane["MEDV"])

#Statystyki opisowe
summary(dane)

#Wykres 1 - MEDV & CRIM (wskaźnik przestępczości na mieszkańca według miasta)
wykres1 <- ggplot(dane, aes(CRIM, MEDV)) +
  geom_point() +
  geom_point(
   colour = 'maroon', size = 1
  )

#Wykres 2 - MEDV & RM (średnia liczba pokoi na mieszkanie)
wykres2 <- ggplot(dane, aes(RM, MEDV)) +
  geom_point() +
  geom_point(
    colour = 'royalblue', size = 1
  )

#Wykres 3 - MEDV & LSTAT (% ludności klasy niższej)
wykres3 <- ggplot(dane, aes(LSTAT, MEDV)) +
  geom_point() +
  geom_point(
    colour = 'gold', size = 1
  )

#Wykres 4 - MEDV & PTRATIO (stosunek liczby uczniów do nauczycieli według miasta)
wykres4 <- ggplot(dane, aes(PTRATIO, MEDV)) +
  geom_point() +
  geom_point(
    colour = '#808000', size = 1
  )

layout = matrix(c(1:4), nrow = 2, byrow = FALSE)
marrangeGrob(
  grid.arrange(wykres1,wykres2,wykres3,wykres4, ncol=2), 
  top = NULL, layout_matrix = layout)

#korelacja
korelacja<-c(cor(dane$CRIM, dane$MEDV), cor(dane$RM,dane$MEDV),
             cor(dane$LSTAT, dane$MEDV), cor(dane$PTRATIO,dane$MEDV))
df<-data.frame(korelacja, row.names = c("CRIM","RM","LSTAT","PTRATIO"))

#Histogramy dla zmiennych
hist_CRIM <- ggplot(dane, aes(CRIM)) +
  geom_histogram() +
  geom_histogram(
    colour = 'black', fill = 'maroon'
)

hist_RM <- ggplot(dane, aes(RM)) +
  geom_histogram() +
  geom_histogram(
    colour = 'black', fill = 'royalblue'
  )

hist_LSTAT <- ggplot(dane, aes(LSTAT)) +
  geom_histogram() +
  geom_histogram(
    colour = 'black', fill = 'gold'
  )

hist_PTRATIO <- ggplot(dane, aes(PTRATIO)) +
  geom_histogram() +
  geom_histogram(
    colour = 'black', fill = '#808000'
  )

layout = matrix(c(1:4), nrow = 2, byrow = FALSE)
marrangeGrob(
  grid.arrange(hist_CRIM, hist_RM, hist_LSTAT, hist_PTRATIO, ncol=2), 
  top = NULL, layout_matrix = layout)

#MEDV - mediana wartości domów zajmowanych przez właścicieli w tys. $
hist_MEDV <- ggplot(dane, aes(MEDV)) +
  geom_histogram() +
  geom_histogram(
  colour = 'black', fill = '#d381e3'
  )

#szacowanie rozkładów a priori
rozkład_CRIM <- curve(dcauchy(x, -0.9, 1), -2, 1)
rozkład_RM <- curve(dnorm(x, 10, 2), 5, 15)
rozkład_LSTAT <- curve(dcauchy(x, -1.7, 1), -4, 1)
rozkład_PTRATIO <- curve(dnorm(x, -0.3, 0.3), -1, 1)

#Estymacja modelu  
model <- stan_model("regresja.stan")

fit <-  sampling(model, list(N = nrow(dane), 
                             CRIM=dane$CRIM,
                             MEDV=dane$MEDV,
                             RM=dane$RM,
                             LSTAT=dane$LSTAT,
                             PTRATIO=dane$PTRATIO), iter=5000, chains=4)

post<-extract(fit)
fit

#Rozkłady brzegowe i łączne wybranych par i trójek parametrów
stan_hist(fit, fill="royalblue")
pairs(fit, pars = c("intercept", "beta_CRIM", "beta_RM","beta_LSTAT", "beta_PTRATIO", "sigma"))

plot_ly(x = post$intercept, y = post$beta_RM, z = post$beta_LSTAT, type = "scatter3d", 
        mode="markers", marker=list(size=1))
plot_ly(x = post$intercept, y = post$beta_RM, z = post$beta_PTRATIO, type = "scatter3d", 
        mode="markers", marker=list(size=1))


#porównanie rozkładów a priori i a posteriori
{
ggplot() +
  geom_density(aes(x = rnorm(10000, 0, 1), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$intercept, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru intercept") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))

ggplot() +
  geom_density(aes(x = rcauchy(10000, -0.9, 1), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$beta_CRIM, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru beta_CRIM") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))+xlim(-1.5, 1)

ggplot() +
  geom_density(aes(x = rnorm(10000, 10, 2), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$beta_RM, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru beta_RM") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))

ggplot() +
  geom_density(aes(x = rcauchy(10000, -1.7, 1), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$beta_LSTAT, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru beta_LSTAT") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))+xlim(-2.5, 1)

ggplot() +
  geom_density(aes(x = rnorm(10000, -0.3, 0.3), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$beta_PTRATIO, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru beta_PTRATIO") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))

ggplot() +
  geom_density(aes(x = rnorm(10000, 0, 10), colour = factor("a priori")), fill = "royalblue", alpha = 0.3) +
  geom_density(aes(x = post$sigma, colour = factor("a posteriori")), fill = "maroon", alpha = 0.3) +
  labs(title = "Rozkład a priori i a posteriori parametru sigma") +
  theme_grey() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_colour_manual(name = "Rozkład:", values = c("a priori" = "royalblue", "a posteriori" = "maroon"),
                      guide = guide_legend(override.aes = list(fill = c("maroon", "royalblue"))))+xlim(-7, 7)
}


#Weryfikacja hipotezy o dodatnich parametrach na podstawie próbki z rozkładów a posteriori parametrów
sum(post$beta_CRIM>0)/length(post$beta_CRIM)
sum(post$beta_RM>0)/length(post$beta_RM)
sum(post$beta_LSTAT>0)/length(post$beta_LSTAT)
sum(post$beta_PTRATIO>0)/length(post$beta_PTRATIO)

#Prognoza

#Prognoza punktowa
mean(post$intercept) + mean(post$beta_CRIM)*0.98843 + 
  mean(post$beta_RM)*5.813 + mean(post$beta_LSTAT)*19.88 +
  mean(post$beta_PTRATIO)*21.0

#Prognoza z uwględnieniem niepewności składnika losowego:
hist(rnorm(10000, mean(post$intercept) + mean(post$beta_CRIM)*0.98843 + 
             mean(post$beta_RM)*5.813 + mean(post$beta_LSTAT)*19.88 +
             mean(post$beta_PTRATIO)*21.0, mean(post$sigma)), 
     main = "", xlab = "", col = 'royal blue')

#Prognoza z uwględnieniem niepewności składnika losowego i parametrów:
hist(rnorm(10000, post$intercept + post$beta_CRIM*0.98843 + 
             post$beta_RM*5.813 + post$beta_LSTAT*19.88 +
             post$beta_PTRATIO*21.0, post$sigma), 
     main = "",xlab = "", col = 'royal blue')





