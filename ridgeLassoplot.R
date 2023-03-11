library(ggplot2)
library(glmnet)

# Generar datos aleatorios
set.seed(123)
n <- 100
x <- rnorm(n)
y <- 2*x + rnorm(n)

# Regresión lineal clásica
lm.fit <- lm(y ~ x)

# Regresión ridge con diferentes valores de lambda
ridge.fit1 <- glmnet(as.matrix(cbind(1, x)), y, alpha=0, lambda=0)
ridge.fit2 <- glmnet(as.matrix(cbind(1, x)), y, alpha=0, lambda=0.1)
ridge.fit3 <- glmnet(as.matrix(cbind(1, x)), y, alpha=0, lambda=1)


# Regresión lasso con diferentes valores de lambda
lasso.fit1 <- glmnet(as.matrix(cbind(1, x)), y, alpha=1, lambda=0)
lasso.fit2 <- glmnet(as.matrix(cbind(1, x)), y, alpha=1, lambda=0.1)
lasso.fit3 <- glmnet(as.matrix(cbind(1, x)), y, alpha=1, lambda=1)

# Coeficientes ajustados para cada modelo
lm.coef <- coef(lm.fit)
ridge.coef1 <- coef(ridge.fit1)
ridge.coef2 <- coef(ridge.fit2)
ridge.coef3 <- coef(ridge.fit3)

lasso.coef1 <- coef(lasso.fit1)
lasso.coef2 <- coef(lasso.fit2)
lasso.coef3 <- coef(lasso.fit3)

# Predicciones de cada modelo
x_new <- seq(min(x), max(x), length.out = 100)
lm.predict <- lm.coef[1] + lm.coef[2] * x_new
ridge.predict1 <- predict(ridge.fit1, newx = as.matrix(cbind(1, x_new)))
ridge.predict2 <- predict(ridge.fit2, newx = as.matrix(cbind(1, x_new)))
ridge.predict3 <- predict(ridge.fit3, newx = as.matrix(cbind(1, x_new)))

lasso.predict1 <- predict(lasso.fit1, newx = as.matrix(cbind(1, x_new)))
lasso.predict2 <- predict(lasso.fit2, newx = as.matrix(cbind(1, x_new)))
lasso.predict3 <- predict(lasso.fit3, newx = as.matrix(cbind(1, x_new)))

# Combinar datos en un data frame
data <- data.frame(x = x, s0 = y, model = rep("observaciones", n),
                   type = rep("OLS", 100),
                   prediction = rep(lm.predict, each = 1))
data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict1, model = rep("Ridge (lambda = 0)", length(x_new)), type = rep("Ridge", 100),
                         prediction = rep(ridge.predict1, each = 1)))
data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict2, model = rep("Ridge (lambda = 0.1)", length(x_new)),type = rep("Ridge", length(x_new)),
                         prediction = rep(ridge.predict2, each = 1)))
data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict3, model = rep("Ridge (lambda = 1)", length(x_new)),type = rep("Ridge", length(x_new)),
                         prediction = rep(ridge.predict3, each = 1)))

data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict3, model = rep("Lasso (lambda = 0)", length(x_new)),type = rep("Lasso", length(x_new)),
                         prediction = rep(ridge.predict3, each = 1)))

data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict3, model = rep("Lasso (lambda = 0.1)", length(x_new)),type = rep("Lasso", length(x_new)),
                         prediction = rep(ridge.predict3, each = 1)))

data <- rbind(data,
              data.frame(x = x_new, y = ridge.predict3, model = rep("Lasso (lambda = 1)", length(x_new)),type = rep("Lasso", length(x_new)),
                         prediction = rep(ridge.predict3, each = 1)))

# Crear gráfico con ggplot
ggplot(data = data, aes(x, s0)) +
  geom_point(alpha = 0.5) +
  geom_line(data = data %>% 
             filter(model !="observaciones" & type == "Ridge"),  aes(y = prediction, color = model), size = 0.9) +
  geom_line(data = data %>% 
              filter(model !="observaciones" & type == "Lasso"),  aes(y = prediction, color = model), size = 0.9) +
  
  labs(title = "Regresión lineal clásica vs Regresión Ridge vs Regresión Lasso",
       x = "x", y = "y", color = "Modelo") +
  theme_minimal() +
  geom_smooth(data=data %>% 
                filter(model == "observaciones"), method="lm", color="black",se = FALSE)+
  facet_wrap(~type) +
  scale_color_material_d()
