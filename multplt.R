```{r}
#| echo: false
n1 <- rep(1:100,2)
a1 <- 0
b1 <- 1
sigma21 <- n1^1.3
eps1 <- rnorm(n1,mean=0,sd=10)
y1 <- a1 + b1 * n1 + eps1
datos1 <- data.frame(y1,n1)

model1 <- lm(y1~n1)

png(filename = "figs/multiplot.png",
    width = 18,
    height = 22, units = "cm", res=600)
performance::check_model(model1)
dev.off()
ggsave(filename = "figs/multiplot.png", plot = plote, width = 16,
       height = 20, units = "cm")
```
