sim1 %>%
  ggplot(aes(x,y)) +
  geom_point()


models <- tibble(
              a1 = runif(250, -20, 40),
              a2 = runif(250, -5, 5)
          )

models

sim1 %>%
  ggplot(aes(x,y)) +
  geom_abline(
    data = models,
    mapping = aes(intercept = a1, slope = a2),
    alpha = 1/4
  ) +
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    data = filter(models, rank(dist) <= 10),
    mapping = aes(intercept = a1, slope = a2, color = -dist)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), 
             size = 4, 
             color = "red") +
  geom_point(aes(color = -dist))

best <- optim(c(0,0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x,y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])


sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)


# --------

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
          data_grid(x1, x2) %>%
          gather_predictions(mod1, mod2)

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~model)

sim3 <- sim3 %>%
          gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model~x2)

mod3 <- lm(y ~ x1 + x2, data = sim4)
mod4 <- lm(y ~ x1 * x2, data = sim4)

grid2 <- sim4 %>%
            data_grid(
              x1 = seq_range(x1, 5),
              x2 = seq_range(x2, 5)
            ) %>%
            gather_predictions(mod3, mod4)

ggplot(grid2, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~model)

ggplot(grid2, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~model)

ggplot(grid2, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~model)
