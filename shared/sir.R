p_SI <- 1 - exp(-beta * I / N * dt)
p_IR <- 1 - exp(-gamma * dt)
n_IR <- rbinom(I, p_IR)
n_SI <- rbinom(S, p_SI)

update(time) <- (step + 1) * dt
update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR
update(cases_cumul) <- cases_cumul + n_SI
update(cases_inc) <- if (step %% freq == 0) n_SI else cases_inc + n_SI

initial(time) <- 0
initial(S) <- N - I0
initial(R) <- 0
initial(I) <- I0
initial(cases_cumul) <- 0
initial(cases_inc) <- 0

beta <- user()
gamma <- user()
alpha <- user()
rho <- user()
N <- user(1000)
I0 <- user(10)

freq <- user(4)
dt <- 1.0 / freq

exp_noise <- user(1e6)
cases <- data()

modelled_cases <- cases_inc + rexp(exp_noise)
compare(cases) ~ negative_binomial_mu(1 / alpha, modelled_cases)

n_positives <- data()
n_tests <- data()

model_prob_pos <- I / N
compare(n_positives) ~ beta_binomial(n_tests, model_prob_pos, rho)
