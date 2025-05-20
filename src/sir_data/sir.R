p_SI <- 1 - exp(-beta * I / N * dt)
p_IR <- 1 - exp(-gamma * dt)
n_IR <- Binomial(I, p_IR)
n_SI <- Binomial(S, p_SI)

update(S) <- S - n_SI
update(I) <- I + n_SI - n_IR
update(R) <- R + n_IR
update(cases_cumul) <- cases_cumul + n_SI
update(cases_inc) <- cases_inc + n_SI

I0 <- min(Poisson(lambda), N)
initial(S) <- N - I0
initial(R) <- 0
initial(I) <- I0
initial(cases_cumul) <- 0
initial(cases_inc, zero_every = 1) <- 0

beta <- parameter()
gamma <- parameter()
alpha <- parameter()
N <- parameter()
lambda <- parameter()

exp_noise <- parameter(1e+06)
cases <- data()

modelled_cases <- cases_inc + Exponential(exp_noise)
cases ~ NegativeBinomial(size = 1 / alpha, mu = modelled_cases)
