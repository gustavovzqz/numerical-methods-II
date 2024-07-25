# Preditor-corretor: desenvolvimento

A seguinte expressão base deriva os métodos de predição e correção:

$$
S_{i+1} = S_i + \int_{t_i}^{t_{i+1}} F(S(t), t) dt
$$

O passo seguinte, é reescrever a derivada do estado como uma aproximação:

$$
\frac{dS(t_i)}{dt} = F(S(t_i), t_i) \approx F(S_i, t_i)
$$

Supondo que uma aproximação de $\frac{dS(t_i)}{dt} é conhecida e chamada de $g(t)$. Elá poderá ser usada em:

$$
\int_{t_i}^{t_{i+1}} F(S(t), t) dt = \int_{t_i}^{t_{i+1}} \frac{dS(t)}{dt} \approx \int_{t_i}^{t_{i+1}} g(t)dt.
$$

Assim, finalmente podemos reescrever a expressão base da seguinte maneira:

$$
S_{i+1} = S_i + \int_{t_i}^{t_{i+1}} g(t) dt.
$$

Note que a nossa função $g(t)$ será uma aproximação. A maneira que vamos utilizar é definir a função $g(t)$ como uma função de interpolação que passa pelas derivadas dos últimos $k+1$ estados. 




