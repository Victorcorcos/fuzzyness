# Explanation
The *tnorm_snorm_plots.r* file is an algorithmn writed in *R language* and designed to plot several different **t-norms** and **s-norms** operations to two **Fuzzy Sets**.

# Functions

1. **reload()** => Reload the code of the `tnorm_snorm_plots.r` file.
2. **print_fuzzy_sets()** => Show the two main **Fuzzy Sets**. Note that these **Fuzzy Sets** will be grouped by T-norms or S-norms later on.
3. **t_norms()** => Apply and Show the following T-norms...
```
Mínimo: min(x, y)
Produto Algébrico: x * y
Diferença Limitada (Lukasiewics): max(0, x+y-1)
Intersecção Drástica: y se x=1; x se y=1; 0 c.c
```
4. **s_norms()** => Apply and Show the following S-norms...
```
Máximo: max(x, y)
Soma Algébrica (Probabilística): x + y - x*y
Soma Limitada (Lukasiewicz): min(1, x+y)
União Drástica: x se y=0; y se x=0; 1 c.c
```

# T-norms
![t-norms](https://user-images.githubusercontent.com/7637806/45161934-3faf6b00-b1c3-11e8-90a2-dd7c69123482.png)

# S-norms
![s-norms](https://user-images.githubusercontent.com/7637806/45161938-4342f200-b1c3-11e8-8328-32e765a85ed0.png)

Dedicated for Fuzzy Logic and Systems.
