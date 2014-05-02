#### combine base and grid graphics

```
## option 1
op <- par(no.readonly = TRUE)
par(mar = c(4, 4, 3, 2))

par(mfrow = c(2, 2))
hist(mtcars$mpg)
boxplot(mpg ~ cyl, data = mtcars)
barplot(table(mtcars$am, mtcars$gear))

require(grid)
vp <- viewport(height = unit(.5, "npc"), width = unit(0.5, "npc"), 
               just = c("left", "top"), 
               y = 0.5, x = 0.5)

require(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt, colour = factor(am))) + geom_point() + theme_bw()
print(p, vp = vp)

par(op)
```

![option1](https://raw.githubusercontent.com/raredd/rgraphics/master/figure/option1.png)

```
## option 2
op <- par(no.readonly = TRUE)
par(mar = c(4, 4, 3, 2))

par(mfrow=c(2, 2))
hist(mtcars$mpg)
boxplot(mpg ~ cyl, data = mtcars)
barplot(table(mtcars$am, mtcars$gear))

require(gridBase)
plot.new()
vp <- baseViewports()
pushViewport(vp$figure)
vp <-plotViewport(c(.5, .5, 0, .5))
require(ggplot2)
p <- ggplot(mtcars, aes(mpg, wt, colour = factor(am))) + geom_point() + theme_bw()
print(p, vp = vp)

par(op)
```

![option2](https://raw.githubusercontent.com/raredd/rgraphics/master/figure/option2.png)