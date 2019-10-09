# Caret Model Evaluation

The caret package [(wiki)](http://topepo.github.io/caret/index.html) unifies the syntax for about 200 classification and regression models.  However, picking a model is often a subjective task.  This package aims to provide an example of how to iterate through all available models and quantify accuracy.  

Use case is shown below:

```r
library(datasets)
data <- data(iris)
target <- "Septal.Length"
modeleval(target, data, timeouttime = 60)
```
Other important parameters are given in function discription. 


![Example using the iris dataset with Sepal.Length as target](https://github.com/codychampion/Caret-Model-Evaluation/blob/master/irislength.png)

# Install
Simply grab from github.
```r
library(devtools)
install_github("codychampion/Caret-Model-Evaluation")
```

Once the package is installed make sure you have a complete installation of caret and ALL dependencies.




Some issues with this package are it will not work with the timeout option on Windows, Linux is needed.  Additionally, caret and all dependencies need to be installed (use caret-setup-deLuxe.R).

---
