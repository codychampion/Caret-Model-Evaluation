# Caret Model Evaluation

The caret package [(wiki)](http://topepo.github.io/caret/index.html) unifies the syntax for about 200 classification and regression models.  However, picking a model is often a subjective task.  This package aims to provide an example of how to iterate through all available models and quantify accuracy.  


# Install
Simply grab from github.
```r
library(devtools)
install_github("codychampion/Caret-Model-Evaluation")
```

Once the package is installed make sure you have a complete installation of caret and ALL dependencies.  This can be done using this command:

```r
install.packages("caret",
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))
```


# Usage

Use case is shown below:

```r
library(datasets)
data <- data(iris)
target <- "Septal.Length"
modeleval(target, data, timeouttime = 60)
```
Other important parameters are given in function discription. 

# Example output 

The following is a graph showing optimal models for the iris dataset for Sepal length prediction as evaulesed by R2.

![Example using the iris dataset with Sepal.Length as target](https://github.com/codychampion/Caret-Model-Evaluation/blob/master/irislength.png)

# Issues
Some issues with this package are it will not work with the timeout option on Windows, Linux is needed.

---
