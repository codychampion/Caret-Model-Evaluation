require(caret)
#' Caret model evaulation
#'
#' @param target the target vaiable is characture format
#' @param data the data frame of intrest
#' @param timeouttime the time in seconds each mode is permitted to run
#' @param metric metric of accuracy possibe terms are RMSE, R2, MAE for regression, and Accuracy or Kappa for classification
#' @param holdout hold out percentage for the training set, .8 is 80% of data used for training
#' @param trainmetod method of training ex repeatedcv
#' @param number in train control how many number
#' @param repeats in train control how many repeats
#' @param order the order of sorting
#'
#' @return a dataframe with model and accuracy metric sorted
#' @export
#'
#' @examples
#'
#'
#'
#'
modeleval <- function(target, data, timeouttime = 60, metric, holdout = .8, trainmethod = "repeatedcv", number = 5, repeats = 3, order = "accending") {

  #create a training/testing dataset
  index <- createDataPartition(subset(data, select = target)[,1], p = holdout, list = FALSE)
  train <- data[index,]
  test <- data[-index,]


  # get all caret models for regression
  modNamesreg <- unique(modelLookup()[modelLookup()$forReg, c(1)])
  # get all caret models for classification
  modNamesclass <- unique(modelLookup()[modelLookup()$forClass, c(1)])

  #if the target is a factor then use the classification models and if not factor then use regression
  ifelse(is.factor(subset(data, select = target)[,1]) == T, modNames <- modNamesclass, modNames <- modNamesreg)

  #We interate through all the models in provided list
  i <- 1
  for(i in 1:length(modNames)){
    control <- trainControl(
      method = trainmethod,
      number = number,
      repeats = repeats
    )

    #We can define accuracy throught several different metrics
    accuracy <- function(target, train, test, modNames, i, metric) {
      TrainData <- train[!rownames(train) %in% target, ]
      TrainClasses <- subset(train, select = c(target))
      
      model <- train(
        x = TrainData, 
        y = TrainClasses,
        data = train,
        method = modNames[i],
        trControl = control
      )

      #Find the predicted vaules from the test data set
      pred <- predict(model, test)

      #If using regression then we can find RMSE, R2 or MAE
      if(is.factor(subset(data, select = target)[,1]) == F){

        accfull <-
          as.numeric(postResample(pred = pred, obs = subset(test, select = target)[,1]))

        if(metric == "RMSE"){acctmp <- accfull[1]}
        if(metric == "R2"){acctmp <- accfull[2]}
        if(metric == "MAE"){acctmp <- accfull[3]}
      }

      #if using classification then Accuracy or Kappa is used
      if(is.factor(subset(data, select = target)[,1]) == T){

        bound <- cbind(pred, subset(test, select = target))
        names(bound) <- c("pred", "obs")
        accfull <- as.numeric(defaultSummary(bound))
        if(metric == "Accuracy"){acctmp <- accfull[1]}
        if(metric == "Kappa"){acctmp <- accfull[2]}

      }
      #retun of this function is a number
      return(acctmp)

    }


    #Error handing is difficult with Caret but we use trycatch to set a bad model run with a null vaule
    acc <- NULL

    eval_with_timeout <- function(expr, envir = parent.frame(), timeout, on_timeout = c("error", "warning", "silent")) {
      # substitute expression so it is not executed as soon it is used
      expr <- substitute(expr)

      # match on_timeout
      on_timeout <- match.arg(on_timeout)

      # execute expr in separate fork
      myfork <- parallel::mcparallel({
        eval(expr, envir = envir)
      }, silent = FALSE)

      # wait max n seconds for a result.
      myresult <-
        parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
      # kill fork after collect has returned
      tools::pskill(myfork$pid, tools::SIGKILL)
      tools::pskill(-1 * myfork$pid, tools::SIGKILL)

      # clean up:
      parallel::mccollect(myfork, wait = FALSE)

      # timeout?
      if (is.null(myresult)) {
        if (on_timeout == "error") {
          stop("reached elapsed time limit")
        } else if (on_timeout == "warning") {
          warning("reached elapsed time limit")
        } else if (on_timeout == "silent") {
          myresult <- NA
        }
      }

      # move this to distinguish between timeout and NULL returns
      myresult <- myresult[[1]]

      if ("try-error" %in% class(myresult)) {
        stop(attr(myresult, "condition"))
      }

      # send the buffered response
      return(myresult)
    }


    tryCatch({
      acc <- eval_with_timeout(accuracy(target, train, test, modNames, i, metric)
                               , timeout = timeouttime)
    }, error = function(e)
      e)


    if (is.null(acc) == T) {
      print("Error or Time out")
    }

    ifelse(
      is.null(acc) == F,
      out <-
        data.frame(
          "model" = modNames[i],
          "placeholder_name" = acc
        ),
      out <-
        data.frame(
          "model" = modNames[i],
          "placeholder_name" = NA
        )
    )

    #since we cannot directly set the coulmn name we set it here
    names(out)[2] <- metric

    #this creates a data frame to bind to it this is the first interation
    ifelse(i == 1, outfinal <- out, outfinal <- rbind(outfinal, out))

  }

  #This returns an orderlist of
  complete <- outfinal[complete.cases(outfinal),]
  if(order == "accending"){outfinal <- complete[order(-complete[,2]),]}
  if(order == "decending"){outfinal <- complete[order(complete[,2]),]}

  return(outfinal)
}
