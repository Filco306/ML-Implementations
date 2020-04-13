
#' Decision Tree
#' 
#' Implements a decision tree
#' @param minimum_samples_split
#' @param min_impurity 
#' @param max_depth
#' @param criterion is how to split. Default is \code{gini}, but \code{deviance} is also available. 
#' @export
ClassificationTree = function(X, y, minimum_samples_split, min_impurity, max_depth = 5, criterion = "gini") {
  thisEnv <- environment()
  
  y = as.factor(y)
  n = nrow(X)
  
  if (criterion == "gini") {
    split_func = gini_index
  }
  
  
  tree_model = list(class = "ClassificationTree",
                  target_variable = y,
                  predictors = X,
                  getEnv = function()
                  {
                    return(get("thisEnv",thisEnv))
                  })
  
  
  # Time to build the tree structure
  # For each feature, calculate the maximum possible information gain and extract which split it is
  
  
  
  which.max(splits[2,])
  
  
  
  assign('this',tree_model, thisEnv)
  class(tree_model) = append(class(tree_model), "ClassificationTree")
  tree_model$train_preds = predict(tree_model, X)
  tree_model$train_probs = predict(tree_model, X, ret_probabilities = T)
  return(tree_model)
}

gini_index = function(ps) {
  #print(ps)
  return(sum(ps*(1 - ps)))
}






recursive_split = function(X, y, tree) {
  
  
}


#' @param conditions is a list of conditions that previously have been used. 
TreeNode = function(X, y, conditions, min_impurity = 0, min_samples_split = 4, verbose = T) {
  
  split_func = gini_index
  
  #conditions = NULL
  if (is.null(conditions)) {
    conditions = "1 == 1"
  }
  splits = apply(X,2,function(colu) {
    unique_col_vals = unique(colu)
    entropies = sapply(unique_col_vals, function(cl) {
      a = sapply(levels(y), function(clas) {
        if (cl == min(colu) || cl == max(colu)) { # Ignore the utmost points
          return(rep((1/3), times = 3))
        } else {
          expr2 = paste("colu <= cl", paste(conditions,collapse=" & "), sep = " & ")
          evalu = eval(parse(text = expr2))
          involved = colu[evalu]
          y_tmp = y[evalu]
          print(y_tmp)
          return(c(length(involved[y_tmp == clas])/length(involved), length(involved[y_tmp == clas])))
        }
      })
      print(a)
      return(c(split_func(a[1,]),sum(a[2,])))
    })
    entropies[which(is.na(entropies))] = Inf
    #print(entropies)
    return(c(Best_value = unique_col_vals[which.min(entropies[1,])], Entropy = min(entropies[1,]), NSamples = entropies[2,which.min(entropies[1,])]))}
  )
  thisEnv <- environment()
  print(splits)
  
  Feature = names(which.max(splits[,min(splits[2,]) == splits[2,]][3,]))
  
  node = list(class = "TreeNode",
              Feature = Feature,
              Criterion = paste("X$",Feature, " <= " , splits[1,Feature], sep = ""), 
              Left = NULL,
              Right = NULL,
              is_leaf = FALSE)
  assign('this',node, thisEnv)
  class(node) = append(class(node), "TreeNode")
  
  if (verbose) {
    print("Picked ")
    print(node$Feature)
    print(node$Criterion)
  }
  print(splits[3,which.min(splits[2,])])
  if (min_samples_split < splits[3,which.min(splits[2,])] & min_impurity*(-1) <= splits[2,which.min(splits[2,])] & splits[2,which.min(splits[2,])] != 0 || length(conditions) == 1) {
    node$Left = TreeNode(X, y, conditions = c(conditions,paste("X$",colnames(splits)[which.min(splits[2,])], " <= " , splits[1,which.min(splits[2,])], sep = "")))
    node$Right = TreeNode(X, y, conditions = c(conditions,paste("X$",colnames(splits)[which.min(splits[2,])], " > " , splits[1,which.min(splits[2,])], sep = "")))
  } else {
    node$is_leaf = TRUE
    node$mark = table(y[eval(parse(text = paste(conditions, collapse = " & ")))])/sum(table(y[eval(parse(text = paste(conditions, collapse = " & ")))]))
  }
  return(node)
}
