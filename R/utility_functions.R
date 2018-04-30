library(data.table)
library(forecast)

# split_str<-function(var, col) {
#   'This function will try to split a string by the given column, to get the value of the categorical variable from "var"'
#   out <- tryCatch(
#     {
#       if (col == ':') {
#         strsplit(var, col)[[1]]
#       }
#       else {
#         strsplit(var, col)[[1]][2]
#       }
#     },
#     error <- function(cond) {
#       return(-1)
#     }
#   )
#   return(out)
# }

# em = re.compile(r'(\xf0\S*)')
# rep = re.compile("[\uD800-\uDFFF]", re.UNICODE)
# capture_emojis <- function(s1) {
#   s = s1.encode('utf-8')
#   aux = []
#   emojis = em.findall(s)
#   for elem in emojis:
#     elem = rep.sub('', elem)
#   while len(elem) > 4:
#     aux.append(elem[0:4])
#   elem = elem[4:]
#   aux.append(elem)
#   return ','.join(aux)
#   df['emojis'] = df.apply(lambda x: capture_emojis(x.text), axis = 1)
# } 

get_value <- function(cols, elem) {
  j <- 0
  value <- NA
  while(is.na(value) & j <= length(cols)) {
    j <- j + 1
    value <- strsplit(elem, cols[[j]])[[1]][2]
  }
  return(list(value, cols[[j]]))
}

format_varnames <- function(cols, vars) {
  'A function to format the values inside a vars vector, which are the significant variables obtained from a glm model'
  'We need variables in the form: "I(colname == value)"'
  ind_pca <- -1
  final_vars <- vector("list", length(vars))
  i <- 1
  for (elem in vars) {
    # If the element is not a factor column, we will include it in the new model as it is
    if (substr(elem, 1, 2) == "I(") {
      final_vars[[i]] <- gsub("TRUE", "", elem)
      i <- i + 1
    }
    else if(elem %in% cols) {
      final_vars[[i]] <- elem
      i <- i + 1
    }
    else if (substr(elem, 1, 3) == "pca") {
      if (ind_pca == -1) {
        final_vars[[i]] <- "pca" #paste("I(pca == \"", substr(elem, 4, 10), "\")", sep = "")
      }
      i <- i+1
    }
    else if(substr(elem, 1, 3) == "bs(") {
      values <- strsplit(elem, ')))')
      toappend <- paste("I(", values[[1]][[1]], "))) == \"", values[[1]][[2]], "\")", sep = "")
      final_vars[[i]] <- toappend
      i <- i + 1
    }
    # If not we have to process the name. Two options here: we are looking at a categorical variable or we are looking at an interaction.
    #   Categorical => We have to extract the value from the variable name.
    #   Interaction => We have to split by : and extract both values of both variables.
    else {
      ind_inter <- strsplit(elem, ':')[[1]]
      if (length(ind_inter) == 1) {
        values <- get_value(cols, elem)
        toappend <- paste("I(", values[2], " == \"", values[1], "\")", sep = "")
      }
      else {
        values1 <- get_value(cols, ind_inter[1])
        values2 <- get_value(cols, ind_inter[2])
        toappend <- paste("I(", values1[2], " == \"", values1[1], "\" & ", values2[2], " == \"", values2[1], "\")", sep = "")
      }
      final_vars[[i]] <- toappend
      i <- i + 1
    }
  }
  return(final_vars)
}

optimize_model <- function(model, data) {
  sum <- summary(model)
  vars = names(coef(sum)[-1,4][(!is.na(coef(sum)[-1,4]))])
  final_vars <- format_varnames(names(data), vars)
  sig.formula <- as.formula(paste("label ~" ,paste(final_vars, collapse= "+")))
  model <- glm(formula = sig.formula, data = data, family = 'binomial')
  sum <- summary(model)
  
  while(max(coef(sum)[-1,4]) >= 0.05) {
    vars = names(coef(sum)[-1,4][(coef(sum)[-1,4] <= 0.05)])
    final_vars <- format_varnames(names(data), vars)
    sig.formula <- as.formula(paste("label ~" ,paste(final_vars, collapse= "+")))
    model <- glm(formula = sig.formula, data = data, family = 'binomial')
    sum <- summary(model)
  }
  return(model)
}

normality_analysis <- function(data) {
  qqnorm(data)
  qqline(data)
  if (length(data) > 5000) {
    test <- shapiro.test(data[0:5000])
  }
  else {
    test <- shapiro.test(data)
  }
  return(test)
}

get_lambdas <- function(df) {
  lambdas <- vector("list", length(names(df)))
  names(lambdas) <- names(df)
  i <- 1
  for (col in names(df)) {
    lambda2<- BoxCox.lambda(df[,col], method = "loglik", lower = -20, upper = 20)
    df[,paste(col, 'normal', sep = '_')] <- BoxCox(df[,col], lambda2)
    lambdas[[i]] <-lambda2
    i <- i + 1
  }
  return(lambdas)
}

remove_nzv <- function(data) {
  nzv <- nearZeroVar(data)
 if (length(nzv) > 0) {
    df_aux <- data[,-nzv]
 }
  else {
    df_aux <- data
  }
  return(df_aux)
}

overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}