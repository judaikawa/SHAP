
shap <- function(model,data,top_n=10,var_start=1,var_n=10,
                 list_var=NULL,subsample=0.1) {
  
  # require("SHAPforxgboost")
  shap_values <- shap.values(xgb_model = model, 
                             X_train = data)
  
  shap <- shap_values$mean_shap_score
  
  var_n <- min(var_n,length(shap))
  top_n <- min(top_n,length(shap))
  
  if (is.null(list_var)) {
    list_var <- names(shap)[var_start:var_n]
  }
  
  if (subsample*nrow(data)<1e+05) {
    subsample=1
  }
  
  std1 <- function(x) {
    return((x - min(x, na.rm = T))/(max(x, na.rm = T) - 
                                      min(x, na.rm = T)))
  }
  
  shap_score_sub <- setDT(shap_values$shap_score)[,names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var], 
                                                  with = F]
  shap_score_long <- melt.data.table(shap_score_sub, measure.vars = colnames(shap_score_sub))
  fv_sub <- as.data.table(X1)[,names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var], 
                              with = F]
  fv_sub_long <- melt.data.table(fv_sub, measure.vars = colnames(fv_sub))
  fv_sub_long[, `:=`(stdfvalue, std1(value)), by = "variable"]
  names(fv_sub_long) <- c("variable", "rfvalue", "stdfvalue")
  shap_long2 <- cbind(shap_score_long, fv_sub_long[, c("rfvalue", 
                                                       "stdfvalue")])
  shap_long2[, `:=`(mean_value, mean(abs(value))), by = variable]
  setkey(shap_long2, variable)
  
  var_importance = data.frame(var=names(shap_values$mean_shap_score), 
                              importance=shap_values$mean_shap_score,
                              row.names = NULL)
  
  var_importance = var_importance[1:top_n,]
  
  g1 <- ggplot(var_importance, aes(x=reorder(var,importance), y=importance)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    theme_light() + 
    theme(axis.title.y=element_blank()) 
  
  g2 <- shap.plot.summary(shap_long2, dilute = (1/subsample)) 
  
  # Gráfico 2
  invisible(xgb.plot.shap(data = data,
                model = model, 
                features = names(shap_values$mean_shap_score)[names(shap_values$mean_shap_score)%in%list_var],
                n_col = 2,
                plot_loess = T,
                subsample = subsample
  ))
  
  g3 <- recordPlot()
  
  return(list(shap_values=shap,
              g1=g1,
              g2=g2,
              g3=g3))
}

#Exemplo de uso
data("iris")
X1 = as.matrix(iris[,-5])
mod1 = xgboost::xgboost(
  data = X1, label = iris$Species, gamma = 0, eta = 1,
  lambda = 0,nrounds = 1, verbose = FALSE)

shap_ = shap(mod1,X1
             ,list_var = c('Sepal.Width','Sepal.Length')
             )

shap_$g2
