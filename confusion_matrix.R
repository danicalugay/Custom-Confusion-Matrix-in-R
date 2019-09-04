### predicted.value is a vector of your predicted model
### true.value is the vector of your actual data 

p <- table(predicted.value, true.value)

#confusion_matrix 
con_matrix <- function(p)
{
        n <- length(colnames(p))
        container = rep(NA, n)
        accuracy = rep(NA, n)
        precision = rep(NA, n)
        sensitivity = rep(NA, n)
        specificity = rep(NA, n)
        i = 1
        for (i in (1:n))
        {
                TP <- diag(p)[i]
                FP <- p[i,][-i]
                FN  <- p[,i][-i]
                TN <- sum(diag(p)[-i])
                accuracy[i] <- sum(diag(p))/sum(p)
                precision[i] <-  TP / (TP + sum(FP)) 
                sensitivity[i] <- TP / (TP + sum(FN))
                specificity[i] <- TN / (sum(FP) + TN)
        }
        q <- data.frame(accuracy= accuracy, precision=precision,sensitivity =sensitivity, specificity =specificity)
        q<- rbind(q,colMeans(q))
        rownames(q) <-c(rownames(p),"Average")
        message("Confusion Matrix")
        return(q)
        
}
