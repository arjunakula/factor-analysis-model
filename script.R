load("V:/arjun_google_drive_docs/CourseWork/UCLA_Winter_2017/202b/HW3/SwissBankNotes.rda")
d1 = SwissBankNotes[1:100,]
d1_pca = prcomp(d1,center=TRUE,scale=TRUE)
plot(d1_pca,type='l')
summary(d1_pca)
biplot(d1_pca)

d2 = SwissBankNotes[101:200,]
d2_pca = prcomp(d2,center=TRUE,scale=TRUE)
plot(d2_pca,type='l')
summary(d2_pca)
biplot(d2_pca)

dall = SwissBankNotes[1:200,]
dall_pca = prcomp(dall,center=TRUE,scale=TRUE)
plot(dall_pca,type='l')
summary(dall_pca)
biplot(dall_pca)




fitted_Y_tls = tls(X.toba, Y.toba)$z
fitted_Y_ols = X.toba %*% beta_ols
plot(Y.toba[,1], fitted_Y_tls[,1], pch=0, ylim = c(1, 3), main = "burn rate")
points(Y.toba[,1], fitted_Y_ols[,1], pch=1)
legend(x = 2.0, y = 3.0, legend = c("tls", "ols"), pch= c(0, 1))