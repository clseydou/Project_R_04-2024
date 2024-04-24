install.packages("usethis", "gitcreds")
oui
oui

usethis::create_github_token()
gitcreds::gitcreds_set(url = "https://github.com/settings/tokens")

usethis::use_git()
usethis::use_github()

bu_1 <- subset(bupk,bu_pk1==1)
bu_4 <- subset(bupk,bu_pk1==0)

summary(bupk$bmi)
summary(bu_1$bmi)
summary(bu_4$bmi)
boxplot(bmi~bu_pk1, data=bupk, main="BMI according to Bu administration", ylab= "BMI in kg/m2", xlab="Bu administration")

summary(bupk$dx_age)
summary(bu_1$dx_age)
summary(bu_4$dx_age)

plot(bupk$dx_age, bupk$bmi, main="BMI according to age", ylab= "BMI in kg/m2", xlab="Age in years")
plot(auc~bu_cl, data=bupk, main="Clearance according to AUC", ylab= "AUC in micromol/l*min", xlab= "Clearance in ml/min/kg")

