x=1:100
X=sample(x,30,replace=T)
Y=sample(x,30,replace=T)
Z=7*X+2*Y+3+rnorm(30,0,100)

plot(X,Y)
plot(Z,X)
plot(Z,Y)

regZX=lm(Z~X)
print(regZX)
regZY=lm(Z~Y)
print(regZY)
regZXY=lm(Z~X+Y)
print(regZXY)

print(anova(regZX))
print(anova(regZY))
print(anova(regZXY))

D=cbind(X,Y,replicate(30,1))

beta=solve(t(D)%*%D)%*%(t(D)%*%Z)
betaX=beta[1]
betaY=beta[2]
betaResidual=beta[3]
cat("Z*=",betaX,"* X +",betaY,"* Y +", betaResidual, "\n")

Zstar=predict(regZXY)
print(Zstar)

print(confint(regZXY,level = 0.05))