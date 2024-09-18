PSID1982 <- read.csv("PSID1982.csv")

par(mfrow=c(2,2))
reg <- lm(wage~education,data=PSID1982)
plot(reg)         

summary(reg)

AIC(reg)

"在残差与拟合图中，残差的均值看起来不像是0，而Q-Q图显示，高值偏离了直线。让我们
看看非线性变换是否能解决这些问题。我们先看看如果加上平方项会怎样。"
#要加上平方项，只需把变量名放在I()中，然后加上^2，表示你想要平方项。
par(mfrow=c(2,2))

reg2 <- (lm(wage~education+I(education^2), data=PSID1982))
plot(reg2)

summary(reg2)

AIC(reg2)

#That didn’t do a lot, although the AIC did go down slightly. 
#Let’s see if using the log of wage improves the fit.
par(mfrow=c(2,2))

reg3 <- lm(log(wage)~education,data=PSID1982)
plot(reg3)

summary(reg3)

AIC(reg3)
#对工资这样的连续变量取对数是很常见的因为它们往往是非线性的。


"现在，让我们添加职业和教育之间的交互作用。我们可以添加交互作用，只需在我们感兴趣
的第二个变量中加上*(乘法符号)，而不是加号。这助于我们理解，
对于白领和蓝领工人来说，接受额外教育的工资增长是否存在差异"
reg4 <- lm(log(wage)~education*occupation, data=PSID1982)
summary(reg4)
AIC(reg4)

"When our dependent variable is logged we need to take the exponent of the
coefficient to interpret the size of change."
exp(0.04509) # exponent for the coefficient for education
## [1] 1.046122

"没有受过教育的白领工人的工资预计比没有受过教育的蓝领工人低21%，
但这种差异微不足道。这个结果本身并不完全有意义，因为我们期望白领工人赚得更多，
但这只是针对没有受过教育的工人，而数据中没有人真正受过教育。"
exp(-0.23689) # exponent for the coefficient for occupation
## [1] 0.7890781

"早些时候，我们发现教育提高了蓝领工人的工资。通过相互作用，我们发现，白领工人
的教育水平每提高一个单位，其工资增长就比蓝领工人的工资增长多2.7%，而且这种变化
略显重要。因此，所有工人都从教育中获得了积极的收益，但对白领工人来说，
工资的增长更大。"
exp(0.02701)# exponent for the coefficient for the interaction between education and occupation
## [1] 1.027378








