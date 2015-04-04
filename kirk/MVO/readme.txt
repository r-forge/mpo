datafile:
crsp.short.RData
crsp.short.6.Rdata (for shiny demo)
crsp.short.15.Rdata (for shiny demo)

Shiny-GUI files:
ui.R:  R graphical interface file
server.R: R graphical interface file
run.R :R graphical interface file
efrontplot.shiny: R graphical interface file

Source file:
constraint.set.R:  each individual constraint and functions to combine constraints
mvo.constrainted.R: mvo,gmv, maxmu,minmu of portfolio
efront.constrained.R: calculate and plot efront
barplot.wts: barplot of the weight.
 
test.file:
constraint.set.test.R: test constraints
efrontplot.shiny.test.R: test of R graphical interface

report file:
constraints_tutorial.pdf
constraints_tutorial.Rnw ( Rmd file for knitr)



05/27
5. only display the parameter values of toc and ptc on efplot. 
6. remove old example files

05/27

1. change parameter names:  "turnover"->"toc",  "propcost"->"ptc"
2. revise the output message:  "turnover/propcost constraints triggered for calculating mu.max"  to "turnover/propcost constraints reduced the max mean return in efficient frontier plot"
3 display the parameter values on ef plot (suppress the ones longer than 50 characters)
4 add mean metric to barplot in addition to vol metric ( working)

05/23

1. I promote the two scenarios  sum+box+turnover and box+propcost  in constraint.set.text.R

2. The issue of propcost:   the reason of efficient frontier of propcost reaches mean=0, sd=0 is because the ``loss of weight/wealth" due to transaction cost. One can achieve mean=0 and sd=0 by buying and selling a large portion of each stock, causes the final weight of each stock equals 0, hence the mean and sd are both 0 in this case. 

3. R Shiny GUI 



05/10
Following the discussion we had last Friday, this week I am coding up the constraint.set.r in a way that can combine all type of constraints for user to call from, e.g., 
convention set :  "cset.sum",  "cset.lo",   "cset.box",  "cset.groups",  "cset.mu.target", 
modern set:         "cset.turnover",  "cset.propcost", "cset.fixedcost"

for example, user can call a certain combination of constraints set in the list above. e.g, the previous turnover constraint set in slides is a combination of cset.sum, cset.mu.target, cset.turnover, and we can freely add group/box constraints to it. 

The difficulties are the optimization object is different for modern set compared with convention set.  E.g., for turnover constraint, the optimization object is weight.initial+weight.buy+weight.sell, and for long only constraint, the optimization object is weight vector. 

I am writing a function set to automatic trigger the matrix conversion of convention set to adapt to modern set whenever any constraints in modern set is called. 

It has been taking a while due to the A,b,meq format differently when different combination of constraints sets are called. However, I am getting very close to completion. 


