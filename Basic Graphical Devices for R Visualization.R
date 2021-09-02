# Basic Graphical Devices for R Visualizations


#############################################


# I. Basic Graphical Devices in R

# R itself contains many built-in graphical features, which can be used extensively for explorative data analyses, model diagnostics and final presentation of reserach results.
# Common functions in base R include plot(), abline(), points(), line(), text(), hist(), par(), curve(), density() etc.
# We illustrate our examples using a few datasets that are directly stored in R already.

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("ggplot2")
PackageTest("grDevices")

data(trees)
data(mtcars)
data(cars)
object <- c(class(trees), class(mtcars), class(cars))
object
rm(object) # removing the object after the check 

# We first study the plot() function, which is used to plot R objects (this is one of the most generic function in R).
# This function is extremely useful for scatterplots (so the plot doesn't have any lines connecting the dots).
# Thus it is common to apply this function paired with the abline() function, which is used to draw a single straight line (not necessarily vertical or horizontal).

attach(trees)
plot(x=Girth, y=Height, col="dark blue", xlab="Explatory Variable", ylab="Response", type='point', main='Scatterplots of Girth and Heights') # scatterplot
abline(lm(Height~Girth)) # adding a regression line
detach(trees)

# Before digging further into each functions, it's useful to understand how to set parameters and  combine graphs to save spaces.
# The par() function is used to set or query graphical parameters. 
# Parameters can be set by specifying them as arguments to par in tag = value form, or by passing them as a list of tagged values. 
# One can customize many features of your graphs through graphic options. 
# If you set parameter values here, the changes will be in effect for the rest of the session or until you change them again. 
# To view current settings of the graphical devices, simply type par().
# To change the current settings, simply use the objects nested in the par().
# The parameters can be either written in words or in a numerical way. 
# For example, the color chart at http://research.stowers-institute.org/efg/R/Color/Chart/index.htm gives you more color options than English description. 
# For lines, lty gives line types (dotted or straight etc.) while lwy denotes line width relative to its default.
# For margins, mar gives a numerical vector of the form c(bottom, left, top, right) which denotes the number of lines of margin to be specified on the four sides of the plot. 
# The default margin is c(5, 4, 4, 2) + 0.1.
# The value of adj determines the way in which text strings are justified in functions text(), mtext() and title(). 
# A value of 0 produces left-justified text, 0.5 (the default) centered text and 1 right-justified text.

attach(cars)
dev.new()
par(col.lab="red", bg='light yellow', fg='dark green', col.main=4, font.lab=4) # red x and y labels with yellow background and green frame
plot(cars, main = "Stopping Distance versus Speed", type='p', pch=17)
abline(lm(speed~dist))
dev.new()
par(col.lab="blue", family='mono', adj=1, col='grey', col.main='blue') # family is the font family (style) 
plot(cars, main = "Stopping Distance versus Speed", type='p')
abline(lm(speed~dist))
dev.new()
par(family='mono', adj=1, col='purple', pch=16) # family is the font family (style) 
plot(cars, main = "Stopping Distance versus Speed", col='brown')
abline(lm(speed~dist))
detach(cars)

# We now examine the points() function used to draw a sequence of points at the specified coordinates.
# In the points() function, the argument 'pch' is point type (see the online documentation for the options).
# Also in the function, argument 'cex' denotes symbol or character expansion (it controls the point size here).
# Of course, these values can be set in par() too. 
# The general rule of thumb of setting parameters is that we should try to respect the default, unless we are in a corporate setting where there are restrict requirements for presentation.

dev.new()
plot(-8:8, -8:8, type = "n", main='Normal Plots')  # setting up coordinate system (type='n' means noprint)
points(x=rnorm(n=500, mean=0, sd=1), y=rnorm(n=500, mean=0, sd=4), col = c("green", "yellow"), cex=1.2, pch=18)
dev.new()
plot(-8:8, -8:8, type = "n", main='Normal Plots')  # setting up coordinate system (type='n' means noprint)
points(x=rnorm(n=500, mean=0, sd=1), y=rnorm(n=500, mean=0, sd=4), col = c("red", "blue"), cex=1, pch=21)

# We now study the lines() function, which is a generic function taking coordinates given in various ways and joining the corresponding points with line segments.
# Be cautious when using this function, because this can make your graph very jagged.
# Similarly, the text() function should be used with caution too when you have a densely populated graph, as this could make your graph look ugly.
# The argument 'y' is allowed to be missing since xy.coords(x, y) is used for construction of the coordinates.
# The 'offset' variable determines how much is the label situated away from the point.
# The 'pos' value determines the location/direction of the label w.r.t the point.
# The 'cex' value is a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.
# Simply put, the 'cex' value indicates the amount by which plotting text and symbols should be scaled relative to the default. 
# For cex, 1=default, 1.5 is 50% larger, 0.5 is 50% smaller, etc. 
# The cex.axis denotes the magnification of axis annotation relative to cex.
# In the same way, cex.lab controls the magnification of x and y labels relative to cex.

attach(cars)
dev.new()
plot(cars, main = "Stopping Distance versus Speed", type='p')
lines(cars) # connecting by lines (better used when the data size is small)
text(x=speed, y=dist, pos=1, offset=2, cex=0.6, col='purple')
detach(cars)

# Finally we study the hist() and density() function.
# When it comes to plotting density, it's common to combine the histogram and the density curve onto one graph.
# This can be achieved by using command par(new=TRUE).

xs_ <- rnorm(5000, 0, 1)
dev.new()
hist(xs_, breaks=50, col='Azure', main='Frequency Histogram') # the 'breaks' argument gives the number of cells for the histogram
dev.new()
hist(xs_, breaks=50, col='beige', freq=FALSE, main="", xlab=NULL) # freq=FALSE indicates that the plot gives density instead of frequency (counts)
par(new=TRUE) # this helps integrating two graphs into a single one (not juxtaposing them)
plot(density(xs_), main='Density Plot')
rm(xs_)


# II. Graphic Enhancements

# The most powerful package in R ggplot2 adds aesthetic values to graphics in R. 
# The 'ggplot2' package 'thinks' of the data in terms of geometry, and the syntax are designed as layers on top of each other.
# So in command lines there will be a lot of 'geom_*()' type of code calls (* here means wild card).
# For example, you can have geom_points(), geom_lines(), geom_polygons(), geom_bars(), geom_abline(), geom_bar(), and geom_density(). etc.
# The package 'ggplot2' also applies many aesthetics, such as color, shapes, transparency etc.
# Aesthetics are usually applied by 'mapping' a column (variable) in the data frame to that aesthetic.
# For example, the scale_color_gradient() function controls the gradient/step of the color change.
# Also in the package, the function aes() creates a list of unevaluated expressions.
# It also performs partial name matching, converts color to colour, and old style R names to ggplot names (eg. pch to shape, cex to size).
# We give a quick peek at this powerful package in this section, but further studies on 'ggplot2' will need a completely separate lecture.
# We start with the function qplot(), then move onto other functions.

xs <- rnorm(10000, 0, 1)
dev.new()
qplot(xs, main='Frequency Table for xs') # qplot is like a histogram plot (y-axis is the count)
dev.new()
qplot(xs, xlim=c(-3,3), ylim=c(0,900), main='Frequency Table for xs') # the xlim and ylim set limits for the axis (putting a constraint)
dev.new()
plot(x=mtcars$wt, y=mtcars$mpg, pch=24) # basic plot in base R settings

# We now move onto adding aethetics for R (by adding different layers syntaxwise).

m1 <- ggplot(mtcars, aes(wt, mpg)) # no need to use the attach() function to call the variables
typeof(m1) # list
class(m1) # "gg" "ggplot"
mode(m1) # list
dev.new()
m1 + geom_point() # this gives the basic ggplots
dev.new()
m1 + geom_point(aes(color = qsec)) # adding aesthetic color mapping based on the variable qsec
dev.new()
m1 + geom_point(aes(alpha = qsec)) # alpha controls transparency
dev.new()
m1 + geom_point(aes(color = factor(cyl))) # color based on the factor level of other variables (cyl in this case)
dev.new()
m1 + geom_point(aes(shape = factor(cyl))) # shape based on the factor level of other variables (cyl in this case)
dev.new()
m1 + geom_point(aes(size = qsec)) # point size
dev.new()
m1+ geom_point(aes(color = cyl)) + scale_color_gradient(low = "yellow", high='blue')
dev.new()
m1 + geom_point(aes(size = qsec)) + scale_size_area()
dev.new()
m1 + geom_point(aes(size = qsec)) + scale_shape(solid = F)
dev.new()
m2 <- ggplot(mtcars, aes(hp, fill=factor(cyl), color=factor(cyl)))
m2 + geom_density() # creating solid area filled with color based on the factor level of the variable cyl
dev.new()
m3 <- ggplot(mtcars, aes(hp, fill=factor(cyl), color=factor(cyl)))
m3 + geom_density(position='fill', alpha=0.5) # position='fill' indicates the whole graph will be filled (with colors in this case)


# III. Graphical Output and Presentation

# When we making presentations, it's useful to save/output your plots for future use.
# To save png(), jpeg, bmp, or tiff files, we can use their corresponding functions, as long as we put a dev.off() at the end.
# The dev.off() function shuts down the specified (by default the current) device, and if the current device is shut down and any other devices are open, the next open device is made current. 
# If you save multiple graphs, you do not need to use dev.new() between each one.
# For presentation (using lyx), pdf() is the best choice beause it's resizable (jpeg cannot, for example).
# We first get two jpeg files, then we obtain the pdf output.

getwd() # check this location to obtain all outputs later
attach(trees)
jpeg("trees.jpeg")
plot(x=Girth, y=Height, col="dark blue", xlab="Explatory Variable", ylab="Response", type='point', main='Scatterplots of Girth and Heights') # scatterplot
abline(lm(Height~Girth)) # adding a regression line
dev.off()
detach(trees)
attach(cars)
jpeg("cars.jpeg")
par(col.lab="red", bg='light yellow', fg='dark green', col.main=4, font.lab=4) # red x and y labels with yellow background and green frame
plot(cars, main = "Stopping Distance versus Speed", type='p', pch=17)
abline(lm(speed~dist))
dev.off()
detach(cars)

# Producing pdf is similar to producing jpeg, and it works for outputs of 'ggplot2' too.

pdf("m3.pdf", paper='USr') # paper orientation is landscape
m3 <- ggplot(mtcars, aes(hp, fill=factor(cyl), color=factor(cyl)))
m3 + geom_density(position='fill', alpha=0.5) # position='fill' indicates the whole graph will be filled (with colors in this case)
dev.off()


# References:
# http://www.stat.wisc.edu/~gvludwig/327-5/ggplot2_(fabrizzio)
# http://togaware.com/onepager/
# http://handsondatascience.com/GGPlot2O.pdf
# http://www.statmethods.net/advgraphs/parameters.html
# http://www.cookbook-r.com/Graphs/Histogram_and_density_plot/
# http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
# http://www.statmethods.net/advgraphs/ggplot2.html
# http://docs.ggplot2.org/current/ 
# http://www.r-bloggers.com/how-to-replace-a-pie-chart/
# http://www.r-bloggers.com/japans-ageing-population-animated-with-r/
# http://www.r-bloggers.com/creating-shaded-areas-in-r/
# http://www.r-bloggers.com/r-graphics-margins-are-way-to-large/
# http://statistics.berkeley.edu/computing/saving-plots-r

