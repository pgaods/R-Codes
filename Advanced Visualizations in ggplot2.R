# Advanced Visualizations in R


#############################################


# I. Introduction to 'ggplot2'

# Practically, ggplot2 provides beautiful, hassle-free plots that take care of fiddly details like drawing legends, and the plots can be built up iteratively and edited later. 
# A carefully chosen set of defaults means that most of the time you can produce a publication-quality graphic in seconds, but if you do have special formatting requirements, a comprehensive theming system makes it easy to do what you want. 
# Gramatically, the package 'ggplot2' is designed to work in a layered fashion, starting with a layer showing the raw data then adding layers of annotations and statistical summaries.
# In brief, the grammar tells us that a statistical graphic is a mapping from data to aesthetic attributes (color, shape, size) of geometric objects (points, lines, bars).
# The plot may also contain statistical transformations of the data and is drawn on a specific coordinate system.

PackageTest <- function(x){
                 if (!require(x,character.only = TRUE)){
                   install.packages(x,dep=TRUE)
                   if(!require(x,character.only = TRUE)) stop("Package not found")
                 }
               }
PackageTest("ggplot2")
PackageTest("plyr")
PackageTest("gridExtra")
PackageTest("nlme")


# II. The qplot() Function

# The qplot() function ("quick plot") makes it easy to produce complex plots, often requiring several lines of code using other plotting systems, in one line.
# The 'diamonds' dataset consists of prices and quality information about 54,000 diamonds, and is included in the ggplot2 package.
# The data contains the four C's of diamond quality, carat, cut, color and clarity, and five physical measurements (depth, table, x, y and z).
# To faciliate the visualization, we take a random sample of size 100 as an illustrative example.

data(diamonds)
summary(diamonds)
print(diamonds[1:10, ])
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ] # taking a random sample of size 100
str(dsmall)

# We now create a function that does a proc frequency analysis on all of the factors in a data frame automatically.
# This user-defined function first identifies all factor elements in a data frame, then apply the proc.freq function to each factor.
# This is very useful as it will help us perform graphical analyses in the future.

proc.freq.allfactors <- function(dataframe){
                          library(plyr)
                          proc.freq <- function(var, dataset){
                                         y=count(dataset, var)
                                         y$percent <- y$freq/sum(y$freq)
                                         return(y)
                                       }
                          factor_set <- as.list(sapply(dataframe, is.factor))
                          factor_set2 <- names(which(factor_set==TRUE)) # extracting all the factors' names
                          result <- lapply(factor_set2, proc.freq, dataset=dataframe) # the result must be a list (sapply will not work here)
                          if(length(factor_set2)==0){
                            print("No variables are factors in the data frame")
                          }
                          else {
                            print("These are the factors in the current dataset:")
                            print(factor_set2)
                            result
                          }
                        }
proc.freq.allfactors(dsmall)

# We now perform a sequence of simple plots using qplot() after loading the data and taking the random sample.
# The qplot() function can handle transformed variables directly.

g1 <- qplot(carat, price, data = diamonds) # simple scatterplots
g2 <- qplot(log(carat), log(price), data = diamonds) # plotting transformed variables
g3 <- qplot(carat, x*y*z, data = diamonds) # combinations of multiple variables
grid.arrange(g1, g2, g3, nrow=1, ncol=3) # putting all pictures together through the "gridExtra" library

# Color, size and shape are all examples of aesthetic attributes, visual properties that affect the way observations are displayed.
# For every aesthetic attributes, there is a function called 'scale' that maps data values to valid values for that aesthetic (so scale: dataset-aesthetic values).
# It is this scale that controls the appearance of the points and associated legends.
# For example, the 'alpha' aesthetics using the option I() indicates semi-transparency.
# The value of the alpha takes on 0 and 1 (completely transparent and completely opaque).
# This is very useful because for large datasets, semi-transparent points are often useful to alleviate some of the overplotting.
# In general, different types of aesthetic attributes work better with different types of variables.
# For example, color and shape work well with categorical variables, while size works better with continuous variables.

g4 <- qplot(carat, price, data = dsmall, color = color, main="Colors differentiate based on the factor 'color': fit for big data") 
g5 <- qplot(carat, price, data = dsmall, shape = cut, main="Shapes differentiate based on the factor 'cut': not fit for big data")
g6 <- qplot(carat, price, data = dsmall, size = carat, main="Sizes differentiate based on the continuous variable 'carat': not fit for big data")
g7 <- qplot(carat, price, data = dsmall, geom="path", main="connecting all dots with lines, not ideal in this case")
dev.new()
grid.arrange(g4, g5, g6, g7, nrow=2, ncol=2)
g8 <- qplot(carat, price, data = dsmall, alpha = I(9/10), main="alpha aesthetics I=0.9")
g9 <- qplot(carat, price, data = dsmall, alpha = I(5/10), main="alpha aesthetics I=0.5")
g10 <- qplot(carat, price, data = dsmall, alpha = I(1/10), main="alpha aesthetics I=0.1")
g11 <- qplot(carat, price, data = dsmall, alpha = I(0), main="alpha aesthetics I=0")
dev.new()
grid.arrange(g8, g9, g10, g11, nrow=2, ncol=2)

# Among all of the aesthetics, the 'geom' is the most interesting one, because it enables qplot() to be able to produce almost any kind of plot (not just limited to scatterplots). 
# The geoms (geomtric objects) are the visual representations of (subsets of) observations.
# The geoms list is huge and can be found here: "http://sape.inf.usi.ch/quick-reference/ggplot2/geom". 
# The 'geom' aesthetic can also be added for 1D plots (e.g. histogram, freqpoly, and density for continuous variables, and bar for discrete variables etc.) besides the usual 2D plots. 

g12 <- qplot(carat, price, data=dsmall, geom="point", main='default', color='red') # default 
g13 <- qplot(carat, price, data=dsmall, geom="smooth", main='smoother') # this fits a smoother to the data and displays the smooth and its standard error
g14 <- qplot(carat, price, data=dsmall, geom="boxplot", main='boxplot (uncommon)')
g15 <- qplot(carat, price, data=dsmall, geom="line", main='useful for time series') # this is usually used for time series plot
dev.new()
grid.arrange(g12, g13, g14, g15, nrow=2, ncol=2)
g16 <- qplot(price, data=dsmall, geom="histogram", main='Histogram plot for continuous variable') 
g17 <- qplot(price, data=dsmall, geom="freqpoly", main='freqpoly plot for continuous variable')
g18 <- qplot(price, data=dsmall, geom="density", main='density plot for continuous variable') 
g19 <- qplot(color, data=dsmall, geom="bar", main='bar chart for discrete variable') 
dev.new()
grid.arrange(g16, g17, g18, g19, nrow=2, ncol=2)
g20 <- qplot(carat, price, data = dsmall, geom = c("point", "smooth"), main='adding a smoother') 
g21 <- qplot(carat, price, data = dsmall, shape=color, color=color, main='adding another layer of aesthetics')
dev.new()
grid.arrange(g20, g21, nrow=1, ncol=2)

# When the data include a categorical variable and continuous variables, one will probably be interested to know how the values of the continuous variables vary with the levels of the categorical variable.
# Box-plots and jittered points offer two ways to achieve this goal, and each method has its own strengths and weaknesses.
# Boxplots summarize the bulk of the distribution with only five numbers, while jittered plots show every point but can suffer from overplotting.
# In practice, it's better to look at jittered points with transparency control when we have a large amount of data, and distinguishing each category by color is even better.

g22 <- qplot(color, price/carat, data = diamonds, geom = "jitter", alpha = I(0.09), main="how values of continuous variable varies with the levels of the categorical variable")
g23 <- qplot(color, price/carat, data = diamonds, color=color, geom = "jitter", alpha = I(0.09), main="how values of continuous variable varies with the levels of the categorical variable")
dev.new()
grid.arrange(g22, g23, nrow=1, ncol=2)

# When it comes to 1D plot, we can fine-tune our plots using additional arguments and aesthetics.

g24 <- qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.9, fill=color, main='histogram with wide columns')
g25 <- qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.05, xlim = c(0,3), fill=color, main='histogram with narrow columns (recommended)')
g26 <- qplot(carat, data = diamonds, geom = "density", xlim = c(0,3), fill=color, alpha=I(0.5), main='density with faded colors')
g27 <- qplot(carat, data = diamonds, geom = "density", xlim = c(0,3), fill=color, alpha=I(1), main='density with full colors')
dev.new()
grid.arrange(g24, g25, g26, g27, nrow=2, ncol=2)

# Line and path plots are typically used for time series data.
# Line plots join the points from left to right, while path plots join them in the order that they appear in the dataset (a line plot is just a path plot of the data sorted by xvalue).
# Line plots usually have time on the x-axis, showing how a single variable has changed over time, whereas Path plots show how two variables have simultaneously changed over time, with time encoded in the way that the points are joined together. 
# As an example, we use the 'economics' dataset, which contains economic data on the US measured over the last 40 years.
# Two time series measuring amount of unemployment are present in the data, and we would like to examine them together (to compare).
# To examine the relationship in greater detail, we would like to draw both time series on the same plot indicating time progression, and the solution would be to use the 'path' plot.
# Adding color will help us identify the evolution of the data points as time progresses. 
# This technique is very useful if we want to examine two time series similarity. 

data(economics)
summary(economics)
class(economics) # "tbl_df tbl data.frame"
print(economics[1:10, ])
g28 <- qplot(date, unemploy/pop, data = economics, geom = "line", main='unemploy/pop')
g29 <- qplot(date, uempmed, data = economics, geom = "line", main='unemped')
grid.arrange(g28, g29, nrow=1, ncol=2)
year_func <- function(x) { 
               as.POSIXlt(x)$year + 1900
             }
dev.new()
g30 <- qplot(unemploy /pop, uempmed, data = economics, geom = c("point", "path"), color=year_func(date))
g30

# We can see that percent unemployed and length of unemployment are highly correlated, although in recent years the length of unemployment has been increasing relative to the unemployment rate.
# Note from the graph above that the color progresses as time evolves. 

# We now talk about faceting, which creates tables of graphics by splitting the data into subsets and displaying the same graph for each subset in an arrangement that facilitates comparison.
# The default faceting method in qplot() creates plots arranged on a grid specified by a faceting formula which looks like row_var ~ col_var.
# For example, the command "row_var ~ ." will create a single column with multiple rows.
# You can specify as many row and column variables as you like, keeping in mind that using more than two variables will often produce a plot so large that it is difficult to see on screen.
# The idea of faceting is essentially the same as by-group processing.
# For example, one can use this technique to display density or histogram for the count of Hispanic voters among men and women respectively.
# To facet on only one of columns or rows, use the dot "." as a place holder.
# In addition, using "..density.." tells "ggplot2" to map the density to the y-axis instead of the default use of count.

g31 <- qplot(carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = 0.1, xlim = c(0, 3), color=I('orange'), main="histogram by category: color") # orange-rimmed
g32 <- qplot(carat, ..density.., data = diamonds, facets = . ~ clarity, geom = "histogram", binwidth = 0.1, fill=I('brown'), main="histogram by category: clarity") # filled with brown histogram
grid.arrange(g31, g32, nrow=1, ncol=2)


# III. Basic Grammars

# While qplot() is quick and dirty, it's not the most powerful function in the 'ggplot2' library.
# The best part of the 'ggplot2' library is based on layer-building, and we focus on understanding the basic grammar of the library and some important concepts from now on.
# Consider the fuel economy dataset 'mpg', which records make, model, class, engine size, transmission and fuel economy for a selection of US cars in 1999 and 2008. 
# It contains the 38 models that were updated every year, an indicator that the car was a popular model.
# This data comes from the EPA fuel economy website "http://fueleconomy.gov".
# Suppose we want to have a simple scatterplot of the data, and question would be: what's going on behind the scene in R?

data(mpg)
summary(mpg)
str(mpg)
proc.freq.allfactors(mpg)
dev.new()
qplot(displ, hwy, data = mpg, color = factor(cyl))

# If you think about a scatterplot, they not only have a vertical and horizontal position, but also has a size, a color and a shape.
# These attributes are called aesthetics according to the previous section, and are the properties that can be perceived on the graphic.
# Each aesthetic can be mapped to a variable, or set to a constant value.
# Once we have these mappings, we can create a new dataset that records this information, and the new dataset is a result of applying the aesthetic mappings to the original data.
# We can create many different types of plots using this data (the scatterplots uses points, but were we instead to draw lines we would get a line plot).
# So behind the scene, these operations are all gramatically correct but it does not necessarily mean that each operation makes sense (just like English).
# In 'ggplot2', points, lines and bars are all examples of geometric objects, or 'geoms' (recall 'geoms' determine types of the plot).
# Plots that use a single 'geom' are often given a special name, and more complex plots with combinations of multiple geoms don't have a special name, and we have to describe them by hand.

# One of the most important concepts in 'ggplot2' is scaling, which are performed by scales (think about liters, meters etc.)
# What scaling does is that it converts the data behind the scene from data units (e.g. liters, miles per gallon, number of cylinders etc.) to physical units (e.g. pixels and colors).
# It is only after the conversion that the computer can read it (even though it may not make sense to us).
# For examples, colors are represented by a six-letter hexadecimal string, sizes by a number and shapes by an integer.
# A complete list of the aesthetic specifications that are meaningful to R can be found in the appendix section of the 'ggplot2' pdf manual.
# Using the process of mapping colors as an example, colors can be thought of as having 3 components, corresponding to the 3 types of color-detecting cells in the human eye.
# These three cell types give rise to a three-dimensional color space and scaling then involves mapping the data values to points in this space.
# Finally, we need to render this data to create the graphical objects that are displayed on the screen.
# To create a complete plot we need to combine graphical objects from 4 sources: the data, the scales, the coordinate system, and plot annotations (e.g. the background and plot titles etc.).

# We now give an example of a more complex plot, which adds three new components to the mix: facets, multiple layers and statistics.

dev.new()
qplot(displ, hwy, data=mpg, facets = . ~ year) + geom_smooth()

# In this example, the smooth layer is different to the point layer because it doesn't display the raw data, but instead displays a statistical transformation of the data.
# Specifically the smooth layer fits a smooth line through the middle of the data.
# This requires an additional step in the process: after mapping the data to aesthetics, the data is passed to a statistical transformation (or 'stat'), which manipulates the data in some way. 
# In this example particular example with geom_smooth(), the 'stat' fits the data to a loess smoother, and then returns predictions from evenly spaced points within the range of the data. 
# Other useful 'stats' include 1D and 2D binning, group means, quantile regression and contouring.

# As well as adding an additional step to summarize the data, we also need some extra steps when we get to the scales.
# This is because we now have multiple datasets (for the different facets and layers) and we need to make sure that the scales are the same across all of them.
# Scaling actually occurs in three parts: transforming, training and mapping.
# Scale transformation occurs before statistical transformation so that statistics are computed on the scale-transformed data. 
# This ensures that a plot of a function of x vs. a function of y on linear scales looks the same as x vs. y on their transformed scales.
# There are many different transformations that can be used, including taking square roots, logarithms and reciprocals etc..
# After the statistics are computed, each scale is trained on every dataset from all the layers and facets. 
# The training operation combines the ranges of the individual datasets to get the range of the complete data. 
# Without this step, scales could only make sense locally and we wouldn't be able to overlay different layers because their positions wouldn't line up. 
# Sometimes we do want to vary position scales across facets (but never across layers), but this is doable too in 'ggplot2'.
# Finally the scales map the data values into aesthetic values, which is essentially a local operation.
# The variables in each dataset are mapped to their aesthetic values producing a new dataset that can then be rendered by the 'geoms'.
# Here is the flowchart: map variables to aesthetics - facet datasets - transform scales - compuete aesthetics - train scales - map scales - render geoms,
# Below is an example that compares a good layout vs. a bad layout due to the faceting design. 

dev.new()
g33 <- qplot(displ, hwy, data=mpg, facets = . ~ year, main='better layout') + geom_smooth()
g34 <- qplot(displ, hwy, data=mpg, facets = year ~ ., main='bad layout') + geom_smooth()
grid.arrange(g33, g34, nrow=2, ncol=1)

# We now discuss the specific components of the layered grammar. 
# Besides the components of a plot, such as data and aesthetic mappings, geometric objects (geoms), statistical transformation, scales, faceting, and coordinate system, the position adjustment is also important.
# Together, the data, mappings, stat, geom and position adjustment form a layer, and a plot may have multiple layers.
# Layers are responsible for creating the objects that we perceive on the plot.
# A layer is composed of four parts: data and aesthetic mapping, a statistical transformation (stat), a geometric object (geom) and a position adjustment.

# A scale controls the mapping from data to aesthetic attributes, and we need a scale for every aesthetic used on a plot. 
# Each scale operates across all the data in the plot, ensuring a consistent mapping from data to aesthetics.
# A scale is a function, and its inverse, along with a set of parameters.
# For example, the color gradient scale maps a segment of the real line to a path through a colour space.
# The parameters of the function defines whether the path is linear or curved, which color space to use (e.g. LUV or RGB), and the colors at the start and end.
# The inverse function is used to draw a guide so that you can read values from the graph.
# Guides are either axes (for position scales) or legends (for everything else).
# Most mappings have a unique inverse (i.e., the mapping function is one-to-one), but many do not.

# A coordinate system, or 'coord' for short, maps the position of objects onto the plane of the plot.
# Position is often specified by two coordinates (x, y), but potential could be three or more.
# The Cartesian coordinate system is the most common coordinate system for two dimensions, while polar coordinates and various map projections are used less frequently. 
# Coordinate systems affect all position variables simultaneously and differ from scales in that they also change the appearance of the geometric objects (e.g. in polar coordinates, bar geoms look like segments of a circle).
# Additionally, scaling is performed before statistical transformation, while coordinate transformations occur afterward.
# Coordinate systems control how the axes and grid lines are drawn.

# Faceting can be considered a general case of the conditioned or trellised plots, and it is a powerful tool when investigating whether patterns hold across all conditions.
# This makes it easy to create small multiples each showing a different subset of the whole dataset.

# In a nutshell, a plot object is a list with components data, mapping (the default aesthetic mappings), layers, scales, coordinates and facet. 
# The plot object has one other component we haven't discussed yet, called options, which is used to store the plot-specific theme options.
# There are a few things you can do with it after creating the plot, such as printing, saving (using the ggsave() function), describing its structure (using the summary() function), or saving a cached copy of it to a disk using the save() function.
# The save() function saves a complete copy of the plot object, so you can easily recreate that exact plot with load().
# Note that data is stored inside the plot, so that if you change the data outside of the plot, and then redraw a saved plot, it will not be updated.

dev.new()
g35 <- qplot(displ, hwy, data = mpg, color = factor(cyl))
g35
summary(g35)
ggsave("C:/Users/pgao/Documents/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Output for R codes/g35.png", width=5, height=5)
save(g35, file="C:/Users/pgao/Documents/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Output for R codes/g35.rData")
load("C:/Users/pgao/Documents/PGZ Documents/Statistics Workshop/PDF textbooks/Machine Learning/The Elements of Statistical Learning/Output for R codes/g35.rData")


# IV. Building Layers of Graphs

# This section is mainly a technical description of how layers, geoms, statistics and position adjustments etc. are combined together.
# Layering is the mechanism by which additional data elements are added to a plot. 
# Each layer can come from a different dataset and have a different aesthetic mapping, allowing us to create plots that could not be generated using qplot(), which permits only a single dataset and a single set of aesthetic mappings.
# A minimal layer may do nothing more than specify a 'geom', a way of visually representing the data.
# Layers are regular R objects and so can be stored as variables, making it easy to write clean code that reduces duplication.

# To create the plot object ourselves, we will focus on using the ggplot() function which has two arguments: data and aesthetic mapping. 
# These arguments set up defaults for the plot and can be omitted if you specify data and aesthetics when adding each layer. 
# The data argument is the data frame that you want to visualize. 
# For aesthetics, you need to wrap the pairs of aesthetic attribute and variable name in the aes() function.
# The aes() function gives a set of aesthetic mappings, which describe how variables in the data are mapped to visual properties (aesthetics) of the 'geoms'.
# Any variable in an aes() specification must be contained inside the plot or layer data, and aesthetic mappings specified in a layer affect only that layer.

# On a high level note, there are 5 components of a layer: 1) the data, 2) a set of aesthetic mappings, 3) the geom, 4) the stat (responsible for transformation of data), and 5) the positional adjustment.
# When using the ggplot() function, data must be data frames, and it is not necessary to specify a default dataset except when using faceting.
# Faceting is a global operation (i.e., it works on all layers) and it needs to have a base dataset which defines the set of facets for all datasets.
# The data is stored in the plot object as a copy, not a reference.
# If your data changes, the plot will not, and ggplot2 objects are entirely self-contained so that they can be saved to disk and later loaded and plotted without needing anything else from that session.
# We now plot a few set of graphs to illustrate these concepts below, and we showcase the differences when it comes to discrete and continuous data respectively. 

dev.new()
g36 <- ggplot(diamonds, aes(x=carat))
g37 <- g36 + geom_histogram(binwidth = 0.1, fill = "steelblue", alpha=0.4) + ggtitle("histogram/count plot of a single continuous variable") # geom_histogram() is used for continuous data
g38 <- g36 + geom_density(fill = "orange", alpha=0.9) + ggtitle("density plot of a single continuous variable")
grid.arrange(g36, g37, g38, nrow=3, ncol=1)

dev.new()
g39 <- ggplot(diamonds, aes(x=cut)) + ggtitle("Setting up the graph without any plots") # blank picture (default) b/c we have no layers on it
g40 <- g39 + geom_bar(width=0.8, fill='brown', alpha=0.7) + ggtitle("histogram/count plot of a single discrete variable (factor)") # geom_bar() should be used for discrete data
grid.arrange(g39, g40, nrow=2, ncol=1)

dev.new()
g41 <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + ggtitle("Setting up the graph without any plots") # blank picture (default) b/c we have no layers on it
g42 <- g41 + geom_point() + ggtitle("Scatterplot of a two continuous variables based on different factors") 
g43 <- g41 + geom_line() + ggtitle("Line plot of a two continuous variables based on different factors") + xlab('carat (numeric)') + ylab('price (numeric)')
g44 <- g41 + geom_smooth() + ggtitle("Smooth curve of a two continuous variables based on different factors") + xlab('carat (numeric)') + ylab('price (numeric)')
grid.arrange(g41, g42, g43, g44, nrow=2, ncol=2)

# To summarize the codes above, the general way of approaching a graph is to set up a blank graph first by specifying the very basic elements (what needs to be plotted).
# Once we have the basic graph, we can add geom_XXX() or stat_XXX() for additional layers. 

# A caveat must be given here for the conceptual comparison between mapping and setting.
# The aesthetics can vary for each observation being plotted, while parameters do not.
# We map an aesthetic to a variable (e.g., (aes(color=cut))) or set it to a constant (e.g., color = "red").
# Simply put, the aes() function is used for mapping (what type of plots and what variables to be used) instead of setting.

dev.new()
g45 <- ggplot(mtcars, aes(mpg, wt)) + geom_point(color = "green") + ggtitle("Green dots (setting)")
g46 <- ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(color="green")) + ggtitle("Not Green dots (mapping)-wrong way of practicing")
grid.arrange(g45, g46, nrow=2, ncol=1)

# We now discuss groupings issues, as in ggplot2, geoms can be roughly divided into individual and collective geoms.
# An individual geom has a distinctive graphical object for each row in the data frame (e.g., the point geom has a single point for each observation).
# On the other hand, collective geoms represent multiple observations. 
# This may be a result of a statistical summary, or may be fundamental to the display of the geom, as with polygons. 
# Lines and paths fall somewhere in between: each overall line is composed of a set of straight segments, but each segment represents two points. 
# How we can control which observations go in which individual graphical element is the job of the group aesthetic.
# By default, the group is set to the interaction of all discrete variables in the plot, which often partitions the data correctly.
# When it does not, or when no discrete variable is used in the plot, you will need to explicitly define the grouping structure, by mapping group to a variable that has a different value for each group.
# The interaction() function is useful if a single pre-existing variable doesn't cleanly separate groups, but a combination does.

# We now invoke the 'nlme' package and extract the dataset 'Oxboys', which records the heights ('height') and centered ages ('age') of 26 boys ('Subject'), measured on 9 occasions ('Occasion').
# In many situations, you want to separate your data into groups, but render them in the same way.
# When looking at the data in aggregate you want to be able to distinguish individual subjects, but not identify them.
# This is common in longitudinal studies with many subjects, where the plots are often descriptively called spaghetti plots.
# Sometimes we may also want to plot summaries based on distinct levels of aggregation.
# Different layers might have different group aesthetics, so that some display individual level data while others display summaries of larger groups.

library(nlme)
dev.new()
print(Oxboys[10, ])
class(Oxboys)
mode(Oxboys)
g47 <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_line() + ggtitle("series plot by group (Subject)")
g48 <- ggplot(Oxboys, aes(age, height)) + geom_line() + ggtitle("series plot together without grouping")
g49 <- ggplot(Oxboys, aes(age, height, group=Subject, color=Subject)) + geom_line()+ ggtitle("series plot by group with colors")
g50 <- ggplot(Oxboys, aes(age, height, group=Subject, color=Subject)) + geom_line(size=1.5)+ ggtitle("series plot by group with better colorful visualization")
g51 <- g47 + geom_smooth(aes(group=1), method='lm', size=1.2, se=FALSE, color='red') # the new line will be based on all of the data
g52 <- g47 + geom_smooth(aes(group=1), method='lm', size=1.2, se=TRUE, color='orange') # the new line will be based on all of the data
grid.arrange(g47, g48, g49, g50, g51, g52, nrow=3, ncol=2)

# In the above set of plots, the first one (g47) does a plot by each boy so you can see how each boy's height evolves along with age.
# The third one (g49) adds color aesthetics so that each boy is represented with a distinct color. 

# Another important issue with collective geoms is how the aesthetics of the individual observations are mapped to the aesthetics of the complete entity.
# For individual geoms, this isn't a problem, because each observation is represented by a single graphical element.
# However, high data densities can make it hard or impossible to distinguish between individual points and in some sense the point geom becomes a collective geom, a single blob of points.
# Lines and paths operate on an off-by-one principle: there is one more observation than line segment, and so the aesthetic for the first observation is used for the first segment, the second observation for the second segment and so on.
# This means that the aesthetic for the last observation is not used.
# An additional limitation for paths and lines is that that line type must be constant over each individual line, as in R there is no way to draw a joined-up line which has varying line type.
# For all other collective geoms, like polygons, the aesthetics from the individual components are only used if they are all the same, otherwise the default value is used.
# This makes sense for 'fill' as it is a property of the entire object: it doesn't make sense to think about having a different fill color for each point on the border of the polygon.
# In general, these issues are most relevant when mapping aesthetics to continuous variable, because when you introduce a mapping to a discrete variable, it will by default split apart collective geoms into smaller pieces. 
# This works particularly well for bar and area plots, because stacking the individual pieces produces the same shape as the original ungrouped data.

# We now give a quick summary on the geoms which perform the actual rendering of the layer and control the type of plot that you create.
# Each geom has a set of aesthetics that it understands, and a set that are required for drawing. 
# For example, a point requires x and y position, and understands color, size and shape aesthetics, whereas a bar requires height and understands width, border color and fill color.
# Some geoms differ primarily in the way that they are parameterized.
# For example, the 'tile' geom is specified in terms of the location of its centre and its height and width, while the 'rect' geom is parameterized in terms of its top (ymax), bottom (ymin), left (xmin) and right (right) positions.
# Internally, the 'rect' geom is described as a polygon, and its parameters are the locations of the four corners (this is useful for non-Cartesian coordinate systems).
# Every geom has a default statistic, and every statistic a default geom.
# For example, the bin statistic defaults to using the bar geom to produce a histogram.
# The complete list of 'geoms' can be found in the Wickham's original pdf tutorial on ggplot2 (pp.55-56).

# We now zoom in on the concept of stat (statistical transformations).
# For example, a useful stat is the smoother, which calculates the mean of y, conditional on x, subject to some restriction that ensures smoothness.
# The complete list of 'stats' can be found in the Wickham's original pdf tutorial on ggplot2 (pp.57-58).
# To make sense in a graphic context, a stat must be location-scale invariant: f(x+a)=f(x)+a and f(bx) = bf(x). 
# This ensures that the transformation stays the same when you change the scales of the plot.
# A stat takes a dataset as input and returns a dataset as output, and so a stat can add new variables to the original dataset.
# It is also possible to map aesthetics to these new variables.
# For example, stat_bin, the statistic used to make histograms, produces the following variables: count (the number of observations in each bin), density (percentage of total/bar width), and x (the center of the bin).
# These generated variables can be used instead of the variables present in the original dataset.
# For example, the default histogram geom assigns the height of the bars to the number of observations (count), but if you'd prefer a more traditional histogram, you can use the density (density).
# Below are equivalent graphs using ggplot() and qplot(), and note that the names of generated variables from 'stats' must be surrounded with two dots ".." when used.
# This prevents confusion in case the original dataset includes a variable with the same name as a generated variable, and it makes it clear to any later reader of the code that this variable was generated by a stat.

dev.new()
g53 <- ggplot(diamonds, aes(carat)) + geom_histogram(aes(y = ..density..), binwidth = 0.1, fill='orange')+ggtitle("ggplotting histogram with orange") # the variable 'density' is a generated variable
g54 <- qplot(carat, ..density.., data = diamonds, geom="histogram", binwidth = 0.1, fill='red', main='qplotting histogram with red')
grid.arrange(g53, g54, nrow=1, ncol=2)

# We now discuss positional adjustments which apply minor tweaks to the position of elements within a layer and are normally used with discrete data.
# Continuous data typically doesn't overlap exactly, and when it does (because of high data density) minor adjustments, like jittering, are usually insufficient to fix the problem.
# There are 5 types of positional adjustments: dodge (adjust position by dodging overlaps to the side), fill (stack overlapping objects and standardize), identity, jitter, and stack (stack overlapping objects on top of one another).
# The word 'jitter' per se means 'slight irregular movement, variation, or unsteadiness, especially in an electrical signal or electronic device', but here it jitters points to avoid overplotting.
# If you have data which has already been summarised, and you just want to use it, you'll need to use stat_identity(), which leaves the data unchanged, and then map the appropriate variables to the appropriate aesthetics.

dev.new()
g55 <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
g56 <- g55 + stat_bin(aes(size = ..density..), binwidth = 0.1, geom = "point", position="identity")
g56

# Let's pull everything togehter and mention a few other key points. 
# A number of the geoms available in ggplot2 were derived from other geoms in a process like the one just described, starting with an existing geom and making a few changes in the default aesthetics or stat. 
# For example, the jitter geom is simply the point geom with the default position adjustment set to jitter. 
# Once it becomes clear that a particular variant is going to be used a lot or used in a very different context, it makes sense to create a new geom.
# Here is a list of aliased geoms that are created by modifying other geoms:
#   1) area: this is the ribbon geom in base geom with changes in default aes(min = 0, max = y)., position = "stack".
#   2) density: this is the area geom in base geom with changes in default stat='density'.
#   3) frepoly: this is the line geom in base geom with changes in default stat='bin'.
#   4) histogram: this is the bar geom in base geom with changes in default stat='bin'
#   5) jitter: this is the point geom in base geom with changes in default position='jitter'.
#   6) quantile: this is the line geom in base geom with changes in default stat='quantile'.
#   7) smooth: this is the ribbon geom in base geom with changes in default stat='smooth'.


# V. Toolbox

# The layered structure of ggplot2 encourages you to design and construct graphics in a structured manner. 
# This section lists some of the many geoms and stats included in ggplot2, broken down by their purpose.
# This section is broken up into the following sections, each of which deals with a particular graphical challenge:
#   1) basic plots
#   2) displaying distribution (including joint and conditional)
#   3) dealing with overplotting
#   4) Surface plots
#   5) statistical summaries
#   6) drawing maps
#   7) revealing uncertainties and errors
#   8) annotating a plot
#   9) plotting weighted data

# Before diving into the specific topics, we need to figure out the overall layering strategy first.
# The purpose of layering is three-fold: 1) to display the data, 2) to display a statistical summary of the data, and 3) to add meta data information.
# These skills are very useful when it comes to statistical modeling presentation, in which one could be often asked to compare graphs and add predictions on top of the original plots etc.

# Subsection 1 Basic Plots:
# The first question when it comes to graphical presentation is to ask what type of plots you would like based on the purpose of the plot. 
# This means the first step is to figure out the geom component of the plot. 
# Most of these geoms are associated with a named plot: when that geom is used by itself in a plot, that plot has a special name.
# Each of these geoms is two dimensional and requires both x and y aesthetics.
# All geoms understand 'color' and 'size' aesthetics, and the filled geoms('bar', 'tile' and 'polygon') also understand 'fill'. 
# The 'point' geom uses 'shape' and 'line' and 'path' geoms understand 'linetype'. 
# Below are the major functions for geoms:
# 1) The geom_area() draws an area plot which is a line plot filled to the y-axis(filled lines), and multiple groups will be stacked on top of each other.
# 2) The geom_bar() makes a barchart.
#    We need stat="identity" because the default stat automatically counts values (so is essentially a 1d geom). 
#    The identity stat leaves the data unchanged.
#    By default, multiple bars in the same location will be stacked on top of one another.
# 3) The geom_line() makes a line plot. 
#    The group aesthetic determines which observations are connected. 
#    The geom_path() is similar to a geom_line(), but lines are connected in the order they appear in the data, not from left to right.
# 4) The geom_point() produces the scatterplots.
# 5) The geom_polygon() draws polygons, which are filled paths. 
#    Each vertex of the polygon requires a separate row in the data. 
#    It is often useful to merge a data frame of polygon coordinates with the data just prior to plotting.
# 6) The geom_text() adds labels at the specified points. 
#    This is the only geom in this group that requires another aesthetic: label.
#    It also has optional aesthetics 'hjust' and 'vjust' that control the horizontal and vertical position of the text, and angle which controls the rotation of the text.
# 7) The geom_tile() makes a image plot or level plot. 
#    The tiles form a regular tessellation of the plane and typically have the fill aesthetic mapped to another variable.
# Let's now see some examples by using a very small dataset.

df <- data.frame(
          x = c(3, 1, 5, 8, 2),
          y = c(2, 4, 6, 8, 1),
          labels = c("a","b","c","c","a")
          )
print(df)
dev.new()
g57 <- ggplot(df, aes(x, y, label = labels)) + xlab(NULL) + ylab(NULL) + ggtitle("Blank")
g58 <- g57 + geom_point() + ggtitle("geom_point")
g59 <- g57 + geom_bar(stat="identity") + ggtitle("geom_bar(stat=\"identity\")")
g60 <- g57 + geom_line() + ggtitle("geom_line")
g61 <- g57 + geom_area() + ggtitle("geom_area")
g62 <- g57 + geom_path() + ggtitle("geom_path")
g63 <- g57 + geom_text() + ggtitle("geom_text")
g64 <- g57 + geom_tile() + ggtitle("geom_tile")
g65 <- g57 + geom_polygon() + ggtitle("geom_polygon")
grid.arrange(g57,g58, g59, g60, g61, g62, g64, g64, g65, nrow=3, ncol=3)

# Subsection 2 Displaying Distributions:
# When it comes to distributions plotting, the most important thing is to figure out the dimensionality of the data and whether the data are continuous or discrete.
# For 1d continuous distributions the most important geoms are the histograms and densities.
# If you want to compare the distribution between groups, you have a few options (e.g. creating a small multiple of histograms or plotting conditional density plot).
# Below are some examples. 

g66 <- ggplot(diamonds, aes(depth)) + xlim(58, 68) + ggtitle("blank setup")
g67 <- g66 + geom_histogram(aes(y = ..density.., fill=cut), binwidth = 0.1) + facet_grid(cut ~ .) + ggtitle("density plot by group (cut) with distinct colors")
g68 <- g66 + geom_histogram(aes(fill = cut), binwidth = 0.1, position = "fill") + ggtitle('histogram plot with area filled with color')
g69 <- g66 + geom_freqpoly(aes(y = ..density.., color = cut), binwidth = 0.1) + ggtitle("by-group density plot with different color on each group")
grid.arrange(g66,g67, g68, g69, nrow=2, ncol=2)

# 

# Subsection 3 Dealing with Overplotting:

# Subsection 4 Surface Plots:

# Subsection 5 Statistical Summaries:

# Subsection 6 Drawing Maps:

# Subsection 7 Revealing Uncertainties and Errors

# Subsection 8 Annotating a Plot

# Subsection 9 Plotting Weighted Data





# VI. Scales, Axes, and Legends

# VII. Positioning

# VIII. Polishing Graphs

# IX. Manipulating Data

# X. Reducing Duplication


# References:
# http://docs.ggplot2.org/current/
# http://moderngraphics11.pbworks.com/f/ggplot2-Book09hWickham.pdf
# https://cran.r-project.org/web/views/Graphics.html
# http://www.cookbook-r.com/
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom

