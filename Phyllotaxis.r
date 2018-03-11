
# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

# Loading in the ggplot2 package
library(ggplot2)


library(testthat) 
library(IRkernel.testthat)

run_tests({
    test_that("Test that ggplot2 is loaded", {
    expect_true( "package:ggplot2" %in% search(), 
        info = "The ggplot2 package should be loaded using library().")
    })
})

t <- seq(0, 2*pi, length.out = 50)
x <- sin(t)
y <- cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p +  geom_point() 

run_tests({
    test_that("Check that a geom_point plot was plotted.", {
    expect_true( "GeomPoint" %in% class( last_plot()$layers[[1]]$geom ) , 
        info = "Add geom_point() to produce a scatter plot.")
    })
})

# Defining the number of points
points <- 500

# Defining the Golden Angle
angle <- pi*(3-sqrt(5))  

t <- (1:points) * angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p +  geom_point() 

run_tests({
    test_that("points are 500.", {
    expect_equal(points, 500, 
        info = "There should be 500 points.")
    })
    
    test_that("angle is golden.", {
    expect_equal(angle, pi*(3-sqrt(5)), 
        info = "angle should be set to the Golden Angel. Check the hint!")
    })
})

df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point() +
    theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
          panel.background=element_rect(fill="white")) 
          

# Maybe just check if one of the this are blank, 
# seems too much to check them all...

run_tests({
    test_that("Background is white.", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "The background should be white.")
    })
    test_that("Grid is removed.", {
    expect_true("element_blank" %in% class(last_plot()$theme$panel.grid), 
        info = "The grid lines should be removed.")
    })
})

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(size=8,alpha=0.5,color="darkgreen") +
     theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
     panel.background=element_rect(fill="white"))  

run_tests({
    test_that("Point size equal to 8.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$size, 8, 
        info = "size should be set 8.")
    })
    test_that("alpha equal to 0.5.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$alpha, 0.5, 
        info = "alpha should be set 0.5.")
    })
})

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=t),shape=8,alpha=0.5,color="black") +
     theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
     panel.background=element_rect(fill="white"),legend.position="none")  

run_tests({
    test_that("Map size of points to t.", {
    expect_equal(last_plot()$labels$size, "t", 
        info = "Map size of points to t. Check the hint!")
    })
    test_that("point shape is asterisk.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$shape, 8, 
        info = "Change the shape of all points to asterisks.")
    })
    test_that("Legend is removed.", {
    expect_equal(last_plot()$theme$legend.position, "none", 
        info = "Remove the legend from the plot.")
    })
})

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=t),shape=17,alpha=0.5,color="yellow") +
     theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
     panel.background=element_rect(fill="darkmagenta"),legend.position="none")  

run_tests({
    test_that("point shape is filled triangles.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$shape, 17, 
        info = "Change the shape of all points to filled triangles. Check the hint!")
    })
    test_that("Legend is removed.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$colour, "yellow", 
        info = "Remove the legend from the plot.")
    })
})

angle <- 2.0
points <- 1000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=t),shape=17,alpha=0.5,color="yellow") +
     theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
     panel.background=element_rect(fill="darkmagenta"),legend.position="none")  

run_tests({
    test_that("angle is 2.", {
    expect_equal(angle, 2, 
        info = "angle should be equal to 2")
    })
})

angle <- 13 * pi / 180 
points <- 2000

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p +  geom_point(aes(size=80),shape=1,alpha=0.1,color="magenta4") +
     theme(panel.grid=element_blank(),axis.ticks=element_blank(), title=element_blank(), text=element_blank(), 
     panel.background=element_rect(fill="white"),legend.position="none")  

run_tests({
    test_that("points is equal to 2000.", {
    expect_equal(points, 2000, 
        info = "There should be 2000 points.")
    })
    test_that("point shape is asterisk.", {
    expect_equal(last_plot()$layers[[1]]$aes_params$shape, 1, 
        info = "Change the shape of all points to empty circles. Check the hint!")
    })
    test_that("Background is white.", {
    expect_equal(last_plot()$theme$panel.background$fill, "white", 
        info = "The background should be white.")
    })
    test_that("angle is 13*pi/180.", {
    expect_equal(angle, 13*pi/180, 
        info = "angle should be set to 13*pi/180.")
    })
})
