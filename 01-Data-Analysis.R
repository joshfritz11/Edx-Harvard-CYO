##########################################################
# Data Analysis
##########################################################


if (!require(purrr)) install.packages("purrr")
library(purrr)

# Take a look at how the different fields differ by class.
Dry_Bean_Dataset |> ggplot(aes(Class, Area)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Perimeter)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, MajorAxisLength)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, MinorAxisLength)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, AspectRation)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Eccentricity)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ConvexArea)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, EquivDiameter)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Extent)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Solidity)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, roundness)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, Compactness)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor1)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor2)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor3)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(Class, ShapeFactor4)) +
    geom_boxplot()

Dry_Bean_Dataset |> ggplot(aes(roundness, Compactness)) +
    geom_point(aes(color = Class))

summary(Dry_Bean_Dataset)

Dry_Bean_Dataset %>%
    split(.$Class) %>%
    map(summary)

Dry_Bean_Dataset %>%
    group_by(.$Class) %>%
    summarize(n())
