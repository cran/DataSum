
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DataSum

<!-- badges: start -->
<!-- badges: end -->

The goal of DataSum is to provide functions for summarizing data frames
by calculating various statistical measures, including measures of
central tendency, dispersion, skewness, kurtosis, and normality tests.
The package leverages the moments package for calculating statistical
moments and related measures, the dplyr package for data manipulation,
and the nortest package for normality testing. DataSum includes
functions such as `getmode` for finding the mode(s) of a data vector,
`shapiro_normality_test` for performing Shapiro-Wilk normality tests (or
Anderson-Darling tests when the data length is outside the valid range
for the Shapiro-Wilk test), `Datum` for generating a comprehensive
summary of a data vector with various statistics (including data type,
sample size, mean, mode, median, variance, standard deviation, maximum,
minimum, range, skewness, kurtosis, and normality test result), and
`DataSumm` for applying the `Datum` function to each column of a data
frame. Emphasizing the importance of normality testing, the package
provides robust tools to validate whether data follows a normal
distribution, a fundamental assumption in many statistical analyses and
models.

Functions getmode: Takes a data vector as input and returns the mode(s)
of the data. shapiro_normality_test: Performs a Shapiro-Wilk normality
test on the input data. If the data length is outside the valid range
for the Shapiro-Wilk test (3 to 5000), it performs an Anderson-Darling
normality test instead. Datum: Takes a data vector as input and returns
a data frame with various summary statistics, including data type,
sample size, mean, mode, median, variance, standard deviation, maximum,
minimum, range, skewness, kurtosis, and normality test result. DataSumm:
Takes a data frame as input and applies the Datum function to each
column, returning a data frame with the summary statistics for each
column. Measures of Central Tendency Mean: The average of the values.
Median: The middle value when the data is arranged in order. Mode: The
value that appears most frequently in the data set. Measures of
Dispersion Range: The difference between the largest and smallest values
in the data set. Variance: A measure of how spread out the values are
from the mean. Standard Deviation: The square root of the variance.
Other Measures Skewness: A measure of the asymmetry of the probability
distribution. Kurtosis: A measure of the “peakedness” of the probability
distribution. Normality: A test to determine if the data follows a
normal (Gaussian) distribution, such as the Shapiro-Wilk test.

## Installation

You can install the released version of DataSum from CRAN with:

``` r
install.packages("DataSum")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(DataSum)

# Example data
data <- mtcars

#Top Portion of data
head(data)

# Get the summary statistics
summary_statistics <- DataSumm(data)

# Print the summary statistics
print(summary_statistics)
```

## CRAN Note

The following terms were flagged as potential spelling errors during the
CRAN submission process. However, they are intentionally used in the
package and are relevant to its functionality:

- **DataSum**: The name of the package.
- **DataSumm**: A function within the package that summarizes data
  frames.
- **Wilk**: Refers to the Shapiro-Wilk normality test.
- **dplyr**: A widely-used R package for data manipulation.
- **getmode**: A function that returns the mode(s) of a dataset.
- **nortest**: An R package for performing normality tests.
- **shapiro**: Refers to the Shapiro-Wilk normality test.
- **skewness**: A measure of asymmetry in the distribution of data.
- **kurtosis**: A measure of the “peakedness” of the probability
  distribution.

These terms are not misspelled but are specific to the package’s
context.
