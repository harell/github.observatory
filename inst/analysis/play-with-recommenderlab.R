# Setup -------------------------------------------------------------------
# remotes::install_cran("recommenderlab")


# Get the data ------------------------------------------------------------
data("MovieLense", package = "recommenderlab")

### use only users with more than 100 ratings
MovieLense100 <- MovieLense[recommenderlab::rowCounts(MovieLense) >100,]
MovieLense100


# Model data --------------------------------------------------------------
# Train a user-based collaborative filtering recommender using a small training set.
train <- MovieLense100[1:50]
rec <- recommenderlab::Recommender(data = train, method = "UBCF")
rec


# Create top-N recommendations for new users (users 101 and 102)
# ?recommenderlab::predict
pre <- recommenderlab::predict(object = rec, newdata = MovieLense100[101:102], n = 10)
pre
as(pre, "list")
