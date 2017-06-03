createTestMatrix <- function() {
        ## create hopefully invertable matrix
        matrix(rexp(200), 10, nrow=10, ncol=10)  
}

m <- createTestMatrix()
cacheable_m <- makeCacheMatrix(m)

# ensure cacheSolve produces same inverse
expect_that(solve(m), equals(cacheSolve(cacheable_m)))

# make sure cached version is also correct
expect_that(solve(m), equals(cacheSolve(cacheable_m)))

# make sure cached version is still correct after multiple calls
expect_that(solve(m), equals(cacheSolve(cacheable_m)))

