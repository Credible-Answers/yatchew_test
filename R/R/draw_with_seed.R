#' Internal function to make a random draw that is reproducible when a seed is given.
#' @param seed NULL or a number passed to set.seed()
#' @param draw A function without arguments that makes the draw
#' @returns The value of draw(). If seed is not NULL, the draw follows set.seed(seed) and the caller's random number generator state (.Random.seed) is restored afterwards.
#' @noRd
draw_with_seed <- function(seed, draw) {
    if (is.null(seed)) {
        return(draw())
    }
    old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit({
        if (is.null(old_seed)) {
            if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
                rm(".Random.seed", envir = globalenv())
            }
        } else {
            assign(".Random.seed", old_seed, envir = globalenv())
        }
    })
    set.seed(seed)
    draw()
}
