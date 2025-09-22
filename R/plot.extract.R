globalVariables(c("period", "latent1", "latent2"))

#' Plot Method for the extract Function. 
#' 
#' This function generates a line plot of the latent variable estimates obtained from the `extract()` function. It can handle both one-dimensional and two-dimensional latent variable estimates.
#' 
#' @param x An object of class "extract", typically a result of the `extract()` function.
#' @param ... Additional graphical parameters to be passed to the plot function.
#' @return A ggplot of the latent variable estimate(s) over time. 
#' @usage NULL
#' @method plot extract
#' @importFrom ggplot2 ggplot geom_line labs aes
#' @export
#' @examples
#' data(jennings)
#' dr_out <- extract(varname = jennings$variable, 
#'                   date = jennings$date, 
#'                   index = jennings$value, 
#'                   ncases = jennings$n, 
#'                   begindt = min(jennings$date), 
#'                   enddt = max(jennings$date), 
#'                   npass=1)
#' plot(dr_out)
plot.extract <- function(x, ...) {
  mood <- get_mood(x)
  if(!("latent2" %in% names(x))){
    g <- ggplot(mood, aes(x=period, y=latent1)) + 
      geom_line() +
      labs(x="Time Period", y="Dimension 1 Estimate") 
  }else{
    g <- ggplot(mood, aes(x=period)) + 
      geom_line(aes(y=latent1, color="Dimension 1")) +
      geom_line(aes(y=latent2, color="Dimension 2")) +
      labs(x="Time Period", y="Latent Variable Estimates", color="Legend") 
  }
  return(g)
  }
