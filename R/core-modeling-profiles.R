## "Official" portraits of the core modeling functions

#' All of these are hidden, to be called with triple colons, e.g.  QRA:::osc_graph()

osc_graph <- function() {
  slice_plot(sin(2*pi*t) ~ t, domain(t=0:5), npts = 500) |>
  gf_labs(y = "Output", x = "Input") |>
  gf_hline(yintercept = ~ 0, color = "blue") |>
  gf_vline(xintercept = ~ 0, color = "tomato") |>
  gf_refine(scale_y_continuous(breaks = -1:1, 
                               labels = c("lowest", "midway", "highest"))) |>
  gf_theme(theme_minimal())
    
}

hillside_graph <- function() {
  slice_plot(pnorm(t) ~ t, domain(t=-4:4), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous(breaks = c(0, .5, 1))) |>
    gf_theme(theme_minimal())
}

hill_graph <- function() {
  slice_plot(dnorm(t) ~ t, domain(t=-4:4), npts = 500) |>
  gf_labs(y = "Output", x = "Input") |>
  gf_hline(yintercept = ~ 0, color = "blue") |>
  gf_vline(xintercept = ~ 0, color = "tomato") |>
  gf_refine(scale_y_continuous(breaks = c(0, .399), labels = c("0", "top"))) |>
  gf_theme(theme_minimal())
}

exp2_graph <- function() {
  slice_plot(2^t ~ t, domain(t=-2:3), npts = 500) |>
  gf_labs(y = "Output", x = "Input") |>
  gf_hline(yintercept = ~ 0, color = "blue") |>
  gf_vline(xintercept = ~ 0, color = "tomato") |>
  gf_refine(scale_y_continuous(breaks = c(.25, .5, 1, 2, 4, 8))) |>
  gf_theme(theme_minimal())
}

log2_graph <- function() {
  slice_plot(log2(t) ~ t, domain(t=.01:8), npts = 500) |>
    gf_labs(y = "Input until", x = "Output") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 1, color = "tomato") |>
    gf_refine(scale_y_continuous()) |>
    gf_theme(theme_minimal())
}

recip_graph <- function() {
  slice_plot(1/t ~ t, domain(t=0.1:3), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous(breaks = c(0, 1/2, 1, 2, 5, 10)),
              scale_x_continuous(breaks = c(.1, .2, .5, 1, 2,3))) |>
    gf_theme(theme_minimal())
}

same_graph <- function() {
  slice_plot(t ~ t, domain(t=-2:3), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous()) |>
    gf_theme(theme_minimal()) 
}

flat_graph <- function() {
  slice_plot(1.0 ~ t, domain(t=-2:3), npts = 500) |>
  gf_labs(y = "Output", x = "Input") |>
  gf_hline(yintercept = ~ 0, color = "blue") |>
  gf_vline(xintercept = ~ 0, color = "tomato") |>
  gf_point(1.5 ~ 3, alpha = 0) |>
  gf_refine(scale_y_continuous(breaks = c(0,0.5, 1, 1.5))) |>
  gf_theme(theme_minimal()) 
}

dflat_graph <- function() {
  slice_plot(0 ~ t, domain(t=-2:3), npts = 500, linewidth = 3, alpha = 0.5) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_point(1.5 ~ 3, alpha = 0) |>
    gf_refine(scale_y_continuous(breaks = c(0,0.5, 1, 1.5), limits = c(-1,1))) |>
    gf_theme(theme_minimal()) 
}

drecip_graph <- function() {
  slice_plot(-1/t^2 ~ t, domain(t = -4:4), npts = 1500) |>
  gf_labs(y = "Output", x = "Input") |>
  gf_hline(yintercept = ~ 0, color = "blue") |>
  gf_vline(xintercept = ~ 0, color = "tomato") |>
  gf_point(1.5 ~ 3, alpha = 0) |>
  gf_refine(scale_y_continuous(breaks = c(-10, -5, -4, -3, -2, -1, 0, 1), limits = c(-10,1))) |>
  gf_theme(theme_minimal()) 
}


dsame_graph <- function() {
  slice_plot(1 ~ t, domain(t=-2:3), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_point(1.5 ~ 3, alpha = 0) |>
    gf_refine(scale_y_continuous(limits = c(-2,3))) |>
    gf_theme(theme_minimal()) 
}

ddouble_graph <- function() {
  slice_plot(0.6931472 * 2^t ~ t, domain(t=-2:3), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous(breaks = c(.25, .5, 1, 2, 4, 8))) |>
    gf_theme(theme_minimal()) 
}

dhill_graph <- function() {
  slice_plot(t*dnorm(t) ~ t, domain(t=-4:4), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_theme(theme_minimal())
}

dhillside_graph <- function() {
  hill_graph() |> gf_lims(y = c(0,1))
}

dosc_graph <- function() {
  slice_plot(2*pi*cos(2*pi*t) ~ t, domain(t=0:5), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous(breaks = -1:1, 
                                 labels = c("lowest", "midway", "highest"))) |>
    gf_theme(theme_minimal()) 
}

dlog2_graph <- function() {
  slice_plot(1/(0.6931472 * t) ~ t, domain(t=0.1:3), npts = 500) |>
    gf_labs(y = "Output", x = "Input") |>
    gf_hline(yintercept = ~ 0, color = "blue") |>
    gf_vline(xintercept = ~ 0, color = "tomato") |>
    gf_refine(scale_y_continuous(breaks = c(0, 1/2, 1, 2, 5, 10)),
              scale_x_continuous(breaks = c(.1, .2, .5, 1, 2,3))) |>
    gf_theme(theme_minimal()) 
}

graph_names <- c("hill_graph", "hillside_graph", "recip_graph", 
                 "same-graph", "double_graph", "log2(_graph", 
                 "osc_graph" , "flat_graph",
                 "dflat_graph","dhillside_graph", 
                 "dosc_graph", "dlog2(_graph",
                 "ddouble_graph", "dsame-graph",
                 "drecip_graph", "dhill_graph" )

make_png_versions <- function( ) {
  for (nm in c("dsame-graph")) {
    f <- as.name(nm)
    P <- eval(f)() |> .bigger_text(size = 16)
    fname <- glue::glue("www/{nm}.png")
    ggsave(fname, plot = P)
  }
}

