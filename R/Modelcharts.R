#'Gain Chart and Lift Chart     \cr
#'
#' @description This Package provides two important functions for producing Gain chart and Lift chart for any classification model.
#'
#' @section  GAIN_CHART():
#'Creates a gain chart based on calculated probability values and actual outcome.
#' @section LIFT_CHART():
#'creates a lift chart based on calculated probability values and actual outcome.
#' @seealso \code{\link{GAIN_CHART}}, \code{\link{LIFT_CHART}}
#' @docType package
#' @name Modelcharts
NULL
#'
#'Functions
#'Gain Chart
#' @description Creates a Gain chart.
#' @param INPUT Input data
#' @param Probability Probability values between zero and one
#' @param cutoffs probability cutoffs(c(0.80,0.60,0.40,0.20,0)/c(0.5,0))
#' @param Outcome outcome variable(target variable)
#' @param Event outcome representation ("YES"/"Y"/"1")
#'
#' @return A gain chart
#' @seealso \code{\link{Modelcharts}}
#' @import dplyr
#' @import plotly
#' @importFrom stats quantile
#' @export
#' @examples \dontrun{
#'# Run it and see for yourself
#'}
#'data.tmp<-read.csv(system.file("ext", "testdata.csv", package="Modelcharts"))
#'GAIN_CHART(data.tmp,data.tmp$Probability,seq(0.95,0,-0.05),data.tmp$Outcome,"Y")
GAIN_CHART <- function(INPUT, Probability,cutoffs,Outcome, Event)

{
  x <- cutoffs

  for (PROBABILITY_CUTOFFS in x){

    VALUE = quantile (Probability, c(PROBABILITY_CUTOFFS))

    OUTPUT_TEMP = bind_cols (
      INPUT %>%
        filter (Outcome == Event) %>%
        group_by () %>%
        summarize (TOTAL_CHURN_CNT = n()),

      INPUT %>%
        filter ((Probability >= VALUE) & (Outcome == Event)) %>%
        group_by () %>%
        summarize (CHURN_CNT = n())
    ) %>%
      mutate (Model = CHURN_CNT/TOTAL_CHURN_CNT,
              Random = (1 - PROBABILITY_CUTOFFS),
              Percentile = PROBABILITY_CUTOFFS,
              Actual_Percentile_Value = VALUE)

    if (PROBABILITY_CUTOFFS == x[1]) {
      OUTPUT = OUTPUT_TEMP
    } else {
      OUTPUT = bind_rows(OUTPUT, OUTPUT_TEMP)
    }

  }

  plot_ly(OUTPUT, x = ~Random, y = ~Random, name = 'Random', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~Model, name = 'Model', mode = 'lines+markers') %>%
    layout(
      title = 'Gain Chart',
      xaxis = list(title = 'Probability CutOff', range = c(0,1.1)),
      yaxis = list(title = 'Percentage of Actual Events', range = c(0,1.1))
    )

}
#'
#'Functions
#'Lift Chart
#' @description Creates a Lift chart.
#' @param INPUT Input data
#' @param Probability Probability values between zero and one
#' @param cutoffs probability cutoffs(c(0.80,0.60,0.40,0.20,0)/c(0.5,0))
#' @param Outcome outcome variable(target variable)
#' @param Event outcome representation ("YES"/"Y"/"1")
#'
#' @return A lift chart
#' @seealso \code{\link{Modelcharts}}
#' @import dplyr
#' @import plotly
#' @importFrom stats quantile
#' @export
#' @examples \dontrun{
#'# Run it and see for yourself
#'}
#'data.tmp<-read.csv(system.file("ext", "testdata.csv", package="Modelcharts"))
#'LIFT_CHART(data.tmp,data.tmp$Probability,seq(0.95,0,-0.05),data.tmp$Outcome,"Y")
LIFT_CHART <- function(INPUT, Probability,cutoffs, Outcome, Event)

{
  x <- cutoffs

  for (PROBABILITY_CUTOFFS in x){

    VALUE = quantile (Probability, c(PROBABILITY_CUTOFFS))

    OUTPUT_TEMP = bind_cols (
      INPUT %>%
        filter (Outcome == Event) %>%
        group_by () %>%
        summarize (TOTAL_CHURN_CNT = n()),

      INPUT %>%
        filter ((Probability >= VALUE) & (Outcome == Event)) %>%
        group_by () %>%
        summarize (CHURN_CNT = n())
    ) %>%
      mutate (Model = CHURN_CNT/TOTAL_CHURN_CNT,
              Random = (1 - PROBABILITY_CUTOFFS),
              Percentile = PROBABILITY_CUTOFFS,
              Actual_Percentile_Value = VALUE,
              Model_Lift = (Model/Random),
              Random_Lift = (Random/Random))


    if (PROBABILITY_CUTOFFS == x[1]) {
      OUTPUT = OUTPUT_TEMP
    } else {
      OUTPUT = bind_rows(OUTPUT, OUTPUT_TEMP)
    }

  }

  plot_ly(OUTPUT, x = ~Random, y = ~Random_Lift, name = 'Random', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~Model_Lift, name = 'Model_Lift', mode = 'lines+markers') %>%
    layout(
      title = 'Lift Chart',
      xaxis = list(title = 'Probability CutOff', range = c(0,1.1)),
      yaxis = list(title = 'Lift of Actual Events Compared to Random Pick')
    )

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("CHURN_CNT","TOTAL_CHURN_CNT","Model","Random"))


