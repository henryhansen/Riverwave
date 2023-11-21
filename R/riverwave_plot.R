riverwave_prep <- function(df, flow_var, year_var, day_var) {
    #calculate average qmax across time series
    df %>%
        group_by({{year_var}}) %>%
        summarise(avgmaxQ = mean(max({{flow_var}})), na.rm = T) -> maxQ

    #calculate daily average and convert to qmax proportion
    df %>%
        group_by({{day_var}}) %>%
        summarise(avgQ = mean({{flow_var}}, na.rm = T)) -> dayQ

    #calculate flow duration curves

    #return data.frame with values
}

riverwave_plot <- function(df) {
    ggplot(data = df, )
}
