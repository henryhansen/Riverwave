
#' Richards-Baker Flashiness index (RBI)
#' @description Adds RBI of observation (flow).
#'
#' @param data A daily value data.frame.
#' @param flow_var A column name for flow, e.g. Flow.
#' @param ... values to pass to `dplyr::group_by()` to perform RBI calculation on. (optional)
#'
#' @return A rbi column within data.frame.
#' @note The user must specify the `flow_var` column but it's optional whether to use `dplyr::group_by()` or not.
#' @export
#'
#' @examples
#' stream_flow <- data.frame(flow = c(seq(30, 60), seq(60, 30, length.out = 60)))
#' stream_flow %>% get_rbi(flow)

get_rbi <- function(data, flow_var, ...) {

    data %>%
        dplyr::mutate(lagged_flow = dplyr::lag({{flow_var}})) %>%
        dplyr::group_by(...) %>%
        dplyr::transmute(rbi = sum(abs({{flow_var}}-lagged_flow), na.rm = T)/sum({{flow_var}}, na.rm = T)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

}

