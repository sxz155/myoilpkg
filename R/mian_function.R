#' Main function
#'
#' @param input_data A data frame as raw input.
#' @return A list containing a list of meta data and a data frame including all
#'   the raw input and new transformations.
#' @description Interactive function to ask users for their coice of input and
#'   transformations.
main_fun <- function(input_data = default_input_data) {
  ui_info("oil data is the raw input. If you want to use other dataframes,
          please load it as the input data!")
  ui_info("I can proceed with column name or comlun index!")
  if (ui_yeah("Do you want to proceed with column name?",
    yes = "Yes",
    no = "No", shuffle = FALSE
  )) {
    column_names <- colnames(input_data)
    ui_info("The column names are: {ui_value(column_names)}")
    col_name <- readline(prompt = "Enter column name: ")

    if (col_name %in% colnames(input_data)) {
      input_entry <- input_data[, col_name]
      input_entry_name <- col_name
      ui_done("Target is assigned!")
    } else {
      ui_warn("I need a valid column name. Please provide a valid column name!")
      ui_info("I'm going ot use the default column name OILPRICE!")
      input_data <- oil
      input_entry <- input_data[, "OILPRICE"]
      input_entry_name <- "OILPRICE"
      ui_done("Target is assigned!")
    }
  } else if (ui_yeah("Do you want to proceed with column index?",
    yes = "Yes",
    no = "No", shuffle = FALSE
  )) {
    column_numbers <- ncol(input_data)
    ui_info("There are {ui_value(column_numbers)} columns in the input data")
    col_index <- as.numeric(readline(prompt = "Enter column index: "))

    if (0 < col_index & col_index <= ncol(input_data)) {
      input_entry <- input_data[, col_index]
      input_entry_name <- colnames(input_data)[col_index]
      ui_done("Target is assigned!")
    } else {
      ui_warn("Column index should be between:
              {ui_value(1)} and {ui_value(column_numbers)}")
      ui_info("I'm going ot use the default column number 1!")
      input_data <- oil
      input_entry <- input_data[, 1]
      input_entry_name <- colnames(input_data)[1]
      ui_done("Target is assigned!")
    }
  } else {
    ui_warn("You did not provide any column name or index!")
    if (ui_yeah("Do you want to proceed with the default?",
      yes = "Yes",
      no = "No", shuffle = FALSE
    )) {
      input_data <- oil
      input_entry <- input_data[, 1]
      input_entry_name <- colnames(input_data)[1]
      ui_done("Target is assigned!")
    } else {
      ui_stop("Stopping the program!")
    }
  }

  cond_stop <- 0
  counter <- 0
  selection_object <- list()
  while (cond_stop == 0) {
    if (counter == 0) {
      message <- "Do you want to proceed with a transformation of your choice?"
    } else {
      message <- "Should I proceed with new transformation?"
    }
    if (ui_yeah(message,
      yes = "Yes",
      no = "No", shuffle = FALSE
    )) {
      counter <- counter + 1
      ui_info("Here are the list of available transformations: ")
      ui_code_block(c(
        "1: Rolling standard deviation",
        "2: Rolling mean",
        "3: Lagging",
        "4: Leading",
        "5: Differencing",
        "6: Spread (between two input drivers)",
        "7: Ratio (between two input drivers)",
        "8: Product (between two input drivers)"
      ))
      ui_info("Please provide a number as your choice:")
      selection <- as.numeric(readline(prompt = "Selection: "))
      ui_done("Transformation choice is received!")
      if (selection == 1) {
        ui_info(
          "Please provide a number as the window for rolling standard deviation:
          ")
        win <- as.numeric(readline(prompt = "Window: "))
        output_driver <- rolling_sd(input_entry, win)
        ui_done("Rolling standard deviation is calculated!")
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0("rolling_sd_", win),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0("rolling_sd_", win)
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 2) {
        ui_info("Please provide a number as the window for rolling mean:")
        win <- as.numeric(readline(prompt = "Window: "))
        output_driver <- rolling_mean(input_entry, win)
        ui_done("Rolling mean is calculated!")
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0("rolling_mean_", win),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0("rolling_mean_", win)
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 3) {
        ui_info("Please provide a number as the lag:")
        order_lag <- as.numeric(readline(prompt = "Lag: "))
        output_driver <- lag_fun(input_entry, order_lag)
        ui_done(paste0("Lag ", order_lag, " is calculated!"))
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0("lag_", order_lag),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0("lag_", order_lag)
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 4) {
        ui_info("Please provide a number as the lead:")
        order_lead <- as.numeric(readline(prompt = "Lead: "))
        output_driver <- lead_fun(input_entry, order_lead)
        ui_done(paste0("Lead ", order_lead, " is calculated!"))
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0("lead_", order_lead),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0("lead_", order_lead)
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 5) {
        output_driver <- diff_fun(input_entry)
        ui_done("Difference is calculated!")
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = "Difference",
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- "Difference"
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 6) {
        ui_info(
          "Please provide the column index or name of the second inputdriver."
        )
        column_numbers <- ncol(input_data)
        ui_info("There are {ui_value(column_numbers)} columns in the input
                data.")
        column_names <- colnames(input_data)
        ui_info("Column names in the current input data are:
                {ui_value(column_names)}")
        if (ui_yeah("Do you want to proceed with column index?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_index_2 <- as.numeric(readline(prompt = "Enter column index: "))
          output_driver <- spread_fun(input_entry, input_data[, col_index_2])
        } else if (ui_yeah("Do you want to proceed with column name?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_name_2 <- readline(prompt = "Enter column name: ")
          output_driver <- spread_fun(input_entry, input_data[, col_name_2])
          col_index_2 <- which(colnames(input_data) == col_name_2)
        }

        ui_done("Spread between input driver 1 and 2 is calculated!")
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0(
            "Spread w.r.t ",
            colnames(input_data)[col_index_2]
          ),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0(
          "Spread w.r.t ",
          colnames(input_data)[col_index_2]
        )
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 7) {
        ui_info(
          "Please provide the column index or name of the second input driver."
        )
        column_numbers <- ncol(input_data)
        ui_info(
          "There are {ui_value(column_numbers)} columns in the input data."
        )
        column_names <- colnames(input_data)
        ui_info("Column names in the current input data are:
                {ui_value(column_names)}")
        if (ui_yeah("Do you want to proceed with column index?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_index_2 <- as.numeric(readline(prompt = "Enter column index: "))
          output_driver <- ratio_fun(input_entry, input_data[, col_index_2])
        } else if (ui_yeah("Do you want to proceed with column name?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_name_2 <- readline(prompt = "Enter column name: ")
          output_driver <- ratio_fun(input_entry, input_data[, col_name_2])
          col_index_2 <- which(colnames(input_data) == col_name_2)
        }

        ui_done("Ratio between input driver 1 and 2 is calculated!")
        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0(
            "Ratio w.r.t ",
            colnames(input_data)[col_index_2]
          ),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0(
          "Ratio w.r.t ",
          colnames(input_data)[col_index_2]
        )
        ui_done("Transformed column is added to input data!")
      }

      if (selection == 8) {
        ui_info(
          "Please provide the column index or name of the second input driver."
        )
        column_numbers <- ncol(input_data)
        ui_info(
          "There are {ui_value(column_numbers)} columns in the input data."
        )
        column_names <- colnames(input_data)
        ui_info("Column names in the current input data are:
                {ui_value(column_names)}")
        if (ui_yeah("Do you want to proceed with column index?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_index_2 <- as.numeric(readline(prompt = "Enter column index: "))
          output_driver <- prod_fun(input_entry, input_data[, col_index_2])
        } else if (ui_yeah("Do you want to proceed with column name?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          col_name_2 <- readline(prompt = "Enter column name: ")
          output_driver <- prod_fun(input_entry, input_data[, col_name_2])
          col_index_2 <- which(colnames(input_data) == col_name_2)
        }
        ui_done("Product of input driver 1 and 2 is calculated!")

        normality_test <- shapiro.test(output_driver)
        stationarity_test <- adf.test(na.omit(output_driver))
        corr_coef <- cor(input_entry, output_driver, use = "complete.obs")
        selection_object[[counter]] <- list(
          "Input entry" = input_entry_name,
          "Transformation" = paste0(
            "Product with ",
            colnames(input_data)[col_index_2]
          ),
          "Normality at 0.05" =
            (normality_test$p.value > 0.05),
          "Stationarity at 0.05" =
            (stationarity_test$p.value < 0.05),
          "Correlation with target" = corr_coef
        )
        ui_done("Transformation and tests are added to the meta data object!")
        input_data <- cbind(input_data, output_driver)
        colnames(input_data)[ncol(input_data)] <- paste0(
          "Product with ",
          colnames(input_data)[col_index_2]
        )
        ui_done("Transformed column is added to input data!")
      }

      if (ui_yeah("Do you want to proceed with another transformation?",
        yes = "Yes",
        no = "No", shuffle = FALSE
      )) {
        if (ui_yeah("Do you want to change the target input column?",
          yes = "Yes",
          no = "No", shuffle = FALSE
        )) {
          ui_info("I can proceed with column name or comlun index!")
          if (ui_yeah("Do you want to proceed with column name?",
            yes = "Yes",
            no = "No", shuffle = FALSE
          )) {
            column_names <- colnames(input_data)
            ui_info("The column names are: {ui_value(column_names)}")
            col_name <- readline(prompt = "Enter column name: ")

            if (col_name %in% colnames(input_data)) {
              input_entry <- input_data[, col_name]
              input_entry_name <- col_name
              ui_done("New target is assigned!")
            } else {
              ui_warn("I need a valid column name.
                      Please provide a valid column name!")
              ui_info("I'm going ot use the default column name OILPRICE!")
              input_data <- data(oil)
              input_entry <- input_data[, "OILPRICE"]
              input_entry_name <- "OILPRICE"
              ui_done("New target is assigned!")
            }
          } else if (ui_yeah("Do you want to proceed with column index?",
            yes = "Yes",
            no = "No", shuffle = FALSE
          )) {
            column_numbers <- ncol(input_data)
            ui_info(
              "There are {ui_value(column_numbers)} columns in the input data"
            )
            col_index <- as.numeric(readline(prompt = "Enter column index: "))

            if (0 < col_index & col_index <= ncol(input_data)) {
              input_entry <- input_data[, col_index]
              input_entry_name <- colnames(input_data)[col_index]
              ui_done("New target is assigned!")
            } else {
              ui_warn("Column index should be between:
                      {ui_value(1)} and {ui_value(column_numbers)}")
              ui_info("I'm going ot use the default column number 1!")
              input_data <- data(oil)
              input_entry <- input_data[, 1]
              input_entry_name <- colnames(input_data)[1]
              ui_done("New target is assigned!")
            }
          }
        }
      } else {
        cond_stop <- 1
      }
    } else {
      cond_stop <- 1
    }
  }

  return(list(meta_object = selection_object, extended_data = input_data))
}
