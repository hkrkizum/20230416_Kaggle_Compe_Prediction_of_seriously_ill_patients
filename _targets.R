# 0. load package -------------------------
library(targets)


# 1. set optins --------------------------------------------------------------
options(tidyverse.quiet = TRUE)
tar_option_set(
  packages = c("tidyverse",
               "data.table",
               "here",
               "skimr",
               "qs",
               
               "gtsummary",
               "smd",
               
               "gghalves",
               "patchwork"),
  format = "qs",
  seed = 54147
)

# 2. set functions ----------------------------------------------------------
convert_df_num_to_factor <- function(df, param_col_to_factor){
  df |>
    dplyr::mutate(across(.cols = tidyselect::all_of(param_col_to_factor),
                         .fns = as.character)) |>
    dplyr::mutate(across(.cols = tidyselect::all_of(param_col_to_factor),
                         .fns = ~fct_relevel(.x, sort)))
}

make_gg_single_Continuous <- function(df, param_val){
  df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    # dplyr::group_by(target_label) |> 
    # dplyr::summarise(mean = mean({{ param_val }}),
    #                  sd = sd({{ param_val }})) |> 
    ggplot(aes(x = target_label,
               y = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    
    # geom_bar(stat = "identity") +
    geom_half_violin(nudge = 0.05, show.legend = FALSE) +
    geom_half_boxplot(nudge = 0.05,
                      side = "r",
                      fill = "white",
                      show.legend = FALSE) +
    # geom_half_point(transformation = position_jitter(height = 0,
    #                                                  width = 0.05),
    #                 alpha = 0.1, shape = 19) +
    
    ylab(as.character(as_label( enquo(param_val) ))) +
    theme_bw(base_size = 15)
}

make_gg_single_Categoly <- function(df, param_val){
  g1 <- 
    df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    ggplot(aes(x = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    geom_bar(stat = "count", show.legend = FALSE) +
    # ylab(as.character(as_label( enquo(param_val) ))) +
    theme_bw(base_size = 15)
  
  g2 <- 
    df |> 
    dplyr::filter(if_any(.cols = {{ param_val }}, .fns = ~!is.na(.x))) |> 
    ggplot(aes(x = {{ param_val }},
               colour = target_label,
               fill = target_label)) +
    geom_bar(position = "fill", show.legend = TRUE) +
    scale_y_continuous(labels = scales::label_percent()) +
    ylab("") +
    theme_bw(base_size = 15)
  
  g1 + g2
}

# 3. define pipeline --------------------------------------------------------
list(
  ## 1. ファイル名の設定 -------------------------------------------------
  tar_target(
    name = in_f_train,
    command = {
      here::here("Rawdata", "train_df.csv")
    },
    format = "file"
  ),
  tar_target(
    name = in_f_test,
    command = {
      here::here("Rawdata", "test_df.csv")
    },
    format = "file"
  ),
  tar_target(
    name = in_f_submitt,
    command = {
      here::here("Rawdata", "submission.csv")
    },
    format = "file"
  ),
  
  ## 2. データの読み込み ------------------------------------
  tar_target(
    name = df_train,
    command = {
      data.table::fread(in_f_train)
    }
  ),
  tar_target(
    name = df_test,
    command = {
      data.table::fread(in_f_test)
    }
  ),
  tar_target(
    name = df_submitt,
    command = {
      data.table::fread(in_f_submitt)
    }
  ),
  
  ## 3. EDA -----------
  ### 1. ユニーク数の確認 --------------------
  tar_target(
    name = df_Check_unique_record_train,
    command = {
      df_train %>% 
        summarise(across(dplyr::everything(), n_distinct)) |> 
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Variable",
                            values_to = "unique") |> 
        dplyr::arrange(unique) 
    }
  ),
  tar_target(
    name = df_Check_unique_record_test,
    command = {
      df_test %>% 
        summarise(across(dplyr::everything(), n_distinct)) |> 
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Variable",
                            values_to = "unique") |> 
        dplyr::arrange(unique) 
    }
  ),
  
  ### 2. 数値2ファクターへの変形 ----------------------------------
  #### 列名の抽出
  tar_target(
    name = param_col_to_factor,
    command = {
      tmp <- 
        df_train %>% 
        summarise(across(dplyr::everything(), n_distinct)) |> 
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Variable",
                            values_to = "unique") |> 
        dplyr::filter(unique <= 12) |> 
        dplyr::pull(Variable)
      
      tmp <- c(tmp, "icu_id", "icu_5")
    }
  ),
  #### acrossで一括変換
  tar_target(
    name = df_train_num2factor,
    command = {
      convert_df_num_to_factor(df = df_train, 
                               param_col_to_factor = param_col_to_factor)
    }
  ),
  tar_target(
    name = df_test_num2factor,
    command = convert_df_num_to_factor(df = df_test, 
                                       param_col_to_factor = param_col_to_factor[!param_col_to_factor == "target_label"]) 
  ),
  
  ### 3. 要約テーブル作成 ------------------------------------------
  tar_target(
    name = gts_df_train,
    command = {
      df_train_num2factor |> 
        gtsummary::tbl_summary(
          by = target_label,
          missing = "no"
        ) |> 
        gtsummary::modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") |> 
        gtsummary::bold_labels() |> 
        # gtsummary::add_difference() |> 
        gtsummary::add_difference(test = list(
          all_continuous() ~ "t.test",
          all_categorical() ~ "fisher.test")) |> 
        gtsummary::add_q(method = "holm") 
    }
  ),
  tar_target(
    name = gts_df_test,
    command = {
      df_test_num2factor |> 
        gtsummary::tbl_summary(
          # by = target_label,
          missing = "no"
        ) |> 
        gtsummary::modify_header(all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") |> 
        gtsummary::bold_labels()
        # gtsummary::add_p(test = list(
        #   all_continuous() ~ "t.test",
        #   all_categorical() ~ "fisher.test")) |> 
        # gtsummary::add_q(method = "holm") 
    }
  ),
  
  ### 4. 欠損値の確認 --------------
  tar_target(
    name = df_Check_NA_train,
    command = {
      df_train_num2factor |> 
        dplyr::group_by(target_label) |> 
        dplyr::summarise(across(dplyr::everything(), ~sum(is.na(.x)))) |> 
        tidyr::pivot_longer(cols = -target_label,
                            names_to = "Variable",
                            values_to = "Misssing") |> 
        dplyr::mutate(Col_type = case_when(
          str_detect(Variable,
                     glue::glue_collapse(param_col_to_factor, sep = "|")) ~ "Categoly",
          TRUE ~ "Continuous"
        )) |> 
        dplyr::left_join(df_train_num2factor |> 
                           dplyr::group_by(target_label) |> 
                           dplyr::summarise(Total = n())) |> 
        tidyr::pivot_wider(names_from = target_label,
                           values_from = c(Misssing,Total)) |> 
        dplyr::mutate(Ratio_0 = Misssing_0/Total_0*100,
                      Ratio_1 = Misssing_1/Total_1*100)
    }
  ),
  tar_target(
    name = df_Check_NA_test,
    command = {
      df_test_num2factor |> 
        dplyr::summarise(across(dplyr::everything(), ~sum(is.na(.x)))) |> 
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Variable",
                            values_to = "Misssing") |> 
        dplyr::mutate(Col_type = case_when(
          str_detect(Variable,
                     glue::glue_collapse(param_col_to_factor, sep = "|")) ~ "Categoly",
          TRUE ~ "Continuous"
        )) |> 
        dplyr::select(Variable, Col_type, Misssing) |> 
        dplyr::mutate(Total = dim(df_test_num2factor)[1]) |> 
        dplyr::mutate(Ratio = Misssing/Total*100 |> round(digits = 1))
    }
  ),
  
  ### 5. グラフ ---------------------------------------- 
  #### 1. 基礎情報 -------------------------------------
  ##
  ## age, bmi, hight, weight
  ##
  tar_target(
    name = g_age,
    command = make_gg_single_Continuous(df = df_train_num2factor,
                                        param_val = age)
  ),
  tar_target(
    name = g_bmi,
    command = make_gg_single_Continuous(df = df_train_num2factor,
                                        param_val = bmi)
  ),
  tar_target(
    name = g_height,
    command = make_gg_single_Continuous(df = df_train_num2factor,
                                        param_val = height)
  ),
  tar_target(
    name = g_weight,
    command = make_gg_single_Continuous(df = df_train_num2factor,
                                        param_val = weight)
  ),
  
  #### 2. 基礎情報 - カテゴリ -----------------------------
  tar_target(
    name = g_situation_1,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = situation_1)
  ),
  tar_target(
    name = g_situation_2,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = situation_2)
  ),
  tar_target(
    name = g_ethnicity,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = ethnicity) & theme(axis.text.x = element_text(angle = 30, hjust = 1))
  ),
  tar_target(
    name = g_gender,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = gender)
  ),
  
  #### 3. ICU ---------------------------------------------
  
  ##### 0. ICU ID ------------------------------------------
  tar_target(
    name = g_icu_id,
    command = {
      df_train_num2factor |> 
        dplyr::group_by(target_label, icu_id) |> 
        dplyr::summarise(n = n(), .groups = "drop") |> 
        tidyr::pivot_wider(names_from = target_label,
                           values_from = n, names_prefix = "target_label_", 
                           values_fill = 0) |> 
        dplyr::mutate(Ratio = target_label_1/(target_label_0 + target_label_1)) |> 
        dplyr::filter((target_label_0 + target_label_1) > 10) |>
        dplyr::arrange(desc(Ratio)) |> 
        dplyr::mutate(icu_id = fct_inorder(as.character(icu_id))) |> 
        ggplot(aes(x = icu_id, y = Ratio)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::label_percent()) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) 
    }
  ),
  ##### 1. ICU 1 ------------------------------------------
  tar_target(
    name = g_icu_1,
    command = {
      make_gg_single_Categoly(df = df_train_num2factor,
                              param_val = icu_1) & 
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  ),
  ##### 2. ICU 2 ------------------------------------------
  tar_target(
    name = g_icu_2,
    command = {
      make_gg_single_Categoly(df = df_train_num2factor,
                              param_val = icu_2) 
    }
  ),
  ##### 3. ICU 3 ------------------------------------------
  tar_target(
    name = g_icu_3,
    command = {
      make_gg_single_Categoly(df = df_train_num2factor,
                              param_val = icu_3)  & 
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    }
  ),
  ##### 4. ICU 4 ------------------------------------------
  tar_target(
    name = g_icu_4,
    command = {
      make_gg_single_Continuous(df = df_train_num2factor,
                              param_val = icu_4) 
    }
  ),
  tar_target(
    name = g_icu_4_log,
    command = {
      make_gg_single_Continuous(df = df_train_num2factor |> 
                                dplyr::mutate(icu_4 = log10(icu_4+1)),
                              param_val = icu_4) 
    }
  ),
  ##### 5. ICU 5 ------------------------------------------
  tar_target(
    name = g_icu_5,
    command = {
      df_train_num2factor |> 
        dplyr::group_by(target_label, icu_5) |> 
        dplyr::summarise(n = n(), .groups = "drop") |> 
        tidyr::pivot_wider(names_from = target_label,
                           values_from = n, names_prefix = "target_label_", 
                           values_fill = 0) |> 
        dplyr::mutate(Ratio = target_label_1/(target_label_0 + target_label_1)) |> 
        # dplyr::filter((target_label_0 + target_label_1) > 10) |>
        dplyr::arrange(desc(Ratio)) |> 
        dplyr::mutate(icu_5 = fct_inorder(as.character(icu_5))) |> 
        ggplot(aes(x = icu_5, y = Ratio)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::label_percent()) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) 
    }
  ),
  ##### 6. ICU 6 ------------------------------------------
  tar_target(
    name = g_icu_6,
    command = {
      make_gg_single_Continuous(df = df_train_num2factor,
                                param_val = icu_6)
    }
  ),
  ##### 7. ICU 7 ------------------------------------------
  tar_target(
    name = g_icu_7,
    command = {
      make_gg_single_Categoly(df = df_train_num2factor,
                              param_val = icu_7)
    }
  ),
  ##### 8. ICU 8 ------------------------------------------
  tar_target(
    name = g_icu_8,
    command = {
      make_gg_single_Categoly(df = df_train_num2factor,
                              param_val = icu_8)
    }
  ),
  
  #### 4. グラスゴー・コーマ・スケール ----------------------
  ##### 1. --------------------------------
  tar_target(
    name = g_GSC_1,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = glasgow_coma_scale_1)
  ),
  ##### 2. --------------------------------
  tar_target(
    name = g_GSC_2,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = glasgow_coma_scale_2)
  ),
  ##### 3. --------------------------------
  tar_target(
    name = g_GSC_3,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = glasgow_coma_scale_3)
  ),
  ##### 4. --------------------------------
  tar_target(
    name = g_GSC_4,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = glasgow_coma_scale_4)
  ),
  
  ### 5. 生理パラメータ -----------------------
  #### 1. HR --------
  tar_target(
    name = g_heart_rate,
    command = make_gg_single_Continuous(df = df_train_num2factor,
                                         param_val = heart_rate)
  ),
  #### 2. blood_oxy --------
  tar_target(
    name = g_blood_oxy,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                        param_val = blood_oxy)
  ),
  #### 3. arterial_pressure --------
  tar_target(
    name = g_blood_oxy,
    command = make_gg_single_Categoly(df = df_train_num2factor,
                                      param_val = blood_oxy)
  ),
)
