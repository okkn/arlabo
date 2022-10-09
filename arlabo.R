require(tidyverse)
require(lubridate)
require(assertr)
require(imputeTS)
require(RcppRoll) # 移動和を計算する

#' 「AR-laboが出力したCSVファイル」からメタデータを抽出する関数
#'
#' @param file_path 「AR-laboが出力したCSVファイル」のファイルパスを表す文字列
#' @return 「AR-laboが出力したCSVファイル」に記録されたメタデータのデータフレーム
#' @examples
#' read_metadata("./data/0522-3-C1.csv")
read_metadata <- function(file_path) {
  read_csv(file_path, skip = 1, n_max = 7,
           col_names = F, show_col_types = F) %>%
    pivot_wider(names_from = 1, values_from = 2) %>%
    assertr::verify(
      assertr::has_all_names("Exp.Date", "Start Time", "Finish Time",
                             "Project Name", "Session Name", "Camera No",
                             "Analysis No")
    ) %>%
    assertr::verify(
      nrow(.) == 1,
      description = paste("Expected 1 row, found", nrow(.), "rows.")
    ) %>%
    mutate(
      `Exp.Date` = lubridate::as_datetime(`Exp.Date`),
      `Start Time` = lubridate::as_datetime(`Start Time`),
      `Finish Time` = lubridate::as_datetime(`Finish Time`),
    )
}

#' 「AR-laboが出力したCSVファイル」からマウスのマーカー情報を抽出する関数
#'
#' @param file_path 「AR-laboが出力したCSVファイル」のファイルパスを表す文字列
#' @return 「AR-laboが出力したCSVファイル」に記録されたマウスの情報のデータフレーム
#' @examples
#' read_marker("./data/0522-3-C1.csv")
read_marker <- function(file_path) {
  read_csv(file_path, skip = 8, n_max = 1, col_select=c(2, 3, 4, 5),
           col_names = F, show_col_types = F) %>%
    t() %>%
    as_tibble_col(column_name = "value") %>%
    mutate(id = as.character(row_number())) %>%
    select(id, everything()) %>%
    separate(col = value, into = c("marker_id", "description"), sep=":") %>%
    assertr::verify(
      assertr::has_all_names("id", "marker_id", "description")
    ) %>%
    assertr::verify(
      nrow(.) == 4,
      description = paste("Expected 4 rows, found", nrow(.), "rows.")
    )
}

#' マウスの情報を抽出する関数
#'
#' @param file_path 「AR-laboが出力したCSVファイル」のファイルパスを表す文字列
#' @param mouse_list_path 「実験マウス一覧のCSVファイル」のファイルパスの文字列
#' @return 「AR-laboが出力したCSVファイル」のマウスの情報のデータフレーム
#' @examples
#' read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv")
read_mice_list <- function(mice_list_path, comp_col, comparison, paired_col = NULL) {
  mice_list <- read_csv(mice_list_path, show_col_types = F) %>%
    mutate(Camera = as.character(Camera), ID = as.character(ID)) %>%
    assertr::verify(
      assertr::has_all_names("Session", "Camera", "ID"),
      description = "`Session`, `Camera`, and `ID` must exist in mice_list"
    ) %>%
    assertr::verify(
      assertr::has_all_names(comp_col),
      description = "`comp_col` must be a column that exists in mice_list"
    ) %>%
    assertr::verify(
      length(comparison) == 2,
      description = "`comparison` must be of lengh 2"
    ) %>%
    assertr::verify(
      all(comparison %in% .[[comp_col]]),
      description = "`comparison` must be values that exists in `comp_col`"
    )
  
  attr(mice_list, "comp_col") = comp_col
  attr(mice_list, "comparison") = comparison
  attr(mice_list, "paired_col") = paired_col
  
  if (!is.null(paired_col)) {
    mice_list %>%
      assertr::verify(
        assertr::has_all_names(paired_col),
        description = "`paired_col` must be a column that exists in mice_list"
      )
  }
  
  return(mice_list)
}

#' マウスの情報を抽出する関数
#'
#' @param file_path 「AR-laboが出力したCSVファイル」のファイルパスを表す文字列
#' @param mouse_list_path 「実験マウス一覧のCSVファイル」のファイルパスの文字列
#' @return 「AR-laboが出力したCSVファイル」のマウスの情報のデータフレーム
#' @examples
#' read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv")
read_mice_info <- function(file_path, mice_list) {
  meta = read_metadata(file_path)
  analysis <- read_marker(file_path) %>%
    mutate(
      session = meta$`Session Name`,
      camera = meta$`Camera No` %>% str_sub(end = 1)
    ) %>%
    left_join(
      mice_list,
      by = c("session" = "Session", "camera" = "Camera", "marker_id" = "ID")
    )
  attr(analysis, "comp_col") = attr(mice_list, "comp_col")
  attr(analysis, "comparison") = attr(mice_list, "comparison")
  attr(analysis, "paired_col") = attr(mice_list, "paired_col")
  return(analysis)
}


#' 「AR-laboが出力したCSVファイル」から座標、移動距離、マウス間距離を計算する
#'
#' @param file_path 「AR-laboが出力したCSVファイル」のファイルパスを表す文字列
#' @param fps 元の動画のfps（フレーム/秒、1秒あたりのフレーム数）の数値
#'
#' @return 各時刻における、各マウスの座標とマウス間距離のデータフレーム
#'
#' @examples
#' read_exp_behavior("./data/0522-3-C1.csv")
read_mice_behavior <- function(file_path, fps) {
  # メタデータの13行分をskipして座標のデータをデータフレームとして読み込む
  mice_behavior <- read_csv(file_path, skip = 13, show_col_types = F) %>%
    suppressWarnings()
  
  # AR-LABOのバージョンの違いで出力される列名が異なるため場合分け
  # また、Rで扱いやすいように列名を変更する。
  if ("ID-1 [X(mm)]" %in% names(mice_behavior)) {
    mice_behavior <- mice_behavior %>%
      rename(
        time = `date time`,
        id_1_x = `ID-1 [X(mm)]`, id_1_y = `ID-1 [Y(mm)]`,
        id_2_x = `ID-2 [X(mm)]`, id_2_y = `ID-2 [Y(mm)]`,
        id_3_x = `ID-3 [X(mm)]`, id_3_y = `ID-3 [Y(mm)]`,
        id_4_x = `ID-4 [X(mm)]`, id_4_y = `ID-4 [Y(mm)]`
      )
  } else {
    mice_behavior <- mice_behavior %>%
      rename(
        time = `date time`,
        id_1_x = `ID-1 [Marker-X(mm)]`, id_1_y = `ID-1 [Marker-Y(mm)]`,
        id_2_x = `ID-2 [Marker-X(mm)]`, id_2_y = `ID-2 [Marker-Y(mm)]`,
        id_3_x = `ID-3 [Marker-X(mm)]`, id_3_y = `ID-3 [Marker-Y(mm)]`,
        id_4_x = `ID-4 [Marker-X(mm)]`, id_4_y = `ID-4 [Marker-Y(mm)]`
      )
  }
  
  # 古いバージョンのAR-LABOの仕様でコンマの数が合わないことがある
  # その場合、右端の値が、数値にコンマのついた文字列となってしまうため修正
  if (!is.numeric(mice_behavior$id_4_y)) {
    mice_behavior <- mice_behavior %>%
      mutate(
        id_4_y = id_4_y %>%
          str_sub(end = -2) %>%
          as.numeric()
      )
  }
  
  mice_behavior %>%
    select( # 必要な変数のみをselect
      time, id_1_x, id_1_y, id_2_x, id_2_y, id_3_x, id_3_y, id_4_x, id_4_y
    ) %>%
    distinct(time, .keep_all = T) %>% # timeに重複がないことを保証
    
    # 一旦time以外の変数をWide形式 → Long形式に変換（処理の都合）
    pivot_longer(cols = -time, 
                 names_pattern = "id_([1-4])_([xy])",
                 names_to = c("id", "axis")) %>%
    pivot_wider(names_from = axis) %>% # Axisはwide形式とする（idはlongのまま）
    group_nest(id) %>% # idごとに処理するためにnest
    mutate(
      # 各idが最初に出現する時刻求め、`mintime`として保存する
      mintime = map(data, ~ drop_na(.)) %>% map(~ min(.$time)),
      
      # 座標の欠損値を線形補間（imputeTS::na_interpolation）
      data = map(
        data, # IDごとに分けたサブセットのデータフレームに対して、
        ~ mutate(., across(-time, #時刻以外の列（=xy座標）の欠損値を線形補間
                           ~ imputeTS::na_interpolation(., option = "linear")))
      )
    ) %>%
    unnest(cols = c(data, mintime)) %>% # nestを解除
    filter(time >= max(mintime)) %>% # すべてのidが出揃った時刻以降を使用する
    select(-mintime) %>% # mintimeは以降使わないため外す
    # 再度Long形式→Wide形式に戻す。
    pivot_wider(names_from = id,
                values_from = c(x, y),
                names_glue = "id_{id}_{.value}") %>%
    
    mutate(
      # time0: 開始時刻を0とした時刻
      time0 = difftime(time, min(time), unit = "mins"),
      
      # move: 1フレームごとの移動距離（足し合わせたものが活動量activity）
      id_1_move = sqrt((id_1_x - lag(id_1_x)) ^ 2 + (id_1_y - lag(id_1_y)) ^ 2),
      id_2_move = sqrt((id_2_x - lag(id_2_x)) ^ 2 + (id_2_y - lag(id_2_y)) ^ 2),
      id_3_move = sqrt((id_3_x - lag(id_3_x)) ^ 2 + (id_3_y - lag(id_3_y)) ^ 2),
      id_4_move = sqrt((id_4_x - lag(id_4_x)) ^ 2 + (id_4_y - lag(id_4_y)) ^ 2),
      
      # dist: マウス間距離
      dist_1_2 = sqrt((id_1_x - id_2_x) ^ 2 + (id_1_y - id_2_y) ^ 2),
      dist_1_3 = sqrt((id_1_x - id_3_x) ^ 2 + (id_1_y - id_3_y) ^ 2),
      dist_1_4 = sqrt((id_1_x - id_4_x) ^ 2 + (id_1_y - id_4_y) ^ 2),
      dist_2_1 = dist_1_2,
      dist_2_3 = sqrt((id_2_x - id_3_x) ^ 2 + (id_2_y - id_3_y) ^ 2),
      dist_2_4 = sqrt((id_2_x - id_4_x) ^ 2 + (id_2_y - id_4_y) ^ 2),
      dist_3_1 = dist_1_3,
      dist_3_2 = dist_2_3,
      dist_3_4 = sqrt((id_3_x - id_4_x) ^ 2 + (id_3_y - id_4_y) ^ 2),
      dist_4_1 = dist_1_4,
      dist_4_2 = dist_2_4,
      dist_4_3 = dist_3_4,
      
      # 前後1秒間（`fps`フレーム）のmoveの移動和（Reverse, Forward）
      across(ends_with("_move"),
             list(rollR = ~ RcppRoll::roll_sum(., n = fps, align = "right", fill = NA),
                  rollF = ~ RcppRoll::roll_sum(., n = fps, align = "left", fill = NA)))
    )
}

#' マウス間距離の閾値を指定してinteractionの解析に使うデータを準備
#'
#' @param behavior read_exp_interaction_raw()関数の出力するデータフレーム
#' @param threshold_dist interactionありと判断するマウス間距離の閾値の数値
#'
#' @return 各時刻における、閾値を考慮したinteractionの準備データのデータフレーム
#'
#' @examples
#' analyze_mice_interaction(
#'   read_exp_behavior("./data/0522-3-C1.csv"),
#'   threshold_dist = 30
#' )
analyze_mice_interaction <- function(behavior, threshold_dist) {
  behavior %>%
    mutate(
      # マウス間距離が`threshold_dist`未満なら1、それ以外は0
      # 作られる変数名の例：dist_1_2_nearing
      across(starts_with("dist_"),
             ~ if_else(. < threshold_dist, 1, 0),
             .names = "{col}_nearing"),
      
      # マウス同士が近づくevent発生で-1、離れるevent発生で1、それ以外は0
      # 作られる変数名の例：dist_1_2_nearing_event
      across(ends_with("_nearing"),
             ~ {lag(.) - .},
             .names = "{col}_event")
    ) %>%
    
    # IDa, IDb, nearing, nearing_eventという列が作られる
    pivot_longer(cols = starts_with("dist_"),
                 names_to = c("IDa", "IDb", ".value"),
                 names_pattern = "^dist_([1-4])_([1-4])_(.+)$") %>%
    
    # マウス同士が近づくevent発生で-1、離れるevent発生で1、それ以外は0
    filter(nearing_event != 0) %>%
    mutate(
      IDa_move_rollR = case_when(IDa == "1" ~ id_1_move_rollR,
                                 IDa == "2" ~ id_2_move_rollR, 
                                 IDa == "3" ~ id_3_move_rollR,
                                 IDa == "4" ~ id_4_move_rollR),
      IDb_move_rollR = case_when(IDb == "1" ~ id_1_move_rollR,
                                 IDb == "2" ~ id_2_move_rollR, 
                                 IDb == "3" ~ id_3_move_rollR,
                                 IDb == "4" ~ id_4_move_rollR),
      IDa_move_rollF = case_when(IDa == "1" ~ id_1_move_rollF,
                                 IDa == "2" ~ id_2_move_rollF, 
                                 IDa == "3" ~ id_3_move_rollF,
                                 IDa == "4" ~ id_4_move_rollF),
      IDb_move_rollF = case_when(IDb == "1" ~ id_1_move_rollF,
                                 IDb == "2" ~ id_2_move_rollF, 
                                 IDb == "3" ~ id_3_move_rollF,
                                 IDb == "4" ~ id_4_move_rollF),
      
      # IDaとIDbのうち直前に動いている方のID（Approachの時はこっちで判定）
      movingR_id = ifelse(IDa_move_rollR > IDb_move_rollR, IDa, IDb),
      # IDaとIDbのうち直後に動いている方のID（Departの時はこっちで判定）
      movingF_id = ifelse(IDa_move_rollF > IDb_move_rollF, IDa, IDb)
    ) %>%
    group_by(IDa, IDb) %>%
    mutate(
      duration = lead(time) - time,
      
      #depart_id: Approach後のどのIDがdepartしたか
      depart_id = ifelse(nearing_event == -1, lead(movingF_id), NA),
      
      #approach_id: どのIDからApproachされたときのdepartのデータか
      approach_id = ifelse(nearing_event == 1, lag(movingR_id), NA)
    ) %>%
    select(
      time, IDa, IDb, nearing_event, movingR_id, movingF_id,
      approach_id, depart_id, duration, nearing, everything()
    ) %>%
    ungroup()
}

#' 総移動距離total activityを計算する（0～`within_time`[min]のデータを使用）
#'
#' @param behaivor `read_mice_behavior()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#'
#' @return 総移動距離の計算結果；中間ファイルに追記するためのデータフレーム
#'
#' @examples
#' calc_total_activity(
#'  behavior = read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90
#' )
calc_total_activity <- function(behavior, mice_info, within_time) {
  behavior %>%
    filter(time0 < within_time) %>%
    select(time, ends_with("move")) %>% #必要な列のみselect
    pivot_longer(cols = -time,
                 names_to = "id",
                 values_to = "activity",
                 names_pattern = "id_([1-4])_move") %>%
    group_by(id) %>%
    summarise(
      total_activity = sum(activity, na.rm = T) / 1000 # [m]単位にする
    ) %>%
    left_join(mice_info, by = "id")
}

#' 各binの移動距離bin activityを計算する（0～`within_time`[min]のデータを使用）
#'
#' @param behaivor `read_mice_behavior()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#' @param bin_width binの幅[min]を表す数値
#'
#' @return 各binの移動距離の計算結果；中間ファイルに追記するためのデータフレーム
#'
#' @examples
#' calc_bin_activity(
#'  behavior = read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90,
#'  bin_width = 30
#' )
calc_bin_activity <- function(behavior, mice_info, within_time, bin_width) {
  behavior %>%
    select(time, time0, ends_with("move")) %>% #必要な列のみselect
    filter(time0 < within_time) %>%
    mutate(
      # bin: 所属するbinの開始時刻[min]（どのbinに属するか）
      bin = as.integer(trunc(time0 / bin_width) * bin_width)
    ) %>%
    pivot_longer(cols = -c(time, time0, bin),
                 names_to = "id",
                 values_to = "activity",
                 names_pattern = "id_([1-4])_move") %>%
    group_by(id, bin) %>%
    summarise(
      bin_activity = sum(activity, na.rm = T) / 1000, # [m]単位にする
      .groups="drop"
    ) %>%
    # 欠損しているbinの行があれば補う
    complete(
      id, bin = full_seq(c(0, (within_time %/% bin_width - 1) * bin_width),
                         period = bin_width)
    ) %>%
    # 欠損している欄があれば0で埋める
    complete(
      id, bin,
      fill = list(bin_activity = 0)
    ) %>%
    left_join(mice_info, by = "id")
}

# マウス間距離の解析（0～`within`分のデータを使用）
#' マウス間距離を計算する（0～`within_time`[min]のデータを使用）
#'
#' @param behaivor `read_mice_behavior()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#'
#' @return マウス間距離の計算結果；中間ファイルに追記するためのデータフレーム
#'
#' @examples
#' calc_total_distance(
#'  behavior = read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90
#' )
calc_total_distance <- function(behavior, mice_info, within_time) {
  behavior %>%
    filter(time0 < within_time) %>%
    select(time, starts_with("dist")) %>% #必要な列のみselect
    pivot_longer(cols = -time,
                 names_to = c("IDa", "IDb"),
                 values_to = "dist",
                 names_pattern = "dist_([1-4])_([1-4])") %>%
    group_by(time, IDa) %>%
    summarise(
      mean_dist = mean(dist, na.rm = T),
      closest_dist = min(dist, na.rm = T),
      .groups="drop"
    ) %>%
    group_by(IDa) %>%
    summarise(total_mean_dist = mean(mean_dist, na.rm = T),
              total_closest_dist = mean(closest_dist, na.rm = T)) %>%
    left_join(mice_info, by = c("IDa" = "id"))
}

#' 各binのマウス間距離を計算する（0～`within_time`[min]のデータを使用）
#'
#' @param behaivor `read_mice_behavior()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#' @param bin_width binの幅[min]を表す数値
#'
#' @return 各binのマウス間距離の計算結果；中間ファイルに追記するデータフレーム
#'
#' @examples
#' calc_bin_distance(
#'  behavior = read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90,
#'  bin_width = 30
#' )
calc_bin_distance <- function(behavior, mice_info, within_time, bin_width) {
  behavior %>%
    select(time, time0, starts_with("dist")) %>% #必要な列のみselect
    filter(time0 < within_time) %>%
    mutate(
      # bin: 所属するbinの開始時刻[min]（どのbinに属するか）
      bin = as.integer(trunc(time0 / bin_width) * bin_width)
    ) %>%
    pivot_longer(cols = -c(time, time0, bin),
                 names_to = c("IDa", "IDb"),
                 values_to = "dist",
                 names_pattern = "dist_([1-4])_([1-4])") %>%
    group_by(time, IDa, bin) %>%
    summarise(
      mean_dist = mean(dist, na.rm = T),
      closest_dist = min(dist, na.rm = T),
      .groups = "drop"
    ) %>%
    group_by(IDa, bin) %>%
    summarise(
      bin_mean_dist = mean(mean_dist, na.rm = T),
      bin_closest_dist = mean(closest_dist, na.rm = T),
      .groups="drop"
    ) %>%
    left_join(mice_info, by = c("IDa" = "id")) %>%
    return()
}

#' マウス間interactionを計算する（0～`within_time`[min]のデータを使用）
#'
#' @param interaction `analyze_mice_interaction()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#'
#' @return マウス間interactionの計算結果；中間ファイルに追記するためのデータフレーム
#'
#' @examples
#' calc_total_interaction(
#'  behavior = analyze_mice_interaction(
#'    read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'    threshold_dist = 30
#'  ),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90
#' )
calc_total_interaction <- function(d_inter, mice_info, within_time) {
  d_inter %>%
    filter(nearing_event == -1) %>% # マウス同士が近づくevent発生で-1
    filter(time0 < within_time) %>%
    group_by(IDa) %>%
    summarise(
      total_event_count = n(),
      total_approach_count = sum(movingR_id == IDa, na.rm = T),
      total_receive_count = sum(movingR_id != IDa, na.rm = T),
      total_duration = sum(duration, na.rm = T),
      total_approach_duration = sum(duration * ifelse(movingR_id == IDa, 1, 0), na.rm = T),
      total_receive_duration = sum(duration * ifelse(movingR_id != IDa, 1, 0), na.rm = T),
    ) %>%
    left_join(mice_info, by = c("IDa" = "id"))
}

#' 各binのマウス間interactionを計算する（0～`within_time`[min]のデータを使用）
#'
#' @param interaction `analyze_mice_interaction()`関数の戻り値のデータフレーム
#' @param mice_info `read_mice_info()`関数の戻り値のデータフレーム
#' @param within_time 解析の対象とする時間[min]を表す数値
#' @param bin_width binの幅[min]を表す数値
#'
#' @return 各binのinteractionの計算結果；中間ファイルに追記するデータフレーム
#'
#' @examples
#' calc_bin_interaction(
#'  behavior = analyze_mice_interaction(
#'    read_mice_behavior("./data/0522-3-C1.csv", fps = 20),
#'    threshold_dist = 30
#'  ),
#'  mice_info = read_mice_info("./data/0522-3-C1.csv", "./list/Oxtr-list.csv"),
#'  within_time = 90,
#'  bin_width = 30
#' )
calc_bin_interaction <- function(d_inter, mice_info,
                                 within_time, bin_width) {
  d_inter %>%
    filter(nearing_event == -1) %>% #マウス同士が近づくevent発生で-1
    filter(time0 < within_time) %>%
    mutate(
      # bin: 所属するbinの開始時刻[min]（どのbinに属するか）
      bin = as.integer(trunc(time0 / bin_width) * bin_width)
    ) %>%
    group_by(IDa, bin) %>%
    summarise(
      bin_event_count = n(),
      bin_approach_count = sum(movingR_id == IDa, na.rm = T),
      bin_receive_count = sum(movingR_id != IDa, na.rm = T),
      bin_duration = sum(duration, na.rm = T),
      bin_approach_duration = sum(duration * ifelse(movingR_id == IDa, 1, 0), na.rm = T),
      bin_receive_duration = sum(duration * ifelse(movingR_id != IDa, 1, 0), na.rm = T),
      .groups="drop"
    ) %>%
    # 欠損しているbinの行があれば補う
    complete(
      IDa, bin = full_seq(c(0, (within_time %/% bin_width - 1) * bin_width),
                          period = bin_width)
    ) %>%
    # 欠損している欄があれば0で埋める（累積値はその後で計算する）
    complete(
      IDa, bin,
      fill = list(
        bin_duration = lubridate::dseconds(0),
        bin_approach_duration = lubridate::dseconds(0),
        bin_receive_duration = lubridate::dseconds(0),
        bin_event_count = 0, bin_approach_count = 0, bin_receive_count = 0
      )
    ) %>%
    group_by(IDa) %>%
    mutate(
      bin_event_cumul = cumsum(bin_event_count),
      bin_approach_cumul = cumsum(bin_approach_count),
      bin_receive_cumul = cumsum(bin_receive_count)
    ) %>%
    ungroup() %>%
    left_join(mice_info, by = c("IDa" = "id"))
}

#' 解析結果の一時ファイル（生の結果）をプロジェクトのディレクトリ内に作成する
#'
#' @param project_dir 実験プロジェクトのファイルを置くディレクトリ
#' @param mice_list `read_mice_list()`関数の戻り値のデータフレーム
#' @param fps 実験の動画のfps
#' @param activity Activityの解析をするかどうか
#' @param distance Distanceの解析をするかどうか
#' @param interaction Interactionの解析をするかどうか
#' @param within_time Totalでの解析の対象とする時間[min]を表す数値（複数指定可）
#' @param bin_within_time Bin毎の解析の対象とする時間[min]を表す数値
#' @param bin_width binの幅[min]を表す数値
#' @param threshold_dist interactionありと判断するマウス間距離の閾値の数値
#'
#' @return NULL
#'
#' @examples
#' create_temporary_files(
#'   project_dir = "./projects/btbr",
#'   mice_list = read_mice_list(
#'     "./projects/btbr/list/btbr_b6_list.csv",
#'     comp_col = "Line",
#'     comparison = c("BTBR", "B6")
#'   ),
#'   within_time = c(30, 60),
#'   bin_within_time = 60,
#'   bin_width = c(5, 10, 15, 30),
#'   threshold_dist = c(20, 30, 40)
#' )
create_temporary_files <- function(project_dir,
                                   mice_list,
                                   fps = 20,
                                   activity = T,
                                   distance = T,
                                   interaction = T,
                                   within_time = c(30, 60, 90),
                                   bin_within_time = 90,
                                   bin_width = c(5, 10, 15, 30),
                                   threshold_dist = 20) {
  cat("Start working in", project_dir, "\r\n")
  
  # 一時ファイルを保存するディレクトリが存在するか確認する。
  # 存在しなければ作成する。
  if (!dir.exists(file.path(project_dir, "temp"))) {
    dir.create(file.path(project_dir, "temp"))
    cat(file.path(project_dir, "temp"), "not found. Created.\r\n")
  }
  
  skip_activity = T
  if (activity) {
    activity_path = file.path(project_dir, "temp/activity")
    if (dir.exists(activity_path)) {
      cat(activity_path, "exists. Skip generation of temporary files.\r\n")
    } else {
      skip_activity = F
      dir.create(activity_path)
      cat(activity_path, "not found. Created.\r\n")
    }
  }
  
  skip_dist = T
  if (distance) {
    dist_path = file.path(project_dir, "temp/dist")
    if (dir.exists(dist_path)) {
      cat(dist_path, "exists. Skip generation of temporary files.\r\n")
    } else {
      skip_dist = F
      dir.create(dist_path)
      cat(dist_path, "not found. Created.\r\n")
    }
  }
  
  skip_inter = T
  if (interaction) {
    inter_path = file.path(project_dir, "temp/inter")
    if (dir.exists(inter_path)) {
      cat(inter_path, "exists. Skip generation of temporary files.\r\n")
    } else {
      skip_inter = F
      dir.create(inter_path)
      cat(inter_path, "not found. Created.\r\n")
    }
  }
  
  if (skip_activity && skip_dist && skip_inter) return() # 早期に関数を抜ける
  
  
  file_list = list.files(path = file.path(project_dir, "data"),
                         full.names = T, pattern = "*.csv")
  
  # Variables used in `for (f in file_list)`
  is_first = T # write_csv()でheaderを出力するかを制御するためのフラグ
  file_counter = 1
  
  for (f in file_list) {
    cat("Processing", f, paste0("(", file_counter, " of ", length(file_list), ") \r\n"))
    flush.console()
    
    df <- read_mice_behavior(f, fps)
    info <- read_mice_info(f, mice_list)
    
    if(!skip_activity) {
      for (t in within_time) {
        df %>%
          calc_total_activity(mice_info = info, within_time = t) %>%
          write_csv(
            file.path(activity_path, paste0("activity_total_", t, "min.csv")),
            append = T, col_names = is_first
          )
      }
      
      for (b in bin_width) {
        df %>%
          calc_bin_activity(mice_info = info, within_time = bin_within_time, bin_width = b) %>%
          write_csv(
            file.path(
              activity_path,
              paste0(
                "activity_bin", str_pad(b, 2, pad = "0"),
                "_", bin_within_time, "min.csv"
              )
            ),
            append = T, col_names = is_first
          )
      }
    } # end of `if(!skip_activity)`
    
    if(!skip_dist) {
      for (t in within_time) {
        df %>%
          calc_total_distance(mice_info = info, within_time = t) %>%
          write_csv(
            file.path(dist_path, paste0("dist_total_", t, "min.csv")),
            append = T, col_names = is_first
          )
      }
      
      for (b in bin_width) {
        df %>%
          calc_bin_distance(mice_info = info, within_time = bin_within_time, bin_width = b) %>%
          write_csv(
            file.path(
              dist_path,
              paste0(
                "dist_bin", str_pad(b, 2, pad = "0"),
                "_", bin_within_time, "min.csv"
              )
            ),
            append = T, col_names = is_first
          )
      }
    } # end of `if(!skip_dist)`
    
    if(!skip_inter) {
      for (d in threshold_dist) {
        d_inter <- analyze_mice_interaction(df, threshold_dist = d)
        for (t in within_time) {
          d_inter %>%
            calc_total_interaction(mice_info = info, within_time = t) %>%
            write_csv(
              file.path(
                inter_path,
                paste0(
                  "inter", d, "_total", "_", t, "min.csv"
                )
              ),
              append = T, col_names = is_first
            )
        }
        
        for (b in bin_width) {
          d_inter %>%
            calc_bin_interaction(mice_info = info, within_time = bin_within_time, bin_width = b) %>%
            write_csv(
              file.path(
                inter_path,
                paste0(
                  "inter", d, "_bin", str_pad(b, 2, pad = "0"),
                  "_", bin_within_time, "min.csv"
                )
              ),
              append = T, col_names = is_first
            )
        }
      }
    } # end of `if(!skip_inter)`
    
    
    ## tail of `for (f in file_list)`
    file_counter = file_counter + 1
    is_first = F
  } # end of `for (f in file_list)`
  
}