# 工具：翻譯與單位換算模組

# 1. 單位換算函數
mph_to_kph <- function(mph) {
    round(mph * 1.60934, 1)
}

ft_to_m <- function(ft) {
    round(ft * 0.3048, 1)
}

# 2. 字典定義
player_name_dict <- c(
    # --- 台灣隊 (2026 WBC CSV 名單) ---
    "Yi Chang" = "張奕 (Chang Yi)",
    "Chang Yi" = "張奕 (Chang Yi)",
    "Po-Yu Chen" = "陳柏毓 (Chen Po-Yu)",
    "Chen Po-Yu" = "陳柏毓 (Chen Po-Yu)",
    "Hao-Chun Cheng" = "鄭浩均 (Cheng Hao-Chun)",
    "Cheng Hao-Chun" = "鄭浩均 (Cheng Hao-Chun)",
    "Ruei-Yang Gu Lin" = "古林睿煬 (Gu Lin Ruei-Yang)",
    "Gu Lin Ruei-Yang" = "古林睿煬 (Gu Lin Ruei-Yang)",
    "Jui-Yang Gulin" = "古林睿煬 (Gu Lin Ruei-Yang)",
    "Jo-Hsi Hsu" = "徐若熙 (Hsu Jo-Hsi)",
    "Hsu Jo-Hsi" = "徐若熙 (Hsu Jo-Hsi)",
    "Chih-Wei Hu" = "胡智為 (Hu Chih-Wei)",
    "Hu Chih-Wei" = "胡智為 (Hu Chih-Wei)",
    "Kai-Wei Lin" = "林凱威 (Lin Kai-Wei)",
    "Lin Kai-Wei" = "林凱威 (Lin Kai-Wei)",
    "Shih-Hsiang Lin" = "林詩翔 (Lin Shih-Hsiang)",
    "Lin Shih-Hsiang" = "林詩翔 (Lin Shih-Hsiang)",
    "Tzu-Chen Sha" = "沙子宸 (Sha Tzu-Chen)",
    "Sha Tzu-Chen" = "沙子宸 (Sha Tzu-Chen)",
    "Yi-Lei Sun" = "孫易磊 (Sun Yi-Lei)",
    "Sun Yi-Lei" = "孫易磊 (Sun Yi-Lei)",
    "Chun-Yueh Tseng" = "曾峻岳 (Tseng Chun-Yueh)",
    "Tseng Chun-Yueh" = "曾峻岳 (Tseng Chun-Yueh)",
    "Chun-Wei Chang" = "張峻瑋 (Chang Chun-Wei)",
    "Chang Chun-Wei" = "張峻瑋 (Chang Chun-Wei)",
    "Zhong-Ao Zhuang Chen" = "莊陳仲敖 (Zhuang Chen Zhong-Ao)",
    "Zhuang Chen Zhong-Ao" = "莊陳仲敖 (Zhuang Chen Zhong-Ao)",
    "Chen Zhong-Ao Zhuang" = "莊陳仲敖 (Zhuang Chen Zhong-Ao)",
    "Kuan-Yu Chen" = "陳冠宇 (Chen Kuan-Yu)",
    "Chen Kuan-Yu" = "陳冠宇 (Chen Kuan-Yu)",
    "Yu-Min Lin" = "林昱珉 (Lin Yu-Min)",
    "Lin Yu-Min" = "林昱珉 (Lin Yu-Min)",
    "Wei-En Lin" = "林維恩 (Lin Wei-En)",
    "Lin Wei-En" = "林維恩 (Lin Wei-En)",
    "Kungkuan Giljegiljaw" = "吉力吉撈·鞏冠 (Giljegiljaw Kungkuan)",
    "Giljegiljaw Kungkuan" = "吉力吉撈·鞏冠 (Giljegiljaw Kungkuan)",
    "Lyle Lin" = "林家正 (Lin Chia-Cheng)",
    "Chia-Cheng Lin" = "林家正 (Lin Chia-Cheng)",
    "Lin Chia-Cheng" = "林家正 (Lin Chia-Cheng)",
    "Shao-Hung Chiang" = "蔣少宏 (Chiang Shao-Hung)",
    "Chiang Shao-Hung" = "蔣少宏 (Chiang Shao-Hung)",
    "Yu Chang" = "張育成 (Yu Chang)",
    "Yu-Cheng Chang" = "張育成 (Yu Chang)",
    "Tsung-Che Cheng" = "鄭宗哲 (Cheng Tsung-Che)",
    "Cheng Tsung-Che" = "鄭宗哲 (Cheng Tsung-Che)",
    "Kun-Yu Chiang" = "江坤宇 (Chiang Kun-Yu)",
    "Chiang Kun-Yu" = "江坤宇 (Chiang Kun-Yu)",
    "Nien-Ting Wu" = "吳念庭 (Wu Nien-Ting)",
    "Wu Nien-Ting" = "吳念庭 (Wu Nien-Ting)",
    "Tzu-Wei Lin" = "林子偉 (Lin Tzu-Wei)",
    "Lin Tzu-Wei" = "林子偉 (Lin Tzu-Wei)",
    "Cheng-Yu Chang" = "張政禹 (Chang Cheng-Yu)",
    "Chang Cheng-Yu" = "張政禹 (Chang Cheng-Yu)",
    "Jonathon Long" = "龍恩 (Jonathon Long)",
    "Chieh-Hsien Chen" = "陳傑憲 (Chen Chieh-Hsien)",
    "Chen Chieh-Hsien" = "陳傑憲 (Chen Chieh-Hsien)",
    "Chen-Wei Chen" = "陳晨威 (Chen Chen-Wei)",
    "Chen Chen-Wei" = "陳晨威 (Chen Chen-Wei)",
    "An-Ko Lin" = "林安可 (Lin An-Ko)",
    "Lin An-Ko" = "林安可 (Lin An-Ko)",
    "Stuart Fairchild" = "費爾柴德 (Stuart Fairchild)",

    # --- 澳洲隊 (Australia) 焦點打者 ---
    "Travis Bazzana" = "T. Bazzana",
    "Robbie Perkins" = "R. Perkins",
    "Curtis Mead" = "C. Mead",
    "Tim Kennelly" = "T. Kennelly",
    "Aaron Whitefield" = "A. Whitefield",
    "Alex Hall" = "A. Hall",

    # --- 日本隊 焦點球員 ---
    "Shohei Ohtani" = "大谷翔平 (Shohei Ohtani)",
    "Yoshinobu Yamamoto" = "山本由伸 (Yoshinobu Yamamoto)"
)

team_dict <- c(
    "Chinese Taipei" = "台灣 (Chinese Taipei)",
    "Australia" = "澳洲 (Australia)",
    "Japan" = "日本 (Japan)",
    "Korea" = "韓國 (Korea)",
    "Czechia" = "捷克 (Czechia)"
)

pitch_type_dict <- c(
    "Four-Seam Fastball" = "四縫線速球 (Four-Seam Fastball)",
    "Slider" = "滑球 (Slider)",
    "Changeup" = "變速球 (Changeup)",
    "Curveball" = "曲球 (Curveball)",
    "Sinker" = "伸卡球 (Sinker)",
    "Cutter" = "卡特球 (Cutter)",
    "Splitter" = "指叉球 (Splitter)",
    "Sweeper" = "橫掃球 (Sweeper)",
    "Knuckleball" = "蝴蝶球 (Knuckleball)",
    "Knuckle Curve" = "彈指曲球 (Knuckle Curve)",
    "Forkball" = "指叉球 (Forkball)",
    "Slurve" = "滑曲球 (Slurve)"
)

result_event_dict <- c(
    "Single" = "一壘安打 (Single)",
    "Double" = "二壘安打 (Double)",
    "Triple" = "三壘安打 (Triple)",
    "Home Run" = "全壘打 (Home Run)",
    "Strikeout" = "三振 (Strikeout)",
    "Walk" = "四壞保送 (Walk)",
    "Groundout" = "滾地球出局 (Groundout)",
    "Flyout" = "飛球出局 (Flyout)",
    "Lineout" = "平飛球出局 (Lineout)",
    "Pop Out" = "內野高飛出局 (Pop Out)",
    "Forceout" = "封殺 (Forceout)",
    "Double Play" = "雙殺 (Double Play)",
    "Grounded Into DP" = "滾地球雙殺 (Grounded Into DP)",
    "Sac Fly" = "高飛犧牲打 (Sac Fly)",
    "Hit By Pitch" = "觸身球 (Hit By Pitch)",
    "Field Error" = "失誤上壘 (Field Error)",
    "Fielders Choice" = "野手選擇 (Fielders Choice)"
)

# 3. 欄位自動翻譯擴充函數
enrich_stats_with_translation <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }

    # 翻譯打者姓名
    if ("打者" %in% names(df)) {
        df$打者 <- sapply(df$打者, function(x) ifelse(x %in% names(player_name_dict), player_name_dict[[x]], x))
    }

    # 翻譯投手姓名
    if ("投手" %in% names(df)) {
        df$投手 <- sapply(df$投手, function(x) ifelse(x %in% names(player_name_dict), player_name_dict[[x]], x))
    }

    # 翻譯隊伍名稱
    if ("隊伍" %in% names(df)) {
        df$隊伍 <- sapply(df$隊伍, function(x) ifelse(x %in% names(team_dict), team_dict[[x]], x))
    }
    if ("防守方" %in% names(df)) {
        df$防守方 <- sapply(df$防守方, function(x) ifelse(x %in% names(team_dict), team_dict[[x]], x))
    }

    # 翻譯球種
    if ("球種" %in% names(df)) {
        df$球種 <- sapply(df$球種, function(x) ifelse(x %in% names(pitch_type_dict), pitch_type_dict[[x]], x))
    }

    # 翻譯擊球結果
    if ("結果" %in% names(df)) {
        df$結果 <- sapply(df$結果, function(x) ifelse(x %in% names(result_event_dict), result_event_dict[[x]], x))
    }

    # 公制單位換算 (初速 mph -> kph)
    if ("最快擊球初速_mph" %in% names(df)) {
        df <- df %>%
            mutate(最快擊球初速_kph = mph_to_kph(最快擊球初速_mph)) %>%
            relocate(最快擊球初速_kph, .after = 最快擊球初速_mph)
    }
    if ("平均初速_mph" %in% names(df)) {
        df <- df %>%
            mutate(平均初速_kph = mph_to_kph(平均初速_mph)) %>%
            relocate(平均初速_kph, .after = 平均初速_mph)
    }
    if ("最快初速_mph" %in% names(df)) {
        df <- df %>%
            mutate(最快初速_kph = mph_to_kph(最快初速_mph)) %>%
            relocate(最快初速_kph, .after = 最快初速_mph)
    }
    if ("擊球初速_mph" %in% names(df)) {
        df <- df %>%
            mutate(擊球初速_kph = mph_to_kph(擊球初速_mph)) %>%
            relocate(擊球初速_kph, .after = 擊球初速_mph)
    }

    # 投手球速 (mph -> kph)
    if ("平均球速_mph" %in% names(df)) {
        df <- df %>%
            mutate(平均球速_kph = mph_to_kph(平均球速_mph)) %>%
            relocate(平均球速_kph, .after = 平均球速_mph)
    }
    if ("最快球速_mph" %in% names(df)) {
        df <- df %>%
            mutate(最快球速_kph = mph_to_kph(最快球速_mph)) %>%
            relocate(最快球速_kph, .after = 最快球速_mph)
    }

    # 距離單位換算
    if ("距離_ft" %in% names(df)) {
        df <- df %>%
            mutate(距離_m = ft_to_m(距離_ft)) %>%
            relocate(距離_m, .after = 距離_ft)
    }

    return(df)
}
