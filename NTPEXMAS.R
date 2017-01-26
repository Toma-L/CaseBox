# Christmasland in New Taipei City
# Brian Tsai, Thomas Lin, Vincent Chen
# 2017/01/17 - 2017/01/24

xmas <- read.csv("新北歡樂耶誕城問卷.csv", header = TRUE, fileEncoding = "BIG-5")
head(xmas)
dim(xmas)


# 色票
#E3B505 - Citrine
#4F6D7A - Payne's Grey
#56A3A6 - Cadet Blue
#DB504A - Jelly Bean
#2C5530 - Mughal Green
#363F3B - Onyx
#00284C - Oxford Blue
#D5D9DE - Gainsbord


# 問卷回收狀況分析
2095 / 2150 * 100


# 遊客基本資料分析

# 性別分析
as.data.frame(table(xmas[which(xmas$X401 < 3), "X401"]))
sex_tot <- sum(as.data.frame(table(xmas[which(xmas$X401 < 3), "X401"]))[2:3, 2])
male_rate <- round(as.data.frame(table(xmas$X401))[2, 2] / sex_tot * 100, 2)
female_rate <- round(as.data.frame(table(xmas$X401))[3, 2] / sex_tot * 100, 2)
male_rate; female_rate
pie(c(male_rate, female_rate), labels = c("男(33.6%)", "女(66.4%)"), family = "Microsoft JhengHei", 
    col = c("#56A3A6", "#DB504A"), clockwise = TRUE)


# 年齡分析
as.data.frame(table(xmas$X402))
age_tot <- sum(as.data.frame(table(xmas$X402))[2:7, 2])
age1_rate <- round(as.data.frame(table(xmas$X402))[2, 2] / age_tot * 100, 2)
age2_rate <- round(as.data.frame(table(xmas$X402))[3, 2] / age_tot * 100, 2)
age3_rate <- round(as.data.frame(table(xmas$X402))[4, 2] / age_tot * 100, 2)
age4_rate <- round(as.data.frame(table(xmas$X402))[5, 2] / age_tot * 100, 2)
age5_rate <- round(as.data.frame(table(xmas$X402))[6, 2] / age_tot * 100, 2)
age6_rate <- round(as.data.frame(table(xmas$X402))[7, 2] / age_tot * 100, 2)
age_df <- data.frame(group = c("20歲（含以下）", "21歲 ~ 30歲", "31歲 ~ 40歲", "41歲 ~ 50歲", "51歲 ~ 60歲", "60歲（含以上）"), age = rbind(age1_rate, age2_rate, age3_rate, age4_rate, age5_rate, age6_rate))
age_df[order(age_df$age, decreasing = TRUE), ]
sum(age_df$age)
age_df$age <- c(18.4, 21.5, 32.3, 14.6, 7.5, 5.7)
age_df$group <- factor(age_df$group, levels = c("20歲（含以下）", "21歲 ~ 30歲", "31歲 ~ 40歲", "41歲 ~ 50歲", "51歲 ~ 60歲", "60歲（含以上）"))
ggplot(age_df, aes(x = group, y = age, fill = group)) + geom_bar(stat = "identity") +
        xlab("年齡") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A", "#2C5530", "#363F3B")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = age_df$age, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 婚姻狀況分析
as.data.frame(table(xmas$X403))
mar_tot <- sum(as.data.frame(table(xmas[which(xmas$X403 < 5), "X403"]))[2:5, 2])
mar1_rate <- round(as.data.frame(table(xmas$X403))[2, 2] / mar_tot * 100, 2)
mar2_rate <- round(as.data.frame(table(xmas$X403))[3, 2] / mar_tot * 100, 2)
mar3_rate <- round(as.data.frame(table(xmas$X403))[4, 2] / mar_tot * 100, 2)
mar4_rate <- round(as.data.frame(table(xmas$X403))[5, 2] / mar_tot * 100, 2)
mar_df <- data.frame(
        group = c("未婚（無小孩）", "未婚（有小孩）", "已婚（無小孩）", "已婚（有小孩）"), 
        mar = rbind(mar1_rate, mar2_rate, mar3_rate, mar4_rate))
mar_df[order(mar_df$mar, decreasing = TRUE), ]
sum(mar_df$mar)
mar_df$mar <- c(35.9, 12.3, 10.6, 41.2)
mar_df$group <- factor(mar_df$group, levels = c("未婚（無小孩）", "未婚（有小孩）", "已婚（無小孩）", "已婚（有小孩）"))
ggplot(mar_df, aes(x = group, y = mar, fill = group)) + geom_bar(stat = "identity") +
        xlab("婚姻狀況") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = mar_df$mar, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 職業分析
as.data.frame(table(xmas$X404))
job_tot <- sum(as.data.frame(table(xmas[which(xmas$X404 < 8), "X404"]))[2:8, 2])
job1_rate <- round(as.data.frame(table(xmas$X404))[2, 2] / job_tot * 100, 2)
job2_rate <- round(as.data.frame(table(xmas$X404))[3, 2] / job_tot * 100, 2)
job3_rate <- round(as.data.frame(table(xmas$X404))[4, 2] / job_tot * 100, 2)
job4_rate <- round(as.data.frame(table(xmas$X404))[5, 2] / job_tot * 100, 2)
job5_rate <- round(as.data.frame(table(xmas$X404))[6, 2] / job_tot * 100, 2)
job6_rate <- round(as.data.frame(table(xmas$X404))[7, 2] / job_tot * 100, 2)
job7_rate <- round(as.data.frame(table(xmas$X404))[8, 2] / job_tot * 100, 2)
job_df <- data.frame(group = c("農林漁牧", "工業、製造業", "商\n（自由業、服務業）", "軍公教", "學生", "無", "其他"), 
                     job = rbind(job1_rate, job2_rate, job3_rate, job4_rate, job5_rate, job6_rate, job7_rate))
job_df[order(job_df$job, decreasing = TRUE), ]
sum(job_df$job)
job_df$job <- c(1.1, 14.2, 36.2, 7.4, 20.6, 19.4, 1.1)
job_df$group <- factor(job_df$group, levels = c("農林漁牧", "工業、製造業", "商\n（自由業、服務業）", "軍公教", "學生", "無", "其他"))
ggplot(job_df, aes(x = group, y = job, fill = group)) + geom_bar(stat = "identity") +
        xlab("職業") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A", "#2C5530", "#363F3B", "#00284C")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = job_df$job, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 居住地分析
as.data.frame(table(xmas$X405))
res_tot <- sum(as.data.frame(table(xmas$X405))[2:9, 2])
res1_rate <- round(as.data.frame(table(xmas$X405))[2, 2] / res_tot * 100, 2)
res2_rate <- round(as.data.frame(table(xmas$X405))[3, 2] / res_tot * 100, 2)
res3_rate <- round(as.data.frame(table(xmas$X405))[4, 2] / res_tot * 100, 2)
res4_rate <- round(as.data.frame(table(xmas$X405))[5, 2] / res_tot * 100, 2)
res5_rate <- round(as.data.frame(table(xmas$X405))[6, 2] / res_tot * 100, 2)
res6_rate <- round(as.data.frame(table(xmas$X405))[7, 2] / res_tot * 100, 2)
res7_rate <- round(as.data.frame(table(xmas$X405))[8, 2] / res_tot * 100, 2)
res8_rate <- round(as.data.frame(table(xmas$X405))[9, 2] / res_tot * 100, 2)
res_df <- data.frame(group = c("外籍旅客", "新北市", "北部", "中部", "南部", "離島", "東部", "其他"), 
                     res = rbind(res1_rate, res2_rate, res3_rate, res4_rate, res5_rate, res6_rate, res7_rate, res8_rate))
res_df[order(res_df$res, decreasing = TRUE), ]
sum(res_df$res, na.rm = TRUE)
res_df$res <- c(2.2, 48.1, 35.7, 6.8, 2.0, 1.3, 3.2, 0.7)
res_df$group <- factor(res_df$group, levels = c("外籍旅客", "新北市", "北部", "中部", "南部", "離島", "東部", "其他"))
ggplot(res_df, aes(x = group, y = res, fill = group)) + geom_bar(stat = "identity") +
        xlab("居住地") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A", "#2C5530", "#363F3B", "#00284C", "#D5D9DE")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = res_df$res, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 教育程度分析
as.data.frame(table(xmas$X406))
edu_tot <- sum(as.data.frame(table(xmas[which(xmas$X406 < 5), "X406"]))[2:5, 2])
edu1_rate <- round(as.data.frame(table(xmas$X406))[2, 2] / edu_tot * 100, 2)
edu2_rate <- round(as.data.frame(table(xmas$X406))[3, 2] / edu_tot * 100, 2)
edu3_rate <- round(as.data.frame(table(xmas$X406))[4, 2] / edu_tot * 100, 2)
edu4_rate <- round(as.data.frame(table(xmas$X406))[5, 2] / edu_tot * 100, 2)
edu_df <- data.frame(group = c("國中（含以下）", "高中職", "大專院校", "研究所（含以上）"), 
                     edu = rbind(edu1_rate, edu2_rate, edu3_rate, edu4_rate))
edu_df[order(edu_df$edu, decreasing = TRUE), ]
sum(edu_df$edu, na.rm = TRUE)
edu_df$edu <- c(17.4, 29.6, 46.8, 6.2)
edu_df$group <- factor(edu_df$group, levels = c("國中（含以下）", "高中職", "大專院校", "研究所（含以上）"))
ggplot(edu_df, aes(x = group, y = edu, fill = group)) + geom_bar(stat = "identity") +
        xlab("教育程度") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A", "#2C5530", "#363F3B", "#00284C", "#D5D9DE")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = edu_df$edu, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 月收入分析
as.data.frame(table(xmas$X407))
inc_tot <- sum(as.data.frame(table(xmas$X407))[2:8, 2])
inc1_rate <- round(as.data.frame(table(xmas$X407))[2, 2] / inc_tot * 100, 2)
inc2_rate <- round(as.data.frame(table(xmas$X407))[3, 2] / inc_tot * 100, 2)
inc3_rate <- round(as.data.frame(table(xmas$X407))[4, 2] / inc_tot * 100, 2)
inc4_rate <- round(as.data.frame(table(xmas$X407))[5, 2] / inc_tot * 100, 2)
inc5_rate <- round(as.data.frame(table(xmas$X407))[6, 2] / inc_tot * 100, 2)
inc6_rate <- round(as.data.frame(table(xmas$X407))[7, 2] / inc_tot * 100, 2)
inc7_rate <- round(as.data.frame(table(xmas$X407))[8, 2] / inc_tot * 100, 2)
inc_df <- data.frame(group = c("無經常性收入", "1萬以下", "1 ~ 2萬", "2 ~ 3萬", "3 ~ 4萬", "4 ~ 5萬", "5萬以上"), 
                     inc = rbind(inc1_rate, inc2_rate, inc3_rate, inc4_rate, inc5_rate, inc6_rate, inc7_rate))
inc_df[order(inc_df$inc, decreasing = TRUE), ]
sum(inc_df$inc)
inc_df$inc <- c(26.2, 9.3, 15.7, 13.4, 22.5, 8.0, 4.9)
inc_df$group <- factor(inc_df$group, levels = c("無經常性收入", "1萬以下", "1 ~ 2萬", "2 ~ 3萬", "3 ~ 4萬", "4 ~ 5萬", "5萬以上"))
ggplot(inc_df, aes(x = group, y = inc, fill = group)) + geom_bar(stat = "identity") +
        xlab("月收入") + ylab("百分比") +
        theme(axis.title.y = element_text(angle = 0)) + 
        scale_fill_manual(values = c("#E3B505", "#4F6D7A", "#56A3A6", "#DB504A", "#2C5530", "#363F3B", "#00284C")) + 
        guides(fill = FALSE) + 
        geom_text(aes(label = inc_df$inc, vjust = -.75)) + 
        theme(text = element_text(family = "Microsoft JhengHei"))


# 路標指示清楚
as.data.frame(table(xmas$X201))
sum(as.data.frame(table(xmas$X201))[2:6, 2])
round(prop.table(table(xmas$X201[which(xmas$X201 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X201[which(xmas$X201 > 0)])), 3))
sat_1 <- sum(round(prop.table(table(xmas$X201[which(xmas$X201 > 0)])), 2)[1:2])
mean_1 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X201))[2:6, 1])) * 
                    as.data.frame(table(xmas$X201))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X201))[2:6, 2]))), 3)


# 道路交通順暢
as.data.frame(table(xmas$X202))
sum(as.data.frame(table(xmas$X202))[2:6, 2])
round(prop.table(table(xmas$X202[which(xmas$X202 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X202[which(xmas$X202 > 0)])), 3))
sat_2 <- sum(round(prop.table(table(xmas$X202[which(xmas$X202 > 0)])), 2)[1:2])
mean_2 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X202))[2:6, 1])) * 
                    as.data.frame(table(xmas$X202))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X202))[2:6, 2]))), 3)

# 服務態度良好
as.data.frame(table(xmas$X203))
sum(as.data.frame(table(xmas$X203))[2:6, 2])
round(prop.table(table(xmas$X203[which(xmas$X203 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X203[which(xmas$X203 > 0)])), 3))
sat_3 <- sum(round(prop.table(table(xmas$X203[which(xmas$X203 > 0)])), 2)[1:2])
mean_3 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X203))[2:6, 1])) * 
                    as.data.frame(table(xmas$X203))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X203))[2:6, 2]))), 3)

# 秩序管理良好
as.data.frame(table(xmas$X204))
sum(as.data.frame(table(xmas$X204))[2:6, 2])
round(prop.table(table(xmas$X204[which(xmas$X204 > 0 & xmas$X204 < 6)])), 3)
cumsum(round(prop.table(table(xmas$X204[which(xmas$X204 > 0 & xmas$X204 < 6)])), 3))
sat_4 <- sum(round(prop.table(table(xmas$X204[which(xmas$X204 > 0)])), 2)[1:2])
mean_4 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X204))[2:6, 1])) * 
                    as.data.frame(table(xmas$X204))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X204))[2:6, 2]))), 3)


# 節慶氣氛濃厚
as.data.frame(table(xmas$X205))
sum(as.data.frame(table(xmas$X205))[2:5, 2])
round(prop.table(table(xmas$X205[which(xmas$X205 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X205[which(xmas$X205 > 0)])), 3))
sat_5 <- sum(round(prop.table(table(xmas$X205[which(xmas$X205 > 0)])), 2)[1:2])
mean_5 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X205))[2:5, 1])) * 
                    as.data.frame(table(xmas$X205))[2:5, 2] / 
                    sum(as.data.frame(table(xmas$X205))[2:5, 2]))), 3)


# 會場皆有特色
as.data.frame(table(xmas$X206))
sum(as.data.frame(table(xmas$X206))[2:6, 2])
round(prop.table(table(xmas$X206[which(xmas$X206 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X206[which(xmas$X206 > 0)])), 3))
sat_6 <- sum(round(prop.table(table(xmas$X206[which(xmas$X206 > 0)])), 2)[1:2])
mean_6 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X206))[2:6, 1])) * 
                    as.data.frame(table(xmas$X206))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X206))[2:6, 2]))), 3)


# 環境整潔乾淨
as.data.frame(table(xmas$X207))
sum(as.data.frame(table(xmas$X207))[2:6, 2])
round(prop.table(table(xmas$X207[which(xmas$X207 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X207[which(xmas$X207 > 0)])), 3))
sat_7 <- sum(round(prop.table(table(xmas$X207[which(xmas$X207 > 0)])), 2)[1:2])
mean_7 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X207))[2:6, 1])) * 
                    as.data.frame(table(xmas$X207))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X207))[2:6, 2]))), 3)


# 垃圾桶地點數量良好
as.data.frame(table(xmas$X208))
sum(as.data.frame(table(xmas$X208))[2:6, 2])
round(prop.table(table(xmas$X208[which(xmas$X208 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X208[which(xmas$X208 > 0)])), 3))
sat_8 <- sum(round(prop.table(table(xmas$X208[which(xmas$X208 > 0)])), 2)[1:2])
mean_8 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X208))[2:6, 1])) * 
                    as.data.frame(table(xmas$X208))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X208))[2:6, 2]))), 3)


# 廁所位置數量整潔衛生
as.data.frame(table(xmas$X209))
sum(as.data.frame(table(xmas$X209))[2:6, 2])
round(prop.table(table(xmas$X209[which(xmas$X209 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X209[which(xmas$X209 > 0)])), 3))
sat_9 <- sum(round(prop.table(table(xmas$X209[which(xmas$X209 > 0)])), 2)[1:2])
mean_9 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X209))[2:6, 1])) * 
                    as.data.frame(table(xmas$X209))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X209))[2:6, 2]))), 3)


# 攤位整體服務品質良好
as.data.frame(table(xmas$X210))
sum(as.data.frame(table(xmas$X210))[2:6, 2])
round(prop.table(table(xmas$X210[which(xmas$X210 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X210[which(xmas$X210 > 0)])), 3))
sat_10 <- sum(round(prop.table(table(xmas$X210[which(xmas$X210 > 0)])), 2)[1:2])
mean_10 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X210))[2:6, 1])) * 
                    as.data.frame(table(xmas$X210))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X210))[2:6, 2]))), 3)


# 表演內容豐富有吸引力
as.data.frame(table(xmas$X211))
sum(as.data.frame(table(xmas$X211))[2:6, 2])
round(prop.table(table(xmas$X211[which(xmas$X211 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X211[which(xmas$X211 > 0)])), 3))
sat_11 <- sum(round(prop.table(table(xmas$X211[which(xmas$X211 > 0)])), 2)[1:2])
mean_11 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X211))[2:6, 1])) * 
                    as.data.frame(table(xmas$X211))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X211))[2:6, 2]))), 3)

# 活動有互動體驗
as.data.frame(table(xmas$X212))
sum(as.data.frame(table(xmas$X212))[2:6, 2])
round(prop.table(table(xmas$X212[which(xmas$X212 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X212[which(xmas$X212 > 0)])), 3))
sat_12 <- sum(round(prop.table(table(xmas$X212[which(xmas$X212 > 0)])), 2)[1:2])
mean_12 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X212))[2:6, 1])) * 
                    as.data.frame(table(xmas$X212))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X212))[2:6, 2]))), 3)

# 活動內容多樣化
as.data.frame(table(xmas$X213))
sum(as.data.frame(table(xmas$X213))[2:6, 2])
round(prop.table(table(xmas$X213[which(xmas$X213 > 0)])), 4)
cumsum(round(prop.table(table(xmas$X213[which(xmas$X213 > 0)])), 4))
sat_13 <- sum(round(prop.table(table(xmas$X213[which(xmas$X213 > 0)])), 2)[1:2])
mean_13 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X213))[2:6, 1])) * 
                    as.data.frame(table(xmas$X213))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X213))[2:6, 2]))), 3)


# 宣傳管道多元
as.data.frame(table(xmas$X214))
sum(as.data.frame(table(xmas$X214))[2:6, 2])
round(prop.table(table(xmas$X214[which(xmas$X214 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X214[which(xmas$X214 > 0)])), 3))
sat_14 <- sum(round(prop.table(table(xmas$X214[which(xmas$X214 > 0)])), 2)[1:2])
mean_14 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X214))[2:6, 1])) * 
                    as.data.frame(table(xmas$X214))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X214))[2:6, 2]))), 3)


# 宣傳內容豐富
as.data.frame(table(xmas$X215))
sum(as.data.frame(table(xmas$X215))[2:6, 2])
round(prop.table(table(xmas$X215[which(xmas$X215 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X215[which(xmas$X215 > 0)])), 3))
sat_15 <- sum(round(prop.table(table(xmas$X215[which(xmas$X215 > 0)])), 2)[1:2])
mean_15 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X215))[2:6, 1])) * 
                    as.data.frame(table(xmas$X215))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X215))[2:6, 2]))), 3)


# 宣傳的持續性
as.data.frame(table(xmas$X216))
sum(as.data.frame(table(xmas$X216))[2:6, 2])
round(prop.table(table(xmas$X216[which(xmas$X216 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X216[which(xmas$X216 > 0)])), 3))
sat_16 <- sum(round(prop.table(table(xmas$X216[which(xmas$X216 > 0)])), 2)[1:2])
mean_16 <- round((
        sum((7 - as.numeric(as.data.frame(table(xmas$X216))[2:6, 1])) * 
                    as.data.frame(table(xmas$X216))[2:6, 2] / 
                    sum(as.data.frame(table(xmas$X216))[2:6, 2]))), 3)


sat_df <- data.frame(c(79.9, 82.2, 88.1, 84.4, 90.4, 86.9, 79.4, 82.3, 85.1, 87.2, 81.3, 80.9, 83.5, 85.9, 82.2, 85.8))
colnames(sat_df) <- "sat"
sat_df$mean <- c(mean_1, mean_2, mean_3, mean_4, mean_5, mean_6, mean_7, mean_8, mean_9, mean_10, mean_11, mean_12, mean_13, mean_14, mean_15, mean_16)
sat_df$rank <- (17 - rank(sat_df$mean))
sat_df

# 1. 去年是否參加
as.data.frame(table(xmas$X101))
sum(as.data.frame(table(xmas$X101))[2:3, 2])
round(prop.table(table(xmas$X101[which(xmas$X101 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X101[which(xmas$X101 > 0)])), 3))


# 2. 同行成員
as.data.frame(table(xmas$X102))
xmas[, "X102"] <- as.character(xmas$X102)
xmas[which(xmas$X102 == "5男朋友"), "X102"] <- "5"
xmas[which(xmas$X102 == "5情人"), "X102"] <- "5"
sum(as.data.frame(table(xmas$X102))[2:6, 2])
round(prop.table(table(xmas$X102[which(xmas$X102 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X102[which(xmas$X102 > 0)])), 3))

# 3. 交通工具
tra_a <- NROW(grep("a", xmas$X103))
tra_b <- NROW(grep("b", xmas$X103))
tra_c <- NROW(grep("c", xmas$X103))
tra_d <- NROW(grep("d", xmas$X103))
tra_e <- NROW(grep("e", xmas$X103))
tra_f <- NROW(grep("f", xmas$X103))
tra_g <- NROW(grep("g", xmas$X103))
tra_h <- NROW(grep("h", xmas$X103))
tra_i <- NROW(grep("i", xmas$X103))
tra_j <- NROW(grep("j", xmas$X103))
tra_k <- NROW(grep("k", xmas$X103))
tra_df <- data.frame(
        freq = c(tra_a, tra_b, tra_c, tra_d, tra_e, tra_f, tra_g, tra_h, tra_i, tra_j, tra_k))
rownames(tra_df) <- paste("tra_", letters[1:11], sep = "")
tra_df$prop <- round(tra_df$freq / sum(xmas$X103 != 0) * 100, 3)
tra_df$rank <- 12 - rank(tra_df$prop)
tra_df

# 4. 主要動機
mot_a <- NROW(grep("a", xmas$X104))
mot_b <- NROW(grep("b", xmas$X104))
mot_c <- NROW(grep("c", xmas$X104))
mot_d <- NROW(grep("d", xmas$X104))
mot_e <- NROW(grep("e", xmas$X104))
mot_f <- NROW(grep("f", xmas$X104))
mot_g <- NROW(grep("g", xmas$X104))
mot_h <- NROW(grep("h", xmas$X104))
mot_i <- NROW(grep("i", xmas$X104))
mot_j <- NROW(grep("j", xmas$X104))
mot_k <- NROW(grep("k", xmas$X104))
mot_df <- data.frame(
        freq = c(mot_a, mot_b, mot_c, mot_d, mot_e, mot_f, mot_g, mot_h, mot_i, mot_j, mot_k))
rownames(mot_df) <- paste("mot_", letters[1:11], sep = "")
mot_df$prop <- round(mot_df$freq / sum(xmas$X104 != 0) * 100, 3)
mot_df$rank <- 12 - rank(mot_df$prop)
mot_df

# 5. 專程 OR 順道
as.data.frame(table(xmas$X105))
sum(as.data.frame(table(xmas$X105))[2:3, 2])
round(prop.table(table(xmas$X105[which(xmas$X105 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X105[which(xmas$X105 > 0)])), 3))

# 6. 今年是否會再次造訪
as.data.frame(table(xmas$X106))
sum(as.data.frame(table(xmas$X106))[2:3, 2])
round(prop.table(table(xmas$X106[which(xmas$X106 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X106[which(xmas$X106 > 0)])), 3))

# 7. 是否瀏覽網路資訊
as.data.frame(table(xmas$X107))
sum(as.data.frame(table(xmas$X107))[2:3, 2])
xmas[which(xmas$X107 == 12), "X107"] <- 0
round(prop.table(table(xmas$X107[which(xmas$X107 > 0)])), 4)
cumsum(round(prop.table(table(xmas$X107[which(xmas$X107 > 0)])), 4))

# 8. 桑塔熊直播
as.data.frame(table(xmas$X108))
sum(as.data.frame(table(xmas$X108))[2:3, 2])
round(prop.table(table(xmas$X108[which(xmas$X108 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X108[which(xmas$X108 > 0)])), 3))

# 9. 資訊管道
inf_a <- NROW(grep("a", xmas$X109))
inf_b <- NROW(grep("b", xmas$X109))
inf_c <- NROW(grep("c", xmas$X109))
inf_d <- NROW(grep("d", xmas$X109))
inf_e <- NROW(grep("e", xmas$X109))
inf_f <- NROW(grep("f", xmas$X109))
inf_g <- NROW(grep("g", xmas$X109))
inf_h <- NROW(grep("h", xmas$X109))
inf_i <- NROW(grep("i", xmas$X109))
inf_j <- NROW(grep("j", xmas$X109))
inf_k <- NROW(grep("k", xmas$X109))
inf_df <- data.frame(
        freq = c(inf_a, inf_b, inf_c, inf_d, inf_e, inf_f, inf_g, inf_h, inf_i, inf_j, inf_k))
rownames(inf_df) <- paste("inf_", letters[1:11], sep = "")
inf_df$prop <- round(inf_df$freq / sum(xmas$X109 != 0) * 100, 3)
inf_df$rank <- 12 - rank(inf_df$prop)
inf_df


# 10. 喜歡節慶嗎
as.data.frame(table(xmas$X110))
sum(as.data.frame(table(xmas$X110))[2:5, 2])
round(prop.table(table(xmas$X110[which(xmas$X110 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X110[which(xmas$X110 > 0)])), 3))


# 11. 提升幸福感
as.data.frame(table(xmas$X111))
sum(as.data.frame(table(xmas$X111))[2:6, 2])
round(prop.table(table(xmas$X111[which(xmas$X111 > 0)])), 4)
cumsum(round(prop.table(table(xmas$X111[which(xmas$X111 > 0)])), 4))


# 整體滿意度
as.data.frame(table(xmas$X301))
sum(as.data.frame(table(xmas$X301))[2:6, 2])
round(prop.table(table(xmas$X301[which(xmas$X301 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X301[which(xmas$X301 > 0)])), 3))


# 3D主燈秀滿意度
as.data.frame(table(xmas$X302))
sum(as.data.frame(table(xmas$X302))[2:6, 2])
round(prop.table(table(xmas$X302[which(xmas$X302 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X302[which(xmas$X302 > 0)])), 3))


# 燈飾滿意度
as.data.frame(table(xmas$X303))
sum(as.data.frame(table(xmas$X303))[2:6, 2])
round(prop.table(table(xmas$X303[which(xmas$X303 > 0)])), 4)
cumsum(round(prop.table(table(xmas$X303[which(xmas$X303 > 0)])), 4))


# 貨櫃市集滿意度
as.data.frame(table(xmas$X304))
sum(as.data.frame(table(xmas$X304))[2:6, 2])
round(prop.table(table(xmas$X304[which(xmas$X304 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X304[which(xmas$X304 > 0)])), 3))


# 桑塔熊周邊商品
as.data.frame(table(xmas$X305))
sum(as.data.frame(table(xmas$X305))[2:6, 2])
round(prop.table(table(xmas$X305[which(xmas$X305 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X305[which(xmas$X305 > 0)])), 3))


# 他人推薦
as.data.frame(table(xmas$X306))
sum(as.data.frame(table(xmas$X306))[2:6, 2])
round(prop.table(table(xmas$X306[which(xmas$X306 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X306[which(xmas$X306 > 0)])), 3))


# 明年再次參與
xmas[which(xmas$X307 == 6), "X307"] <- 0
as.data.frame(table(xmas$X307))
sum(as.data.frame(table(xmas$X307))[2:6, 2])
round(prop.table(table(xmas$X307[which(xmas$X307 > 0)])), 3)
cumsum(round(prop.table(table(xmas$X307[which(xmas$X307 > 0)])), 3))


# 交通費用
as.data.frame(table(xmas$X1121))
sum(as.data.frame(table(xmas[which(xmas$X1121 > 0), "X1121"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1121 > 1), "X1121"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1121 > 0), "X1121"]))[, 2]) * 100
tra_exp <- sum(as.data.frame(table(xmas$X1121))[2:8, 2], na.rm = TRUE)
tra1_exp <- round(as.data.frame(table(xmas$X1121))[2, 2] / tra_exp * 100, 2)
tra2_exp <- round(as.data.frame(table(xmas$X1121))[3, 2] / tra_exp * 100, 2)
tra3_exp <- round(as.data.frame(table(xmas$X1121))[4, 2] / tra_exp * 100, 2)
tra4_exp <- round(as.data.frame(table(xmas$X1121))[5, 2] / tra_exp * 100, 2)
tra5_exp <- round(as.data.frame(table(xmas$X1121))[6, 2] / tra_exp * 100, 2)
tra6_exp <- 0
tra7_exp <- round(as.data.frame(table(xmas$X1121))[7, 2] / tra_exp * 100, 2)
tra_df <- rbind(tra1_exp, tra2_exp, tra3_exp, tra4_exp, tra5_exp, tra6_exp, tra7_exp)
tra_df
tra_df[order(tra_df, decreasing = TRUE), ]
sum(tra_df, na.rm = TRUE)

(1928 / 2 - 422) / 1186 * 250 + 1


# 娛樂費用
as.data.frame(table(xmas$X1122))
sum(as.data.frame(table(xmas[which(xmas$X1122 > 0), "X1122"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1122 > 1), "X1122"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1122 > 0), "X1122"]))[, 2]) * 100
ent_exp <- sum(as.data.frame(table(xmas$X1122))[2:8, 2], na.rm = TRUE)
ent1_exp <- round(as.data.frame(table(xmas$X1122))[2, 2] / ent_exp * 100, 3)
ent2_exp <- round(as.data.frame(table(xmas$X1122))[3, 2] / ent_exp * 100, 3)
ent3_exp <- round(as.data.frame(table(xmas$X1122))[4, 2] / ent_exp * 100, 3)
ent4_exp <- round(as.data.frame(table(xmas$X1122))[5, 2] / ent_exp * 100, 3)
ent5_exp <- round(as.data.frame(table(xmas$X1122))[6, 2] / ent_exp * 100, 3)
ent6_exp <- round(as.data.frame(table(xmas$X1122))[7, 2] / ent_exp * 100, 3)
ent7_exp <- round(as.data.frame(table(xmas$X1122))[8, 2] / ent_exp * 100, 3)
ent_df <- rbind(ent1_exp, ent2_exp, ent3_exp, ent4_exp, ent5_exp, ent6_exp, ent7_exp)
ent_df
ent_df[order(ent_df, decreasing = TRUE), ]
sum(ent_df, na.rm = TRUE)

(1721 / 2 - 704) / 608 * 250 + 1


# 住宿費用
as.data.frame(table(xmas$X1123))
sum(as.data.frame(table(xmas[which(xmas$X1123 > 0), "X1123"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1123 > 1), "X1123"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1123 > 0), "X1123"]))[, 2]) * 100
res_exp <- sum(as.data.frame(table(xmas$X1123))[2:8, 2], na.rm = TRUE)
res1_exp <- round(as.data.frame(table(xmas$X1123))[2, 2] / res_exp * 100, 2)
res2_exp <- round(as.data.frame(table(xmas$X1123))[3, 2] / res_exp * 100, 2)
res3_exp <- round(as.data.frame(table(xmas$X1123))[4, 2] / res_exp * 100, 2)
res4_exp <- round(as.data.frame(table(xmas$X1123))[5, 2] / res_exp * 100, 2)
res5_exp <- round(as.data.frame(table(xmas$X1123))[6, 2] / res_exp * 100, 2)
res6_exp <- round(as.data.frame(table(xmas$X1123))[7, 2] / res_exp * 100, 2)
res7_exp <- round(as.data.frame(table(xmas$X1123))[8, 2] / res_exp * 100, 2)
res_df <- rbind(res1_exp, res2_exp, res3_exp, res4_exp, res5_exp, res6_exp, res7_exp)
res_df
res_df[order(res_df, decreasing = TRUE), ]
sum(res_df, na.rm = TRUE)


# 餐飲費用
as.data.frame(table(xmas$X1124))
sum(as.data.frame(table(xmas[which(xmas$X1124 > 0), "X1124"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1124 > 1), "X1124"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1124 > 0), "X1124"]))[, 2]) * 100
eat_exp <- sum(as.data.frame(table(xmas$X1124))[2:8, 2], na.rm = TRUE)
eat1_exp <- round(as.data.frame(table(xmas$X1124))[2, 2] / eat_exp * 100, 2)
eat2_exp <- round(as.data.frame(table(xmas$X1124))[3, 2] / eat_exp * 100, 2)
eat3_exp <- round(as.data.frame(table(xmas$X1124))[4, 2] / eat_exp * 100, 2)
eat4_exp <- round(as.data.frame(table(xmas$X1124))[5, 2] / eat_exp * 100, 2)
eat5_exp <- round(as.data.frame(table(xmas$X1124))[6, 2] / eat_exp * 100, 2)
eat6_exp <- round(as.data.frame(table(xmas$X1124))[7, 2] / eat_exp * 100, 2)
eat7_exp <- round(as.data.frame(table(xmas$X1124))[8, 2] / eat_exp * 100, 2)
eat_df <- rbind(eat1_exp, eat2_exp, eat3_exp, eat4_exp, eat5_exp, eat6_exp, eat7_exp)
eat_df
eat_df[order(eat_df, decreasing = TRUE), ]
sum(eat_df, na.rm = TRUE)

(1929 / 2 - 471) / 818 * 250 + 1


# 購物費用
as.data.frame(table(xmas$X1125))
sum(as.data.frame(table(xmas[which(xmas$X1125 > 0), "X1125"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1125 > 1), "X1125"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1125 > 0), "X1125"]))[, 2]) * 100
buy_exp <- sum(as.data.frame(table(xmas$X1125))[2:8, 2], na.rm = TRUE)
buy1_exp <- round(as.data.frame(table(xmas$X1125))[2, 2] / buy_exp * 100, 2)
buy2_exp <- round(as.data.frame(table(xmas$X1125))[3, 2] / buy_exp * 100, 2)
buy3_exp <- round(as.data.frame(table(xmas$X1125))[4, 2] / buy_exp * 100, 2)
buy4_exp <- round(as.data.frame(table(xmas$X1125))[5, 2] / buy_exp * 100, 2)
buy5_exp <- round(as.data.frame(table(xmas$X1125))[6, 2] / buy_exp * 100, 2)
buy6_exp <- round(as.data.frame(table(xmas$X1125))[7, 2] / buy_exp * 100, 2)
buy7_exp <- round(as.data.frame(table(xmas$X1125))[8, 2] / buy_exp * 100, 2)
buy_df <- rbind(buy1_exp, buy2_exp, buy3_exp, buy4_exp, buy5_exp, buy6_exp, buy7_exp)
buy_df
buy_df[order(buy_df, decreasing = TRUE), ]
sum(buy_df, na.rm = TRUE)

(1854 / 2 - 714) / 426 * 250 + 1


# 總消費
as.data.frame(table(xmas$X1126))
sum(as.data.frame(table(xmas[which(xmas$X1126 > 0), "X1126"]))[, 2])
sum(as.data.frame(table(xmas[which(xmas$X1126 > 1), "X1126"]))[, 2]) / 
        sum(as.data.frame(table(xmas[which(xmas$X1126 > 0), "X1126"]))[, 2]) * 100
tot_exp <- sum(as.data.frame(table(xmas$X1126))[2:8, 2], na.rm = TRUE)
tot1_exp <- round(as.data.frame(table(xmas$X1126))[2, 2] / tot_exp * 100, 2)
tot2_exp <- round(as.data.frame(table(xmas$X1126))[3, 2] / tot_exp * 100, 2)
tot3_exp <- round(as.data.frame(table(xmas$X1126))[4, 2] / tot_exp * 100, 2)
tot4_exp <- round(as.data.frame(table(xmas$X1126))[5, 2] / tot_exp * 100, 2)
tot5_exp <- round(as.data.frame(table(xmas$X1126))[6, 2] / tot_exp * 100, 2)
tot6_exp <- round(as.data.frame(table(xmas$X1126))[7, 2] / tot_exp * 100, 2)
tot7_exp <- round(as.data.frame(table(xmas$X1126))[8, 2] / tot_exp * 100, 2)
tot_df <- rbind(tot1_exp, tot2_exp, tot3_exp, tot4_exp, tot5_exp, tot6_exp, tot7_exp)
tot_df
tot_df[order(tot_df, decreasing = TRUE), ]
sum(tot_df, na.rm = TRUE)

(1880 / 2 - 927) / 290 * 250 + 251