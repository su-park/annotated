## SET LIBRARY
library(data.table)
library(tidyverse)
library(knitr)
library(RColorBrewer)
library(showtext)
library(ggthemes)
library(gridExtra)

## SET THEME
font_add_google("Nanum Gothic")
showtext_auto()
theme_set(theme_bw(base_family = "Nanum Gothic") + theme(legend.position = "bottom"))

## SET WORKPATH
work_path <- "/Users/boO/data8/dps/annotated/0_220407_market"
setwd(work_path)

## LOAD DATA
tbl <- data.table(read.csv(sprintf("%s/data/ECOS_TABLE_20220407_103647.csv", work_path), header=FALSE, fileEncoding='EUC-KR'))
setnames(tbl, c("strd_ym", "ktb2", "ktb3", "ktb10"))
tbl <- tbl[grepl("\\d{4}\\/\\d{2}", strd_ym)]
tbl[, ":="(ktb2, as.numeric(ktb2))]
tbl[, ":="(ktb3, as.numeric(ktb3))]
tbl[, ":="(ktb10, as.numeric(ktb10))]
tbl[, ":="(strd_ym, as.Date(paste0(strd_ym, '/01'), "%Y/%m/%d"))]
tbl <- rbind(tbl, data.table(strd_ym=c(as.Date("2022-04-06")), ktb2=c(2.748), ktb3=c(2.941), ktb10=c(3.129)))
tbl[, ":="(rate_diff, ktb10 - ktb3)]

colfunc <- colorRampPalette(c(brewer.pal(8, 'Paired')[2], "darkgray"))
col <- colfunc(2)
p1 <- ggplot(tbl[strd_ym>as.Date('2005-01-01')], aes(x=strd_ym, y=rate_diff, color="국채 10년-3년 장단기 금리")) + 
  scale_color_manual(values=col[2]) + 
  geom_line() +
  geom_point(data=tbl[strd_ym==as.Date('2022-04-06')], size=2.4, color=col[1]) +
  geom_text(data=tbl[strd_ym==as.Date('2022-04-06')], aes(label=sprintf("%s%%p", rate_diff)), position = position_dodge(width=1), size=2.5, vjust=2.4, hjust=0.1, color=col[1]) +
  scale_x_date(breaks = "3 year", date_labels="%Y/%m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  labs(title="장단기 국채 금리차이", x="", y="% points", color="", caption="source : 한국은행 경제통계시스템 (As of 6th Apr.")
  # guides(colour="none")

tbl <- tbl %>% select(-ktb2, -rate_diff) %>%
  melt(id.vars='strd_ym') %>%
  filter(!is.na(value))
setnames(tbl, c("strd_ym", "ktb_typ", "rate"))


colfunc <- colorRampPalette(c(brewer.pal(8, 'Paired')[2], "gray"))
col <- colfunc(2)
p2 <- ggplot(tbl[strd_ym>as.Date('2005-01-01')], aes(x=strd_ym, y=rate, group=ktb_typ, colour=ktb_typ)) + 
  scale_color_manual(values=col) + 
  geom_line() +
  scale_x_date(breaks = "3 year", date_labels="%Y/%m") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  labs(title="장단기 국채 금리 추이", x="", y="%", color="", caption="")

ggsave(file=sprintf("%s/output/장단기_국고채_금리_추이.png", work_path), plot=p2, width=18, height=18, units="cm")
ggsave(file=sprintf("%s/output/장단기_국고채_금리_차이.png", work_path), plot=p1, width=18, height=18, units="cm")