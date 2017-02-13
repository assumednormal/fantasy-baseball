
# imports and options -----------------------------------------------------

options(stringsAsFactors = FALSE)

library(baseballr)
library(plyr)
library(dplyr)
library(rvest)

# define weeks for 2016 regular season ------------------------------------

weeks <- data.frame(
  week = 1:22,
  start_date = c('2016-04-03', '2016-04-11', '2016-04-18', '2016-04-25',
                 '2016-05-02', '2016-05-09', '2016-05-16', '2016-05-23',
                 '2016-05-30', '2016-06-06', '2016-06-13', '2016-06-20',
                 '2016-06-27', '2016-07-04', '2016-07-11', '2016-07-25',
                 '2016-08-01', '2016-08-08', '2016-08-15', '2016-08-22',
                 '2016-08-29', '2016-09-05'),
  end_date = c('2016-04-10', '2016-04-17', '2016-04-24', '2016-05-01',
               '2016-05-08', '2016-05-15', '2016-05-22', '2016-05-29',
               '2016-06-05', '2016-06-12', '2016-06-19', '2016-06-26',
               '2016-07-03', '2016-07-10', '2016-07-24', '2016-07-31',
               '2016-08-07', '2016-08-14', '2016-08-21', '2016-08-28',
               '2016-09-04', '2016-09-11')
)

# pull down weekly batting data and store in sqlite db --------------------

get_batting_stats <- function(row) {
  batting_stats <- daily_batter_bref(row$start_date, row$end_date)
  batting_stats$week <- row$week
  return(batting_stats)
}

weekly_batting_stats <- dlply(weeks, 'week', get_batting_stats) %>%
  bind_rows()

fantasy_baseball_db <- src_sqlite('fantasy-baseball.sqlite3', create = TRUE)

copy_to(fantasy_baseball_db, weekly_batting_stats, 'weekly_batting_stats',
        temporary = FALSE)

# pull down weekly pitching data and store in sqlite db -------------------

get_pitching_stats <- function(row) {
  pitching_stats <- daily_pitcher_bref(row$start_date, row$end_date)
  pitching_stats$week <- row$week
  return(pitching_stats)
}

weekly_pitching_stats <- dlply(weeks, 'week', get_pitching_stats) %>%
  bind_rows()

copy_to(fantasy_baseball_db, weekly_pitching_stats, 'weekly_pitching_stats',
        temporary = FALSE)

# we also need the position of each player --------------------------------

urls <- sprintf('http://www.baseball-reference.com/players/%s/', letters)

player_lists <- lapply(urls, read_html)

player_url_extractor <- function(page, tag) {
  # find the div containing player urls
  div <- page %>%
    html_node(xpath = '//div[@id="page_content"]')

  # find all tags with links to player pages and active years
  parent_xpath <- sprintf('./parent::%s[contains(text(), "-")]', tag)
  b <- div %>%
    html_nodes(xpath = '//a[starts-with(@href, "/players")]') %>%
    html_nodes(xpath = parent_xpath)

  # extract info from nodes
  years <- b %>%
    html_nodes(xpath = './text()') %>%
    gsub(' ', '', .)
  start_year <- as.integer(substr(years, 1, 4))
  end_year <- as.integer(substr(years, 6, 9))

  a <- b %>%
    html_nodes(xpath = './/a')

  player_url <- a %>%
    html_attr('href')

  player_name <- a %>%
    html_text()

  player_urls <- data.frame(
    player = player_name,
    url = sprintf('http://www.baseball-reference.com%s', player_url),
    start_year = start_year,
    end_year = end_year
  )

  return(player_urls)
}

player_urls_b <- lapply(player_lists, player_url_extractor, tag = 'b') %>%
  bind_rows()

player_urls_pre <- lapply(player_lists, player_url_extractor, tag = 'pre') %>%
  bind_rows()

player_urls <- bind_rows(player_urls_b, player_urls_pre) %>%
  distinct()

copy_to(fantasy_baseball_db, player_urls, 'player_urls', temporary = FALSE)

# for each batter, figure out position ------------------------------------

batter_urls <- semi_join(player_urls, weekly_batting_stats,
                         by = c('player' = 'Name'))








batter_urls <- semi_join(player_urls, weekly_batting_stats,
                         by = c('player' = 'Name'))
















