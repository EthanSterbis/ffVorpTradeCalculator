# Sterb's Fantasy Football VORP Trade Calculator
# @EthanSterbis on X

library(shiny)
library(dplyr)
library(nflreadr)
library(rlang)
library(bslib)
library(DT)

SEASON_RANGE <- 2021:2025
REG_WEEKS    <- 1:18

favicon_href <- NULL
if (file.exists("vorpLogo.png")) {
  shiny::addResourcePath("appstatic", ".")
  favicon_href <- "appstatic/vorpLogo.png"
}

col0 <- function(df, nm) if (nm %in% names(df)) df[[nm]] else 0

startable_slots_for_pos <- function(pos, R){
  stopifnot(pos %in% c("QB","RB","WR","TE"))
  can <- switch(pos,
                "QB" = R$QB + R$SFLX,
                "RB" = R$RB + R$FLX + R$SFLX,
                "WR" = R$WR + R$FLX + R$SFLX,
                "TE" = R$TE)
  as.numeric(can)
}
total_starting_slots <- function(R) as.numeric(R$QB + R$RB + R$WR + R$TE + R$FLX + R$SFLX)

default_repl_idx <- function(league_settings){
  L <- league_settings$league_size
  R <- league_settings$roster
  tot <- total_starting_slots(R)
  scp_qb <- startable_slots_for_pos("QB", R)
  scp_rb <- startable_slots_for_pos("RB", R)
  scp_wr <- startable_slots_for_pos("WR", R)
  scp_te <- startable_slots_for_pos("TE", R)
  list(
    QB = max(1L, round( L * ((scp_qb + R$BEN * (scp_qb / tot)) - 0.25) ) + 1L),
    RB = max(1L, round( L * ((scp_rb + R$BEN * (scp_rb / tot)) - 2.00) ) + 1L),
    WR = max(1L, floor( L * ((scp_wr + R$BEN * (scp_wr / tot)) - 0.50) ) + 1L),
    TE = max(1L, round( L * ((scp_te + R$BEN * (scp_te / tot)) - 0.50) ) + 1L)
  )
}

load_ffopp_weekly <- function(seasons){
  seasons <- as.integer(seasons)
  seasons <- seasons[seasons %in% SEASON_RANGE]
  if (!length(seasons)) seasons <- SEASON_RANGE
  
  if (file.exists("data/ffopp.rds")) {
    dat <- readRDS("data/ffopp.rds")
    return(
      dat %>%
        dplyr::filter(season %in% seasons,
                      position %in% c("QB","RB","WR","TE"),
                      week %in% REG_WEEKS)
    )
  }
  
  nflreadr::load_ff_opportunity(seasons = seasons) %>%
    dplyr::rename(
      game_id   = dplyr::any_of("game_id"),
      player_id = dplyr::any_of("player_id"),
      full_name = dplyr::any_of(c("full_name","player_name")),
      posteam   = dplyr::any_of(c("posteam","team")),
      position  = dplyr::any_of(c("position","pos")),
      season    = dplyr::any_of("season"),
      week      = dplyr::any_of("week")
    ) %>%
    dplyr::filter(position %in% c("QB","RB","WR","TE"), week %in% REG_WEEKS)
}

prep_ffopp_weekly <- function(ffOpp, scoring){
  id_cols <- c("game_id","player_id","full_name","posteam","position","season","week")
  pass_cols <- c("pass_attempt","pass_yards_gained","pass_touchdown","pass_two_point_conv",
                 "pss_interception","pass_yards_gained_exp","pass_touchdown_exp",
                 "pass_two_point_conv_exp","pss_interception_exp")
  rush_cols <- c("rush_yards_gained","rush_touchdown","rush_two_point_conv","rush_first_down",
                 "rush_fumble_lost","rush_yards_gained_exp","rush_touchdown_exp",
                 "rush_two_point_conv_exp","rush_first_down_exp","rush_fumble_lost_exp")
  rec_cols  <- c("rec_attempt","receptions","rec_yards_gained","rec_touchdown","rec_two_point_conv",
                 "rec_first_down","rec_fumble_lost","receptions_exp","rec_yards_gained_exp",
                 "rec_touchdown_exp","rec_two_point_conv_exp","rec_first_down_exp","rec_fumble_lost_exp")
  keep <- intersect(c(id_cols, pass_cols, rush_cols, rec_cols), names(ffOpp))
  
  ffOpp %>%
    select(all_of(keep)) %>%
    mutate(
      is_te    = .data$position == "TE",
      te_bonus = ifelse(is_te, scoring$te_ppr_bonus, 0),
      fp_pass_yd = col0(cur_data(),"pass_yards_gained") / scoring$pass_yds_per_pt,
      fp_pass_td = col0(cur_data(),"pass_touchdown")   * scoring$pass_td_pts,
      fp_int     = col0(cur_data(),"pss_interception") * scoring$int_pts,
      fp_rush_yd = col0(cur_data(),"rush_yards_gained") / scoring$rush_yds_per_pt,
      fp_rush_td = col0(cur_data(),"rush_touchdown") * 6,
      fp_rec_yd  = col0(cur_data(),"rec_yards_gained") / scoring$rec_yds_per_pt,
      fp_rec_td  = col0(cur_data(),"rec_touchdown") * 6,
      fp_rec     = (scoring$ppr + te_bonus) * col0(cur_data(),"receptions"),
      fp_two_pt  = scoring$two_pt_pts * ( col0(cur_data(),"pass_two_point_conv") +
                                            col0(cur_data(),"rush_two_point_conv") +
                                            col0(cur_data(),"rec_two_point_conv") ),
      fp_fumbles = scoring$fum_lost_pts * ( col0(cur_data(),"rec_fumble_lost") +
                                              col0(cur_data(),"rush_fumble_lost") ),
      total_fantasy_points =
        fp_pass_yd + fp_pass_td + fp_int +
        fp_rush_yd + fp_rush_td +
        fp_rec_yd  + fp_rec_td + fp_rec +
        fp_two_pt + fp_fumbles,
      fpX_pass_yd = col0(cur_data(),"pass_yards_gained_exp") / scoring$pass_yds_per_pt,
      fpX_pass_td = col0(cur_data(),"pass_touchdown_exp")   * scoring$pass_td_pts,
      fpX_int     = col0(cur_data(),"pss_interception_exp") * scoring$int_pts,
      fpX_rush_yd = col0(cur_data(),"rush_yards_gained_exp") / scoring$rush_yds_per_pt,
      fpX_rush_td = col0(cur_data(),"rush_touchdown_exp") * 6,
      fpX_rec_yd  = col0(cur_data(),"rec_yards_gained_exp") / scoring$rec_yds_per_pt,
      fpX_rec_td  = col0(cur_data(),"rec_touchdown_exp") * 6,
      fpX_rec     = (scoring$ppr + te_bonus) * col0(cur_data(),"receptions_exp"),
      fpX_two_pt  = scoring$two_pt_pts * ( col0(cur_data(),"pass_two_point_conv_exp") +
                                             col0(cur_data(),"rush_two_point_conv_exp") +
                                             col0(cur_data(),"rec_two_point_conv_exp") ),
      fpX_fumbles = scoring$fum_lost_pts * ( col0(cur_data(),"rec_fumble_lost_exp") +
                                               col0(cur_data(),"rush_fumble_lost_exp") ),
      total_fantasy_points_exp =
        fpX_pass_yd + fpX_pass_td + fpX_int +
        fpX_rush_yd + fpX_rush_td +
        fpX_rec_yd  + fpX_rec_td + fpX_rec +
        fpX_two_pt + fpX_fumbles
    ) %>%
    select(
      all_of(id_cols[id_cols %in% names(.)]),
      total_fantasy_points, total_fantasy_points_exp,
      everything(), -is_te, -te_bonus
    )
}

compute_weekly_replacement <- function(ffOpp_weekly_ready, league_settings,
                                       which_points=c("actual","expected"),
                                       override_idx=NULL){
  which_points <- match.arg(which_points)
  L <- league_settings$league_size
  R <- league_settings$roster
  tot <- total_starting_slots(R)
  pts_col <- if (which_points=="actual") "total_fantasy_points" else "total_fantasy_points_exp"
  if (is.null(override_idx)) override_idx <- default_repl_idx(league_settings)
  
  ffOpp_weekly_ready %>%
    filter(position %in% c("QB","RB","WR","TE")) %>%
    group_by(season, week, position) %>%
    arrange(desc(.data[[pts_col]]), .by_group = TRUE) %>%
    mutate(
      scp = case_when(
        position=="QB" ~ startable_slots_for_pos("QB", R),
        position=="RB" ~ startable_slots_for_pos("RB", R),
        position=="WR" ~ startable_slots_for_pos("WR", R),
        position=="TE" ~ startable_slots_for_pos("TE", R)
      )
    ) %>%
    summarise(
      pos = first(position),
      n   = n(),
      N_wr_real = if_else(pos=="WR",
                          L * ((first(scp) + R$BEN*(first(scp)/tot)) - 0.50), NA_real_),
      N_rb      = if_else(pos=="RB",
                          round(L * ((first(scp) + R$BEN*(first(scp)/tot)) - 2.00)), NA_real_),
      N_qb      = if_else(pos=="QB",
                          round(L * ((first(scp) + R$BEN*(first(scp)/tot)) - 0.25)), NA_real_),
      N_te      = if_else(pos=="TE",
                          round(L * ((first(scp) + R$BEN*(first(scp)/tot)) - 0.50)), NA_real_),
      idx_calc = case_when(
        pos=="WR" ~ floor(N_wr_real) + 1L,
        pos=="RB" ~ as.integer(N_rb) + 1L,
        pos=="QB" ~ as.integer(N_qb) + 1L,
        pos=="TE" ~ as.integer(N_te) + 1L
      ),
      idx_user = case_when(
        pos=="WR" ~ as.integer(override_idx$WR),
        pos=="RB" ~ as.integer(override_idx$RB),
        pos=="QB" ~ as.integer(override_idx$QB),
        pos=="TE" ~ as.integer(override_idx$TE)
      ),
      idx_repl = pmin(pmax(if_else(!is.na(idx_user), idx_user, idx_calc), 1L), n),
      repl_val = nth(.data[[pts_col]], idx_repl),
      .groups="drop"
    ) %>% select(-pos)
}

add_weekly_vorp <- function(ffOpp_weekly_ready, league_settings, override_idx){
  ra <- compute_weekly_replacement(ffOpp_weekly_ready, league_settings,"actual",   override_idx) %>%
    rename(replacement_fp = repl_val)
  re <- compute_weekly_replacement(ffOpp_weekly_ready, league_settings,"expected", override_idx) %>%
    rename(replacement_fp_exp = repl_val)
  ffOpp_weekly_ready %>%
    left_join(ra, by=c("season","week","position")) %>%
    left_join(re, by=c("season","week","position")) %>%
    mutate(
      VORP_weekly     = total_fantasy_points     - replacement_fp,
      VORP_weekly_exp = total_fantasy_points_exp - replacement_fp_exp
    )
}

ui <- fluidPage(
  title = "Sterb's Fantasy Football VORP Trade Calculator",
  theme = bslib::bs_theme(
    version = 5,
    bg      = "#2B2B2B",
    fg      = "rgb(234,234,234)",
    primary = "#43B6FF",
    secondary = "#F0F0F0",
    success = "#28B62C",
    info    = "#75CAEB",
    warning = "#FF851B",
    "spacer" = "2rem"
  ),
  tags$head(
    if (!is.null(favicon_href)) tags$link(rel = "icon", type = "image/png", href = favicon_href),
    tags$title("Sterb's Fantasy Football VORP Trade Calculator")
  ),
  tags$style(HTML("
    .pill {padding:6px 10px;margin:4px;border-radius:14px;background:#f1f3f5;
           display:inline-flex;align-items:center;font-size:13px;color:#2B2B2B;}
    .pill .rm {margin-left:8px;color:#888;cursor:pointer;font-weight:700}
    .meter-label{margin-top:8px;font-weight:600}
    .roster-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(90px,1fr));grid-gap:8px;align-items:start;}
    .scoring-grid,.repl-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(120px,1fr));grid-gap:8px;align-items:start;}
    .roster-item .lbl,.grid-label{font-size:12px;font-weight:600;margin:0 0 2px 2px;display:block;}
    .roster-item .shiny-input-container{margin-bottom:0;}
    .form-label, .control-label, label, .form-check-label { color: rgba(234,234,234,.95) !important; }
    .form-control, .form-select { color: rgba(234,234,234,.95) !important; }
    .form-control::placeholder { color: rgba(234,234,234,.7) !important; opacity:1 !important; }
    .selectize-input, .selectize-input input { color: rgba(234,234,234,.95) !important; }
    .selectize-control .item,
    .selectize-control.single .selectize-input > .item { color: rgba(234,234,234,.95) !important; }
    .selectize-input > input::placeholder { color: rgba(234,234,234,.7) !important; opacity:1 !important; }
    .selectize-dropdown .option { color: rgba(234,234,234,.95) !important; }
    table.dataTable thead th, table.dataTable tbody td { text-align:center !important; }
  ")),
  tags$script(HTML("
    $(document).on('click','.rmA', function(){ Shiny.setInputValue('rmA', $(this).data('pid'), {priority:'event'}); });
    $(document).on('click','.rmB', function(){ Shiny.setInputValue('rmB', $(this).data('pid'), {priority:'event'}); });
  ")),
  titlePanel("Sterb's Fantasy Football VORP Trade Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters"),
      selectInput("position","Position",choices=c("ALL","QB","RB","WR","TE"),selected="ALL"),
      uiOutput("weeks_ui"),
      uiOutput("min_weeks_ui"),
      radioButtons("which_points","Points Basis",
                   choices=c("Actual"="actual","Expected"="expected"),
                   selected="actual", inline=TRUE),
      numericInput("top_n","Show Top N", value=50, min=5, max=500, step=5),
      actionButton("recalc","Recalculate", class="btn-primary"),
      br(), br(),
      h4("Data"),
      selectInput("season","Season(s)", choices=as.character(SEASON_RANGE),
                  selected=as.character(max(SEASON_RANGE)), multiple=TRUE),
      tags$small("Hold Ctrl/Cmd to select multiple seasons."), br(), br(),
      h4("League Settings"),
      selectInput("league_size","League Size",
                  choices=c(6,8,10,12,14,16,18,20,22,24,32), selected=12),
      div(class="roster-grid",
          div(class="roster-item", div(class="lbl","# QBs"),
              selectInput("QB", label=NULL, choices=c(1,2,3), selected=1, width="100%")),
          div(class="roster-item", div(class="lbl","# RBs"),
              selectInput("RB", label=NULL, choices=c(1,2,3,4), selected=2, width="100%")),
          div(class="roster-item", div(class="lbl","# WRs"),
              selectInput("WR", label=NULL, choices=c(1,2,3,4), selected=2, width="100%")),
          div(class="roster-item", div(class="lbl","# TEs"),
              selectInput("TE", label=NULL, choices=c(1,2), selected=1, width="100%")),
          div(class="roster-item", div(class="lbl","# FLEX"),
              selectInput("FLX", label=NULL, choices=c(0,1,2,3), selected=1, width="100%")),
          div(class="roster-item", div(class="lbl","# SuperFLEX"),
              selectInput("SFLX", label=NULL, choices=c(0,1,2,3), selected=0, width="100%")),
          div(class="roster-item", div(class="lbl","# Bench"),
              selectInput("BEN", label=NULL, choices=4:12, selected=6, width="100%"))
      ),
      h4("Scoring"),
      div(class="scoring-grid",
          div(class="roster-item", div(class="lbl","Points per Reception"),
              selectInput("ppr", label=NULL, choices=c(0,0.5,1.0), selected=1.0, width="100%")),
          div(class="roster-item", div(class="lbl","Points per Pass TD"),
              selectInput("pass_td_pts", label=NULL, choices=c(4,6), selected=4, width="100%")),
          div(class="roster-item", div(class="lbl","TE PPR Bonus"),
              selectInput("te_ppr_bonus", label=NULL, choices=c(0,0.5,1.0), selected=0, width="100%"))
      ),
      h4("Replacement Override (rank)"),
      uiOutput("repl_override_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Trade Calculator",
                 fluidRow(
                   column(
                     width=5,
                     wellPanel(
                       h4("Team A gets..."),
                       uiOutput("teamA_add_ui"),
                       uiOutput("teamA_pills"),
                       tags$h4(textOutput("teamA_total_text"))
                     )
                   ),
                   column(width=2, align="center", br(), br(), uiOutput("trade_meter")),
                   column(
                     width=5,
                     wellPanel(
                       h4("Team B gets..."),
                       uiOutput("teamB_add_ui"),
                       uiOutput("teamB_pills"),
                       tags$h4(textOutput("teamB_total_text"))
                     )
                   )
                 )
        ),
        tabPanel("Leaderboard", DT::DTOutput("tbl_leader")),
        tabPanel("Replacement", DT::DTOutput("tbl_repl")),
        tabPanel("Player Weeks", DT::DTOutput("tbl_player")),
        tabPanel("About",
                 tags$pre("Weekly VORP = player-week FP minus the position’s weekly replacement FP.

Default replacement ranks (you can override):
• QB: round(L*((Startable + BEN*(Startable/TotalStart)) - 0.25)) + 1
• RB: round(L*((Startable + BEN*(Startable/TotalStart)) - 2.00)) + 1
• WR: floor(L*((Startable + BEN*(Startable/TotalStart)) - 0.50)) + 1
• TE: round(L*((Startable + BEN*(Startable/TotalStart)) - 0.50)) + 1
TE startable = TE-only (FLEX/SFLX not counted).
Min Weeks Played defaults to half of the selected week span.
Seasons: 2021–2025; Weeks: 1–18.")
        )
      )
    )
  )
)

server <- function(input, output, session){
  scoring_rx <- reactive({
    list(ppr=as.numeric(input$ppr), pass_td_pts=as.numeric(input$pass_td_pts),
         te_ppr_bonus=as.numeric(input$te_ppr_bonus),
         pass_yds_per_pt=25, rush_yds_per_pt=10, rec_yds_per_pt=10,
         two_pt_pts=2, int_pts=-2, fum_lost_pts=-2)
  })
  league_rx <- reactive({
    list(league_size=as.integer(input$league_size),
         roster=list(QB=as.integer(input$QB), RB=as.integer(input$RB),
                     WR=as.integer(input$WR), TE=as.integer(input$TE),
                     FLX=as.integer(input$FLX), SFLX=as.integer(input$SFLX),
                     BEN=as.integer(input$BEN)))
  })
  
  output$repl_override_ui <- renderUI({
    def <- default_repl_idx(league_rx())
    tagList(
      tags$small("Calculated from league settings; tweak if benches favor certain positions, etc."), br(),
      div(class="repl-grid",
          div(class="roster-item", div(class="lbl","QB replacement rank"),
              numericInput("repl_qb", label=NULL, value=def$QB, min=1, max=300, step=1, width="100%")),
          div(class="roster-item", div(class="lbl","RB replacement rank"),
              numericInput("repl_rb", label=NULL, value=def$RB, min=1, max=400, step=1, width="100%")),
          div(class="roster-item", div(class="lbl","WR replacement rank"),
              numericInput("repl_wr", label=NULL, value=def$WR, min=1, max=500, step=1, width="100%")),
          div(class="roster-item", div(class="lbl","TE replacement rank"),
              numericInput("repl_te", label=NULL, value=def$TE, min=1, max=300, step=1, width="100%"))
      )
    )
  })
  observeEvent(league_rx(), {
    def <- default_repl_idx(league_rx())
    updateNumericInput(session,"repl_qb",value=def$QB)
    updateNumericInput(session,"repl_rb",value=def$RB)
    updateNumericInput(session,"repl_wr",value=def$WR)
    updateNumericInput(session,"repl_te",value=def$TE)
  }, ignoreInit=TRUE)
  
  override_rx <- reactive({
    def <- default_repl_idx(league_rx())
    list(
      QB = as.integer(if (!is.null(input$repl_qb)) input$repl_qb else def$QB),
      RB = as.integer(if (!is.null(input$repl_rb)) input$repl_rb else def$RB),
      WR = as.integer(if (!is.null(input$repl_wr)) input$repl_wr else def$WR),
      TE = as.integer(if (!is.null(input$repl_te)) input$repl_te else def$TE)
    )
  })
  
  trigger <- reactiveVal(0)
  observeEvent(input$recalc,{ trigger(isolate(trigger())+1) }, ignoreInit=TRUE)
  observe({ if (trigger()==0) trigger(1) })
  
  ffOpp_ready <- eventReactive(list(input$season, trigger()), {
    load_ffopp_weekly(as.integer(input$season)) %>% prep_ffopp_weekly(scoring_rx())
  }, ignoreInit=FALSE)
  
  output$weeks_ui <- renderUI({
    wks <- REG_WEEKS
    fluidRow(
      column(6, selectInput("week_start","Start Week",choices=wks,selected=min(wks))),
      column(6, selectInput("week_end","End Week",choices=wks,selected=max(wks)))
    )
  })
  output$min_weeks_ui <- renderUI({
    ws <- if (!is.null(input$week_start)) as.integer(input$week_start) else min(REG_WEEKS)
    we <- if (!is.null(input$week_end))   as.integer(input$week_end)   else max(REG_WEEKS)
    span <- max(1L, abs(we-ws)+1L)
    numericInput("min_weeks","Minimum Weeks Played",value=0,min=1,max=span,step=1)
  })
  
  ffOpp_vorp <- reactive({
    req(ffOpp_ready())
    add_weekly_vorp(ffOpp_ready(), league_rx(), override_rx())
  })
  
  vorp_agg <- reactive({
    req(ffOpp_vorp(), input$week_start, input$week_end, input$min_weeks)
    ws <- min(as.integer(input$week_start), as.integer(input$week_end))
    we <- max(as.integer(input$week_start), as.integer(input$week_end))
    minW <- max(1L, as.integer(input$min_weeks))
    ffOpp_vorp() %>%
      filter(week >= ws, week <= we) %>%
      group_by(player_id, full_name, position, posteam, season) %>%
      summarise(
        Weeks_Played = n(),
        Avg_FP       = mean(total_fantasy_points, na.rm=TRUE),
        Avg_FP_Exp   = mean(total_fantasy_points_exp, na.rm=TRUE),
        Avg_VORP     = mean(VORP_weekly, na.rm=TRUE),
        Avg_VORP_Exp = mean(VORP_weekly_exp, na.rm=TRUE),
        .groups="drop"
      ) %>% filter(Weeks_Played >= minW)
  })
  
  repl_tbl <- reactive({
    req(ffOpp_ready(), input$week_start, input$week_end)
    ws <- min(as.integer(input$week_start), as.integer(input$week_end))
    we <- max(as.integer(input$week_start), as.integer(input$week_end))
    L <- league_rx(); ov <- override_rx()
    ra <- compute_weekly_replacement(ffOpp_ready(), L, "actual", ov)   %>% rename(replacement_fp = repl_val)
    re <- compute_weekly_replacement(ffOpp_ready(), L, "expected", ov) %>% rename(replacement_fp_exp = repl_val)
    inner_join(ra, re, by=c("season","week","position")) %>%
      filter(week >= ws, week <= we)
  })
  
  output$tbl_leader <- DT::renderDT({
    req(vorp_agg(), input$position, input$which_points, input$top_n)
    dat <- vorp_agg() %>% { if (input$position!="ALL") filter(., position==input$position) else . }
    if (input$which_points=="actual") {
      dat <- dat %>% arrange(desc(Avg_VORP)) %>% mutate(Rank = row_number()) %>%
        select(Rank, season, position, full_name, posteam, Weeks_Played, Avg_FP, Avg_VORP)
      colnames(dat) <- c("RANK","SEASON","POSITION","PLAYER","TEAM","WEEKS PLAYED","AVG FP","AVG VORP")
    } else {
      dat <- dat %>% arrange(desc(Avg_VORP_Exp)) %>% mutate(Rank = row_number()) %>%
        select(Rank, season, position, full_name, posteam, Weeks_Played, Avg_FP_Exp, Avg_VORP_Exp)
      colnames(dat) <- c("RANK","SEASON","POSITION","PLAYER","TEAM","WEEKS PLAYED","AVG FP EXP","AVG VORP EXP")
    }
    dat <- head(dat, input$top_n)
    tbl <- DT::datatable(dat, rownames=FALSE, options=list(pageLength=25))
    num_cols <- which(vapply(dat, is.numeric, TRUE))
    if ("RANK" %in% names(dat)) num_cols <- setdiff(num_cols, which(names(dat)=="RANK"))
    if (length(num_cols)) tbl <- DT::formatRound(tbl, columns=num_cols, digits=2)
    tbl
  })
  
  output$tbl_repl <- DT::renderDT({
    req(repl_tbl())
    dat <- repl_tbl() %>%
      group_by(season, position) %>%
      summarise(
        Weeks = n(),
        replacement_fp     = mean(replacement_fp, na.rm = TRUE),
        replacement_fp_exp = mean(replacement_fp_exp, na.rm = TRUE),
        .groups = "drop"
      ) %>% arrange(season, position)
    colnames(dat) <- c("SEASON","POSITION","WEEKS PLAYED","REPLACEMENT FP","REPLACEMENT FP EXP")
    tbl <- DT::datatable(dat, rownames=FALSE, options=list(pageLength=10))
    tbl <- DT::formatRound(tbl, columns=c("REPLACEMENT FP","REPLACEMENT FP EXP"), digits=2)
    tbl
  })
  
  output$tbl_player <- DT::renderDT({
    req(ffOpp_vorp(), input$week_start, input$week_end)
    ws <- min(as.integer(input$week_start), as.integer(input$week_end))
    we <- max(as.integer(input$week_start), as.integer(input$week_end))
    dat <- ffOpp_vorp() %>%
      filter(week >= ws, week <= we) %>%
      select(season, week, position, full_name, posteam,
             total_fantasy_points, total_fantasy_points_exp,
             replacement_fp, replacement_fp_exp,
             VORP_weekly, VORP_weekly_exp) %>%
      arrange(season, week, position, desc(VORP_weekly))
    tbl <- DT::datatable(dat, rownames=FALSE, options=list(pageLength=25))
    num_cols <- which(vapply(dat, is.numeric, TRUE))
    if (length(num_cols)) tbl <- DT::formatRound(tbl, columns=num_cols, digits=2)
    tbl
  })
  
  player_choices <- reactive({
    dat <- vorp_agg()
    valcol <- if (input$which_points=="actual") "Avg_VORP" else "Avg_VORP_Exp"
    dat <- dat %>% arrange(desc(.data[[valcol]]))
    labels <- sprintf("%s — %s • %s", dat$full_name, dat$position, dat$posteam)
    stats::setNames(dat$player_id, labels)
  })
  player_values <- reactive({
    dat <- vorp_agg()
    valcol <- if (input$which_points=="actual") "Avg_VORP" else "Avg_VORP_Exp"
    stats::setNames(dat[[valcol]], dat$player_id)
  })
  label_with_vorp <- function(pid){
    dat <- vorp_agg()
    valcol <- if (isolate(input$which_points)=="actual") "Avg_VORP" else "Avg_VORP_Exp"
    row <- dat[match(pid, dat$player_id), ]
    if (NROW(row) && !is.na(row$player_id))
      sprintf("%s — %s • %s (%.2f VORP)", row$full_name, row$position, row$posteam, row[[valcol]])
    else NULL
  }
  
  selA <- reactiveValues(ids=character(0))
  selB <- reactiveValues(ids=character(0))
  
  output$teamA_add_ui <- renderUI({
    selectizeInput("a_add","Add Player", choices = player_choices(), selected = character(0),
                   options = list(placeholder = "Search for a player",
                                  onInitialize = I('function(){ this.clear(true); }')))
  })
  output$teamB_add_ui <- renderUI({
    selectizeInput("b_add","Add Player", choices = player_choices(), selected = character(0),
                   options = list(placeholder = "Search for a player",
                                  onInitialize = I('function(){ this.clear(true); }')))
  })
  
  observeEvent(input$a_add, {
    if (!is.null(input$a_add) && nzchar(input$a_add) && !(input$a_add %in% selA$ids))
      selA$ids <- unique(c(selA$ids, input$a_add))
    updateSelectizeInput(session, "a_add", selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$b_add, {
    if (!is.null(input$b_add) && nzchar(input$b_add) && !(input$b_add %in% selB$ids))
      selB$ids <- unique(c(selB$ids, input$b_add))
    updateSelectizeInput(session, "b_add", selected = "")
  }, ignoreInit = TRUE)
  
  output$teamA_pills <- renderUI({
    if (!length(selA$ids)) return(NULL)
    tagList(lapply(selA$ids, function(pid){
      lbl <- label_with_vorp(pid)
      tags$span(class="pill", lbl, tags$span(class="rm rmA", `data-pid`=pid, HTML("&times;")))
    }))
  })
  output$teamB_pills <- renderUI({
    if (!length(selB$ids)) return(NULL)
    tagList(lapply(selB$ids, function(pid){
      lbl <- label_with_vorp(pid)
      tags$span(class="pill", lbl, tags$span(class="rm rmB", `data-pid`=pid, HTML("&times;")))
    }))
  })
  
  observeEvent(input$rmA, { selA$ids <- setdiff(selA$ids, input$rmA) })
  observeEvent(input$rmB, { selB$ids <- setdiff(selB$ids, input$rmB) })
  
  teamA_total <- reactive(sum(player_values()[selA$ids], na.rm=TRUE))
  teamB_total <- reactive(sum(player_values()[selB$ids], na.rm=TRUE))
  trade_diff  <- reactive(teamA_total() - teamB_total())
  
  output$teamA_total_text <- renderText(sprintf("Total: %.2f", teamA_total()))
  output$teamB_total_text <- renderText(sprintf("Total: %.2f", teamB_total()))
  
  output$trade_meter <- renderUI({
    d   <- trade_diff()
    ad  <- abs(d)
    col <- if (ad <= 1) "#2e7d32" else if (ad <= 2) "#6c757d" else "#c62828"
    rng <- max(10, 1.2 * max(ad, teamA_total(), teamB_total(), 1))
    pct <- max(0, min(100, 50 + 50 * d / rng))
    msg <- if (ad < 1e-9) "Fair Trade"
    else if (d > 0) sprintf("Favors Team A by %.2f VORP", ad)
    else sprintf("Favors Team B by %.2f VORP", ad)
    
    tags$div(
      style="width:100%;",
      tags$div(style="position:relative;height:14px;background:#eee;border-radius:7px;",
               tags$div(style=sprintf(
                 "position:absolute;left:%0.1f%%;top:-6px;width:14px;height:26px;border-radius:7px;background:%s;transform:translateX(-50%%);box-shadow:0 0 6px rgba(0,0,0,.2);",
                 pct, col))
      ),
      tags$div(class="meter-label", style=sprintf("color:%s;", col), msg)
    )
  })
}

shinyApp(ui, server)
