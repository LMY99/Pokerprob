#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


source("texas holdem.R")
load('Starting strategy.rda')
library(shiny)
library(reshape)
library(ggplot2)
library(dplyr)
library(DT)

card_list <- sapply(1:52,function(x) sprintf("%s_of_%s",ranks_face[x],suits_face[x]))
card_num <- setNames(1:52, card_list)
hand_type_abbr <- c("NoPair", "Pair", "2Pair",
  "3o1K", "Str", "Flush",
  "Full", "4o1K", "StrFlush")
loading_messages <- c(
  "Running Monte Carlo simulations...",
  "Repeatedly playing poker at background...",
  "Shuffling and dealing virtual cards...",
  "Calculating probability of winning...",
  "Calling the bluff of another computer...",
  "Distributing poker chips around CPUs...",
  "Looking up cheat sheets for poker hand types...",
  "Wondering if flush beats straight or the other way around...",
  "Borrowing extra card decks from the Blackjack program..."
)

ui <- fluidPage(
    tabsetPanel(
    tabPanel('Simulator', fluid=TRUE,
    # Application title
    titlePanel("Poker Probability Simulator"),

    sidebarLayout(
        sidebarPanel(
            actionButton("push","Simulate"),
            numericInput("B", "Simulation Size",
                         2500L, min=1, step=1),
            selectInput('visible_cards',"Stage",
                      c('Pre-flop'=0,'Flop'=3,'Turn'=4,'River'=5)),
            selectInput('p1_visible','Player1 visible?',
                        c('Yes','No')),
            numericInput("p1_bluff", "Player1 Bluff",
                         1, min=1, step=0.1),
            selectInput('p2_visible','Player2 visible?',
                        c('Yes','No')),
            numericInput("p2_bluff", "Player2 Bluff",
                         1, min=1, step=0.1),
            selectInput('p3_visible','Player3 visible?',
                        c('Yes','No')),
            numericInput("p3_bluff", "Player3 Bluff",
                         1, min=1, step=0.1),
            selectInput('p4_visible','Player4 visible?',
                        c('Yes','No')),
            numericInput("p4_bluff", "Player4 Bluff",
                         1, min=1, step=0.1),
            selectInput('p5_visible','Player5 visible?',
                        c('Yes','No')),
            numericInput("p5_bluff", "Player5 Bluff",
                         1, min=1, step=0.1),
            
            selectInput('common_card1', 'Common Card1', card_list,
                        selected=card_list[1]),
            selectInput('common_card2', 'Common Card2', card_list,
                        selected=card_list[2]),
            selectInput('common_card3', 'Common Card3', card_list,
                        selected=card_list[3]),
            selectInput('common_card4', 'Common Card4', card_list,
                        selected=card_list[4]),
            selectInput('common_card5', 'Common Card5', card_list,
                        selected=card_list[5]),
            
            selectInput('P1_Card1', 'P1 Card1', card_list,
                        selected=card_list[6]),
            selectInput('P1_Card2', 'P1 Card2', card_list,
                        selected=card_list[7]),
            selectInput('P2_Card1', 'P2 Card1', card_list,
                        selected=card_list[8]),
            selectInput('P2_Card2', 'P2 Card2', card_list,
                        selected=card_list[9]),
            selectInput('P3_Card1', 'P3 Card1', card_list,
                        selected=card_list[10]),
            selectInput('P3_Card2', 'P3 Card2', card_list,
                        selected=card_list[11]),
            selectInput('P4_Card1', 'P4 Card1', card_list,
                        selected=card_list[12]),
            selectInput('P4_Card2', 'P4 Card2', card_list,
                        selected=card_list[13]),
            selectInput('P5_Card1', 'P5 Card1', card_list,
                        selected=card_list[14]),
            selectInput('P5_Card2', 'P5 Card2', card_list,
                        selected=card_list[15]),
            width=2
            
        ),
        
        mainPanel(
          
          htmlOutput("asset_credit"),
          
          fluidRow(
            column(1,imageOutput("imgC1"),offset=0,style='padding:0px;'),
            column(1,imageOutput("imgC2"),offset=0,style='padding:0px;'),
            column(1,imageOutput("imgC3"),offset=0,style='padding:0px;'),
            column(1,imageOutput("imgC4"),offset=0,style='padding:0px;'),
            column(1,imageOutput("imgC5"),offset=0,style='padding:0px;'),
            column(1,htmlOutput('Common_label'),offset=0,style='padding:0px;')
          ),
           fluidRow(
             column(1,imageOutput("img1"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(1,imageOutput("img2"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(8,htmlOutput('P1'),offset=0,style='padding:0px;margin-top:-20em;')
           ),
           fluidRow(
             column(1,imageOutput("img3"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(1,imageOutput("img4"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(8,htmlOutput('P2'),offset=0,style='padding:0px;margin-top:-20em;')
           ),
           fluidRow(
             column(1,imageOutput("img5"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(1,imageOutput("img6"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(8,htmlOutput('P3'),offset=0,style='padding:0px;margin-top:-20em;')
           ),
           fluidRow(
             column(1,imageOutput("img7"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(1,imageOutput("img8"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(8,htmlOutput('P4'),offset=0,style='padding:0px;margin-top:-20em;')
           ),
           fluidRow(
             column(1,imageOutput("img9"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(1,imageOutput("img10"),offset=0,style='padding:0px;margin-top:-20em;'),
             column(8,htmlOutput('P5'),offset=0,style='padding:0px;margin-top:-20em;')
           ),
           
           width=10
        )
    )
), tabPanel("Pre-flop Probability Heatmap", fluid=TRUE,
            plotOutput("Same_suit"),
            plotOutput("Diff_suit")
  
),
   tabPanel("Pre-flop Probability Data", fluid=TRUE,
            p("Pre-flop probability when private cards are of same suit:"),
            DTOutput("Same_suit_df"),
            p("Pre-flop probability when private cards are of different suit:"),
            DTOutput("Diff_suit_df")
         
)
)
)

server <- function(input, output) {
  
  output$Same_suit_df <- renderDT(datatable(same_suit, 
                                            options=list(paging=TRUE,pageLength=20)))
  output$Diff_suit_df <- renderDT(datatable(diff_suit, 
                                            options=list(paging=TRUE,pageLength=20)))
  
  output$Same_suit <- renderPlot(suppressWarnings(
    (
      ggplot(melt(same_suit*100,c('Card1','Card2')) 
             %>% rename(WinPercentage=value),aes(Card1,Card2)) +
        geom_tile(aes(fill=WinPercentage)) +
        scale_fill_gradientn(limits=c(0,60),
                             colours=c("red", "yellow", 'green',"blue")) +
        ggtitle("Pre-flop winning probability when private cards are from same suit") +
        theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18,face="bold"))
    )
  ))
  output$Diff_suit <- renderPlot(suppressWarnings(
    (
      ggplot(melt(diff_suit*100,c('Card1','Card2')) 
             %>% rename(WinPercentage=value),aes(Card1,Card2)) +
        geom_tile(aes(fill=WinPercentage)) +
        scale_fill_gradientn(limits=c(0,60),
                             colours=c("red", "yellow", 'green',"blue")) +
        ggtitle("Pre-flop winning probability when private cards are from diff suit") +
        theme(axis.text=element_text(size=18),
              axis.title=element_text(size=18,face="bold"))
    )
  ))
  
  
  output$Common_label <- renderText("")
  
  output$imgC1<-renderImage({
    list(src = ifelse(as.numeric(input$visible_cards)>=1,
                      sprintf("png/%s.png",input$common_card1),
                      "png/back.png"
    ),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$imgC2<-renderImage({
    list(src = ifelse(as.numeric(input$visible_cards)>=2,
                      sprintf("png/%s.png",input$common_card2),
                      "png/back.png"
    ),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$imgC3<-renderImage({
    list(src = ifelse(as.numeric(input$visible_cards)>=3,
                      sprintf("png/%s.png",input$common_card3),
                      "png/back.png"
    ),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$imgC4<-renderImage({
    list(src = ifelse(as.numeric(input$visible_cards)>=4,
                      sprintf("png/%s.png",input$common_card4),
                      "png/back.png"
    ),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$imgC5<-renderImage({
    list(src = ifelse(as.numeric(input$visible_cards)>=5,
                      sprintf("png/%s.png",input$common_card5),
                      "png/back.png"
    ),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  
  output$img1<-renderImage({
    list(src = ifelse(input$p1_visible=='Yes',sprintf("png/%s.png",input$P1_Card1),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img2<-renderImage({
    list(src = ifelse(input$p1_visible=='Yes',sprintf("png/%s.png",input$P1_Card2),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img3<-renderImage({
    list(src = ifelse(input$p2_visible=='Yes',sprintf("png/%s.png",input$P2_Card1),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img4<-renderImage({
    list(src = ifelse(input$p2_visible=='Yes',sprintf("png/%s.png",input$P2_Card2),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img5<-renderImage({
    list(src = ifelse(input$p3_visible=='Yes',sprintf("png/%s.png",input$P3_Card1),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img6<-renderImage({
    list(src = ifelse(input$p3_visible=='Yes',sprintf("png/%s.png",input$P3_Card2),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img7<-renderImage({
    list(src = ifelse(input$p4_visible=='Yes',sprintf("png/%s.png",input$P4_Card1),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img8<-renderImage({
    list(src = ifelse(input$p4_visible=='Yes',sprintf("png/%s.png",input$P4_Card2),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img9<-renderImage({
    list(src = ifelse(input$p5_visible=='Yes',sprintf("png/%s.png",input$P5_Card1),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  output$img10<-renderImage({
    list(src = ifelse(input$p5_visible=='Yes',sprintf("png/%s.png",input$P5_Card2),'png/back.png'),
         width="56px",height="81px",
         contentType = 'image/png',
         alt = "This is alternate text")
  }, deleteFile = FALSE)
  
  card_sets <- reactive(card_num[
                c(
                 input$P1_Card1,input$P1_Card2,
                 input$P2_Card1,input$P2_Card2,
                 input$P3_Card1,input$P3_Card2,
                 input$P4_Card1,input$P4_Card2,
                 input$P5_Card1,input$P5_Card2,
                 input$common_card1,
                 input$common_card2,
                 input$common_card3,
                 input$common_card4,
                 input$common_card5
                 )
  ])
  
  visible_common <- reactive(as.numeric(input$visible_cards))
  
  card_sets_masked <- reactive({
    x <- card_sets()
    if(visible_common()<5)
      x[(10+visible_common()+1):15] <- NA
    
    if(input$p1_visible=='No')
      x[1:2] <- NA
    if(input$p2_visible=='No')
      x[3:4] <- NA
    if(input$p3_visible=='No')
      x[5:6] <- NA
    if(input$p4_visible=='No')
      x[7:8] <- NA
    if(input$p5_visible=='No')
      x[9:10] <- NA
    x
  })
  cards_is_unique <- reactive(
    length(unique(card_sets_masked()[!is.na(card_sets_masked())]))==
      sum(!is.na(card_sets_masked()))
  )
  
  rv <- reactiveValues(j=sample(length(loading_messages),1))
  
  result <- eventReactive(c(input$push),
    {
      if(input$push>0&cards_is_unique()){
        prompt <- loading_messages[rv$j]
        rv$j <- sample(seq_along(loading_messages)[-rv$j],1)
        showModal(modalDialog(prompt, footer=NULL, style='font-size:175%'))
        game <- simulate_game2(cards=card_sets_masked(), MC_size=input$B)
        removeModal()
        game
      }   
    }             
  )

  
  output$P1 <- renderText(sprintf(
    "Player 1:Winning Probability: %5.2f%%<br /> Hand type probability: <br />%s<br /> Bluffed probability: <br />%s<br />",
    100*result()$prob_win[1],
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*result()$prob_hands[1,]),collapse='||'),
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*bluff(result()$prob_hands[1,],as.numeric(input$p1_bluff))),collapse='||')))
  output$P2 <- renderText(sprintf(
    "Player 2:Winning Probability: %5.2f%%<br /> Hand type probability: <br />%s<br /> Bluffed probability: <br />%s<br />",
    100*result()$prob_win[2],
    paste(sprintf("%s=%5.2f%%",hand_type_abbr,100*result()$prob_hands[2,]),collapse='||'),
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*bluff(result()$prob_hands[2,],as.numeric(input$p2_bluff))),collapse='||')))
  output$P3 <- renderText(sprintf(
    "Player 3:Winning Probability: %5.2f%%<br /> Hand type probability: <br />%s<br /> Bluffed probability: <br />%s<br />",
    100*result()$prob_win[3],
    paste(sprintf("%s=%5.2f%%",hand_type_abbr,100*result()$prob_hands[3,]),collapse='||'),
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*bluff(result()$prob_hands[3,],as.numeric(input$p3_bluff))),collapse='||')))
  output$P4 <- renderText(sprintf(
    "Player 4:Winning Probability: %5.2f%%<br /> Hand type probability: <br />%s<br /> Bluffed probability: <br />%s<br />",
    100*result()$prob_win[4],
    paste(sprintf("%s=%5.2f%%",hand_type_abbr,100*result()$prob_hands[4,]),collapse='||'),
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*bluff(result()$prob_hands[4,],as.numeric(input$p4_bluff))),collapse='||')))
  output$P5 <- renderText(sprintf(
    "Player 5:Winning Probability: %5.2f%%<br /> Hand type probability: <br />%s<br /> Bluffed probability: <br />%s<br />",
    100*result()$prob_win[5],
    paste(sprintf("%s=%5.2f%%",hand_type_abbr,100*result()$prob_hands[5,]),collapse='||'),
    paste(sprintf("%s=%5.2f%%",hand_type_abbr, 100*bluff(result()$prob_hands[5,],as.numeric(input$p5_bluff))),collapse='||')))
  output$asset_credit <- renderText(
    "<p> Playing Cards images come from https://github.com/hayeah/playing-cards-assets/ </p>"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
