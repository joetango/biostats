library(shiny)
library(shinydashboard)

  ##################################
  ######### Global Functions #######
  ##################################

difficulty_check <- function(dc, mod){
  roll <- sample(1:20, 1)
  if((roll + mod) >= dc){
    paste(" d20 roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Success!")
  }
  else {
    paste("d20 roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Failure")
  }
}

mult_dice <- function(x, y){
  rolls <- c(sample(1:x, y, replace = TRUE))
  sum(rolls)
}

  ##############################
  ####### User Interface #######
  ##############################

ui <- dashboardPage(
  
  dashboardHeader(title = "DM Toolkit"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Dice Roller", tabName = "roller"),
      menuItem("Difficulty Class Check", tabName = "dc_check"),
      menuItem("Worldbuilding Generators", tabName = "gens")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("The Dungeon Master's Multitool")),
      tabItem(tabName = "roller",
              h2("Dice Roller"),
              h2(""),
              actionButton("run_d20", "Roll d20"),
              textOutput("d20"),
              h5(""),
              actionButton("run_d12", "Roll d12"),
              textOutput("d12"),
              h5(""),
              actionButton("run_d10", "Roll d10"),
              textOutput("d10"),
              h5(""),
              actionButton("run_d8", "Roll d8"),
              textOutput("d8"),
              h5(""),
              actionButton("run_d6", "Roll d6"),
              textOutput("d6"),
              h5(""),
              actionButton("run_d4", "Roll d4"),
              textOutput("d4"),
              h5(""),
              actionButton("run_d100", "Roll d100"),
              textOutput("d100")),
      tabItem(tabName = "dc_check",
              h2("Difficulty Class Check"),
              h2(""),
              h4("Instructions:"),
              p("Set the difficulty class to an appropriate level given
                 the obstacle (for example, climbing a tree may have a
                 difficulty class of 5, while jumping over a crevasse
                 may have a DC of 15). Then, add an appropriate modifer
                 based on the player character's stats (acrobatics +2 will
                 add +2 to the roll for jumping over the crevasse). The
                 program will do the rest for you."),
              h2(""),
              selectInput("dc", "Set Difficulty Class", choices = c(1:20)),
              numericInput("mod", "Enter Modifier:", value = 0),
              actionButton("dc_roll", "Roll"),
              textOutput("dc_result")),
      tabItem(tabName = "gens",
              h2("On-the-fly Worldbuilding Generators"),
              h2(""),
              p("All dungeon masters face the challenge of improvisation,
                 and sometimes the improvisation juices are not flowing
                 to their full potential. Below are some helpful tools
                 for generating information pertaining to world-buidling."),
              h2(""),
              actionButton("generate_npc", "Generate NPC Stats"),
              textOutput("npc_stats"),
              h1(""),
              actionButton("run_tavern", "Generate Tavern Name"),
              textOutput("tavern"))
    )
  )
)  

  ######################
  ####### Server #######
  ######################

server <- function(input, output){
  
  ######################################
  ####### Difficulty Class Check #######
  ######################################
  
  dc_result <- eventReactive(input$dc_roll, {
    difficulty_check(as.numeric(input$dc), input$mod)
  })
  
  output$dc_result <- renderText({
    dc_result()
  })
  
  ##################################
  ####### NPC Stat Generator #######
  ##################################
  
  npc_stats <- reactiveVal(NULL)
  
  observeEvent(input$generate_npc, {
    stat_gen <- function() {
      stat <- c(sample(1:6, 4, replace = TRUE))
      stat <- stat[stat != min(stat)]
      sum(stat)
    }
    
    for(i in 1:6) {
      temp <- paste0("stat", i)
      
      assign(temp, stat_gen())
    }
    
    npc <- paste("Strength:", stat1,
                 "Dexterity:", stat2,
                 "Constitution:", stat3,
                 "Intelligence:", stat4,
                 "Wisdom:", stat5,
                 "Charisma:", stat6)
    
    npc_stats(npc)
  })
  
  output$npc_stats <- renderText({
    if (!is.null(npc_stats())) {
      paste(npc_stats())
    }
    else {
      "[click button to generate NPC stats]"
    }
    
  })
  
  #####################################
  ####### Tavern Name Generator #######
  #####################################
  
  tavern <- reactiveVal(NULL)
  
  observeEvent(input$run_tavern, {
    
    tavern_adj <- c("Stout", "Bloody", "Slow", "Dull", "Soaked", "Drunken",
                    "Crooked", "Dark", "Fabulous", "Noble", "Soft", "Red",
                    "Green", "Whtie", "Black", "Yellow", "Blue", "Burning",
                    "Broken", "Shattered", "Mighty", "Strong", "Lonely",
                    "Poor", "Old", "Generous", "Lanky", "Hapless", "Tall",
                    "Remarkable", "Frugal", "Prudent", "Foul", "Evil", "Good",
                    "Rotten", "Shining", "Fragile", "Hungry", "Tired",
                    "Patient", "Merciful", "Immortal", "Faithful", "Friendly",
                    "Forlorn", "Adoring", "Brittle", "Floating", "Sharp",
                    "Worn", "Cursed", "Beautiful", "Beleoved", "Quiet",
                    "Happy", "Courageous", "Wounded", "Blind", "Clairvoyant",
                    "Blushing", "Calm", "Wary", "Cheerful", "Wise", "Clumsy",
                    "Boorish", "Boastful", "Sly", "Daring", "Rebellious",
                    "Diligent", "Disguised", "Ominous", "Determined", "Reliable",
                    "Loyal", "Raging", "Excited", "Shy", "Magical", "Trecherous",
                    "False", "Foolhardy", "Golden", "Frozen", "Gracious", "Hairy",
                    "Hidden", "Hoarse", "Honest", "Humble", "Limping",
                    "Lively", "Lucky", "Lean", "Nefarious", "Ogling",
                    "Subtle", "Crazy")
    
    tavern_noun <- c("Rooster", "Raven", "Crow", "Toad", "Hound", "Fox",
                     "Bull", "Boar", "Clam", "Hawk", "Eagle", "Mouse",
                     "Rat", "Frog", "Elk", "Cat", "Guardian", "Hunter",
                     "Barbarian", "Witch", "Troll", "Sword", "Shield",
                     "Bow", "Dagger", "Hammer", "Helm", "Acrobat", "Lion",
                     "Ghoul", "Druid", "Master", "King", "Queen", "Prince",
                     "Princess", "Shrub", "Tree", "Bear", "Smile", "Eye",
                     "Tounge", "Flounder", "Whale", "Steer", "Stallion",
                     "Mare", "Wish", "Hoof", "Goat", "Tower", "Fist",
                     "Monk", "Sleep", "Fool", "Knight", "Poet", "Thrush",
                     "Diamond", "Ruby", "Emerald", "Lute", "Drum", "Flute",
                     "Farmer", "Songbird", "Mother", "Father", "Solider",
                     "Sailor", "Brewer", "Hornet", "Donkey", "Hare",
                     "Twig", "Barrel", "Boot", "Fang", "Skull", "Snail",
                     "Beetle")
    
    result <- paste("The", sample(tavern_adj, 1), sample(tavern_noun, 1))
    
    tavern(result)
  })
  
  output$tavern <- renderText({
    if (!is.null(tavern())) {
      paste(tavern())
    }
    else {
      "[click button to generate name]"
    }
    
  })
  
  
  ############################
  ####### Dice Rollers #######
  ############################
  
  d20 <- reactiveVal(NULL)
  d12 <- reactiveVal(NULL)
  d10 <- reactiveVal(NULL)
  d8 <- reactiveVal(NULL)
  d6 <- reactiveVal(NULL)
  d4 <- reactiveVal(NULL)
  d100 <- reactiveVal(NULL)
  
  observeEvent(input$run_d20, {
    d20(sample(1:20, 1))
  })
  
  observeEvent(input$run_d12, {
    d12(sample(1:12, 1))
  })
  
  observeEvent(input$run_d10, {
    d10(sample(1:10, 1))
  })
  
  observeEvent(input$run_d8, {
    d8(sample(1:8, 1))
  })
  
  observeEvent(input$run_d6, {
    d6(sample(1:6, 1))
  })
  
  observeEvent(input$run_d4, {
    d4(sample(1:4, 1))
  })
  
  observeEvent(input$run_d100, {
    d100(sample(1:100, 1))
  })
  
  output$d20 <- renderText ({
    paste("d20 result:", d20())
  })
  
  output$d12 <- renderText ({
    paste("d12 result:", d12())
  })
  
  output$d10 <- renderText ({
    paste("d10 result:", d10())
  })
  
  output$d8 <- renderText ({
    paste("d8 result:", d8())
  })
  
  output$d6 <- renderText ({
    paste("d6 result:", d6())
  })
  
  output$d4 <- renderText ({
    paste("d4 result:", d4())
  })
  
  output$d100 <- renderText ({
    paste("d100 result:", d100())
  })
  
}



shinyApp(ui = ui, server = server)