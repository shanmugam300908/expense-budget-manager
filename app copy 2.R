library(shiny)
library(ggplot2)
library(dplyr)

# ============================================================
# PROJECT 4 - EXPENSE & BUDGET MANAGER
# 30 UNIQUE TRANSACTIONS
# ============================================================

transactions <- data.frame(
  
  Transaction_ID = sprintf("T%03d", 1:30),
  
  Date = as.Date(c(
    "2026-09-01",
    "2026-09-02",
    "2026-09-03",
    "2026-09-04",
    "2026-09-05",
    "2026-09-06",
    "2026-09-07",
    "2026-09-08",
    "2026-09-09",
    "2026-09-10",
    "2026-09-11",
    "2026-09-12",
    "2026-09-13",
    "2026-09-14",
    "2026-09-15",
    "2026-09-16",
    "2026-09-17",
    "2026-09-18",
    "2026-09-19",
    "2026-09-20",
    "2026-09-21",
    "2026-09-22",
    "2026-09-23",
    "2026-09-24",
    "2026-09-25",
    "2026-09-26",
    "2026-09-27",
    "2026-09-28",
    "2026-09-29",
    "2026-09-30"
  )),
  
  Description = c(
    "Monthly Allowance",
    "Bus Travel",
    "College Lunch",
    "Mobile Recharge",
    "Physics Book",
    "Coffee",
    "Part-time Income",
    "Stationery",
    "Movie Ticket",
    "Groceries",
    "Online Course",
    "Auto Travel",
    "Evening Snacks",
    "Scholarship",
    "Internet Bill",
    "New Shirt",
    "Breakfast",
    "Freelance Income",
    "Electricity Share",
    "Medicine",
    "Monthly Bus Pass",
    "Restaurant Dinner",
    "Project Materials",
    "Music Subscription",
    "Birthday Gift",
    "Part-time Income",
    "Sports Shoes",
    "Vegetables",
    "Printing Cost",
    "Monthly Allowance"
  ),
  
  Category = c(
    "Income",
    "Transport",
    "Food",
    "Bills",
    "Education",
    "Food",
    "Income",
    "Education",
    "Entertainment",
    "Food",
    "Education",
    "Transport",
    "Food",
    "Income",
    "Bills",
    "Shopping",
    "Food",
    "Income",
    "Bills",
    "Health",
    "Transport",
    "Food",
    "Education",
    "Entertainment",
    "Shopping",
    "Income",
    "Shopping",
    "Food",
    "Education",
    "Income"
  ),
  
  Type = c(
    "Income",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Income",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Income",
    "Expense",
    "Expense",
    "Expense",
    "Income",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Expense",
    "Income",
    "Expense",
    "Expense",
    "Expense",
    "Income"
  ),
  
  Amount = c(
    8000,
    120,
    180,
    299,
    650,
    90,
    3500,
    220,
    250,
    850,
    1200,
    150,
    100,
    5000,
    699,
    1800,
    140,
    4200,
    600,
    350,
    500,
    450,
    900,
    149,
    700,
    3000,
    750,
    650,
    400,
    180
  )
)

# ============================================================
# BUDGET
# ============================================================

monthly_budget <- 12000

# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  titlePanel("💰 Personal Expense & Budget Manager"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Transaction Filters"),
      
      selectInput(
        "type",
        "Transaction Type:",
        choices = c(
          "All",
          "Income",
          "Expense"
        ),
        selected = "All"
      ),
      
      selectInput(
        "category",
        "Category:",
        choices = c(
          "All",
          sort(unique(transactions$Category))
        ),
        selected = "All"
      ),
      
      br(),
      
      h4("Monthly Budget"),
      
      numericInput(
        "budget",
        "Set Budget (₹):",
        value = 12000,
        min = 0,
        step = 500
      )
    ),
    
    mainPanel(
      
      h3("Financial Summary"),
      
      textOutput("income"),
      textOutput("expenses"),
      textOutput("balance"),
      textOutput("budget_used"),
      textOutput("budget_status"),
      
      br(),
      
      plotOutput("category_plot"),
      
      br(),
      
      plotOutput("income_expense_plot"),
      
      br(),
      
      h3("Transaction History"),
      
      tableOutput("transaction_table")
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output) {
  
  # ----------------------------------------------------------
  # FILTER TRANSACTIONS
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- transactions
    
    if (input$type != "All") {
      
      data <- data %>%
        filter(Type == input$type)
      
    }
    
    if (input$category != "All") {
      
      data <- data %>%
        filter(Category == input$category)
      
    }
    
    data
  })
  
  # ----------------------------------------------------------
  # TOTAL INCOME
  # ----------------------------------------------------------
  
  output$income <- renderText({
    
    total_income <- sum(
      transactions$Amount[
        transactions$Type == "Income"
      ]
    )
    
    paste(
      "Total Income: ₹",
      format(total_income, big.mark = ",")
    )
  })
  
  # ----------------------------------------------------------
  # TOTAL EXPENSES
  # ----------------------------------------------------------
  
  output$expenses <- renderText({
    
    total_expenses <- sum(
      transactions$Amount[
        transactions$Type == "Expense"
      ]
    )
    
    paste(
      "Total Expenses: ₹",
      format(total_expenses, big.mark = ",")
    )
  })
  
  # ----------------------------------------------------------
  # REMAINING BALANCE
  # ----------------------------------------------------------
  
  output$balance <- renderText({
    
    total_income <- sum(
      transactions$Amount[
        transactions$Type == "Income"
      ]
    )
    
    total_expenses <- sum(
      transactions$Amount[
        transactions$Type == "Expense"
      ]
    )
    
    balance <- total_income - total_expenses
    
    paste(
      "Remaining Balance: ₹",
      format(balance, big.mark = ",")
    )
  })
  
  # ----------------------------------------------------------
  # BUDGET USED
  # ----------------------------------------------------------
  
  output$budget_used <- renderText({
    
    total_expenses <- sum(
      transactions$Amount[
        transactions$Type == "Expense"
      ]
    )
    
    percentage <- (
      total_expenses / input$budget
    ) * 100
    
    paste(
      "Budget Used:",
      round(percentage, 1),
      "%"
    )
  })
  
  # ----------------------------------------------------------
  # BUDGET STATUS
  # ----------------------------------------------------------
  
  output$budget_status <- renderText({
    
    total_expenses <- sum(
      transactions$Amount[
        transactions$Type == "Expense"
      ]
    )
    
    if (total_expenses <= input$budget) {
      
      "Budget Status: Within Budget"
      
    } else {
      
      "Budget Status: Budget Exceeded"
    }
  })
  
  # ==========================================================
  # GRAPH 1 - CATEGORY-WISE EXPENSES
  # ==========================================================
  
  output$category_plot <- renderPlot({
    
    category_data <- transactions %>%
      
      filter(Type == "Expense") %>%
      
      group_by(Category) %>%
      
      summarise(
        Total = sum(Amount),
        .groups = "drop"
      )
    
    ggplot(
      category_data,
      aes(
        x = reorder(Category, Total),
        y = Total
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Category-wise Expenses",
        x = "Category",
        y = "Amount (₹)"
      )
  })
  
  # ==========================================================
  # GRAPH 2 - INCOME VS EXPENSES
  # ==========================================================
  
  output$income_expense_plot <- renderPlot({
    
    summary_data <- transactions %>%
      
      group_by(Type) %>%
      
      summarise(
        Total = sum(Amount),
        .groups = "drop"
      )
    
    ggplot(
      summary_data,
      aes(
        x = Type,
        y = Total
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "Income vs Expenses",
        x = "Transaction Type",
        y = "Amount (₹)"
      )
  })
  
  # ==========================================================
  # TRANSACTION TABLE
  # ==========================================================
  
  output$transaction_table <- renderTable({
    
    filtered_data() %>%
      
      select(
        Transaction_ID,
        Date,
        Description,
        Category,
        Type,
        Amount
      )
  })
}

# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)