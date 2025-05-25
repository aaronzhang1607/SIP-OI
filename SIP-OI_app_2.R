library(serial)
library(shiny)
library(shinyjs)

# 常量定义
panel_height <- "90vh"

# 样式常量
styles <- list(
  panel = "margin-bottom: 0; padding: 10px; width: 100%; box-sizing: border-box;"
)

# 获取可用COM端口的函数
get_available_ports <- function() {
  if (.Platform$OS.type == "windows") {
    tryCatch(
      {
        ports <- system(
          "wmic path Win32_SerialPort get DeviceID",
          intern = TRUE
        )
        ports <- ports[grepl("^COM", ports)]
        ports <- gsub("\\s+", "", ports)
        ports <- ports[ports != ""]
        return(ports)
      },
      error = function(e) {
        warning("无法获取COM端口列表:", e$message)
        return(character(0))
      }
    )
  } else {
    tryCatch(
      {
        return(listPorts())
      },
      error = function(e) {
        warning("无法获取COM端口列表:", e$message)
        return(character(0))
      }
    )
  }
}

# UI 定义
ui <- fluidPage(
  useShinyjs(),
  titlePanel("注射泵端口通信工具"),

  # 主内容区域 - 水平布局
  fluidRow(
    # 左侧控制面板 - 固定宽度
    column(
      width = 6,
      style = "padding-right: 10px;",
      wellPanel(
        style = styles$panel,
        # 连接控制
        h4("串口设置", style = "margin-top: 0;"),
        selectInput(
          "port",
          "选择COM端口:",
          choices = c("请选择..." = "", get_available_ports()),
          width = "100%"
        ),
        fluidRow(
          column(
            6,
            actionButton(
              "connect",
              "连接串口",
              class = "btn-primary",
              width = "100%"
            )
          ),
          column(
            6,
            actionButton(
              "disconnect",
              "断开连接",
              class = "btn-danger",
              width = "100%"
            )
          )
        ),

        # 泵参数设置
        hr(),
        h4("泵参数设置"),
        fluidRow(
          column(
            6,
            numericInput(
              "pump_number",
              "泵号 (1-2):",
              value = 1,
              min = 1,
              max = 2,
              step = 1,
              width = "100%"
            )
          ),
          column(
            6,
            selectInput(
              "units",
              "单位:",
              choices = c("nL/sec" = "S", "nL/min" = "M"),
              selected = "S",
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            6,
            div(
              style = "padding-right: 10px;",
              numericInput(
                "target_volume",
                "目标体积 (nL):",
                value = 1000,
                min = 0,
                max = 999999.9,
                step = 10,
                width = "100%"
              )
            )
          ),
          column(
            6,
            div(
              style = "padding-left: 10px;",
              numericInput(
                "delivery_rate",
                "输送速率:",
                value = 100,
                min = 0,
                max = 999999.9,
                step = 1,
                width = "100%"
              )
            )
          )
        ),

        # 泵控制
        hr(),
        h4("泵控制"),
        div(
          style = "margin-bottom: 10px;",
          fluidRow(
            column(
              6,
              div(
                style = "padding-right: 5px;",
                actionButton(
                  "set_infuse",
                  "注射",
                  class = "btn-primary",
                  width = "100%"
                )
              )
            ),
            column(
              6,
              div(
                style = "padding-left: 5px;",
                actionButton(
                  "set_withdraw",
                  "吸取",
                  class = "btn-warning",
                  width = "100%"
                )
              )
            )
          )
        ),
        div(
          style = "margin-top: 10px;",
          fluidRow(
            column(
              4,
              div(
                style = "padding-right: 5px;",
                actionButton(
                  "start_delivery",
                  "开始/继续",
                  class = "btn-success",
                  width = "100%"
                )
              )
            ),
            column(
              4,
              div(
                style = "padding-left: 5px; padding-right: 5px;",
                actionButton(
                  "pause_delivery",
                  "暂停",
                  class = "btn-warning",
                  width = "100%"
                )
              )
            ),
            column(
              4,
              div(
                style = "padding-left: 5px;",
                actionButton(
                  "stop_delivery",
                  "停止",
                  class = "btn-danger",
                  width = "100%"
                )
              )
            )
          )
        ),

        # 间歇注射模式
        hr(),
        h4("间歇注射模式"),
        div(
          style = "margin-bottom: 15px;",
          fluidRow(
            column(
              6,
              div(
                style = "padding-right: 10px;",
                numericInput(
                  "injection_time",
                  "注射时间 (秒):",
                  value = 5,
                  min = 0.1,
                  step = 0.1,
                  width = "100%"
                )
              )
            ),
            column(
              6,
              div(
                style = "padding-left: 10px;",
                numericInput(
                  "pause_time",
                  "间隔时间 (秒):",
                  value = 5,
                  min = 0.1,
                  step = 0.1,
                  width = "100%"
                )
              )
            )
          )
        ),
        div(
          style = "margin-top: 10px;",
          actionButton(
            "start_interval",
            "开始间歇注射",
            class = "btn-info",
            width = "100%"
          )
        )
      )
    ),

    # 右侧主显示区域 - 固定宽度
    column(
      width = 6,
      style = "display: flex; flex-direction: column; height: 90vh;",
      
      # 主内容区域
      div(
        style = "flex: 1; display: flex; flex-direction: column; overflow: hidden;",
        # 标签页面板
        tabsetPanel(
          id = "rightTabs",
          tabPanel(
            "通信",
            div(
              style = "display: flex; flex-direction: column; height: 100%; overflow: hidden;",
              h4("串口通信", style = "margin-top: 0;"),
              textAreaInput("output", "输出:", rows = 10, width = "100%"),
              verbatimTextOutput("status")
            )
          ),
          tabPanel(
            "历史记录",
            div(
              style = "height: 100%; overflow-y: auto;",
              h4("通信历史记录"),
              div(style = "margin-top: 10px;", tableOutput("history_table"))
            )
          ),
          tabPanel(
            "关于",
            div(
              style = "position: absolute; top: 45px; bottom: 200px; left: 0; right: 0; overflow-y: auto; padding: 15px;",
              uiOutput("aboutContent")
            )
          )
        )
      ),
      
      # 退针操作面板 - 固定在底部
      div(
        style = "margin-top: 10px;",
        wellPanel(
          style = "padding: 15px; border: 1px solid #ffcccc; background-color: #fff9f9;",
          h4(
            "退针操作",
            style = "color: #d9534f; margin-top: 0; font-weight: bold;"
          ),
          p(
            "警告：此操作将以最大速度回退针头，请确保操作安全！",
            style = "color: #d9534f; font-size: 0.9em; margin-bottom: 10px;"
          ),
          actionButton(
            "retract_needle",
            "执行退针",
            class = "btn-danger btn-lg",
            width = "100%",
            icon = icon("exclamation-triangle")
          )
        )
      )
    )
  )
)

# 服务器逻辑
server <- function(input, output, session) {
  # 渲染关于页面内容
  output$aboutContent <- renderUI({
    if (file.exists("README.md")) {
      includeMarkdown("README.md")
    } else {
      p("未找到README.md文件")
    }
  })

  # 初始化历史记录表格
  output$history_table <- renderTable({
    if (nrow(values$history) > 0) {
      values$history
    } else {
      data.frame(提示 = "暂无历史记录")
    }
  })

  # 初始化响应式值
  values <- reactiveValues(
    con = NULL,
    history = data.frame(
      Time = character(),
      Direction = character(),
      Data = character(),
      stringsAsFactors = FALSE
    ),
    received_data = "",
    interval_timer = NULL,
    is_injecting = FALSE,
    total_injected = 0,
    injection_cycles = 0,
    current_volume = 0,
    cycle_volume = 0
  )

  # 创建响应式表达式
  total_injected <- reactive(values$total_injected)
  injection_cycles <- reactive(values$injection_cycles)
  is_injecting <- reactive(values$is_injecting)
  interval_timer <- reactive(values$interval_timer)
  current_volume <- reactive(values$current_volume)
  cycle_volume <- reactive(values$cycle_volume)
  # 添加日志函数
  add_log <- function(direction, data) {
    new_entry <- data.frame(
      Time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS"),
      Direction = direction,
      Data = data,
      stringsAsFactors = FALSE
    )
    values$history <- rbind(values$history, new_entry)
  }

  # 安全发送命令函数
  safe_send_command <- function(cmd) {
    tryCatch(
      {
        if (!is.null(values$con) && isOpen(values$con)) {
          write.serialConnection(values$con, paste0(cmd, "\r\n"))
          add_log("发送", cmd)
          return(TRUE)
        }
        return(FALSE)
      },
      error = function(e) {
        showNotification(paste("发送命令失败:", e$message), type = "error")
        return(FALSE)
      }
    )
  }

  # 发送命令到设备
  send_command <- function(cmd) {
    if (is.null(cmd) || cmd == "") return(FALSE)
    safe_send_command(cmd)
    showNotification("未连接到设备", type = "warning")
    return(FALSE)
  }

  # 检查设备状态
  observeEvent(input$check_status, {
    send_command("STATUS")
  })

  # 开始刺激
  observeEvent(input$start_stim, {
    send_command("START")
  })

  # 停止刺激
  observeEvent(input$stop_stim, {
    send_command("STOP")
  })

  # 设置目标体积
  observeEvent(input$target_volume, {
    req(input$target_volume)
    vol <- sprintf("V%.1f", input$target_volume)
    send_command(vol)
  })

  # 设置输送速率
  observeEvent(input$delivery_rate, {
    req(input$delivery_rate)
    rate <- sprintf("R%.1f", input$delivery_rate)
    send_command(rate)
  })

  # 设置单位
  observeEvent(input$units, {
    req(input$units)
    send_command(input$units)
  })

  # 设置泵号
  observeEvent(input$pump_number, {
    req(input$pump_number)
    cmd <- sprintf("L%d", input$pump_number)
    send_command(cmd)
  })

  # 退针功能
  observeEvent(input$retract_needle, {
    # 保存当前设置
    current_pump <- input$pump_number
    current_rate <- input$delivery_rate

    # 设置最大速度（假设最大速度为1000 nL/min）
    max_speed <- 1000

    # 设置吸取方向
    send_command("W") # W 命令设置吸取方向

    # 设置最大速度（nL/min）
    send_command(sprintf("R%.1f", max_speed))

    # 设置单位（nL/min）
    send_command("M")

    # 开始吸取（使用最大速度）
    send_command("R")

    # 显示操作信息
    showNotification("正在退针...", type = "message")

    # 添加日志
    add_log("操作", "执行退针操作 - 最大速度吸取")

    # 5秒后停止（或根据实际情况调整时间）
    invalidateLater(5000, session)
    observe({
      req(input$retract_needle)
      send_command("S") # 停止
      showNotification("退针完成", type = "message")
      add_log("操作", "退针完成")
    })
  })

  # 设置注入方向并开始执行
  observeEvent(input$set_infuse, {
    send_command("I") # 设置注入方向
    send_command("G") # 开始执行
  })

  # 设置抽取方向、最大速度并使用设定体积，然后开始执行
  observeEvent(input$set_withdraw, {
    # 设置抽取方向
    send_command("W")
    # 设置最大抽取速度（假设最大速度为1000）
    send_command("R1000")
    # 设置目标体积
    send_command(paste0("V", input$target_volume))
    # 开始执行
    send_command("G")
  })

  # 开始/继续输送
  observeEvent(input$start_delivery, {
    send_command("G")
  })

  # 计算周期注射量
  calculate_cycle_volume <- function(delivery_rate, units, injection_time) {
    rate <- if (units == "M") delivery_rate / 60 else delivery_rate
    rate * injection_time
  }

  # 处理注射状态
  handle_injecting_state <- function(current_vol, target_vol, injection_t) {
    if (current_vol >= target_vol) {
      send_command("H")
      showNotification("注射完成：已达到目标体积", type = "message")
      return(list(continue = FALSE))
    } else {
      # 更新已注射量
      values$total_injected <- values$total_injected + values$cycle_volume
      values$injection_cycles <- values$injection_cycles + 1
      values$is_injecting <- FALSE
      return(list(continue = TRUE))
    }
  }

  # 处理暂停状态
  handle_paused_state <- function(target_vol) {
    if (values$total_injected < target_vol) {
      values$is_injecting <- TRUE
    }
  }

  # 开始间歇注射
  observeEvent(input$start_interval, {
    req(input$injection_time > 0, input$pause_time > 0, input$target_volume > 0)

    # 如果已经在运行，先停止
    if (!is.null(interval_timer())) {
      invalidateLater(0, session)
      interval_timer(NULL)
    }

    # 重置计数
    total_injected(0)
    injection_cycles(0)

    # 设置注入方向
    send_command("I")
    values$is_injecting <- TRUE

    # 设置目标体积
    send_command(paste0("V", input$target_volume))

    # 开始第一个注射周期
    send_command("G")

    # 计算每个周期的注射量
    values$cycle_volume <- calculate_cycle_volume(
      input$delivery_rate,
      input$units,
      input$injection_time
    )
    values$current_volume <- 0

    # 设置定时器
    values$interval_timer <- reactiveTimer(1000, session)

    # 监听定时器
    observeEvent(values$interval_timer(), {
      if (is_injecting()) {
        current_volume <- total_injected() + cycle_volume
        result <- handle_injecting_state(
          current_volume,
          input$target_volume,
          input$injection_time
        )
        if (!result$continue) {
          invalidateLater(0, session)
          interval_timer(NULL)
          is_injecting(FALSE)
        }
      } else {
        invalidateLater(input$pause_time * 1000, session)
        is_injecting(TRUE)
        handle_paused_state(input$target_volume)
      }
    })
  })

  # 停止间歇注射
  observeEvent(input$stop_delivery, {
    if (!is.null(interval_timer())) {
      invalidateLater(0, session)
      interval_timer(NULL)
      is_injecting(FALSE)
    }
    send_command("H") # 停止
    showNotification(
      paste0(
        "注射已停止，已注射体积: ",
        round(total_injected(), 2),
        " nL (",
        round(total_injected() / input$target_volume * 100, 1),
        "%)"
      ),
      type = "message"
    )
  })

  # 暂停输送
  observeEvent(input$pause_delivery, {
    if (!is.null(interval_timer())) {
      invalidateLater(0, session)
      interval_timer(NULL)
      is_injecting(FALSE)
    }
    send_command("U")
  })

  # 停止输送
  observeEvent(input$stop_delivery, {
    send_command("H")
  })

  # 设置独立模式
  observeEvent(input$set_mode_normal, {
    send_command("N")
  })

  # 设置组模式
  observeEvent(input$set_mode_grouped, {
    send_command("P")
  })

  # 禁用模式
  observeEvent(input$set_mode_disabled, {
    send_command("D")
  })

  # 初始化时设置默认泵号
  observe({
    send_command(sprintf("L%d", input$pump_number))
  })

  # 自动检测可用的COM端口
  observe({
    invalidateLater(5000, session) # 每5秒刷新一次端口列表
    ports <- c("请选择..." = "", get_available_ports())
    current <- isolate(input$port)
    if (!current %in% ports && current != "") {
      # 如果当前选择的端口不再可用，保留但标记为不可用
      ports <- c(ports, paste0("(不可用) ", current))
    }
    updateSelectInput(session, "port", choices = ports, selected = current)
  })

  # 连接串口
  observeEvent(input$connect, {
    req(input$port != "")
    tryCatch(
      {
        if (!is.null(values$con) && isOpen(values$con)) {
          close(values$con)
        }
        values$con <- serialConnection(
          port = input$port,
          mode = "115200,8,n,1,N"
        )
        open(values$con)
        add_log("系统", paste("已连接到", input$port))
        showNotification("连接成功", type = "message")
      },
      error = function(e) {
        showNotification(paste("连接失败:", e$message), type = "error")
        add_log("错误", paste("连接失败:", e$message))
      }
    )
  })

  # 断开串口
  observeEvent(input$disconnect, {
    req(values$con)
    tryCatch(
      {
        close(values$con)
        values$con <- NULL
        output$status <- renderText({
          "状态: 已断开"
        })
        showNotification("断开连接成功", type = "message")
      },
      error = function(e) {
        showNotification(paste("断开连接时出错:", e$message), type = "error")
      }
    )
  })

  # 接收数据
  observe({
    invalidateLater(100, session) # 每100毫秒检查一次数据
    req(values$con)
    tryCatch(
      {
        if (isOpen(values$con)) {
          # 尝试读取可用数据
          data <- read.serialConnection(values$con)
          if (nchar(data) > 0) {
            # 更新接收到的数据
            new_data <- paste0(format(Sys.time(), "[%H:%M:%OS] "), data, "\n")
            values$received_data <- paste0(values$received_data, new_data)

            # 添加到历史记录
            new_entry <- data.frame(
              Time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS"),
              Direction = "接收",
              Data = data,
              stringsAsFactors = FALSE
            )
            values$history <- rbind(values$history, new_entry)
          }
        }
      },
      error = function(e) {
        # 发生错误时忽略，避免应用崩溃
      }
    )
  })

  # 显示接收到的数据
  output$output <- renderText({
    values$received_data
  })

  # 显示状态
  output$status <- renderText({
    if (is.null(values$con) || !isOpen(values$con)) {
      return("连接状态: 未连接")
    }
    paste("连接状态: 已连接到", input$port)
  })

  # 显示历史记录
  output$history_table <- renderTable(
    {
      if (nrow(values$history) == 0) {
        return(data.frame(Message = "没有历史记录"))
      }
      values$history
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE
  )

  # 应用关闭时断开连接
  session$onSessionEnded(function() {
    if (!is.null(values$con) && isOpen(values$con)) {
      tryCatch(
        {
          close(values$con)
        },
        error = function(e) {
          message("关闭连接时出错: ", e$message)
        }
      )
    }
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
