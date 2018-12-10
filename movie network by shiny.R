if(!require(shiny)){
    install.packages("shiny")
}
if(!require(shinythemes)){
    install.packages("shinythemes")
}
if(!require(DT)){
    install.packages("DT")
}
if(!require(visNetwork)){
    install.packages("visNetwork")
}
if(!require(stringr)){
    install.packages("stringr")
}

interval_split <- function(df,column,by){
    min_num <- min(df[,column])
    max_num <- max(df[,column])
    if(by > max_num - min_num){
        by <- max_num - min_num
    }
    sequence <- seq(from = min_num,to= max_num,by=by)
    if(!any(sequence == max_num)){
        sequence <- c(sequence,max_num)
    }
    interval <- as.character(cut(df[,column],sequence,include.lowest = T))
    interval <- gsub("\\(","",interval)
    interval <- gsub("\\[","",interval)
    interval <- gsub("]","",interval)
    interval <- gsub(","," - ",interval)
    return(interval)
}

filter_df<-function(df,specified_year,one,two,three,four,category = NULL){
    factor_category <- c("budget","runtime","score","votes")
    filter_data <- subset(df,df$year==specified_year)
    if(!is.null(category)){
        if(any(category %in% factor_category)){
            slider_vector <- factor_category %in% category
            if(slider_vector[1]){
                budget_interval<-interval_split(filter_data,factor_category[1],10^one)
                filter_data$budget<-budget_interval
            }
            if(slider_vector[2]){
                runtime_interval<-interval_split(filter_data,factor_category[2],two)
                filter_data$runtime<-runtime_interval
            }
            if(slider_vector[3]){
                score_interval<-interval_split(filter_data,factor_category[3],three)
                filter_data$score<-score_interval
            }
            if(slider_vector[4]){
                votes_interval<-interval_split(filter_data,factor_category[4],10^four)
                filter_data$votes<-votes_interval
            }
        }
    }
    filter_data <- na.omit(filter_data)
    return(filter_data)
}


null_node <- function(df){
    id <- 1:(nrow(df)+1)
    label <- c(paste0(min(df$year),"년"),df$name)
    value <- c(sum(df$gross),df$gross)
    group <- c("year",rep("movie",length.out=nrow(df)))
    title <- paste0(value,"$ (",round(value*100/value[1],2),"%)")
    node <- data.frame(id,label,value,group,title,stringsAsFactors = F)
    return(node)    
}

null_edge <- function(){
    edge <- data.frame(from=numeric(),to=numeric(),title=character(),value=numeric())
    return(edge)
}

add_node <- function(df,column){
    label <- unique(df[,column])
    id <- 1:length(label)
    value <- rep(NA, length.out = length(label))
    for(num in 1:length(label)){
        value[num] <- sum(df[df[,column]==label[num], "gross"])
    }
    group <- rep(column, length.out= length(label))
    title <- paste0(value,"$ (",round(value*100/sum(value),2),"%)")
    node <- data.frame(id, label, value, group,title,stringsAsFactors = F)
    return(node)
}

add_edge <- function(df,column){
    title <- rep(column,length.out = nrow(df))
    category_data <- unique(df[,column])
    from <- c()
    to <- c()
    value <- c()
    for(nt in 1:length(category_data)){
        category_sum <- sum(df$gross[df[,column] == category_data[nt]])
        match_ind <- which(df[,column] == category_data[nt])
        from <- c(from,match_ind + 1)
        to <- c(to,rep(nt,length.out=length(match_ind)))
        value <- c(value,round(df$gross[match_ind]*10/category_sum))
    }
    edge <- data.frame(from,to,title,value,stringsAsFactors = F)
    return(edge)
}

delete_category <- function(node,edge,category){
    category_id <- node$id[node$group == category]
    node <- node[!node$group == category,]
    node$id <- 1:nrow(node)
    edge <- edge[!edge$title == category,]
    index_to_modify_f <- edge$from > max(category_id)
    edge$from[index_to_modify_f] <- edge$from[index_to_modify_f] - length(category_id)
    index_to_modify_t <- edge$to > max(category_id)
    edge$to[index_to_modify_t] <- edge$to[index_to_modify_t] - length(category_id)
    return(list(node,edge))
}


concat <- function(original_node,adding_node,original_edge,adding_edge){
    # 노드 합치는 과정
    new_node_id <- (nrow(original_node)+1):(nrow(original_node)+nrow(adding_node))
    adding_node$id <- new_node_id
    # 엣지 합치는 과정
    adding_edge$to <- adding_edge$to + nrow(original_node)
    # year로 합치는 과정
    new_from <- adding_node$id
    new_to <- rep(1,nrow(adding_node))
    new_title <- rep(adding_node$group[1],nrow(adding_node))
    new_value <- round(adding_node$value*10/sum(adding_node$value))
    new_edge <- data.frame(from=new_from,to=new_to,title=new_title,value=new_value,stringsAsFactors = F)
    integrated_node <- rbind(original_node,adding_node)
    integrated_edge <- rbind(original_edge,adding_edge,new_edge)
    return(list(node=integrated_node,edge=integrated_edge))
}


combn_category <- function(df,...){
    categories <- c(...)
    ind_df <-combn_ind(df,categories,ind_df=NULL)
    id <- 1:ncol(ind_df)
    label <- names(ind_df)
    group <- label
    node_value <- rep(NA,length.out=ncol(ind_df))
    for(num in 1:length(node_value)){
        node_value[num] <- sum(df$gross[ind_df[,num]])
    }
    gross_sum <- sum(df$gross)
    node_title <- paste0(node_value,"$ (",round(node_value*100/gross_sum,2),"%)")
    edge_title <- rep(paste(categories,collapse=","),length.out=nrow(df))
    from <- c()
    to <- c()
    edge_value <- c()
    for(nt in 1:length(label)){
        match_ind <- which(ind_df[,nt])
        category_sum <- sum(df$gross[match_ind])
        from <- c(from,match_ind + 1)
        to <- c(to,rep(nt,length.out=length(match_ind)))
        edge_value <- c(edge_value,round(df$gross[match_ind]*10/category_sum))
    }
    node <- data.frame(id, label, value=node_value,group,title=node_title,stringsAsFactors = F)
    edge <- data.frame(from,to,title=edge_title,value=edge_value,stringsAsFactors = F)
    return(list(node,edge))
}

combn_ind <- function(df,...,ind_df=NULL){
    categories <- c(...)
    category <- categories[1]
    if(is.null(ind_df)){
        label <- unique(df[,category])
        new_ind_df <- data.frame(matrix(F,nrow=nrow(df),ncol=length(label)))
        names(new_ind_df) <- label
        for(num in 1:length(label)){
            new_ind_df[,num] <- df[,category] == label[num] 
        }
    }
    else{ 
        label <- unique(df[,category])
        colnames_rep <- rep(names(ind_df),each=length(label))
        label_rep <- rep(label,ncol(ind_df))
        concat_label <- paste(colnames_rep,label_rep,sep=",") 
        new_ind_df <- data.frame(matrix(F,nrow=nrow(df),ncol=length(concat_label)))
        names(new_ind_df) <- concat_label
        delete_ind <- c()
        for(i in 1:ncol(ind_df)){
            for(j in 1:length(label)){
                bool <- ind_df[,i] & (df[,category] == label[j])
                new_ind_df[,(length(label)*(i-1) + j)] <- bool
                if(!any(bool)){
                    delete_ind <- c(delete_ind,(length(label)*(i-1) + j))
                }
            }
        }
        new_ind_df <- new_ind_df[,-delete_ind]
    }
    if(length(categories)!=1){
        return(combn_ind(df,categories[2:length(categories)],ind_df=new_ind_df))
    }
    else{
        return(new_ind_df)
    }
}

rounding <- function(value){
    min_value <- min(value)
    max_value <- max(value)
    by <- (max_value - min_value)/9
    factor_value <- rep(NA,length.out=length(value))
    for(ind in 1:length(value)){
        factor_value[ind] <- round((value[ind] - min_value)/by) + 1 # 0 ~ 9 -> 1 ~ 10 
    }
    return(factor_value)
}


print_top_degree <- function(node,edge){
    category_id <- node$id[!(node$group %in% c("year","movie"))]
    extract <- data.frame(node[!(node$group %in% c("year","movie")),c("label","title","group")],degree=NA)
    for(ind in seq(category_id)){
        extract$degree[ind] <- nrow(edge[edge$to == category_id[ind],])
    }
    percent <- str_extract(extract$title,"[0-9.]*%")
    percent <- as.numeric(gsub("%","",percent))
    extract <- extract[order(percent,decreasing = T),]
    rownames(extract) <- 1:nrow(extract)
    return(extract)
}

movie_sim <- function(df,categories){
    df <- df[,c("name",categories)]
    sim_df <- data.frame(matrix(NA,nrow=nrow(df),ncol=nrow(df)))
    colnames(sim_df) <- df$name
    rownames(sim_df) <- df$name
    for(i in 1:nrow(sim_df)){
        if(i != nrow(sim_df)){
            for(j in (i+1):nrow(sim_df)){
                i_category <- unlist(df[i,categories,drop=T])
                j_category <- unlist(df[j,categories,drop=T])
                union_set <- union(i_category,j_category)
                inter_set <- intersect(i_category,j_category)
                sim_df[i,j] <- length(inter_set)/length(union_set)
            }
            
        }
    }
    sim_df[sim_df==0] <- NA
    return(sim_df)
}


category_sim <- function(df,categories){
    combine_df <- data.frame(name=character(),category=character())
    for(category in categories){
        combine_df <- rbind(combine_df,data.frame(name=df[,"name"],category=df[,category]))
    }
    category_data <- unique(combine_df$category)
    sim_df <- data.frame(matrix(numeric(),nrow=length(category_data),ncol=length(category_data)))
    colnames(sim_df) <- category_data
    rownames(sim_df) <- category_data
    for(i in 1:nrow(sim_df)){
        if(i != nrow(sim_df)){
            for(j in (i+1):nrow(sim_df)){
                i_category <- combine_df$name[combine_df$category == category_data[i]]
                j_category <- combine_df$name[combine_df$category == category_data[j]]
                union_set <- union(i_category,j_category)
                inter_set <- intersect(i_category,j_category)
                sim_df[i,j] <- length(inter_set)/length(union_set)
            }
        }
    }
    sim_df[sim_df==0] <- NA
    return(sim_df)
}


#### shiny

category_list <- c("budget","company","country","director","genre","rating",
                   "runtime","score","star","votes","writer")


ui <- fluidPage(
    theme=shinytheme("paper"),
    titlePanel("영화 트렌드 1986 - 2016"),
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "method",
                         label = "방식을 선택하세요",
                         choices = c("카테고리","카테고리 묶음"),
                         selected = "카테고리"),
            sliderInput(inputId = "year",
                        label = "연도를 선택하세요",
                        min = 1986,
                        max = 2016,
                        value = 2001),
            checkboxGroupInput(inputId = "categories",
                               label = "카테고리를 선택하세요",
                               choices = category_list), 
            conditionalPanel(condition = "$.inArray('budget', input.categories) > -1",
                             sliderInput("sliderOne", "예산 범위를 설정하세요", 
                                         min=3, max=7, value=5 )),
            conditionalPanel(condition = "$.inArray('runtime', input.categories) > -1",
                             sliderInput("sliderTwo", "상영시간 범위를 설정하세요", 
                                         min=0, max=100, value=50)),
            conditionalPanel(condition = "$.inArray('score', input.categories) > -1",
                             sliderInput("sliderThree", "점수 범위를 설정하세요", 
                                         min=0, max=10, value=5,step=0.1)), 
            conditionalPanel(condition = "$.inArray('votes', input.categories) > -1",
                             sliderInput("sliderFour", "투표 범위를 설정하세요", 
                                         min=0, max=100, value=50)),
            actionButton("submit", label = "변경"),
            width = 2
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Network", visNetworkOutput("network",width="100%",height="800px"),
                                 dataTableOutput("top_degree")),
                        tabPanel("Movie Similarity", dataTableOutput("m_sim")),
                        tabPanel("Category Similarity", dataTableOutput("c_sim"))
            )
        )
    )
)


server <- function(input, output) {
    isolate(movie <- read.csv("https://raw.githubusercontent.com/jhoijune/jhoijune.github.io/master/movie.csv",stringsAsFactors = F))
    preserve <- reactiveValues(method="카테고리",year=2001,categories=NULL,
                               sliderOne=5,sliderTwo=50,sliderThree=5,sliderFour=50,
                               filter_movie=NULL,node=NULL,edge=NULL,network=NULL,
                               adding_node=NULL,adding_edge=NULL,ne=NULL,top_degree=NULL,
                               m_sim = NULL,c_sim=NULL)
    observeEvent(input$method,preserve$method <- input$method)
    observeEvent(input$year,preserve$year <- input$year)
    observeEvent(input$categories,preserve$categories <- input$categories)
    observeEvent(input$sliderOne,preserve$sliderOne <- input$sliderOne)
    observeEvent(input$sliderTwo,preserve$sliderTwo <- input$sliderTwo)
    observeEvent(input$sliderThree,preserve$sliderThree <- input$sliderThree)
    observeEvent(input$sliderFour,preserve$sliderFour <- input$sliderFour)
    observeEvent(input$submit,{
        if(preserve$method == "카테고리"){
            preserve$filter_movie <- filter_df(movie,preserve$year,preserve$sliderOne,preserve$sliderTwo,
                                               preserve$sliderThree,preserve$sliderFour,preserve$categories)
            preserve$node <- null_node(preserve$filter_movie)
            preserve$edge <- null_edge()
            if(!is.null(preserve$categories)){
                for(category in preserve$categories){
                    preserve$adding_node <- add_node(preserve$filter_movie,category)
                    preserve$adding_edge <- add_edge(preserve$filter_movie,category)
                    preserve$ne <- concat(preserve$node,preserve$adding_node,preserve$edge,preserve$adding_edge)
                    preserve$node <- preserve$ne[[1]]
                    preserve$edge <- preserve$ne[[2]]
                }
            }    
        }
        else if(preserve$method == "카테고리 묶음"){
            preserve$filter_movie <- filter_df(movie,preserve$year,preserve$sliderOne,preserve$sliderTwo,
                                               preserve$sliderThree,preserve$sliderFour,preserve$categories)
            preserve$node <- null_node(preserve$filter_movie)
            preserve$edge <- null_edge()
            preserve$ne <- combn_category(preserve$filter_movie,preserve$categories)
            preserve$ne <- concat(preserve$node,preserve$ne[[1]],preserve$edge,preserve$ne[[2]])
            preserve$node <- preserve$ne[[1]]
            preserve$edge <- preserve$ne[[2]]
        }
        preserve$top_degree <- datatable(print_top_degree(preserve$node,preserve$edge))
        if(length(preserve$categories) > 1 ){
            preserve$m_sim <- datatable(movie_sim(preserve$filter_movie,preserve$categories))
            preserve$c_sim <- datatable(category_sim(preserve$filter_movie,preserve$categories))
        }
        preserve$node$value <- rounding(preserve$node$value)
        preserve$network <- visNetwork(preserve$node,preserve$edge) %>%
            visLayout(randomSeed = 12) %>%
            visOptions(highlightNearest = list(enabled=T,degree=1,hover=F,labelOnly=F),selectedBy = list(variable="group",multiple=T),
                       nodesIdSelection = T) %>%
            visLegend() %>%
            visPhysics(stabilization = TRUE,maxVelocity = 10,minVelocity = 1)
    })
    output$network <- renderVisNetwork(preserve$network)
    output$top_degree <- renderDataTable(preserve$top_degree)
    output$m_sim <- renderDataTable(preserve$m_sim)
    output$c_sim <- renderDataTable(preserve$c_sim)
}


shinyApp(ui = ui, server = server)
