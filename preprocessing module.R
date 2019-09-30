index_to_num <- function(ind){
    # 엑셀의 열 변형 모듈
    col_to_num <- function(col){
        len <- nchar(col)
        num_sum <- 0
        for(i in 1:len){
            temp <- substr(col,i,i)
            temp <- utf8ToInt(temp)-64
            num_sum <- num_sum+26**(len-i)*temp
        }
        return(num_sum)
    }
    temp_ind <- NULL
    ind <- toupper(ind)
    for(i in seq(ind)){
        if(length(grep(":",ind[i]))!=0){
            temp <- unlist(strsplit(ind[i],":"))
            temp_ind <- c(temp_ind,col_to_num(temp[1]):col_to_num(temp[2]))
        }
        else{
            temp_ind <- c(temp_ind,col_to_num(ind[i]))
        }
    }
    return(temp_ind)
}

WN_to_Day <- function(year,week,dw,length,interval=7){
    # 주번호 날짜 생성 모듈  
    #
    # year: 처음시작하는 연도    
    # week: 처음시작하는 주번호  
    # dw: 요일(한글자 혹은 세글자)  
    # length: 날짜를 만들 갯수    
    # interval: 날짜간의 간격 (기본 설정값:7)  
    if(!require(lubridate)){
        install.packages("lubridate")
    }
    library(lubridate)
    dw <- substr(dw,1,1)
    dw_list<-c("일","월","화","수","목","금","토")
    d_ind<-which(dw_list==dw)
    temp<-as.Date(paste0(year,"-01-01"))
    temp<-temp+7*(week-1)
    if(wday(temp)>d_ind){
        temp <- temp-(wday(temp)-d_ind)
    }
    else if(wday(temp)<d_ind){
        temp <- temp+(d_ind-wday(temp))
    }
    date <- seq(temp,by=interval,length.out=length)
    return(as.character(date))
}


y_split <- function(df,date_ind,d_ind){
    # 생산량 분할 모듈
    #
    # df: 생산량 데이터 프레임
    # date_ind: 날짜 데이터 열
    # d_ind: 생산량 데이터 열
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    df <- generating_plyr(df,date_ind,d_ind,"누적")
    date_temp <- as.Date(df[,1])
    date <- as.character(seq(date_temp[1],date_temp[length(date_temp)],by=1))
    date_diff <- diff(as.numeric(date_temp))
    temp_df <- data.frame(matrix(nrow=length(date),ncol=length(d_ind)+1))
    names(temp_df) <- c("date",names(df)[2:ncol(temp_df)])
    temp_df[,1] <- date
    temp_df[1,2:ncol(temp_df)] <- df[1,2:ncol(temp_df)]
    for(j in 2:ncol(temp_df)){
        ind <- 1
        for(i in seq(date_diff)){
            temp_df[(ind+1):(ind+date_diff[i]),j] <- (df[i+1,j])/date_diff[i]
            ind <- ind + date_diff[i]
        }
    }
    return(temp_df)
}

# y_split <- function(df,date_ind,d_ind){
#     if(is.character(date_ind)){
#         date_ind <- index_to_num(date_ind)
#     }
#     if(is.character(d_ind)){
#         d_ind <- index_to_num(d_ind)
#     }
#     if(any(class(df) %in% c("data.table","tbl"))){
#         df <- as.data.frame(df)
#     }
#     df <- mean_df(df,date_ind,d_ind)
#     date_temp <- as.Date(df[,1])
#     date <- as.character(seq(date_temp[1],date_temp[length(date_temp)],by=1))
#     date_diff <- diff(as.numeric(date_temp))
#     temp_df <- data.frame(date,yield=NA,stringsAsFactors = F)
#     ind <- 1
#     temp_df[1,2] <- df[1,2]
#     for(i in seq(date_diff)){
#         temp_df[(ind+1):(ind+date_diff[i]),2] <- (df[i+1,2])/date_diff[i]
#         ind <- ind + date_diff[i]
#     }
#     return(temp_df)
# }


mean_df <-function(df,date_ind,ind,select=T){
    # 날짜별로 평균내거나 담는 모듈
    #
    # df: 데이터 프레임  
    # date_ind: 날짜 있는 인덱스  
    # ind: 인덱싱할 열들   
    # select: ind열을 선택할 것인가 제거할 것인가  
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(ind)){
        ind <- index_to_num(ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    df[,date_ind] <- substr(df[,date_ind],1,10) # 리턴이 문자형
    date <- unique(df[,date_ind])
    if(select){
        select_ind <- ind
    }
    else {
        delete_ind<-ind
        if(date_ind %in% delete_ind){
            select_ind <- c(1:ncol(df))[-delete_ind]
        }
        else{
            select_ind<-c(1:ncol(df))[-c(date_ind,delete_ind)] 
        }
    }
    return(date_sync(df[,c(date_ind,select_ind)],date))
}

mean_df_h <-function(df,date_ind,ind,select=T){
    # 시간별로
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(ind)){
        ind <- index_to_num(ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    df[,date_ind] <- substr(df[,date_ind],1,13)
    df[,date_ind] <- gsub(":","",df[,date_ind])
    date <- unique(df[,date_ind])
    if(select){
        select_ind <- ind
    }
    else {
        delete_ind<-ind
        if(date_ind %in% delete_ind){
            select_ind <- c(1:ncol(df))[-delete_ind]
        }
        else{
            select_ind<-c(1:ncol(df))[-c(date_ind,delete_ind)] 
        }
    }
    return(date_sync(df[,c(date_ind,select_ind)],date))
}



date_sync <- function(df,date,date_ind=1){  
    # 날짜 맞춰서 데이터 담는 모듈
    #
    # 기본적으로 date_ind외에 다 가공한다고 가정함
    # df: 데이터 프레임  
    # date: 날짜만 있는 유일한 데이터  
    # date_ind: 데이터 프레임에서 날짜 있는 열 (기본설정값:1)  
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    d_ind <- setdiff(1:ncol(df),date_ind)
    temp_df <- as.data.frame(matrix(NA,nrow=length(date),ncol=ncol(df)))
    names(temp_df) <- c("date",names(df)[d_ind])
    temp_df[,1] <- date
    for(i in 1:length(date)){
        ind<-which(df[,1]==date[i])
        if(length(ind)==1){ 
            temp_df[i,2:ncol(df)] <- df[ind,d_ind]
        } 
        else if(length(ind)>1){
            for(j in 1:length(d_ind)){
                if(!is.character(df[ind,d_ind[j]])){
                    if(all(is.na(df[ind,d_ind[j]])))
                        temp_df[i,j+1] <- NA
                    else 
                        temp_df[i,j+1] <- mean(df[ind,d_ind[j]],na.rm=T)
                }
                else{
                    if(all(is.na(df[ind,j])))
                        temp_df[i,j+1] <- NA
                    else 
                        temp_df[i,j+1] <- max(df[ind,d_ind[j]])
                }
            }
        }
    }
    return(temp_df)
}



transpose_df <- function(df,col_names=F){
    # 전치 모듈
    # 
    # 파일을 불러들일 때 header를 안쓰는 것을 추천함
    # df: 데이터 프레임  
    # col_names: 열의 이름이 있는가 (기본 설정값:F)
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(col_names){
        df <- rbind(names(df),df)
    }
    temp_df <- as.data.frame(t(df),stringsAsFactors=F)
    rownames(temp_df) <- NULL
    temp_names<- temp_df[1,]
    temp_df <- temp_df[-1,]
    names(temp_df) <- temp_names
    return(temp_df)
}

transform_df <- function(df,year,sw="일",ew="토",col_names=T){
    # 환경 변형 모듈
    #
    # df: 데이터 프레임  
    # year: 처음시작하는 연도  
    # sw: 데이터프레임에서 시작하는 요일(한글자)  
    # ew: 데이터프레임에서 끝나는 요일(한글자)  
    # col_names: 열의 이름이 있는가 (기본설정값:T) 
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(col_names){
        det_row<-names(df)
        date<-WN_to_Day(year,as.numeric(df[1,1]),sw,nrow(df)*7,1)
    }
    else{
        det_row<-df[1,]
        df<-df[-1,]
        date<-WN_to_Day(year,as.numeric(df[1,1]),sw,nrow(df)*7,1)
    }
    week_end <- which(substr(det_row,1,1)==ew)
    col_ind <- NULL
    for(i in seq(week_end)){
        col_ind<-c(col_ind,seq(week_end[i]-6,week_end[i],by=1))
    }
    col_ind_matrix <- matrix(col_ind,nrow=7,byrow=F)
    weather_name <- det_row[week_end-7]
    temp_df <- data.frame(matrix(NA,ncol=length(weather_name)+1,nrow=length(date)))
    names(temp_df) <- c("날짜",weather_name)
    temp_df[,1] <- date
    options(warn=-1)
    for(col_i in seq(ncol(col_ind_matrix))){ 
        trans_i<-1
        col_seq<-col_ind_matrix[,col_i,drop=T]
        for(i in seq(nrow(df))){ 
            for(j in col_seq){
                temp_df[trans_i,col_i+1]<-df[i,j]
                trans_i<-trans_i+1
            }
        }
        det <- as.numeric(temp_df[,col_i+1])
        if(!all(is.na(det))){
            temp_df[,col_i+1] <- as.numeric(temp_df[,col_i+1])
        }
    }
    options(warn=0)
    return(temp_df)
}




reg_sun <- function(reg,delay=1){
    # 일출,일몰지역 리턴하고 지도 보여주는 모듈
    #
    # reg: 전체, 도, 정확한 지역
    # output: 전체지역, 도에 있는 지역, 맞거나 근사 지역과 지도
    # delay: over_query_limit 방지용 지연 시간 (기본 설정값: 1초)
    if(!require(gtable)){
        install.packages("gtable")
    }
    if(!require(ggmap)){
        install.packages("ggmap")
    }
    library(ggmap)
    regions <- list(City = c("서울","대전","광주","대구","부산","울산","세종","인천"),
                    Chonnam = c("영광","무안","목포","춘양","승주","순천","광양","여수공항",   
                                "여수","고흥","보성","장흥","해남","완도","진도","흑산도"),
                    Chonbuk = c("군산","익산","전주","부안","변산","정읍","임실","장수","남원"),
                    Gyeongnam = c("거창","밀양","김해","진해","창원","마산",
                                  "거제","통영","사천","진주","남해"),
                    Gyeongbuk = c("울진","영주","안동","영덕","의성","보현산",
                                  "포항","영천","경주","구미","김천","상주"),
                    Chungnam = c("태안","서산","당진","천안","아산","보령","서천","논산"),
                    Chungbuk = c("소백산","제천","충주","청주공항","청주","추풍령"),
                    Gyeonggi = c("파주","고양","부천","양평","광주(경기)","안양",
                                 "시흥","안산","수원","용인","화성","여주","평택"),
                    Gangwon = c("고성(강원)","속초","양양","주문진","강릉","동해",
                                "삼척","대관령","태백","영월","원주","춘천"),
                    Jeju = c("제주","제주(레이다기지)","성산일출봉","서귀포"),
                    Island= c("울릉도","독도","강화도"))
    regions_cpy <- regions
    names(regions) <- NULL
    province <- list(c("도시"),c("전라남도","전남"),c("전라북도","전북"),c("경상남도","경남"),
                     c("경상북도","경북"),c("충청남도","충남"),c("충청북도","충북"),c("경기도","경기"),
                     c("강원도","강원"),c("제주도"),c("섬"))
    region_axis <- read.csv("./loc axis.csv"
                            ,stringsAsFactors=F,fileEncoding="UTF-8")
    Sys.sleep(delay)
    options(warn=-1)
    if(reg=="전체" || tolower(reg)=="all"){
        map_load <- get_map(c(127.7295,35.88672),zoom=7,maptype="satellite") 
        map_spe <- ggmap(map_load) + geom_point(data=region_axis,aes(x=LON,y=LAT),size = 3 ,colour="red")+
            geom_text(data=region_axis,aes(x=LON,y=LAT,label=region),size=3,vjust=0,hjust=-.5,colour="white")
        print(map_spe)
        options(warn=0)
        return(regions_cpy)
    }
    else if(any(reg==unlist(province))){
        province_axis <- read.csv("./cen pro.csv",
                                  stringsAsFactors=F,fileEncoding = "UTF-8")
        for(i in 1:length(province)){
            if(any(reg==province[[i]])){
                ind <- i
                break
            }
        }
        province_spe <- regions[[ind]] 
        region_bool <- region_axis[,1] %in% province_spe
        temp_axis <- region_axis[region_bool,]
        map_load <- get_map(province_axis[ind,2:3],zoom=province_axis$zoom[ind],maptype="satellite") 
        map_spe <- ggmap(map_load) + geom_point(data=temp_axis,aes(x=LON,y=LAT),size = province_axis$p_size[ind],
                                                colour="red")+ geom_text(data=temp_axis,aes(x=LON,y=LAT,label=region),size=province_axis$t_size[ind],
                                                                         vjust=0,hjust=-.5,colour="white")
        print(map_spe)
        options(warn=0)
        return(province_spe)
    }
    else{
        if(!any(reg==unlist(regions))){
            reg_axis <- data.frame(region=reg,geocode(enc2utf8(reg)))
            ind <- which.min((region_axis[,2]-reg_axis[,2])^2 +(region_axis[,3]-reg_axis[,3])^2) 
            Sys.sleep(delay)
            map_load <- get_map(reg_axis[,2:3],zoom=10,maptype="satellite") 
            map_spe <- ggmap(map_load) + geom_point(data=reg_axis,aes(x=lon,y=lat),size=5,colour="blue") +
                geom_text(data=reg_axis,aes(x=lon,y=lat,label=region),size=5,vjust=0,hjust=-.5,colour="white")+
                geom_point(data=region_axis,aes(x=LON,y=LAT),size = 5 ,colour="orange")+
                geom_text(data=region_axis,aes(x=LON,y=LAT,label=region),size=5,vjust=0,hjust=-.5,colour="white") +
                geom_point(data=region_axis[ind,],aes(x=LON,y=LAT),size = 5 ,colour="red")+
                geom_text(data=region_axis[ind,],aes(x=LON,y=LAT,label=region),size=5,vjust=0,hjust=-.5,colour="white") 
            print(map_spe)
            options(warn=0)
            return(region_axis[ind,1])
        }
        else{ 
            ind <- reg==region_axis[,1]
            map_load <- get_map(region_axis[ind,2:3],zoom=10,maptype="satellite") 
            map_spe <- ggmap(map_load) + geom_point(data=region_axis,aes(x=LON,y=LAT),size=5,colour="orange")+
                geom_text(data=region_axis,aes(x=LON,y=LAT,label=region),size=6,vjust=0,hjust=-.5,colour="white")+
                geom_point(data=region_axis[ind,],aes(x=LON,y=LAT),size = 5 ,colour="red")+
                geom_text(data=region_axis[ind,],aes(x=LON,y=LAT,label=region),size=6,vjust=0,hjust=-.5,colour="white")
            print(map_spe)
            options(warn=0)
            return(reg)
        }
    }
}

get_sun <- function(loc,st,ed){
    # 일출,일몰 크롤링 모듈
    #
    # loc: 위치  
    # st: 시작년 시작월 "2017-10" "201710" 201710 
    # ed: 끝년 끝월 "2018-06" "201806" 201806
    if(!require(readxl)){
        install.packages("readxl")
    }
    if(!require(httr)){
        install.packages("httr")
    }
    library(readxl)
    library(httr)
    if(is.character(st) && length(grep("-",st))!=0){
        temp <- unlist(strsplit(st,"-"))
        sy <- as.numeric(temp[1])
        sm <- as.numeric(temp[2])
        
    }
    else{
        sy <- as.numeric(substr(st,1,4))
        sm <- as.numeric(substr(st,5,6))
    }
    if(is.character(ed) && length(grep("-",ed))!=0){
        temp <- unlist(strsplit(ed,"-"))
        ey <- as.numeric(temp[1])
        em <- as.numeric(temp[2])
    }
    else{
        ey <- as.numeric(substr(ed,1,4))
        em <- as.numeric(substr(ed,5,6))
    }
    URL_1 <- "https://astro.kasi.re.kr:444/life/sunmoon/excel?location="
    URL_2 <- URLencode(iconv(loc,to="UTF-8"))
    URL_3 <- "&date="
    nt <- 1
    while(sy!=ey || sm!=em+1){
        URL_4 <- sy
        URL_5 <- ifelse(nchar(sm)==1,paste0("0",sm),sm)
        URL <- paste0(URL_1,URL_2,URL_3,URL_4,URL_5)
        GET(URL, write_disk("temp.xlsx", overwrite=TRUE))
        if(is.null(nt)){
            temp_sun <- data.frame(read_xlsx("temp.xlsx",skip=1))
            temp_df <- rbind(temp_df,temp_sun[,c(1,3,5)])
        }
        else{
            temp_df <- data.frame(read_xlsx("temp.xlsx",skip=1))[,c(1,3,5)]
            nt <- NULL
        }
        if(sm+1>12){
            sm <- 1
            sy <- sy+1
        }
        else{
            sm <- sm+1
        }
    }
    file.remove("temp.xlsx")
    temp_df <- cbind(temp_df,loc,stringsAsFactors=F)
    names(temp_df) <- c("날짜","일출","일몰","지역")
    return(temp_df)
} 



ND_div <- function(YMDHM,sun_data){
    # 밤낮 구분 모듈
    #
    # sun_data: 일출,일몰 있는 데이터 (get_sun 모듈의 리턴값)   
    # YMDHM : (2017-04-23 18:00 형식의 데이터 있는 열 자체)  
    if(any(class(YMDHM) %in% c("list","tbl"))){
        YMDHM <- as.data.frame(YMDHM)
        YMDHM <- YMDHM[,1]
    }
    sun_data[,2] <- as.numeric(gsub(":","",substr(sun_data[,2],1,5)))
    sun_data[,3] <- as.numeric(gsub(":","",substr(sun_data[,3],1,5))) 
    YMD <- substr(YMDHM,1,10)
    YMD_fixed <- gsub("-","",YMD)
    YMD <- as.Date(YMD)
    HM <- substr(YMDHM,12,16)
    HM <- as.numeric(gsub(":","",HM))
    temp_div<-rep(NA,length=length(YMDHM))
    for(i in 1:nrow(sun_data)){
        YMD_ident <- (YMD_fixed==sun_data[i,1]) 
        if(any(YMD_ident,na.rm=T)){
            all_sun <- (HM > sun_data[i,2] & HM <= sun_data[i,3]) 
            all_before_night <- HM <= sun_data[i,2] 
            all_today_night <-  HM > sun_data[i,3]
            day_sun <- which(YMD_ident & all_sun)
            day_before_night <- which(YMD_ident & all_before_night)
            day_today_night <- which(YMD_ident & all_today_night)
            temp_div[day_sun] <- paste("주간",YMD[YMD_ident][1])
            temp_div[day_before_night] <- paste("야간",YMD[YMD_ident][1]-1)
            temp_div[day_today_night] <- paste("야간",YMD[YMD_ident][1])
        }
    }
    return(temp_div)
}




generating_variable <- function(df,date_ind,d_ind,kind,div_DN=NULL,tbase=15){
    # 특정 열에 대해 함수 적용시키는 모듈
    #
    # df: 데이터 프레임  
    # d_ind: 가공할 열들 번호  
    # date_ind: 날짜 있는 열번호    
    # kind: 최소,최대,평균,누적,DIF,GDD 중에 선택하고 주간,야간을 추가 가능
    # div_DN: 밤낮 구분 데이터 (ND_div 모듈의 리턴 값)
    # 초기의 데이터 변환
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    # 추가할 함수 부분
    DIF <- function(data,na.rm){
        return(max(data,na.rm=na.rm)-min(data,na.rm=na.rm))
    }
    GDD <- function(data,na.rm){
        temp <- (max(data,na.rm=na.rm)+min(data,na.rm=na.rm))/2 - tbase
        return(ifelse(temp>=0,temp,0))
    }
    # kind의 함수 판단
    functions_list <- list(최소=min,최대=max,평균=mean,누적=sum,DIF=DIF,GDD=GDD)
    kind_div <- rep(0,length(kind))
    for(i in seq(kind)){
        for(j in seq(names(functions_list))){
            temp<-grep(names(functions_list)[j],kind[i])
            if(length(temp)!=0) kind_div[i] <- j
        }
    }
    # kind의 주간,야간 판단
    kind_ND <- rep("전체",length(kind))
    if(!is.null(div_DN)){
        kind_ND[grep("야간",kind)] <- "야간"
        kind_ND[grep("주간",kind)] <- "주간"
    }
    # Date 생성
    date_seq <- substr(df[,date_ind],1,10)
    date <- as.Date(unique(date_seq))
    if(any(kind_ND=="야간")){ # 함수 종류중에 야간이 있는 경우
        date <- seq(date[1]-1,date[length(date)],by=1)
    }
    # 데이터 프레임 생성
    temp_df <- data.frame(matrix(NA,nrow=length(date),ncol=length(kind)*length(d_ind)))
    ind_name <- names(df)[d_ind]
    temp_name<-vector(length=length(kind)*length(d_ind))
    for(i in seq(ind_name)){
        for(j in seq(kind)){
            temp_name[length(kind)*(i-1)+j]<-paste0(kind[j]," ",ind_name[i])
        }
    }
    names(temp_df) <- temp_name
    # 데이터 프레임에 데이터 담는 과정
    for(i in 1:length(date)){ # 한 날짜에 대해
        if(any(kind_ND=="전체")) today_ind <- which(date_seq==date[i])
        if(any(kind_ND=="야간")) night_ind <- which(div_DN==paste("야간",date[i]))
        if(any(kind_ND=="주간")) daytime_ind <- which(div_DN==paste("주간",date[i]))
        for(j in d_ind){ # 한 변수에 대해
            loc<-which(d_ind==j) 
            for(kind_num in seq(kind)){ # 한 함수 종류에 대해
                if(kind_ND[kind_num]=="주간"){ # 낮
                    if(!all(is.na(df[daytime_ind,j]))){
                        temp_df[i,length(kind)*(loc-1)+kind_num] <- 
                            functions_list[[kind_div[kind_num]]](df[daytime_ind,j],na.rm=T) 
                    }
                }
                else if(kind_ND[kind_num]=="야간"){
                    if(!all(is.na(df[night_ind,j]))){
                        temp_df[i,length(kind)*(loc-1)+kind_num] <- 
                            functions_list[[kind_div[kind_num]]](df[night_ind,j],na.rm=T)
                    }
                }
                else if(kind_ND[kind_num]=="전체"){
                    if(!all(is.na(df[today_ind,j]))){
                        temp_df[i,length(kind)*(loc-1)+kind_num] <- 
                            functions_list[[kind_div[kind_num]]](df[today_ind,j],na.rm=T) 
                    }
                }    
            }
        }
    }    
    temp_df <- data.frame(날짜=date,temp_df)
    temp_df[,1] <- as.character(temp_df[,1])
    return(temp_df)
}

# generating_variable plyr 버전

generating_plyr <- function(df,date_ind,d_ind,kind,div_DN=NULL,tbase=15){
    # 특정 열에 대해 함수 적용시키는 모듈
    #
    # df: 데이터 프레임  
    # d_ind: 가공할 열들 번호  
    # date_ind: 날짜 있는 열번호    
    # kind: 최소,최대,평균,누적,DIF,GDD 중에 선택하고 주간,야간을 추가 가능
    # div_DN: 밤낮 구분 데이터 (ND_div 모듈의 리턴 값)
    # 초기 설정
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(!require(plyr)){
        install.packages("plyr")
    }
    library(plyr)
    # 추가할 함수 부분
    DIF <- function(data,na.rm){
        return(max(data,na.rm=na.rm)-min(data,na.rm=na.rm))
    }
    GDD <- function(data,na.rm){
        temp <- (max(data,na.rm=na.rm)+min(data,na.rm=na.rm))/2 - tbase
        return(ifelse(temp>=0,temp,0))
    }
    # kind의 함수 판단
    functions_list <- list(최소=min,최대=max,평균=mean,누적=sum,DIF=DIF,GDD=GDD)
    kind_div <- rep(0,length(kind))
    for(i in seq(kind)){
        for(j in seq(names(functions_list))){
            temp<-grep(names(functions_list)[j],kind[i])
            if(length(temp)!=0) kind_div[i] <- j
        }
    }
    # 데이터 프레임 변환
    df <- df[,c(date_ind,d_ind)]
    df[,1] <- substr(df[,1],1,10)
    # kind의 주간,야간 판단
    kind_ND <- rep("전체",length(kind))
    if(!is.null(div_DN)){
        kind_ND[grep("야간",kind)] <- "야간"
        kind_ND[grep("주간",kind)] <- "주간"
        df_d <- df[grep("주간",div_DN),]
        which_n <- grep("야간",div_DN)
        df_n <- df[which_n,]
        df_n[,1] <- substr(div_DN[which_n],4,13)
    }
    # 데이트 생성
    date <- as.Date(unique(df[,1]))
    if(any(kind_ND=="야간")){ # 함수 종류중에 야간이 있는 경우
        date <- seq(date[1]-1,date[length(date)],by=1)
    }
    # 데이터 프레임 생성
    temp_df <- data.frame(matrix(NA,nrow=length(date),ncol=length(kind)*length(d_ind)))
    ind_name <- names(df)[2:ncol(df)]
    temp_name<-vector(length=length(kind)*length(d_ind))
    for(i in seq(ind_name)){
        for(j in seq(kind)){
            temp_name[length(kind)*(i-1)+j]<-paste0(kind[j]," ",ind_name[i])
        }
    }
    names(temp_df) <- temp_name
    # 데이터 프레임에 데이터 담는 과정
    for(i in 1:length(d_ind)){
        for(kind_num in seq(kind)){
            if(kind_ND[kind_num]=="주간"){ 
                temp_col <- 
                    ddply(df_d,1,function(part){functions_list[[kind_div[kind_num]]](part[,i+1],na.rm=T)}) 
            }
            else if(kind_ND[kind_num]=="야간"){
                temp_col <- 
                    ddply(df_n,1,function(part){functions_list[[kind_div[kind_num]]](part[,i+1],na.rm=T)})
            }
            else if(kind_ND[kind_num]=="전체"){
                temp_col <- 
                    ddply(df,1,function(part){functions_list[[kind_div[kind_num]]](part[,i+1],na.rm=T)})
                
            } 
            if(any(is.nan(temp_col[,2]))){
                temp_col[is.nan(temp_col[,2]),2] <- NA
            }
            if(any(is.infinite(temp_col[,2]))){
                temp_col[is.infinite(temp_col[,2]),2] <- NA
            }
            temp_df[,length(kind)*(i-1)+kind_num] <- as.numeric(date_sync(temp_col,date)[,2])
        }
    }
    temp_df <- data.frame(date,temp_df)
    temp_df[,1] <- as.character(temp_df[,1])
    return(temp_df)
}

integrated_df<- function(df_list,WN=F,date_ind_list=NULL){
    # 통합하는 모듈
    #
    # df_list: 데이터 프레임의 리스트  
    # WN: 주번호를 넣을건지
    # date_ind_list: 데이터 프레임 날짜 인덱스의 벡터(시간은 없어야 함)  
    # date_ind_list=NULL일때 인덱스 1에 날짜 데이터 있다고 가정함  
    # 초기 설정
    for(i in 1:length(df_list)){
        if(any(class(df_list[[i]]) %in% c("data.table","tbl"))){
            df_list[[i]] <- as.data.frame(df_list[[i]]) # 티블 데이터 데이터프레임으로 변형
        }
    }
    # 날짜 인덱싱 1로 고정함
    if(is.null(date_ind_list)){
        date_ind_list<-rep(1,length(df_list))
    }
    # 날짜를 가장 범용적으로 생성함
    lower_date <- vector(length=length(df_list))
    upper_date <- vector(length=length(df_list))
    for(i in 1:length(df_list)){ 
        df_list[[i]][,date_ind_list[i]] <- as.character(df_list[[i]][,date_ind_list[i]])
        lower_date[i] <- as.numeric(gsub("[-]","",df_list[[i]][1,date_ind_list[i]]))
        upper_date[i] <- as.numeric(gsub("[-]","",df_list[[i]][nrow(df_list[[i]]),date_ind_list[i]]))
    }
    l<-as.Date(as.character(min(lower_date)),"%Y%m%d")
    u<-as.Date(as.character(max(upper_date)),"%Y%m%d")
    date <- as.character(seq(l,u,by=1))
    # 데이터 프레임 생성
    if(!WN){
        temp_df <- data.frame(date,stringsAsFactors = F)
        temp_name <- c("date")
    }
    else{
        if(!require(lubridate)){
            install.packages("lubridate")
        }
        library(lubridate)
        Week_Number <- week(date)
        temp_df <- data.frame(date,Week_Number,stringsAsFactors = F)
        temp_name <- c("date","weeknum")
    }
    # 데이터 프레임에 담음
    for(i in 1:length(df_list)){
        temp <- data.frame(df_list[[i]][,date_ind_list[i]],df_list[[i]][,-date_ind_list[i]],stringsAsFactors = F) 
        temp_df <- data.frame(temp_df,date_sync(temp,date)[,-1,drop=F],stringsAsFactors = F) 
        temp_name <- c(temp_name,names(df_list[[i]])[-date_ind_list[i]])
    }
    names(temp_df) <- temp_name
    return(temp_df)
}

interval_mean <- function(df,date_ind,d_ind,interval=NULL){
    # 원하는 기간으로 구간만드는 모듈
    #
    # df: 데이터 프레임
    # date_ind: 날짜 있는 열
    # d_ind: 가공할 데이터 열들
    # interval: 기간 (기본설정: 주별로 자름)
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(is.null(interval)){
        if(!require(lubridate)){
            install.packages("lubridate")
        }
        library(lubridate)
        date_year <- year(df[,date_ind])
        date_week <- week(df[,date_ind])
        indicator <- ifelse(nchar(date_week)==1,paste0(date_year,"년-0",date_week,"주"),paste0(date_year,"년-",date_week,"주"))
        name <- c("weeknum","period",names(df[,d_ind]))
    }
    else{
        ind_length <- ceiling(nrow(df)/interval)
        indicator <- vector(length=nrow(df))
        for(i in 1:ind_length){
            if(i!=ind_length){
                for(j in 1:interval){
                    indicator[interval*(i-1)+j] <- i
                } 
            }
            else{
                for(j in 1:(length(indicator)-interval*(ind_length-1))){
                    indicator[interval*(i-1)+j] <- i
                } 
            }
        }
        name <- c("interval","period",names(df[,d_ind]))
    }
    temp_df <- data.frame(matrix(NA,nrow=length(unique(indicator)),ncol=length(d_ind)+2))
    names(temp_df) <- name
    temp_df[,1] <- unique(indicator)
    period <- NULL
    for(i in unique(indicator)){
        temp <- df[which(indicator==i),date_ind]
        period_temp <- paste(temp[1],temp[length(temp)],sep=" - ")
        period <- c(period,period_temp)
    }
    temp_df[,2] <- period
    for(i in 1:length(unique(indicator))){
        ind <- indicator==unique(indicator)[i]
        for(j in 1:length(d_ind)){
            if(!all(is.na(df[ind,d_ind[j]]))){
                temp_df[i,j+2] <- mean(df[ind,d_ind[j]],na.rm=T)   
            }
            else{
                temp_df[i,j+2] <- NA
            }
        }
    }
    return(temp_df)
}

interval_mean_y <- function(df,date_ind,y_ind,d_ind,interval=NULL){
    # 생산량은 누적으로 만듬
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(is.null(interval)){
        if(!require(lubridate)){
            install.packages("lubridate")
        }
        library(lubridate)
        date_year <- year(df[,date_ind])
        date_week <- week(df[,date_ind])
        indicator <- ifelse(nchar(date_week)==1,paste0(date_year,"년-0",date_week,"주"),paste0(date_year,"년-",date_week,"주"))
        name <- c("weeknum","period",names(df[,d_ind]))
    }
    else{
        ind_length <- ceiling(nrow(df)/interval)
        indicator <- vector(length=nrow(df))
        for(i in 1:ind_length){
            if(i!=ind_length){
                for(j in 1:interval){
                    indicator[interval*(i-1)+j] <- i
                } 
            }
            else{
                for(j in 1:(length(indicator)-interval*(ind_length-1))){
                    indicator[interval*(i-1)+j] <- i
                } 
            }
        }
        name <- c("interval","period",names(df[,d_ind]))
    }
    d_ind <- c(y_ind,d_ind)
    temp_df <- data.frame(matrix(NA,nrow=length(unique(indicator)),ncol=length(d_ind)+2))
    names(temp_df) <- name
    temp_df[,1] <- unique(indicator)
    period <- NULL
    for(i in unique(indicator)){
        temp <- df[which(indicator==i),date_ind]
        period_temp <- paste(temp[1],temp[length(temp)],sep=" - ")
        period <- c(period,period_temp)
    }
    temp_df[,2] <- period
    for(i in 1:length(unique(indicator))){
        ind <- indicator==unique(indicator)[i]
        for(j in 1:length(d_ind)){
            if(!all(is.na(df[ind,d_ind[j]]))){
                if(d_ind[j]==y_ind){
                    temp_df[i,j+2] <- sum(df[ind,d_ind[j]],na.rm=T)
                }
                else{
                    temp_df[i,j+2] <- mean(df[ind,d_ind[j]],na.rm=T) 
                }
                
            }
            else{
                temp_df[i,j+2] <- NA
            }
        }
    }
    return(temp_df)
}


opt_x<-function(df,y_ind,x_ind){
    # 최적시차
    # df:데이터 프레임
    # y_ind: 종속변수 있는 열
    # x_ind : 독립변수 있는 열들
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    storage<-as.data.frame(matrix(NA,ncol=length(x_ind)+1))
    names(storage)<-c("변수명",names(df)[x_ind])
    storage[1:2,1]<-c("시차","R.Square")
    for(i in 1:length(x_ind)){
        temp<-vector(length=11)
        for(j in 0:10){
            parts<-c(rep(NA,j),df[1:(nrow(df)-j),x_ind[i]])
            temp_df<-data.frame(x=parts,y=df[,y_ind])
            fit<-lm(y~x,data=temp_df)
            temp[j+1]<-summary(fit)[["r.squared"]]
        }
        storage[1:2,i+1] <- c(which.max(temp)-1,max(temp))
    }  
    return(storage)
}

split_fixed <- function(df,d_ind,s_ind=NULL){
    # 쪼개져있는 데이터 수정 모듈
    #
    # df:데이터 프레임
    # d_ind: 가공할 데이터 열
    # s_ind: 특수한 열들
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(!is.null(s_ind) && is.character(s_ind)){
        s_ind <- index_to_num(s_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(!require(stringr)){
        install.packages("stringr")
    }
    library(stringr)
    options(warn=-1)
    temp_df <- df
    total_ind <- sort(unique(c(d_ind,s_ind)))
    for(j in total_ind){
        t_i <- which(nchar(gsub("[[:blank:]]","",temp_df[,j]))!=nchar(temp_df[,j]))
        if(length(t_i)!=0){
            for(i in t_i){
                temp_front <- substr(temp_df[i,j],1,str_locate(temp_df[i,j]," ")[1]-1)
                temp_rear <- substr(temp_df[i,j],str_locate(temp_df[i,j]," ")[1],nchar(temp_df[i,j]))
                temp_rear <- trimws(temp_rear)
                temp_df[i,j] <- temp_front
                if(!is.na(temp_df[i,j+1])){
                    if(j %in% s_ind){
                        if(nchar(gsub("[[:blank:]]","",temp_df[i,j+1]))!=nchar(temp_df[i,j+1])){ 
                            temp_df[i,j+1] <- paste0(temp_rear,temp_df[i,j+1])
                        }
                        else{
                            temp_df[i,j+1] <- paste0(temp_rear,temp_df[i,j+1])
                        }
                    }
                    else{
                        temp_df[i,j+1]<-paste0(temp_rear,temp_df[i,j+1]) 
                    }
                }
                else{
                    temp_df[i,j+1] <- temp_rear
                }
            }
        }
        temp_col <- temp_df[iconv(temp_df[,j])!="UTF-8",j]
        num_temp <- as.numeric(temp_col)
        if(!all(is.na(num_temp))){
            temp_df[,j] <- as.numeric(temp_df[,j])
        }
    }
    options(warn=0)
    return(temp_df)
}


split_detail <- function(df,d_ind){
    # 쪼개져 있는 데이터 수정 모듈
    #
    # 특이한 유형의 경우 이상점을 통해 판단해서 수정함
    # df: 데이터 프레임
    # d_ind: 가공할 데이터 열
    if(!require(stringr)){
        install.packages("stringr")
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    library(stringr)
    options(warn=-1)
    temp_df <- df
    d_ind <- sort(d_ind)
    for(j in d_ind){
        t_i <- which(nchar(gsub("[[:blank:]]","",temp_df[,j]))!=nchar(temp_df[,j]))
        if(length(t_i)!=0){
            num_rear <- as.numeric(temp_df[,j+1])
            back_det <- !all(is.na(num_rear))
            if(back_det){
                lower <- as.numeric(summary(num_rear)[2]) - 1.5*IQR(num_rear,na.rm=T)
                upper <- as.numeric(summary(num_rear)[5]) + 1.5*IQR(num_rear,na.rm=T)
            }
            for(i in t_i){
                temp_front <- substr(temp_df[i,j],1,str_locate(temp_df[i,j]," ")[1]-1)
                temp_rear <- substr(temp_df[i,j],str_locate(temp_df[i,j]," ")[1],nchar(temp_df[i,j]))
                temp_rear <- trimws(temp_rear)
                temp_df[i,j] <- temp_front
                if(!is.na(temp_df[i,j+1])){
                    if(back_det){
                        if(nchar(gsub("[[:blank:]]","",temp_df[i,j+1]))!=nchar(temp_df[i,j+1])){
                            temp_front_back <- substr(temp_df[i,j+1],1,str_locate(temp_df[i,j+1]," ")[1]-1)
                            num_det <- as.numeric(paste0(temp_rear,temp_front_back))
                            det <- (lower<=as.numeric(num_det)) & (upper>=as.numeric(num_det))
                            if(det){
                                temp_df[i,j+1] <- paste0(temp_rear,temp_df[i,j+1])
                            }
                            else{
                                temp_back <- substr(temp_df[i,j+1],2,nchar(temp_df[i,j+1]))
                                temp_df[i,j+1] <- paste0(temp_rear,temp_back)
                            }
                        }
                        else{
                            num_det <- as.numeric(paste0(temp_rear,df[i,j+1]))
                            det <- (lower<=as.numeric(num_det)) & (upper>=as.numeric(num_det))
                            if(det){
                                temp_df[i,j+1] <- num_det
                            }
                            else{
                                temp_df[i,j+1] <- as.numeric(temp_rear)+as.numeric(temp_df[i,j+1])
                            }
                        }
                    }
                    else{
                        temp_df[i,j+1]<-paste0(temp_rear,temp_df[i,j+1])
                    }
                }
                else{
                    temp_df[i,j+1] <- temp_rear
                }
                
            }
        }
        temp_col <- temp_df[iconv(temp_df[,j])!="UTF-8",j]
        num_temp <- as.numeric(temp_col)
        if(!all(is.na(num_temp))){
            temp_df[,j] <- as.numeric(temp_df[,j])
        }
    }
    options(warn=0)
    return(temp_df)
}






data_fixed <- function(df,about,method="linear"){
    # 이상치,NA 변경 모듈
    # df: 데이터 프레임
    # about: NA를 처리할 것인지 outlier를 처리할 것인지
    # method -  NA: NA로 처리함
    #           linear: 이상치를 선형으로 연결함
    #           spline: 스플라인 보간법을 전체적으로 사용함
    #           spline_middle: 스플라인 보간법을 중간의 값에만 사용함
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    temp_df <- df
    for(j in seq(ncol(temp_df))){
        if(is.numeric(temp_df[,j])){
            if(about %in% c("결측치","결측","NA")){
                det <- which(is.na(temp_df[,j]))
            }
            else if(about %in% c("out","outlier","이상점","이상")){
                lower <- as.numeric(summray(temp_df[,j])[2]) - 1.5*IQR(temp_df[,j],na.rm=T)
                upper <- as.numeric(summary(temp_df[,j])[5]) + 1.5*IQR(temp_df[,j],na.rm=T)
                det <- which((temp_df[,j] <= lower) | (temp_df[,j] >= upper))
            }
            if(length(det)!=0){
                if(method=="NA"){
                    temp_df[det,j] <- NA 
                }
                else if(method %in% c("linear","선형")){
                    temp_df[,j] <- linear_method(temp_df[,j],det)
                }
                else if(method %in% c("spline","스플라인","스프라인")){
                    temp_df[,j] <- spline_method(temp_df[,j],det)
                }
                else if(method %in% c("spline_middle")){
                    temp_df[,j] <- spline_middle_method(temp_df[,j],det)
                }
            }
            else{
                next
            }
        }
        else{
            next
        }
    }
    return(temp_df)
}

linear_method <- function(df_part,det){
    # 선형 보간법
    a_ind <- det[1]
    for(i in seq(length(det))){
        if(i==length(det)){
            b_ind <- det[i]
            if(b_ind==length(df_part)){ 
                if(which(a_ind==det)>=2){  
                    temp <- det[which(a_ind==det)-1] 
                    if(a_ind - temp<=2){ 
                        df_part[a_ind:b_ind] <- df_part[a_ind-1]
                    }
                    else{
                        temp_x1 <- a_ind-2
                        temp_x2 <- a_ind-1
                        df_part[a_ind:b_ind] <- (df_part[temp_x2]-df_part[temp_x1])*((a_ind:b_ind)-temp_x2)+df_part[temp_x2]
                    }
                }
                else{
                    if(a_ind-2>=1){
                        temp_x1 <- a_ind-2
                        temp_x2 <- a_ind-1
                        df_part[a_ind:b_ind] <- (df_part[temp_x2]-df_part[temp_x1])*((a_ind:b_ind)-temp_x2)+df_part[temp_x2] 
                    }
                    else{
                        df_part[a_ind:b_ind] <- df_part[a_ind-1]
                    }
                }
            }
            else if(a_ind==1){ 
                if(b_ind+2<=length(df_part)){
                    temp_x1 <- b_ind+1
                    temp_x2 <- b_ind+2
                    df_part[a_ind:b_ind] <- (df_part[temp_x2]-df_part[temp_x1])*((a_ind:b_ind)-temp_x2)+df_part[temp_x2]
                }
                else{
                    df_part[a_ind:b_ind] <- df_part[b_ind+1]
                }
            }
            else{ 
                df_part[a_ind:b_ind] <- seq(df_part[a_ind-1],df_part[b_ind+1],length.out=b_ind-a_ind+3)[2:(b_ind-a_ind+2)]
            }
        }
        else{
            if(det[i+1]-det[i]!=1){
                b_ind <- det[i]
                if(a_ind==1){
                    if(det[i+1]-det[i]<=2){
                        df_part[a_ind:b_ind] <- df_part[b_ind+1]
                    }
                    else{
                        temp_x1 <- b_ind+1
                        temp_x2 <- b_ind+2
                        df_part[a_ind:b_ind] <- (df_part[temp_x2]-df_part[temp_x1])*((a_ind:b_ind)-temp_x2)+df_part[temp_x2]
                    }
                }
                else{
                    df_part[a_ind:b_ind]  <- seq(df_part[a_ind-1],df_part[b_ind+1],length.out=b_ind-a_ind+3)[2:(b_ind-a_ind+2)]
                }
                a_ind <- det[i+1]
            }
        }
        
    }
    return(df_part)
}

spline_method <- function(df_part,det){
    # 스플라인 보간법
    x <- setdiff(1:length(df_part),det)
    y <- df_part[x]
    yout <- spline(x=x,y=y,xout=det,method="fmm")
    df_part[det] <- yout$y
    return(df_part)
}

spline_middle_method <- function(df_part,det){
    # 스플라인 중간 보간법
    bool <- c(1,length(df_part)) %in% det
    if(any(bool)){
        det_temp <- NULL
        if(bool[1]){
            if(any(diff(det)!=1)){
                det_temp <- c(det_temp,1:(which(diff(det)!=1)[1]))
            }
            else{
                det_temp <- det
            }
        }
        if(bool[2]){
            if(any(diff(det)!=1)){
                det_temp <- c(det_temp,(det[which(diff(det)!=1)[sum(diff(det)!=1)]+1]:length(df_part)))
            }
            else{
                det_temp <- det
            }
        }
        df_part <- linear_method(df_part,det_temp)
        det <- setdiff(det,det_temp)
    }
    if(length(det)!=0){
        x <- setdiff(1:length(df_part),det)
        y <- df_part[x]
        yout <- spline(x=x,y=y,xout=det,method="fmm")
        df_part[det] <- yout$y
        return(df_part)
    }
    else{
        return(df_part)
    }
}



outlier_detecter <- function(df,date_ind,d_ind,degree=3){
    # 이상점 탐지
    #
    # 리턴: 리스트안에 전체플롯,부분플롯,바운더리,이상점 데이터들
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(!require(ggplot2)){
        install.packages("ggplot2")
    }
    if(!require(lubridate)){
        install.packages("lubridate")
    }
    library(ggplot2)
    library(lubridate)
    date <- as.POSIXct(df[,date_ind])
    df[,date_ind] <- date
    name <- names(df)
    df$year <- year(date)
    df$month <- month(date)
    df$day <- day(date)
    info_list <- list()
    options(warn=-1)
    if(is.character(df[,d_ind])){
        df[,d_ind] <- as.numeric(df[,d_ind])
        if(all(is.na(df[,d_ind]))){
            return(NULL)
        }
    }
    if(!all(is.na(df[,d_ind]))){
        if(min(df[,d_ind],na.rm=T)!=max(df[,d_ind],na.rm=T)){
            local({
                d_ind <- d_ind
                boundary <- c(as.numeric(summary(df[,d_ind])[2]) - degree*IQR(df[,d_ind],na.rm=T),
                              as.numeric(summary(df[,d_ind])[5]) + degree*IQR(df[,d_ind],na.rm=T))
                names(boundary) <- c("lower bound","upper bound")
                # 아웃라이어 디텍팅
                out_temp <- data.frame(date,loc=1:nrow(df),data=NA,colname=name[d_ind],stringsAsFactors = F)
                out_upper <- data.frame(date,data=NA,total=NA,stringsAsFactors = F)
                out_lower <- data.frame(date,data=NA,total=NA,stringsAsFactors = F)
                for(i in 1:nrow(df)){
                    if(!is.na(df[i,d_ind])){
                        if(df[i,d_ind]<=boundary[1] || df[i,d_ind]>=boundary[2]) {
                            out_temp[i,3] <- df[i,d_ind]
                            if(df[i,d_ind]<=boundary[1]){
                                out_lower[i,2] <- df[i,d_ind]
                                out_lower[i,3] <- with(out_lower,paste(date[i],data[i],sep=" : "))
                            }
                            if(df[i,d_ind]>=boundary[2]){
                                out_upper[i,2] <- df[i,d_ind]
                                out_upper[i,3] <- with(out_upper,paste(date[i],data[i],sep=" : "))
                            }
                        }
                    }
                }
                # 플로팅 부분
                all_plot <- ggplot(df,aes(x=get(name[date_ind]),y=get(name[d_ind])))+geom_line(na.rm=T) # 전체 적인 개괄
                all_plot <- all_plot + geom_hline(aes(yintercept=boundary[1]),linetype="dotted")+geom_hline(aes(yintercept=boundary[2]),linetype="dotted") # 바운더리
                all_plot <- all_plot + labs(x="날짜",y=name[d_ind]) 
                all_plot <- all_plot + ggtitle(paste0("overall ploting about ",name[d_ind]))
                if(!all(is.na(out_temp[,3]))){
                    all_plot <- all_plot + geom_point(data=out_temp,aes(x=date,y=data),size = 1,colour="red",na.rm=T) # 이상점
                }
                if(!all(is.na(out_lower[,3]))){
                    all_plot <- all_plot + geom_text(data=out_lower,aes(x=date,y=data,label=total),size=3,vjust=0.3,hjust=-0.1,colour="black",
                                                     check_overlap = T,angle=315,na.rm=T)
                }
                if(!all(is.na(out_upper[,3]))){
                    all_plot <- all_plot + geom_text(data=out_upper,aes(x=date,y=data,label=total),size=3,vjust=0,hjust=-0.1,colour="black",
                                                     check_overlap = T,angle=45,na.rm=T)
                }
                year <- df$year[1]
                month <- df$month[1]
                month_plot <- list()
                while(year!=df$year[nrow(df)] || month!=df$month[nrow(df)]+1){
                    loc <- year==df$year & month==df$month
                    df_temp <- df[loc,]
                    out_temp_t <- out_temp[loc,]
                    out_upper_t <- out_upper[loc,]
                    out_lower_t <- out_lower[loc,]
                    local_plot <- ggplot(df_temp,aes(x=get(name[date_ind]),y=get(name[d_ind])))+geom_line(na.rm=T) # 전체 적인 개괄
                    local_plot <- local_plot + geom_hline(aes(yintercept=boundary[1]),linetype="dotted") +
                        geom_hline(aes(yintercept=boundary[2]),linetype="dotted") # 바운더리
                    local_plot <- local_plot + labs(x="일",y=name[d_ind]) 
                    local_plot <- local_plot + ggtitle(paste0("local ploting ",year,"년 ",month,"월 about ",name[d_ind]))
                    if(!all(is.na(out_temp_t[,3]))){
                        local_plot <- local_plot + geom_point(data=out_temp_t,aes(x=date,y=data),size = 1,colour="red",na.rm=T) # 이상점
                    }
                    if(!all(is.na(out_lower_t[,3]))){
                        local_plot <- local_plot + geom_text(data=out_lower_t,aes(x=date,y=data,label=total),size=3,vjust=0.3,hjust=-0.1,colour="black",
                                                             check_overlap = T,angle=315,na.rm=T)
                    }
                    if(!all(is.na(out_upper_t[,3]))){
                        local_plot <- local_plot + geom_text(data=out_upper_t,aes(x=date,y=data,label=total),size=3,vjust=0,hjust=-0.1,colour="black",
                                                             check_overlap = T,angle=45,na.rm=T)
                    }
                    ym <- ifelse(nchar(month)==1,paste0(year,"-0",month),paste0(year,"-",month))
                    month_plot[[ym]] <- local_plot
                    if(month+1>12){
                        month <- 1
                        year <- year+1
                    }
                    else{
                        month <- month+1
                    }
                }
                # 담는 부분 
                out_data <- na.omit(out_temp)
                if(!all(is.na(out_temp[,3]))){
                    info_list[["overall_plot"]] <<- all_plot
                    info_list[["month_plot"]] <<- month_plot
                    info_list[["boundary"]] <<- boundary
                    info_list[["out_data"]] <<- out_data
                }
                else{
                    info_list[["overall_plot"]] <<- all_plot
                    info_list[["month_plot"]] <<- month_plot
                    info_list[["boundary"]] <<- boundary
                } 
            })
        }
        else{ # 같은 숫자만 나올 때
            local({
                all_plot <- ggplot(df,aes(x=get(name[date_ind]),y=get(name[d_ind])))+geom_line(na.rm=T) 
                all_plot <- all_plot + labs(x="날짜",y=name[d_ind]) 
                all_plot <- all_plot + ggtitle(paste0("overall ploting about ",name[d_ind]))
                info_list[["overall_plot"]] <<- all_plot
            })
        }
    }
    else{
        return(info.list)
    }
    options(warn=0)
    class(info_list) <- "detecter"
    return(info_list)
}

print.detecter <- function(object){
    # 이상점 데이터 프레임 출력  
    if(length(object)!=0){
        if(length(object)==4){
            return(object[[4]])
        }
        else{
            print("No outlier detected")
            return(NULL)
        }
    }
    else{
        print("No object detected")
    }
}



plot.detecter <- function(object,time=NULL){
    # 그래프를 보여줌    
    if(length(object)!=0){
        if(is.null(time)){
            print(object[[1]])
        }
        else{
            if(length(grep("-",time))==0){
                if(nchar(time)<=2){
                    time_trans <- time
                }
                else if(nchar(time)==3){
                    part_1 <- susbtr(time,1,2)
                    part_2 <- substr(time,3,3)
                    time_trans <- paste0("20",part_1,"-0",part_2)
                }
                else if(nchar(time)==4){
                    part_1 <- substr(time,1,2)
                    part_2 <- substr(time,3,4)
                    time_trans <- paste0("20",part_1,"-",part_2)
                }
                else if(nchar(time)==5){
                    part_1 <- substr(time,1,4)
                    part_2 <- substr(time,5,6)
                    time_trans <- paste0(part_1,"-0",part_2)
                }
                else if(nchar(time)==6){
                    part_1 <- substr(time,1,4)
                    part_2 <- substr(time,5,6)
                    time_trans <- paste0(part_1,"-",part_2)
                    
                }
            }
            else{
                if(nchar(time)==7){
                    time_trans <- time
                }
                else{
                    part_1 <- substr(time,1,4)
                    part_2 <- substr(time,6,6)
                    time_trans <- paste0(part_1,"-",part_2)
                }
            }
            tryCatch(
                print(object[[2]][[time_trans]]),
                error=function(e) print("해당 날짜가 없거나 서식이 잘못되었습니다.")
            )
        }
    }
    else{
        print("no object detected")
    }
}

summary.detecter <- function(object){
    # 이상점 구간과 이상점 약간을 보여줌
    if(length(object) > 2){
        print(object[[3]])
        if(length(object)==4){
            cat("이상점의 개수는",nrow(object[[4]]),"개입니다. \n")
            head(object[[4]])
        }
    }
    else{
        print("No object detected")
    }
}

generating_plyr_temp <- function(df,date_ind,d_ind,kind,div_DN=NULL,tbase=15){
    # 특정 열에 대해 함수 적용시키는 모듈
    #
    # df: 데이터 프레임  
    # d_ind: 가공할 열들 번호  
    # date_ind: 날짜 있는 열번호    
    # kind: 최소,최대,평균,누적,DIF,GDD 중에 선택하고 주간,야간을 추가 가능
    # div_DN: 밤낮 구분 데이터 (ND_div 모듈의 리턴 값)
    # 초기 설정
    if(is.character(date_ind)){
        date_ind <- index_to_num(date_ind)
    }
    if(is.character(d_ind)){
        d_ind <- index_to_num(d_ind)
    }
    if(any(class(df) %in% c("data.table","tbl"))){
        df <- as.data.frame(df)
    }
    if(!require(plyr)){
        install.packages("plyr")
    }
    library(plyr)
    # 추가할 함수 부분
    DIF <- function(data,na.rm){
        return(max(data,na.rm=na.rm)-min(data,na.rm=na.rm))
    }
    GDD <- function(data,na.rm){
        temp <- (max(data,na.rm=na.rm)+min(data,na.rm=na.rm))/2 - tbase
        return(ifelse(temp>=0,temp,0))
    }
    # kind의 함수 판단
    functions_list <- list(최소=min,최대=max,평균=mean,누적=sum,DIF=DIF,GDD=GDD)
    kind_div <- rep(0,length(kind))
    for(i in seq(kind)){
        for(j in seq(names(functions_list))){
            temp<-grep(names(functions_list)[j],kind[i])
            if(length(temp)!=0){
                kind_div[i] <- j
                break
            } 
        }
    }
    # 데이터 프레임 변환
    df[,date_ind[1]] <- substr(df[,1],1,10)
    # kind의 주간,야간 판단
    kind_ND <- rep("전체",length(kind))
    if(!is.null(div_DN)){
        kind_ND[grep("야간",kind)] <- "야간"
        kind_ND[grep("주간",kind)] <- "주간"
        df_d <- df[grep("주간",div_DN),]
        which_n <- grep("야간",div_DN)
        df_n <- df[which_n,]
        df_n[,date_ind[1]] <- substr(div_DN[which_n],4,13)
    }
    # 데이트 생성
    date <- as.Date(unique(df[,date_ind[1]]))
    if(any(kind_ND=="야간")){ # 함수 종류중에 야간이 있는 경우
        date <- seq(date[1]-1,date[length(date)],by=1)
    }
    # 데이터 프레임 생성
    temp_df <- data.frame(matrix(NA,nrow=length(date),ncol=length(kind)*length(d_ind)))
    ind_name <- names(df)[d_ind]
    temp_name <- vector(length=length(kind)*length(d_ind))
    for(i in seq(ind_name)){
        for(j in seq(kind)){
            temp_name[length(kind)*(i-1)+j] <- paste0(kind[j],"_",ind_name[i])
        }
    }
    names(temp_df) <- temp_name
    # 데이터 프레임에 데이터 담는 과정
    for(i in 1:length(d_ind)){
        for(kind_num in seq(kind)){
            if(kind_ND[kind_num]=="주간"){ 
                temp_col <- 
                    ddply(df_d,date_ind,function(part){functions_list[[kind_div[kind_num]]](part[,d_ind[i]],na.rm=T)}) 
            }
            else if(kind_ND[kind_num]=="야간"){
                temp_col <- 
                    ddply(df_n,date_ind,function(part){functions_list[[kind_div[kind_num]]](part[,d_ind[i]],na.rm=T)})
            }
            else if(kind_ND[kind_num]=="전체"){
                temp_col <- 
                    ddply(df,date_ind,function(part){functions_list[[kind_div[kind_num]]](part[,d_ind[i]],na.rm=T)})
            } 
            if(any(is.nan(temp_col[,length(date_ind)+1]))){
                temp_col[is.nan(temp_col[,length(date_ind)+1]),length(date_ind)+1] <- NA
            }
            if(any(is.infinite(temp_col[,length(date_ind)+1]))){
                temp_col[is.infinite(temp_col[,length(date_ind)+1]),length(date_ind)+1] <- NA
            }
            temp_df[,length(kind)*(i-1)+kind_num] <- as.numeric(date_sync(temp_col,date)[,2])
        }
    }
    temp_df <- data.frame(date,temp_df)
    temp_df[,1] <- as.character(temp_df[,1])
    return(temp_df)
}


