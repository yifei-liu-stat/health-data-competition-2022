library(networkD3)
library(RColorBrewer)
theme_map <- function() {
        theme_minimal() +
                theme(
                        text = element_text(family = "mono", color = "#22211d"),
                        axis.line = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
                        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                        panel.grid.minor = element_blank(),
                        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                        legend.background = element_rect(fill = "#f5f5f2", color = NA),
                        panel.border = element_blank()
                )
}
#########

hospital_ratings <- read_csv('data/hospital_features.csv')
hospital_dat <- read_csv('data/hospital_characteristics.csv')
dat <- read_rds("data/normalized_sim.RDS")
hospital_names <- colnames(dat)
hospital_ratings %>% select(contains("rating")) %>%
        mutate(hnames = hospital_names)-> hospital_ratings
rownames(dat) = hospital_names


statelist <- unique(str_sub(hospital_names, start = -2))
colnames(hospital_dat)[ncol(hospital_dat)] <- 'Patient Experience Rating'
hospital_ratings$state <- str_sub(hospital_ratings$hnames, start = -2)
hospital_ratings$zipcode <- str_sub(hospital_ratings$hnames, start = -8, end = -4)
hospital_ratings$name <- str_sub(hospital_ratings$hnames, end = -10)

# Process geographic data
# US Maps
hospital_geo <- read_csv('data/geodata.csv')
hospital_geo %>% 
        drop_na() %>%
        left_join(hospital_dat, 
                  by = c("street" = "Address", "state" = "State", "city" = "City")) -> hospital_geo

hospital_dat$statename <- tolower(state.name[match(hospital_dat$State, state.abb)])
hospital_num <- hospital_dat %>%
        group_by(statename) %>%
        count()

#redcols <- brewer.pal(5, 'Reds')
# blues <- brewer.pal(9, 'Blues')
# blues<- colorRampPalette(blues)
us_state <- map_data("state")
us_county <- map_data("county")

us_state %>% left_join(hospital_num, by=c("region" = "statename")) -> us_state

minlat <- min(us_state$lat)
maxlat <- max(us_state$lat)
minlont<- min(us_state$long)
maxlont <- max(us_state$long)

hospital_geo <- hospital_geo %>%
        filter(long < maxlont & long > minlont & lat > minlat & lat < maxlat) %>%
        drop_na()
hospital_dat <- hospital_dat %>% select(-statename)
hospital_geo <- hospital_geo %>%
        left_join(hospital_ratings %>% select(-H_STAR_RATING,-`Hospital overall rating`), 
                  by = c("Facility Name" = "name", 
                         "ZIP Code" = "zipcode",
                         "state" = "state")) %>% select(!contains("score"))

# Define server logic 
shinyServer(function(input, output) {
        thematic::thematic_shiny()
        ranges <- reactiveValues(x = NULL, y = NULL)
        
        output$geomaps = renderPlot(res= 172, {
                ggplot() + 
                        geom_polygon(data=us_state, 
                                     aes(x=long, 
                                         y=lat, 
                                         fill = n,
                                         group=group)) +
                        geom_path(data = us_state, aes(x = long, 
                                                       y = lat, 
                                                       group = group), 
                                  color = "white", size = 0.1) + 
                        geom_path(data = us_county, aes(x = long, 
                                                        y = lat, 
                                                        group = group), 
                                  color = "gray", size = 0.05) + 
                        geom_point(data = hospital_geo, aes( x = long,
                                                             y = lat,
                                                             group = state,
                                                             color = as.factor(`Patient Experience Rating`)),
                                   size = 0.5, alpha = 0.6) + 
                        scale_color_brewer(palette = "RdYlGn", drop = FALSE) +
                        guides(colour = guide_legend(title = "Experience Rating"))+
                        coord_equal() +
                        theme_map() +
                        labs(x = NULL, 
                             y = NULL, 
                             caption = "United States Hospital Map") +
                        scale_fill_continuous(name = "# of Hospitals") +
                        theme(legend.key.size = unit(0.2, "cm"),
                              legend.title = element_text(size = 5),
                              legend.text = element_text(size = 5,),
                              plot.caption = element_text(size = 8)) +
                        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        })
        
        
        observeEvent(input$map_brush,{
        output$splot <- renderPlot({
                rawdat <- brushedPoints(hospital_geo, input$map_brush)
                if(nrow(rawdat) > 5){
                        rawdat %>% slice(1:5) -> rawdat
                }
                plotdat <- rawdat %>% as_tibble() %>%
                        drop_na() %>%
                        select(`Facility ID`,`Facility Name`, contains("rating"), - `Hospital overall rating`, -`Patient Experience Rating`) %>%
                        gather(key = "rating", value = "value", -`Facility Name`, -`Facility ID`) %>%
                        mutate(rating = factor(rating, levels = c("H_COMP_1_STAR_RATING",
                                                                  "H_COMP_2_STAR_RATING",
                                                                  "H_COMP_3_STAR_RATING",
                                                                  "H_COMP_5_STAR_RATING",
                                                                  "H_COMP_6_STAR_RATING",
                                                                  "H_COMP_7_STAR_RATING",
                                                                  "H_CLEAN_STAR_RATING",
                                                                  "H_QUIET_STAR_RATING",
                                                                  "H_RECMND_STAR_RATING",
                                                                  "H_HSP_RATING_STAR_RATING"),
                                               labels = c("Nurse Communication",
                                                          "Doctor Communication",
                                                          "Staff Responsiveness",
                                                          "Medicine Information",
                                                          "Discharge Information",
                                                          "Care Transition",
                                                          "Cleanliness",
                                                          "Quietness",
                                                          "Recommend Hospital",
                                                          "Overall Patient Rating")))
                
                ggplot(data = plotdat) + 
                geom_bar(aes(x = `Facility ID`, y = value, fill = rating), stat = "identity", position = position_dodge2()) + 
                scale_fill_brewer(palette = "Paired", drop = FALSE) +
                guides(fill = guide_legend(title = "Experience Ratings")) +
                        theme_minimal() +
                        theme(
                                text = element_text(family = "mono", color = "#22211d"),
                                axis.line = element_blank(),
                                axis.text.x = element_text(size = 10),
                                axis.text.y = element_text(size = 10),
                                axis.ticks = element_blank(),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                                panel.grid.minor = element_blank(),
                                plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                                panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                                legend.background = element_rect(fill = "#f5f5f2", color = NA),
                                panel.border = element_blank()
                        )
                
        })
          output$stable <- DT::renderDataTable({
                  rawdat <- brushedPoints(hospital_geo, input$map_brush) %>%
                          select(`Facility ID`,`Facility Name`, `Hospital overall rating`)
                  DT::datatable(rawdat, rownames = FALSE, options = list(pageLength = 5, dom = 't',ordering = F))
          })                    
        }) 
        
        
        # When a double-click happens, check if there's a brush on the plot.
        # If so, zoom to the brush bounds; if not, reset the zoom.
        observeEvent(input$map_dblclick, {
                brush <- input$map_brush
                if (!is.null(brush)) {
                        ranges$x <- c(brush$xmin, brush$xmax)
                        ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        ranges$x <- NULL
                        ranges$y <- NULL
                }
        })
        
        
        
        output$outtable = DT::renderDataTable({
            hospital_dat[hospital_dat$State == input$cstate,] %>%
            arrange(`Hospital overall rating`, `Patient Experience Rating`)
        }, selection = "single")
        
        observeEvent(input$outtable_rows_selected, {
                output$networkd3 = renderForceNetwork({
                        row_index = input$outtable_row_last_clicked
                        hospital_dat = hospital_dat[hospital_dat$State == input$cstate,] %>%
                                arrange(`Hospital overall rating`, `Patient Experience Rating`) 
                        hospital_name = paste(hospital_dat[row_index, c("Facility Name", "ZIP Code","State")], collapse = '-')
                        similarities = dat[,hospital_name, drop = FALSE] %>%
                                `colnames<-`("similarity") %>%
                                rownames_to_column(var = "target") %>%
                                mutate(source = hospital_name) %>%
                                drop_na() %>%
                                filter(str_detect(target, paste0("-",input$fstate,"$"))) %>%
                                arrange(desc(similarity)) %>%
                                #mutate(across(where(is.character), ~str_sub(., start = 1, end = -10))) %>%
                                slice(1:10)
                        
                        Nodes <- data.frame(names  = c(unique(similarities$source), similarities$target),
                                            group = c(1, rep(2,nrow(similarities))))
                        similarities$source <- match(similarities$source, Nodes$names)-1
                        similarities$target <- match(similarities$target, Nodes$names)-1
                        #similarities$similarity <- similarities$similarity * 10
                        
                        ColourScale <- 'd3.scaleOrdinal()
                        .domain(["lions", "tigers"])
                        .range(["#FF6900", "#694489"]);'
                        forceNetwork(Links = similarities, Nodes = Nodes, Source = "source",
                                     Target = "target", Value = "similarity", NodeID = "names",
                                     Group = "group", opacity = 0.85, zoom = F,
                                     width = 1000, height = 1000,
                                     fontSize = 10,
                                     linkDistance = JS("function(d){return d.value * 1000}"),
                                     bounded = T,
                                     opacityNoHover = T,
                                     colourScale = JS(ColourScale)
                        )
                        # simpleNetwork(similarities,Source = "source", Target = "target", 
                        #               width = 1000, height = 500,
                        #               linkDistance = 100,
                        #               linkColour = "#FEC44F",
                        #               nodeColour = "#D95F0E", opacity = 0.8,
                        #               charge = -500, fontSize = 10, zoom = FALSE)
                        
                        
                }) 
                
        })

   })