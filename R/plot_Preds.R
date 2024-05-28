#' Plot diet of predators
#'
#' @param dietsAll
#' @param FG_to_plot
#' @param threshold
#' @param yearsselected
#' @param grp_list
#' @param timeperiod
#' @param startyear
#'
#' @return dietplot
#' @export
#'
#' @examples
plot_Preds<-function(dietsAll, FG_to_plot, threshold, yearsselected, grp_list, timeperiod, startyear){

  grps <- grp_list$grps
  pred_groups <- grp_list$pred_groups

  if(yearsselected=="all"){theseyears<-startyear+unique(dietsAll$Time)/timeperiod}

  FG_code<-pred_groups$Code[pred_groups$Name==FG_to_plot]

  #print(FG_code)
  subDiet <- dietsAll %>%
    dplyr::select(Time,Predator,all_of(FG_code))

  #print(nrow(subDiet))

  if(!sum(subDiet[,6:ncol(subDiet)])%in%c(0,NA)){

    selec_prey<-names(which(colSums(subDiet[6:ncol(subDiet)])>threshold))

    colourCount = length(selec_prey)
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))

    subDiet %>%
      reshape2::melt(id.vars = c("Time", "Predator", "Cohort"), measure.vars=selec_prey) %>%
      dplyr::filter(Time%in%as.character(c((theseyears-startyear)*timeperiod))) %>%
      ggplot2::ggplot(ggplot2::aes(x=startyear+(Time/timeperiod),y=value*100,fill=variable, color=variable))+
      ggplot2::geom_area(stat="identity")+
      ggplot2::scale_fill_manual(values=getPalette(colourCount), name = "Prey", labels = grps$Code[grps$Code%in%selec_prey])+
      scale_colour_manual(values=getPalette(colourCount),name = "Prey", labels = grps$Code[grps$Code%in%selec_prey])+
      ggplot2::facet_wrap(~paste("Age",Cohort))+
      ggplot2::labs(title= paste("Diet of ",grps$`Long Name`[grps$Name==FG_to_plot]),
                    y="Diet proportions (%)", x = "Years",fill = "Prey",
                    color="Prey")+
      theme(legend.position='bottom')-> dietplot

    #return(dietplot)
  }else{
    dietplot <- ggplot2::ggplot() + ggplot2::annotate(geom="text", x = 4, y = 25, label = "plot could not be produced - check the diet output files") + theme_void()
  }
  return(dietplot)

}
