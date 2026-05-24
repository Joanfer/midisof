library(tuneR)
library(gm)

figures<-480*c(0.125, 0.25, 1/3, 0.5, 0.75, 0.875, 1) #Es podria modificar. No agafa 0.65 com a negra de treset

(#Funcions: duradaQuantitzada, mtof, processa.midi
  
  
  duradaQuantitzada<-function(duradaCodificada) {
    sil_aux<-0; 
    err <- abs(-0.5+( ((duradaCodificada)/figures)%%1 )) # err<-abs(duradaCodificada-figures); 
    
    i.fig<-max(which(err==max(err)));#   i.fig<-which(err==min(err));
    multiple <- round(duradaCodificada/figures[i.fig])
    # print( duradaCodificada<= multiple*figures[i.fig] & duradaCodificada*1.06>=multiple*figures[i.fig] )
    #print(duradaCodificada>= multiple*figures[i.fig] & duradaCodificada/1.06<=multiple*figures[i.fig]) 
    if ( (duradaCodificada<= multiple*figures[i.fig] & duradaCodificada*1.06>=multiple*figures[i.fig]) | (duradaCodificada>= multiple*figures[i.fig] & duradaCodificada/1.06<=multiple*figures[i.fig]) ) {
      
      sil_aux<-figures[i.fig]*multiple
      #   print(sil_aux)
    } else {
      
      sil_aux<-sil_aux+
        figures[i.fig] * floor( 1.06* duradaCodificada /figures[i.fig]);
      figura_previa<-sil_aux;
      #  print(c(duradaCodificada, err, sil_aux, abs(( duradaCodificada - sil_aux - figures) / figures)) )
      
      condicio1<-any(0.25>abs( (duradaCodificada-sil_aux     -     figures) / figures ))
      while((duradaCodificada-sil_aux     ) / sil_aux >0.0825 &  #treset respecte de redona:0.0825
            (duradaCodificada-sil_aux     ) >=0.8*figures[1] &  #com a mÃ­nim fusa
            ( (duradaCodificada-sil_aux   ) >=0.8*0.5*figura_previa | #pot ser puntet
              (duradaCodificada-sil_aux   ) <=1.2*0.5*figura_previa ) ) { 
        
        if (length ( which(1.25>abs(( duradaCodificada - sil_aux - figures) / figures) ) )==0 ) { #la condicio te valors per a q passe tot - valorar llevar if-
        } else {
          
          # i.fig<- which(abs(( duradaCodificada - sil_aux - figures) / figures) == min(abs(( duradaCodificada - sil_aux - figures) / figures) ) )
          i.fig<- max(which(figures/1.25 < ( duradaCodificada - sil_aux ) ))
          #     print(figures[i.fig])
          
          if(length(i.fig)>1) {
            print("possible fallada en la detecciÃ³ de la durada del silenci."); 
            sprintf("Durada codificada %f", duradaCodificada - sil_aux - figures);
            print("Figures possibles "); 
            print(abs(( duradaCodificada - sil_aux - figures) / figures));  print(figures[i.fig])
            i.fig<-max(i.fig)}
        }
        figura_previa<-figures[i.fig];
        sil_aux<-sil_aux+figures[i.fig];
        #  print(c(duradaCodificada, err, sil_aux, abs(( duradaCodificada - sil_aux - figures) / figures)) )
      }
    }
    return(sil_aux)
  }
  
  
  
  mtof<-function(x) {440* 2^((x-69)/12)}
  
  processa.midi<-function(midi) {
    midiNotes<-getMidiNotes(midi)
    #Quanititzem durades
    for (ii in 1:nrow(midiNotes)) {
      midiNotes$length[ii]<-duradaQuantitzada(midiNotes$length[ii]);
    }
    midiNotes$time.noteOff<-midiNotes$time +midiNotes$length;
    onoff<-unique(c(midiNotes$time, midiNotes$time.noteOff)); onoff<-onoff[order(onoff)]
    notes=rep(list(NA), 10*length(onoff))
    figs=rep(list(NA), 10*length(onoff))
    
    t=c(0,rep(-1,  -1+10*length(onoff)))
    maxnotesSimultanies<-10;
    mat<-matrix(rep(0,length(onoff)*maxnotesSimultanies), ncol=length(onoff), nrow=maxnotesSimultanies)
    tie<-(list())
    
    #midiFigures<-list()
    #notes<-list()
    #tie<-list()
    
    figuresGM<-c(0.75*2^c(0:3) , 2^c(-3:2))
    
    #l<-data.frame(notes=rep(list(NA), 10*length(onoff)), figs=rep(list(), 10*length(onoff)), t=c(0,rep(-1,  -1+10*length(onoff))) )
    
    iLine.el<-1;
    
    for (ii in 1:(length(onoff)-1)){
      
      if(any(onoff[ii] >= midiNotes$time & onoff[ii]<midiNotes$time.noteOff)) { #SI NOTA
        aux_fig<-onoff[ii+1]-onoff[ii];
        
        while(any(figuresGM<=aux_fig)){
          novaFigura<-max(figuresGM[figuresGM<= (aux_fig/480) ])
          aux_fig<-aux_fig-480*novaFigura
          figs[[iLine.el]]<-novaFigura;
          notes[[iLine.el]]<-midiNotes$note[ which(midiNotes$time <=onoff[ii] & midiNotes$time.noteOff>onoff[ii])]
          
          mat[ 1:length(notes[[iLine.el]]), iLine.el]<- notes[[iLine.el]]
          if(any(aux_fig/480>= figuresGM) ){      tie<-append(tie, iLine.el)}
          iLine.el<-iLine.el+1
        }
        for (iAcord in 1:length(notes[[iLine.el-1]])) {
          if (any(midiNotes$note==notes[[iLine.el-1]][iAcord] & 
                  midiNotes$time<=onoff[ii] & 
                  midiNotes$time.noteOff>onoff[ii] & 
                  onoff[ii+1]<midiNotes$time.noteOff )) {tie<-append(tie, list( c(iLine.el-1, iAcord)))}
        }
      }else{ # SI SILENCI
        figs[[iLine.el]]<-(onoff[ii+1]-onoff[ii])/480;
        iLine.el<-iLine.el+1
      }
    }
    
    return(list(mat, onoff, notes, figs, tie))
  }
  
  get.metricStrength<-function(onoff, compas) {
    t_entreEvents<-diff(onoff)
    strengthPattern<-4-c(0, 2,1,3)
    event_metricStrength<-rep(0, length(t_entreEvents))
    iteracions<-0
    sumaIteracioAnterior<-(-1)
    #no del compas sino de cada nivell (encara que el nivell sup Ã©s el de compÃ s)
    utemps<-compas$utemps
    ntemps<-compas$ntemps
    #while(sum(t_entreEvents[!is.na(event_metricStrength)])<0.9*(sum(t_entreEvents)) &
    #sum(t_entreEvents[!is.na(event_metricStrength)])!=sumaIteracioAnterior & iteracions<6){
    
    for(ii in 1:6){
      sumaIteracioAnterior<-sum(t_entreEvents[!is.na(event_metricStrength)])
      
      esbeat<-((onoff[1:(length(onoff)-1)]%%(480*utemps))==0)
      event_metricStrength[esbeat]<-event_metricStrength[esbeat]+strengthPattern[ 1+(onoff[1:(length(onoff)-1)][esbeat]/(480*utemps))%%ntemps]
      if(utemps==compas$utemps) { ntemps<-utemps/0.5}else{ntemps<-2}
      if(utemps==1.5) { utemps<-utemps/3}else{utemps<-utemps/2}
      
      iteracions<-iteracions+1}
    return(event_metricStrength)
  }
  
)
midiFileModel<-c("Trio.mid" )
midiFiles<-c("Trio.mid" )
compassos<-data.frame(ntemps=c(2,3,4,2,3,4),utemps=c(1,1,1,1.5,1.5,1.5))
i.compas<-c(1) 

incloure<-data.frame(arrancFi=0, durRel=1, mstrengthRel=1, quintilDurada=1)

#la funcio processa.midi de l'arxiu Harmonitza_melodies_senseMT.R no dona errades. Ka de -Neguit_var_markov(1),sí
#varsMidMark<-get.varsMidMark(midiFiles=midiFiles[1], i.compas=i.compas[1])

#1. Durades iguals
#2. Durades i strengths equivalents. Model durRel==2 || mstrenghRel==2 
#3. Durades i strengths intercanviables. Model durRel==2 || mstrenghRel==2 --> Candidat durRel==2 || mstrenghRel==2
                                        # else  Model durRel + mstrenghRel --> Candidat durRel + mstrenghRel
                          #durRel-->   t_entreEvents[i.pianoroll]>t_entreEvents[i.pianoroll-1]...2
                                          #t_entreEvents[i.pianoroll]==t_entreEvents[i.pianoroll-1]...1
                                          #t_entreEvents[i.pianoroll]<t_entreEvents[i.pianoroll-1]...0
midi<-readMidi(midiFileModel[i.midiFile])
llista<-processa.midi(midi)
midi<-readMidi(midiFiles[i.midiFile])
onoff<-llista[[2]]
compas<-compassos[i.compas[i.midiFile],]
