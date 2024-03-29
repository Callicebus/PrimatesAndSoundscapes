tonyPAMGuide_Meta <- function(fullfile,...,atype='PSD',plottype='Both',envi='Air',calib=0,ctype = 'TS',Si=-159,Mh=-36,G=0,vADC=1.414,r=50,N=Fs,winname='Hann',lcut=Fs/N,hcut=Fs/2,timestring="",outdir=dirname(fullfile),outwrite=0,disppar=1,welch="",chunksize="",linlog = "Log", StartTime = NA, CallOnset = NA, seconds = NA, channel = 1){ # StartTime, CallOnset, seconds, and channel added as arguments; if they are not specified, then the function should run equivalent to original PAMGuide_Meta function

  graphics.off()									#close plot windows
  aid <- 0										#reset metadata code
  if (calib == 0) {aid <- aid + 20				#add calibration element to metadata code
  } else {aid <- aid + 10}
  if (timestring != ""){aid <- aid + 1000} else {aid <- aid + 2000}	#add time stamp element to metadata code

  ## Get file info------------------------------------------------------------------
  ifile <- basename(fullfile)						#file name

  message(paste0("Filename: ", ifile, '\n'))
  if (!file.exists(fullfile)){
    cat('Cannot run function... File', fullfile,'does not exist...\n\n')
    return(NA)
  }
  ### TONY: this bit was changed to potentially read a window_length of N "seconds" from the file starting that amount of time before CallOnset
  if (is.na(CallOnset) & is.na(StartTime) & is.na(seconds)) {
    cat('Running function on entire file...\n')
    fIN <- readWave(fullfile,header = TRUE)			#read file header
  } else if (!is.na(CallOnset) & !is.na(StartTime) & !is.na(seconds)) {
    if (difftime(CallOnset, StartTime, units="secs") < seconds){
      cat('Cannot run function... CallOnset is less than', seconds,'seconds after StartTime of sample...\n')
      # cat('StartTime of sample is:', ifelse(!is.na(StartTime),format(StartTime,"%H:%M:%S"),NA), '\n')
      # cat('CallOnset is:', ifelse(!is.na(CallOnset),format(CallOnset,"%H:%M:%S"),NA), '\n')
      # cat('Window Length is:', seconds, 'seconds\n')
      cat('\n')
      return(NA)
    }
    cat('Running function on', seconds, 'seconds extracted from file, starting at', format(CallOnset - seconds, "%H:%M:%S"), 'up to', format(CallOnset, "%H:%M:%S"),'...\n')
    cat('StartTime of sample is:', ifelse(!is.na(StartTime),format(StartTime,"%H:%M:%S"),NA), '\n')
    cat('CallOnset is:', ifelse(!is.na(CallOnset),format(CallOnset,"%H:%M:%S"),NA), '\n')
    cat('Window Length is:', seconds, 'seconds\n')
    fIN <- readWave(fullfile, header = TRUE, from = CallOnset - StartTime - seconds, to = CallOnset - StartTime, units = "seconds")			#read file header
  } else {
    cat('Cannot run function... Needed parameters are missing...\n')
    # cat('StartTime of sample is:', ifelse(!is.na(StartTime),format(StartTime,"%H:%M:%S"),NA), '\n')
    # cat('CallOnset is:', ifelse(!is.na(CallOnset),format(CallOnset,"%H:%M:%S"),NA), '\n')
    # cat('Window Length is:', seconds, 'seconds\n')
    cat('\n')
    return(NA)
  }

  Fs <- fIN[[1]]									#sampling frequency
  cat('Sampling frequency: ', Fs,'\n')
  Nbit <- fIN[[3]]								#bit depth
  cat('Bit depth: ', Nbit, '\n')

  if (is.na(CallOnset) & is.na(StartTime) & is.na(seconds)) {
    xl <- fIN[[4]]									#length of FULL file in samples
    cat('Length of full file in samples:', xl, '\n\n')
  } else {
    xl <- Fs * seconds ### calculate xl based on the length of the "seconds" window
    ### rather than simply taking that from the file header
    cat('Length of', seconds, 'seconds from file in samples:', xl, '\n\n')
  }

  xlglo <- xl										#back-up file length

  ## Read time stamp data if provided-----------------------------------------

  if (timestring != "") {tstamp <- as.POSIXct(strptime(ifile,timestring) ,origin="1970-01-01")
  if (disppar == 1){cat('Time stamp start time: ',format(tstamp),'\n')}}
  else{tstamp <- ""}		#compute time stamp in R POSIXct format


  ## Display user-defined settings------------------------------------------

  if (disppar == 1){
    cat('Analysis type:',atype,'\n')
    cat('Plot type:',plottype,'\n')
    if (calib == 1){
      if (ctype == 'EE'){
        cat('End-to-end system sensitivity =',sprintf('%.01f',Si),'dB\n')
        if (envi == 'Wat') {cat('In-air measurement\n')}
        if (envi == 'Wat') {cat('Underwater measurement\n')}}
      if (ctype == 'RC'){
        cat('System sensitivity of recorder (excluding transducer) =',sprintf('%.01f',Si),'dB\n')}
      if (ctype == 'TS' || ctype == 'RC'){
        if (envi == 'Air') {cat('In-air measurement\n')
          cat('Microphone sensitivity:',Mh,'dB re 1 V/Pa\n')
          Mh <- Mh - 120}		#convert to dB re 1 V/uPa
        if (envi == 'Wat') {cat('Underwater measurement\n')
          cat('Hydrophone sensitivity:',Mh,'dB re 1 V/uPa\n')}}
      if (ctype == 'TS'){
        cat('Preamplifier gain:',G,'dB\n')
        cat('ADC peak voltage:',vADC,'V\n')}
    } else {cat('Uncalibrated analysis. Output in relative units.\n')
    }
    cat('Time segment length:',N,'samples =',N/Fs,'s\n')
    cat('Window function:',winname,'\n')
    cat('Window overlap:',r,'%\n')
  }
  r<-r/100

  ## Read input file------------------------------------------------------------------

  if (chunksize == "") {nchunks = 1
  } else if (chunksize != "") {
    nchunks <- ceiling(xl/(Fs*as.numeric(chunksize)))	#number of chunks of length chunksize in file
  }
  cat('number of chunks:', nchunks, '\n')
  for (q in 1:nchunks){
    ### TONY: I modified the `if (nchunks==1)` routine...
    if (nchunks == 1){
      #t1=proc.time()									#start timer
      #cat('Loading input file... ')
      ### TONY: the xbit code below was modified to read just a window of "seconds" from the file,
      ### beginning at CallOnset
      if (is.na(CallOnset) & is.na(StartTime) & is.na(seconds)) {
        xbit <- readWave(fullfile)						#read file
      } else {
      xbit <- readWave(fullfile, from = CallOnset - StartTime - seconds, to = CallOnset - StartTime, units = "seconds")						#read file
      }
      #cat('done in',(proc.time()-t1)[3],'s.\n')
      ### TONY: I added the if statement to pick which channel to use...
      # print(str(xbit)) # uncommenting this line shows that a
      # data structure with class Wave has an @left and @right channel
      if (channel == 1) {xbit <- xbit@left/(2^(Nbit-1))}
      if (channel == 2) {xbit <- xbit@right/(2^(Nbit-1))}
      #convert to full scale (+/- 1) via bit depth

    ### TONY: I *did not* modify the following routine for where nchunks > 1...
    ### so if an argument of nchunks > 1 is passed, the code is not accurate...
    ### to correct this, the xbit <- readWave() line would need to be altered
    ### I didn't consider how best to do that because it would involve breaking up the
    ### "seconds" window into pieces

    } else if (nchunks > 1) {
      if (q == nchunks){
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=xlglo,units="samples")
        xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)
      } else {
        xbit <- readWave(fullfile,from=((q-1)*as.numeric(chunksize)*Fs+1),to=(q*as.numeric(chunksize)*Fs),units="samples")
        xbit <- xbit@left/(2^(Nbit-1))					#convert to full scale (+/- 1) via bit depth
        xl <- length(xbit)
      }
    }

    if (envi == 'Air'){pref<-20; aid <- aid+100}	#set reference pressure depending for in-air or underwater
    if (envi == 'Wat'){pref<-1; aid <- aid+200}


    ## Compute system sensitivity if provided-----------------------------------------------

    if (calib == 1){
      if (ctype == 'EE') {		#'EE' = end-to-end calibration
        S <- Si}
      if (ctype == 'RC') {		#'RC' = recorder calibration with separate transducer sensitivity defined by Mh
        S <- Si + Mh}
      if (ctype == 'TS') {		#'TS' = manufacturer's specifications
        S <- Mh + G + 20*log10(1/vADC);		#EQUATION 4
      }

      if (disppar == 1){cat('System sensitivity correction factor, S = ',sprintf('%.01f',S),' dB\n')}
    } else {S <- 0}

    ## Compute waveform if selected----------------------------------------------------

    if (atype == 'Waveform') {
      if (calib == 1){
        a <- xbit/(10^(S/20)) 				#EQUATION 21
      } else {a <- xbit/(max(xbit))}
      t <- seq(1/Fs,length(a)/Fs,1/Fs)	#time vector
    }

    ## Compute DFT-based metrics if selected----------------------------------------

    if (atype != 'Waveform') {

      # Divide signal into data segments (corresponds to EQUATION 5)
      N = round(N)
      nsam = ceiling((xl)-r*N)/((1-r)*N)
      xgrid <- matrix(nrow = N,ncol = nsam)
      for (i in 1:nsam) {
        loind <- (i-1)*(1-r)*N+1
        hiind <- (i-1)*(1-r)*N+N
        xgrid[,i] = xbit[loind:hiind]
      }

      M <- length(xgrid[1,])

      # Apply window function (corresponds to EQUATION 6)
      if (winname == 'Rectangular') {			#rectangular (Dirichlet) window
        w <- matrix(1,1,N)
        alpha <- 1 }					#scaling factor
      if (winname == 'Hann') {			#Hann window
        w <- (0.5 - 0.5*cos(2*pi*(1:N)/N))
        alpha <- 0.5 }					#scaling factor
      if (winname == 'Hamming') {			#Hamming window
        w <- (0.54 - 0.46*cos(2*pi*(1:N)/N))
        alpha <- 0.54 }					#scaling factor
      if (winname == 'Blackman') {		#Blackman window
        w <- (0.42 - 0.5*cos(2*pi*(1:N)/N) + 0.08*cos(4*pi*(1:N)/N))
        alpha <- 0.42 }					#scaling factor

      xgrid <- xgrid*w/alpha


      #Compute DFT (corresponds to EQUATION 7)
      X <- abs(mvfft(xgrid))

      #Compute power spectrum (EQUATION 8)
      P <- (X/N)^2

      #Compute single-side power spectrum (EQUATION 9)
      Pss <- 2*P[0:round(N/2)+1,]

      #Compute frequencies of DFT bins
      f <- floor(Fs/2)*seq(1/(N/2),1,len=N/2)
      flow <- which(f >= lcut)[1]
      fhigh <- max(which(f <= hcut))
      nf <- length(f)
      f <- f[flow:fhigh]

      #Compute PSD in dB if selected
      if (atype == 'PSD') {
        B <- (1/N)*(sum((w/alpha)^2))		#noise power bandwidth (EQUATION 12)
        delf <- Fs/N;						#frequency bin width
        a <- 10*log10((1/(delf*B))*Pss[flow:fhigh,]/(pref^2))-S
      }									#PSD (EQUATION 11)

      #Compute power spectrum in dB if selected
      if (atype == 'PowerSpec') {
        a <- 10*log10(Pss[flow:fhigh,]/(pref^2))-S
      }									#EQUATION 10

      #Compute broadband level if selected
      if (atype == 'Broadband') {
        a <- 10*log10(colSums(Pss[flow:fhigh,])/(pref^2))-S
      }									#EQUATION 17

      #Compute 1/3-octave band levels if selected
      if (atype == 'TOL') {
        if (lcut <25){
          lcut <- 25}

        #Generate 1/3-octave freqeuncies
        lobandf <- floor(log10(lcut))	#lowest power of 10 for TOL computation
        hibandf <- ceiling(log10(hcut))	#highest power of 10 for TOL computation
        nband <- 10*(hibandf-lobandf)+1	#number of 1/3-octave bands
        fc <- matrix(0,nband)			#initialise 1/3-octave frequency vector
        fc[1] <- 10^lobandf;

        #Calculate centre frequencies (corresponds to EQUATION 13)
        for (i in 2:nband) {
          fc[i] <- fc[i-1]*10^0.1}
        fc <- fc[which(fc >= lcut)[1]:max(which(fc <= hcut))]

        nfc <- length(fc)				#number of 1/3-octave bands

        #Calculate boundary frequencies of each band (EQUATIONS 14-15)
        fb <- fc*10^-0.05				#lower bounds of 1/3-octave bands
        fb[nfc+1] <- fc[nfc]*10^0.05	#upper bound of highest band
        if (max(fb) > hcut) {			#if upper bound exceeds highest
          nfc <- nfc-1				# frequency in DFT, remove
          fc <- fc[1:nfc]}

        #Calculate TOLs (corresponds to EQUATION 16)
        P13 <- matrix(nrow = M,ncol = nfc)
        for (i in 1:nfc) {
          fli <- which(f >= fb[i])[1]
          fui <- max(which(f < fb[i+1]))
          for (k in 1:M) {
            fcl <- sum(Pss[fli:fui,k])
            P13[k,i] <- fcl
          }
        }
        a <- t(10*log10(P13/(pref^2)))-S
      }

      #cat('done in',(proc.time()-tana)[3],'s.\n')

      # Compute time vector
      tint <- (1-r)*N/Fs
      ttot <- M*tint-tint
      t <- seq(0,ttot,tint)

      if (nchunks>1){
        if (q == 1){
          newa <- a
          newt <- t
          cat('Analysing in',nchunks,'chunks. Analysing chunk 1')
        } else if (q > 1){
          dima <- dim(a)
          newa <- cbind(newa,a)
          if (timestring != ""){
            newt <- c(newt,t)
          } else {
            newt <- c(newt,t+(q-1)*as.numeric(chunksize))
          }
          cat('',q)
        }
      }
    }
  }
  if (nchunks>1){a <- newa
  t <- newt
  cat('\n')
  }

  # If not calibrated, scale relative dB to zero
  if (calib == 0) {a <- a-max(a)}

  if (tstamp != ""){t <- t+tstamp
  tdiff <- max(t)-min(t)					#define time format for x-axis of time plot
  if (tdiff < 10){
    tform <- "%H:%M:%S:%OS3"}
  else if (tdiff > 10 & tdiff < 86400){
    tform <- "%H:%M:%S"}
  else if (tdiff > 86400 & tdiff < 86400*7){
    tform <- "%H:%M \n %d %b"}
  else if (tdiff > 86400*7){tform <- "%d %b %y"}
  }

  ## Construct output array------------------------------------------------

  if (atype == 'PSD' | atype == 'PowerSpec') {
    A <- cbind(t,t(a))
    A <- rbind(c(0,f),A)
    if (atype == 'PSD'){aid <- aid + 1}
    if (atype == 'PowerSpec'){aid <- aid + 2}
    A[1,1] <- aid
  }

  if (atype == 'TOL') {
    A <- cbind(t,t(a))
    A <- rbind(c(0,fc),A)
    aid <- aid + 3
    A[1,1] <- aid
    f <- fc
  }

  if (atype == 'Broadband') {
    A <- t(rbind(t,a))
    aid <- aid + 4
    A[1,1] <- aid
  }

  if (atype == 'Waveform') {
    A <- t(rbind(t,a))		#define output array
    A <- rbind(c(0,0),A)	#add zero top row for metadata
    aid <- aid + 5			#add index to metadata for Waveform
    A[1,1] <- aid			#encode output array with metadata
  }

  ## Reduce time resolution if selected

  dimA <- dim(A)

  if (welch != "" && atype != "Waveform"){
    lout <- ceiling(dimA[1]/welch)+1
    AWelch <- matrix(, nrow = lout, ncol = dimA[2])
    AWelch[1,] <- A[1,]
    tint <- A[3,1] - A[2,1]
    if (disppar == 1){cat('Welch factor =',welch,'x\nNew time resolution =',welch,'(Welch factor) x',N/Fs,'s (time segment length) x',r*100,'% (overlap) =',welch*tint,'s\n')}
    if (lout == 2){
      AWelch[2,] <- 10*log10(mean(10^A[2:dimA[1]]/10))
    }	else {
      for (i in 2:lout) {
        stt <- A[2,1] + (i-2)*tint*welch
        ett <- stt + welch*tint
        stiv <- which(A[2:dimA[1]]>=stt)
        sti <- min(stiv)+1
        etiv <- which(A[2:dimA[1]]<ett)
        eti <- max(etiv)+1
        nowA <- 10^(A[sti:eti,]/10)
        AWelch[i,] <- 10*log10(rowMeans(t(nowA)))
        AWelch[i,1] <- stt+tint*welch/2
      }
    }
    A <- AWelch
    A[1,1] <- aid
    dimA <- dim(A)
    t <- A[2:dimA[1],1]
    f <- A[1,2:dimA[2]]
    a <- t(A[2:dimA[1],2:dimA[2]])

  }


  ## Plot time-domain analyses if selected---------------------------------------------------------------

  jet.colors <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","#7FFF7F","yellow","#FF7F00","red","#7F0000"))
  if (plottype == 'Time' | plottype == 'Both'){
    cat('Plotting...')
    tplot=proc.time()
    options(scipen = 7)


    if (atype == 'PSD') {	 						#spectrogram
      if (tstamp != ""){
        image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('PSD spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(f)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'PowerSpec') {				#spectrogram
      if (tstamp != ""){image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,f,t(a),ylim = c(lcut,hcut),yaxt="n",log = "y",col = jet.colors(512),main=paste('Power spectrum spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(f)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'TOL') {
      if (tstamp != ""){image(t,fc,t(a),ylim = c(min(fb),fb[nfc+1]),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time",ylab="Frequency [ Hz ]",xaxt="n")}
      else {image(t,fc,t(a),ylim = c(min(fb),fb[nfc+1]),yaxt="n",log = "y",col = jet.colors(512),main=paste('TOL spectrogram of ',ifile),xlab="Time [ s ]",ylab="Frequency [ Hz ]")}
      y1 <- floor(log10(range(fc)))
      pow <- seq(y1[1], y1[2]+1)
      ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
      labsat <- as.vector(sapply(pow, function(p) 10^p))
      axis(2, 10^pow,labels = NA)
      axis(2, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
      axis(2, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
    }

    if (atype == 'Broadband') {
      if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]"))
      } else {YLAB <- expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]"))}
      } else {YLAB <- expression(paste("Relative SPL [ dB ]"))}
      if (tstamp != ""){plot(t,a,type = "l",xlab="Time",ylab="",xaxt="n")
      } else {plot(t,a,type = "l",xlab="Time [ s ]",ylab="")}
      if (envi == 'Air'){mtext(text=YLAB, side=2, line=2)}
      if (envi == 'Wat'){mtext(text=YLAB, side=2, line=2)}
    }

    if (atype == 'Waveform') {
      if (calib == 1) {YLAB <- "Pressure [ Pa ]"
      } else {YLAB <- "Relative pressure";a <- a*1e6}
      if (tstamp != "") {
        plot(t,a/1e6,xlab="Time",ylab=YLAB,type = "l",ylim = c(-max(a/1e6),max(a/1e6)),xaxt="n")
        tck <- axis(1, labels=FALSE)
        axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01"),format=tform,label=TRUE)
      } else {plot(t,a/1e6,xlab="Time [ s ]",ylab=YLAB,type = "l",ylim = c(-max(a/1e6),max(a/1e6)))}
    }

    #format x axis if time stamp is provided
    if (tstamp != ""){tck <- axis(1, labels=FALSE)
    axis.POSIXct(1,as.POSIXct.numeric(tck,origin="1970-01-01"),format=tform,label=TRUE)}
    cat('done in',(proc.time()-tplot)[3],'s.\n')
  }

  ## Write output array to CSV file if selected----------------------

  A <- data.matrix(A, rownames.force = NA)
  if (outwrite == 1){
    #if (disppar == 1){cat('Writing output file...')
    #twrite <- proc.time()}
    if (atype == 'Waveform') {
      ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'.csv',sep = "")
      write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
    }
    if (atype != 'Waveform') {
      ofile <- paste(gsub(".wav","",file.path(outdir,basename(fullfile))),'_',atype,'_',N,'samples',winname,'Window_',round(r*100),'PercentOverlap.csv',sep = "")
      write.table(A,file = ofile,row.names=FALSE,quote=FALSE,col.names=FALSE,sep=",")
    }
    #if (disppar == 1){cat('done in',(proc.time()-twrite)[3],'s.\n')}
  }

  ## Statistical analysis----------------------------------------------------

  if (atype != 'Waveform'){
    if (plottype == 'Stats' | plottype == 'Both'){
      if (atype != 'Broadband'){
        M <- length(A[,1])-1

        cat('Computing noise level statistics...')
        tstat <- proc.time()
        dev.new()							#open new plot window

        # Compute mean level and percentiles
        RMSlevel <- 10*log10(rowMeans(10^(a/10)))
        #calculate RMS mean (EQUATION 18)

        p <- apply(a, 1, quantile, probs = c(0.01,0.05,0.5,0.9,0.95),  na.rm = TRUE)
        #percentile levels
        mindB <- floor(min(a)/10)*10		#minimum dB level rounded down to nearest 10
        maxdB <- ceiling(max(a)/10)*10		#maximum dB level rounded up to nearest 10

        # Compute SPD if more than 1000 data points in time domain
        if (M >= 1000) {
          hind <- 0.1							#histogram bin width for probability densities (PD)
          dbvec = seq(mindB,maxdB,hind)		#dB values at which to calculate empirical PD

          #Compute SPD array (corresponds to EQUATION 19)
          d <- t(apply(a,1,function(x) x<- hist(x,breaks = dbvec,plot = FALSE)$counts))
          d <- d/(hind*M)

          #Plot data
          d[which(d>0.05)] <- 0.05			#saturate colour scale at PD = 0.05
          image(f,dbvec[1:length(dbvec)-1]-hind/2,d,zlim <- c(min(d[which(d>0)]),0.05),log = "x",xaxt="n",col = jet.colors(2^12),xlim<-c(min(f),max(f)),main=paste('Noise level statistics for ',ifile,'\nWindow length = ',N/Fs,' s = ',N,' samples'),xlab="Frequency [ Hz ]",ylab="")
        }
        if (M<1000){plot(f,RMSlevel,type="n",log = "x",xaxt="n",main=paste('Noise level statistics for ',ifile,'\nWindow length = ',N/Fs,' s = ',N,' samples'),xlab="Frequency [ Hz ]",ylab="",ylim=c(mindB,maxdB))}
        lines(f,p[5,])
        lines(f,p[4,])
        lines(f,p[3,])
        lines(f,p[2,])
        lines(f,p[1,])
        lines(f,RMSlevel,col = 'magenta')
        if (M<1000){legend('bottomleft',c("99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=c(1),col=c("black","black","black","black","black","magenta"))}
        if (M>=1000){legend('bottomleft',c("SPD","99%","95%","50%","5%","1%","RMS level"),lty=c(1),lwd=c(1),col=c("blue","black","black","black","black","black","magenta"))}
        y1 <- floor(log10(range(f)))
        pow <- seq(y1[1], y1[2]+1)
        ticksat <- as.vector(sapply(pow, function(p) (1:10)*10^p))
        labsat <- as.vector(sapply(pow, function(p) 10^p))
        axis(1, 10^pow,label = NA)
        axis(1, ticksat,labels = NA, tcl=-0.25, lwd=0, lwd.ticks=1)
        axis(1, labsat, tcl=-0.25, lwd=0, lwd.ticks=1)
        if (atype == 'PSD'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("PSD [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("PSD [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
          } else {YLAB <- expression(paste("Relative PSD [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
        }
        if (atype == 'PowerSpec'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("Power spectrum [ dB re 20 ",mu,"Pa"^2,"Hz"^{-1},"]"))} else {YLAB <- expression(paste("Power spectrum [ dB re 1 ",mu,"Pa"^2,"Hz"^{-1},"]"))}
          } else {YLAB <- expression(paste("Relative power spectrum [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
        }
        if (atype == 'TOL'){
          if (calib == 1) {if (envi == 'Air'){YLAB <- expression(paste("SPL [ dB re 20 ",mu,"Pa ]"))
          } else {YLAB <- expression(paste("SPL [ dB re 1 ",mu,"Pa ]"))}
          } else {YLAB <- expression(paste("Relative SPL [ dB ]"))}
          mtext(text=YLAB, side=2, line=2)
          cat('done in',(proc.time()-tstat)[3],'s.\n')
        }
} # I THINK A CLOSING BRACKET WAS NEEDED HERE!

      if (atype == 'Broadband'){ # I reduced indent here...
          RMSlevel <- 10*log10(mean(10^(a/10)))
          p <- apply(t(a), 1, quantile, probs = c(0:100)/100,  na.rm = TRUE)
          dev.new()							#open new plot window
          plot(p,c(0:100)/100,type="n",xlab="",ylab="Cumulative Distribution Function")
          lines(p,c(0:100)/100)
          if (calib == 1) {if (envi == 'Air'){XLAB <- expression(paste("Broadband SPL [ dB re 20 ",mu,"Pa ]"))
          } else {XLAB <- expression(paste("Broadband SPL [ dB re 1 ",mu,"Pa ]"))}
          } else {XLAB <- expression(paste("Relative SPL [ dB ]"))}
          mtext(text=XLAB, side=1, line=2)

##### NEW SECTION FROM VIEWER.R FUNCTION TO CALCULATE LEVELS
          RMSlev <- 10*log10(mean(10^(a/10)))
            medlev <- 10*log10(median(10^(a/10)))
            Mode <- function(x) {
              ux <- unique(x)
              ux[which.max(tabulate(match(x, ux)))]
            }
            modelev <- Mode(round(a*10)/10)
  
            tind <- t(3)-t(2)
            SEL <- 10*log10(tind*sum(10^(a/10)))
  
            if (calib == 1){
              cat('\nRMS level (mean SPL) = ',sprintf('%.1f',RMSlev),'dB re',pref,'uPa\n')
              cat('Median SPL = ',sprintf('%.1f',medlev),'dB re',pref,'uPa\n')
              cat('Mode SPL = ',sprintf('%.1f',modelev),'dB re',pref,'uPa\n')
              cat('SEL = ',sprintf('%.1f',SEL),'dB re',pref,'uPa^2 s. Note: for SEL measurements, set r = 0 (window overlap) and use default N.\n\n')
            } else {
              cat('\nRelative normalised RMS level (mean SPL) = ',sprintf('%.1f',RMSlev),'dB\n')
              cat('Relative normalised median SPL = ',sprintf('%.1f',medlev),'dB\n')
              cat('Relative normalised mode SPL = ',sprintf('%.1f',modelev),'dB\n\n')
              cat('Relative normalised SEL = ',sprintf('%.1f',SEL),'dB. Note: for SEL measurements, set r = 0 (window overlap) and use default N.\n\n')
            }
            lines(c(RMSlev,RMSlev),c(0,1),col='magenta')
            lines(c(medlev,medlev),c(0,1),col='green')
            lines(c(modelev,modelev),c(0,1),col='blue')
            legend('bottomright',c("CDF","RMS level","Median","Mode"),lty=c(1),lwd=2,col=c("black","magenta","green","blue"))

            # the lines below are new as of 2021-07-12 and return
            # a more complete results object if atype = 'Broadband'
            results <- list(A=A, RMSlev=RMSlev, medlev=medlev, modelev=modelev)
            # TONY: the line above generates of list of data elements as results... if you just want to return the average RMS level then replace with "results <- RMSlev"
            return(results)
##### END OF THE NEW SECTION
        }

      if (M < 1000 & atype != 'Broadband') {
          cat('Too few time segments (M = ',M,', i.e. <1000) for SPD analysis: for SPD, use a longer file or shorter time segment length (N).\n')}
      # TONY: I think this is misleading terminology in this warning... M isn't the number of time segments but rather the number of data points or rows in final matrix; N is is the number of time segments; N/Fs = length of each time segment (window) in seconds; M should be equivalent to the length of time being considered (e.g., a range of X seconds) divided by the length of each time segment (N/Fs) * 2, or seconds * 2 * Fs / N
      }
    # } TONY: I THINK THIS IS AN EXTRA BRACKET THAT SHOULD HAVE BEEN AT LINE 528
  }
  
  # TONY: the line below generates of list of data elements as results...
  # results <- list(A= ifelse(!is.na(A), A, NA), RMSlev=NA, medlev=NA, modelev=NA) # uncomment this and change the following line to "return(results)" to return a list of data elements as results... 
  
  return(A)			#return output array
}

