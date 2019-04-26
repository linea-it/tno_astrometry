c
c     PRAIA_header_extraction
c
c
c
c     Extracts image information from the FITS header: (RA,DEC), date, time,
c     exposition, object, filter, FoV size. 
c
c
c
c     Last modification: M. Assafin - 18/Jul/2013
c
c



      IMPLICIT REAL *8 (A-H,O-Z)
      parameter (ihead=1000)

      character*50 lista1,lista2
      character*50 infits
      character*20 ichobj,ichfil
      character*1  isig,iver,ivo
      character*8  kobj,kfil,kras,kdec,kjud,kmjd,kdat,kbeg,kend,kbed,
     ?kexp,naxis1,naxis2,kaxes,kexten
      character*8 iamdx,iamdy



      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

      data ivo/'|'/


c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi
 

c
c     Reads input data
c


c     open (21,file='PRAIA_header_extraction_30_06.dat')

c

      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '
      write (*,1)
 1    format (23x,'PRAIA - Header extraction of fits files')
      write (*,*) ' '
c

c

 2    continue

      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '


      read (*,3,err=100,end=100) lista1,iver
 3    format(a50,a1)

      if (iver.ne.ivo) go to 100

      read (*,4,err=100,end=100) lista2
 4    format(a50)


c

      read (*,5,err=100,end=100) kexten

      read (*,5,err=100,end=100) kaxes 
      read (*,5,err=100,end=100) naxis1
      read (*,5,err=100,end=100) naxis2

      read (*,5,err=100,end=100) kobj

      read (*,5,err=100,end=100) kfil

      read (*,5,err=100,end=100) kras
      read (*,5,err=100,end=100) kdec

      read (*,5,err=100,end=100) kjud
      read (*,5,err=100,end=100) kmjd

      read (*,5,err=100,end=100) kdat

      read (*,5,err=100,end=100) kbeg
      read (*,5,err=100,end=100) kend
      read (*,5,err=100,end=100) kbed

      read (*,5,err=100,end=100) kexp
 5    format(a8)

      read (*,*,end=100) kh,km,sk    

      offtim=hmsgms(kh,km,sk)/24.d0

      read (*,7,end=100) maxes

      read (*,7,end=100) mtim    


      read (*,7,end=100) mras    
      read (*,7,end=100) mdec
 7    format(i2)


      read (*,7,end=100) irafa   
      read (*,7,end=100) idefa

      rafa=1.d0
      defa=1.d0

      if (irafa.eq.2) rafa=radgra
      if (idefa.eq.2) defa=radgra

      if (irafa.eq.3) rafa=15.d0


c

      read (*,7,end=100) iwcs

      read (*,7,end=100) ncox
      read (*,7,end=100) ncoy
      read (*,7,end=100) isaox
      read (*,7,end=100) isaoy

      saox=1.d0
      saoy=1.d0

      if (isaox.eq.1) saox=grarad
      if (isaoy.eq.1) saoy=grarad

      if (isaox.eq.3) saox=15.d0*grarad

      iamdx=''
      iamdy=''

      read (*,5,end=100) iamdx
      read (*,5,end=100) iamdy


c
   
      read (*,*,end=100) iver

c     close (21)


c
      write (*,8)
 8    format (15x,'List of fits images to proccess                      
     ?               -> ',$)
      write(*,4) lista1
c
      write (*,9)
 9    format (15x,'List of extracted fits fields                        
     ?               -> ',$)
      write(*,4) lista2


c

      write (*,*)
      write (*,*)

c
c     Loads images
c

      open (3,file=lista1)
      open (7,file=lista2)

c

      i=0
 10   read (3,*,err=20,end=20)
      i=i+1
      go to 10
 20   rewind (3)
      nfiles=i

      do i=1,nfiles
      read (3,33) infits
 33   format(a50)


      write (*,50) i,nfiles
 50   format(1x,'Header extraction: image ',i5,' of ',i5)

c
c     Extracts image headers
c

      ofra=0.d0
      ofde=0.d0


      call obhead (kexten,maxes,kaxes,naxis1,naxis2,kobj,kfil,kras,kdec,
     ?kjud,kmjd,kdat,kbeg,kend,kbed,kexp,mtim,mras,mdec,rafa,defa,saox,
     ?saoy,ncox,ncoy,iamdx,iamdy,iwcs,offtim,ihead,infits,ichobj,iah,
     ?iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,iutmes,iutdia,djm,dj,
     ?ichfil,iexps,nx,ny,ofra,ofde)


c
c     Applying (RA,DEC) offsets for mosaic CCDs
c
c     ofra, ofde in degrees
c


      if (iah.eq.99 .or. idg.eq.99) go to 59

c

      ra=hmsgms(iah,iam,sa)

      de=hmsgms(idg,idm,ds)
      if (isig.eq.'-') de=-de


      ra=ra+(ofra/dcos(grarad*dabs(de)))/15.d0

      de=de+ofde

      de=dabs(de)

      iah=ra
      iam=(ra-iah)*60.d0
      sa=((ra-iah)*60.d0-iam)*60.d0

      idg=de
      idm=(de-idg)*60.d0
      ds=((de-idg)*60.d0-idm)*60.d0

c


 59   continue

      write (7,60) iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,djm,dj,iexps,ichfil,infits,ichobj,nx,ny
 60   format(1x,i2.2,1x,i2.2,1x,f7.4,1x,a1,i2.2,1x,i2.2,1x,f6.3,2x,i2.2,
     ?1x,i2.2,1x,f5.2,1x,i4.4,1x,i2.2,1x,i2.2,f16.8,1x,f16.8,2x,i4.4,2x,
     ?a20,2x,a50,1x,a20,2(1x,i5.5))

      enddo

      close (3)
      close (7)

c

      go to 2


c
c     End of proccess
c


 100  continue


      write (*,*)
      write (*,*)
      write (*,*) ' Proccess terminated. Status ok.'
      write (*,*)
      write (*,*)


      end



c
c
c     Subroutine obhead
c
c     Extracts info from the FITS header of the images
c
c
c     Last modification: M. Assafin - 18/Jul/2013
c
c
c

      subroutine obhead (kexten,maxes,kaxes,naxis1,naxis2,kobj,kfil,
     ?kras,kdec,kjud,kmjd,kdat,kbeg,kend,kbed,kexp,mtim,mras,mdec,rafa,
     ?defa,saox,saoy,ncox,ncoy,iamdx,iamdy,iwcs,offtim,ihead,infits,
     ?ichobj,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,iutmes,
     ?iutdia,djm,dj,ichfil,iexps,nx,ny,ofra,ofde)

      IMPLICIT REAL *8 (A-H,O-Z)


      dimension coefx(100),coefy(100)

      character*2880 header
      character*(ihead*2880) head

      character*50 infits

      character*8 iamdx,iamdy,jamd,kamdx(100),kamdy(100)

      character*8  kobj,kfil,kras,kdec,kjud,kmjd,kdat,kbeg,kend,kbed,
     ?kexp,naxis1,naxis2,kaxes,kexten


      character*20 ichobj,ichfil
      character*1  isig

      character*80 itrima

c

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0
c
      DEXY(XX,YY,Z,W)=DSIN(YY)*DSIN(W)+DCOS(YY)*DCOS(W)*DCOS(XX-Z)
      XPAD(XX,YY,Z)=DCOS(YY)*DSIN(XX-Z)
      YPAD(XX,YY,Z,W)=DSIN(YY)*DCOS(W)-DCOS(YY)*DSIN(W)*DCOS(XX-Z)
      ALFF (XX,YY,ZZ,WW)=ZZ+DATAN2(XX,DCOS(WW)-YY*DSIN(WW))
      DELTT(XX,YY,ZZ,WW)=DATAN2((YY*DCOS(WW)+DSIN(WW))*DCOS(XX-ZZ),
     ?DCOS(WW)-YY*DSIN(WW))


c
c     Initial values
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi
c

      dj1=2400000.5d0

      ifim80=80

c

      do i=1,100
      coefx(i)=0.d0
      coefy(i)=0.d0
      enddo

      cd11=0.d0
      cd12=0.d0
      cd21=0.d0
      cd22=0.d0



c
c     Initial string settings for WCS (SAO polynomial fit option)
c

      do i=1,50
      if (iamdx(i:i).eq.' ') go to 1
      enddo

 1    lamdx=i

      do i=1,50
      if (iamdy(i:i).eq.' ') go to 2
      enddo

 2    lamdy=i

      igrau=100

c
c     X coefficients key strings
c

      nox=0

      ico=-1

      n=0
      l=0


      do n=1,igrau
      do l=1,n
      ico=ico+1
      if (ico.gt.ncox) go to 4

      jamd=''
      jamd=iamdx

      if (ico.lt.10) write (jamd(lamdx:lamdx),'(i1)') ico
      if (ico.ge.10) write (jamd(lamdx:lamdx+1),'(i2)') ico

      nox=nox+1
      kamdx(nox)=''
      kamdx(nox)=jamd

      enddo
      enddo



c
c     Y coefficients key strings
c

 4    continue

      noy=0

      ico=-1

      n=0
      l=0


 
      
      do n=1,igrau
      do l=1,n
      ico=ico+1
      if (ico.gt.ncoy) go to 6

      jamd=''
      jamd=iamdy

      if (ico.lt.10) write (jamd(lamdy:lamdy),'(i1)') ico
      if (ico.ge.10) write (jamd(lamdy:lamdy+1),'(i2)') ico

      noy=noy+1
      kamdy(noy)=''
      kamdy(noy)=jamd

      enddo
      enddo



 6    continue




c
c     Opens FITS file
c


      open (1,file=infits,access='direct',form='unformatted',recl=2880)


c
c     Loads header
c

      mexten=0
      jhead=0

      do 20 i=1,ihead

      header='' 

      read (1,rec=i) header

      head(2880*(i-1)+1:2880*i)=header(1:2880)
 
c
c     Finds if there are header extensions
c
c     mexten = 0  -> no extension
c            = 1  -> there is an extension or extensions


      do 10 k = 1,2880,80

      itrima=''
      itrima(1:80)=header(k:k+79)

      do j=10,80
      if (itrima(j:j).eq.'/') go to 7
      enddo

 7    ifim=j-1

      if (itrima(1:8).eq.kexten) THEN

      do j=10,ifim
      if (itrima(j:j).eq.'t'.or.itrima(j:j).eq.'T') mexten=1
      enddo

      ENDIF



 10   continue


c

      do k = 1,2880,80
      if (header(k:k+3).eq.'END ') jhead=jhead+1
      enddo



      if (mexten.eq.0.and.jhead.gt.0) go to 30
 
      if (mexten.eq.1.and.jhead.gt.1) go to 30



 
c
 20   continue

c

 30   nheads=i

      if (nheads.gt.ihead) then
      write (*,31) ihead
 31   format('Header size exceeded. More than ',i5,' pages. Exiting.')
      endif 


c
c     Extracts header information
c


c
c     (Ra,Dec), filter and object default (unknown) values
c

      iah=99
      iam=99
      sa=99.d0

      isig='+'
      idg=99
      idm=99
      ds=99.d0

      ichobj=''
      ichfil=''

      exps=-1.d0
      ihh2=-1

      nx=0
      ny=0


c
c     Loads header
c

      do 70 m=1,nheads

      header=''
      header(1:2880)=head(2880*(m-1)+1:2880*m)


      do 60 k = 1,2880,80

      itrima=''
      itrima(1:80)=header(k:k+79)


c
c     Checks out the position of the standard FITS header comment
c     character "/" in the string
c

      do i=10,80
      if (itrima(i:i).eq.'/') go to 33
      enddo

 33   ifim=i-1


c
c     Pixel matrix dimensions
c

      if (maxes.eq.1) THEN

      if (itrima(1:8).eq.naxis1) THEN

      do i=10,ifim
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      enddo

      read (itrima(10:ifim),*) nx

      ENDIF



      if (itrima(1:8).eq.naxis2) THEN


      do i=10,ifim
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      enddo

      read (itrima(10:ifim),*) ny

      ENDIF


      ENDIF


      if (maxes.eq.2) THEN

      if (itrima(1:8).eq.kaxes) THEN

      do i=10,ifim
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      enddo

      read (itrima(10:ifim),*) n11,n12,n21,n22

      nx=n12-n11+1
      ny=n22-n21+1

      ENDIF



      ENDIF

 

c
c     Object name   
c

      if (itrima(1:8).eq.kobj) THEN

c     do i=10,ifim80
c     if (itrima(i:i).eq."'") itrima(i:i)=' '
c     if (itrima(i:i).eq."/") itrima(i:i)=' '
c     enddo
      ichobj=itrima(10:ifim80)

      ENDIF


c
c     Filter
c

      if (itrima(1:8).eq.kfil) THEN

c     do i=10,ifim80
c     if (itrima(i:i).eq."'") itrima(i:i)=' '
c     if (itrima(i:i).eq."/") itrima(i:i)=' '
c     enddo
      ichfil=itrima(10:ifim80)

      ENDIF



c
c     Right ascension
c


      if (itrima(1:8).eq.kras) THEN

      do i=10,ifim
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

c

      if (mras.eq.1) then

      read (itrima(10:ifim),*,err=36,end=36) iah,iam,sa
 36   continue

      endif

c

      if (mras.eq.2) then

      read (itrima(10:ifim),*,err=37,end=37) iah,iam,sa
 37   continue

      ra=hmsgms(iah,iam,sa)
      ra=ra/15.d0
      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0

      endif

c

      if (mras.eq.3) then

      read (itrima(10:ifim),*,err=38,end=38) ra
 38   continue

      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0

      endif

c

      if (mras.eq.4) then

      read (itrima(10:ifim),*,err=39,end=39) ra
 39   continue

      ra=ra/15.d0
      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0

      endif

c


      if (mras.eq.5) then

      read (itrima(10:ifim),*,err=40,end=40) ra
 40   continue

      ra=ra*radgra

      ra=ra/15.d0
      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0

      endif

      ENDIF


c
c     Declination
c


      if (itrima(1:8).eq.kdec) THEN

      isig='+'
      do i=10,ifim
      if (itrima(i:i).eq.'-') isig='-'
      enddo

      do i=10,ifim
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

c

      if (mdec.eq.1) then

      read (itrima(10:ifim),*,err=41,end=41) idg,idm,ds
 41   continue

      endif

c

c

      if (mdec.eq.2) then

      read (itrima(10:ifim),*,err=42,end=42) de
 42   continue

      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0

      endif

c


      if (mdec.eq.3) then

      read (itrima(10:ifim),*,err=43,end=43) de
 43   continue

      de=de*radgra

      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0

      endif


      ENDIF



c
c     WCS data extraction: tangent plane projection  (RA---TAN,DEC--TAN)
c


      if (itrima(1:8).eq.'CRPIX1  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crpix1
      endif

      if (itrima(1:8).eq.'CRPIX2  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crpix2
      endif

      if (itrima(1:8).eq.'CRVAL1  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crval1
      crval1=crval1*rafa
      endif

      if (itrima(1:8).eq.'CRVAL2  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crval2
      crval2=crval2*defa
      endif


c
c     WCS: Standard system keys
c


      if (itrima(1:8).eq.'CD1_1   ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cd11
      cd11=cd11*rafa
      endif

      if (itrima(1:8).eq.'CD1_2   ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cd12
      cd12=cd12*rafa
      endif


      if (itrima(1:8).eq.'CD2_1   ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cd21
      cd21=cd21*defa
      endif

      if (itrima(1:8).eq.'CD2_2   ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cd22
      cd22=cd22*defa
      endif



c
c     WCS: old system (AIPS) keys
c


      if (itrima(1:8).eq.'CDELT1  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cdelt1
      cdelt1=cdelt1*rafa
      endif

      if (itrima(1:8).eq.'CROTA1  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crota1
      crota1=crota1*rafa
      endif


      if (itrima(1:8).eq.'CDELT2  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) cdelt2
      cdelt2=cdelt2*defa
      endif

      if (itrima(1:8).eq.'CROTA2  ') then
      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo
      read (itrima(10:ifim),*) crota2
      crota2=crota2*defa
      endif


c
c     WCS: SAO polynomial fit keys
c


      if (iwcs.eq.3) then

c
c     X coefficients
c

      do j=1,nox
      jamd=kamdx(j)

      if (itrima(1:8).eq.jamd) then

      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) coefx(j)

      endif

      enddo


c
c     Y coefficients
c

      do j=1,noy
      jamd=kamdy(j)

      if (itrima(1:8).eq.jamd) then

      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) coefy(j)

      endif

      enddo



      endif


c
c     Exposure time
c


      if (itrima(1:8).eq.kexp) THEN

      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) exps

      ENDIF


c
c     Julian Date
c

      if (mtim.eq.3) THEN

      if (itrima(1:8).eq.kjud) then

      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) dju

      endif

      ENDIF



c
c     Modified Julian Date
c

      if (mtim.eq.4) THEN

      if (itrima(1:8).eq.kmjd) then

      do i=10,ifim
      if (itrima(i:i).ne.'.'.and.itrima(i:i).ne.'E'.and.itrima(i:i).ne.
     ?'e'.and.itrima(i:i).ne.'D'.and.itrima(i:i).ne.'d'.and.itrima(i:i).
     ?ne.'+'.and.itrima(i:i).ne.'-') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) djum

      endif


      ENDIF


c
c     Date of observation
c

      if (mtim.eq.3) go to 50
      if (mtim.eq.4) go to 50


      if (itrima(1:8).eq.kdat) THEN

      do i=10,ifim80
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

c

      if (mtim.eq.1) then

      read (itrima(10:ifim80),*) ib1,ib2,ib3,ihh1,ihm1,hs1

      if (ib1.gt.ib3) then
      iutano=ib1
      iutmes=ib2
      iutdia=ib3
      else
      iutano=ib3
      iutmes=ib2
      iutdia=ib1
      endif


      else 


      read (itrima(10:ifim80),*) ib1,ib2,ib3

      if (ib1.gt.ib3) then
      iutano=ib1
      iutmes=ib2
      iutdia=ib3
      else
      iutano=ib3
      iutmes=ib2
      iutdia=ib1
      endif


      endif     


      ENDIF

c

 50   continue


c
c     Exposure start instant 
c


      if (itrima(1:8).eq.kbeg) THEN

      do i=09,ifim
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo


      iii=0
      do i=10,ifim
      if (itrima(i-1:i-1).eq.' '.and.itrima(i:i).ne.' ') iii=iii+1
      enddo


      if (iii.eq.3) then
      read (itrima(10:ifim),*) ihh1,ihm1,hs1
      endif

      if (iii.eq.2) then
      read (itrima(10:ifim),*) ihh1,hm1
      ihm1=hm1
      hs1=(hm1-ihm1)*60.d0
      endif


      if (iii.eq.1) then
      read (itrima(10:ifim),*) hh1

      hh1=hh1/86400.d0
      hh1=hh1*24.d0
      ihh1=hh1
      hm1=(hh1-ihh1)*60.d0
      ihm1=hm1
      hs1=(hm1-ihm1)*60.d0
      endif




      ENDIF



c
c     Exposure end instant 
c


      if (itrima(1:8).eq.kend) THEN

      do i=09,ifim
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo



      iii=0
      do i=10,ifim
      if (itrima(i-1:i-1).eq.' '.and.itrima(i:i).ne.' ') iii=iii+1
      enddo


      if (iii.eq.3) then
      read (itrima(10:ifim),*) ihh2,ihm2,hs2
      endif

      if (iii.eq.2) then
      read (itrima(10:ifim),*) ihh2,hm2
      ihm2=hm2
      hs2=(hm2-ihm2)*60.d0
      endif


      if (iii.eq.1) then
      read (itrima(10:ifim),*) hh2

      hh2=h21/86400.d0
      hh2=hh2*24.d0
      ihh2=hh2
      hm2=(hh2-ihh2)*60.d0
      ihm2=hm2
      hs2=(hm2-ihm2)*60.d0
      endif


      ENDIF




c
c     Start/END exposure instant at the same key 
c


      if (itrima(1:8).eq.kbed) THEN

      do i=09,ifim
      if (itrima(i:i).ne.'.') then
      icomp=ichar(itrima(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) itrima(i:i)=' '
      endif
      enddo

      read (itrima(10:ifim),*) ihh1,ihm1,hs1,ihh2,ihm2,hs2

      ENDIF

c


 60   continue

 70   continue


c
c     Computes the mid-instant of the exposure (JD and UT date)
c


      if (mtim.eq.3) THEN

      djm=dju-dj1

      djm=djm+(exps/2.d0)/86400.d0

      dj=djm+dj1

      go to 100

      ENDIF

c

      if (mtim.eq.4) THEN

      djm=djm+(exps/2.d0)/86400.d0

      dj=djm+dj1

      go to 100

      ENDIF

c

      if (exps.lt.-0.5d0) THEN

      if (ihh2.lt.0) then

      exps=0.d0

      else

      fd1=hmsgms(ihh1,ihm1,hs1)
      fd2=hmsgms(ihh2,ihm2,hs2)

      if (fd2.lt.fd1) fd2=fd2+24.d0

      exps=dabs(fd2-fd1)*3600.d0

      endif


      ENDIF

c

      fd=hmsgms(ihh1,ihm1,hs1+exps/2.d0)
      
      fd=fd/24.d0



c
c     Julian Date 
c

      call iau_CAL2JD (iutano,iutmes,iutdia,djm0,djm,iflag)

      djm=djm+fd
            
      dj=djm+djm0



c
c     Date (Gregorian)
c


 100  continue

c
c     Applying time offset
c

      dj=dj+offtim
      djm=djm+offtim


      call iau_jd2cal (dj1,djm,iutano,iutmes,iutdia,fd,j)

      hora=fd*24.d0
      iuth=hora
      iutm=(hora-iuth)*60.d0
      sut=((hora-iuth)*60.d0-iutm)*60.d0

      iexps=exps


c
c     (RA,DEC) extracted from WCS
c


      if (mras.eq.6 .or. mdec.eq.4) then

      xc=nx/2.d0
      yc=ny/2.d0


      x=xc-crpix1
      y=yc-crpix2


c

      if (iwcs.eq.2) then

      cd11=+cdelt1*dcos(crota1*grarad)
      cd12=-cdelt2*dsin(crota2*grarad)
      cd21=+cdelt1*dsin(crota1*grarad)
      cd22=+cdelt2*dcos(crota2*grarad)

      endif

c

      if (iwcs.ne.3) then

      xx=(cd11*x+cd12*y)*grarad
      yy=(cd21*x+cd22*y)*grarad

      else


      ico=1
      n=0
      l=0
      xx=0.d0

      
      do n=2,igrau
      do l=1,n
      ico=ico+1
      if (ico.gt.nox) go to 115
      k=n-l
      xx=xx+coefx(ico)*(x**k)*(y**(l-1))
      enddo
      enddo


 115  xx=xx*saox

      ico=1
      n=0
      l=0
      yy=0.d0

      
      do n=2,igrau
      do l=1,n
      ico=ico+1
      if (ico.gt.noy) go to 120
      k=n-l
      yy=yy+coefy(ico)*(y**k)*(x**(l-1))
      enddo
      enddo


 120  yy=yy*saoy

     

      endif

c

      c1=crval1*grarad
      c2=crval2*grarad

      rar=alff(xx,yy,c1,c2)
      der=deltt(rar,yy,c1,c2)

      ra=rar*radgra
      de=der*radgra


      ra=ra/15.d0
      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0


      isig='+'
      if (de.lt.0.d0) isig='-'
      de=dabs(de)


      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0



      endif

c


      return
 
      end





c
c
c
c     Subrotina cal2JD
c
c
      SUBROUTINE iau_CAL2JD ( IY, IM, ID, DJM0, DJM, J )
*+
*  - - - - - - - - - - -
*   i a u _ C A L 2 J D
*  - - - - - - - - - - -
*
*  Gregorian Calendar to Julian Date.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     IY,IM,ID    i     year, month, day in Gregorian calendar (Note 1)
*
*  Returned:
*     DJM0        d     MJD zero-point: always 2400000.5
*     DJM         d     Modified Julian Date for 0 hrs
*     J           i     status:
*                           0 = OK
*                          -1 = bad year   (Note 3: JD not computed)
*                          -2 = bad month  (JD not computed)
*                          -3 = bad day    (JD computed)
*
*  Notes:
*
*  1) The algorithm used is valid from -4800 March 1, but this
*     implementation rejects dates before -4799 January 1.
*
*  2) The Julian Date is returned in two pieces, in the usual SOFA
*     manner, which is designed to preserve time resolution.  The
*     Julian Date is available as a single number by adding DJM0 and
*     DJM.
*
*  3) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P.Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*  This revision:  2000 December 15
*
*  Copyright (C) 2001 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION DJM0, DJM
      INTEGER J, MY, IYPMY

*  Earliest year allowed (4800BC)
      INTEGER IYMIN
      PARAMETER ( IYMIN = -4799 )

*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Preset status.
      J = 0

*  Validate year.
      IF ( IY.LT.IYMIN ) THEN
         J = -1
      ELSE

*     Validate month.
         IF ( IM.GE.1 .AND. IM.LE.12 ) THEN

*        Allow for leap year.
            IF ( MOD(IY,4) .EQ. 0 ) THEN
               MTAB(2) = 29
            ELSE
               MTAB(2) = 28
            END IF
            IF ( MOD(IY,100).EQ.0 .AND. MOD(IY,400).NE.0 ) MTAB(2) = 28

*        Validate day.
            IF ( ID.LT.1 .OR. ID.GT.MTAB(IM) ) J = -3

*        Result.
            MY = ( IM - 14 ) / 12
            IYPMY = IY + MY
            DJM0 = 2400000.5D0
            DJM = DBLE( ( 1461 * ( IYPMY + 4800 ) ) / 4
     :                + (  367 * ( IM-2 - 12*MY ) ) / 12
     :                - (    3 * ( ( IYPMY + 4900 ) / 100 ) ) / 4
     :                + ID - 2432076)

*        Bad month
         ELSE
            J = -2
         END IF
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2001
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END










c
c     Subrotina jd2cal
c
c


      SUBROUTINE iau_jd2cal ( DJ1, DJ2, IY, IM, ID, FD, J )
*+
*  - - - - - - - - - - -
*   i a u _ J D 2 C A L
*  - - - - - - - - - - -
*
*  Julian Date to Gregorian year, month, day, and fraction of a day.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DJ1,DJ2     d     Julian Date (Notes 1, 2)
*
*  Returned:
*     IY          i     year
*     IM          i     month
*     ID          i     day
*     FD          d     fraction of day
*     J           i     status:
*                           0 = OK
*                          -1 = unacceptable date (Note 3)
*
*  Notes:
*
*  1) The earliest valid date is -68569.5 (-4900 March 1).  The
*     largest value accepted is 10^9.
*
*  2) The Julian Date is apportioned in any convenient way between
*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
*     be expressed in any of these ways, among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*  3) In early eras the conversion is from the "Proleptic Gregorian
*     Calendar";  no account is taken of the date(s) of adoption of
*     the Gregorian Calendar, nor is the AD/BC numbering convention
*     observed.
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P.Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 12.92 (p604).
*
*  This revision:  2000 December 19
*
*  Copyright (C) 2001 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

*  Minimum and maximum allowed JD
      DOUBLE PRECISION DJMIN, DJMAX
      PARAMETER ( DJMIN = -68569.5D0, DJMAX = 1D9 )

      INTEGER JD, L, N, I
      DOUBLE PRECISION DJ, D1, D2, F1, F2, F, D

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Check if date is acceptable.
      DJ = DJ1 + DJ2
      IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
         J = -1
      ELSE
         J = 0

*     Copy the date, big then small, and re-align to midnight.
         IF ( DJ1 .GE. DJ2 ) THEN
            D1 = DJ1
            D2 = DJ2
         ELSE
            D1 = DJ2
            D2 = DJ1
         END IF
         D2 = D2 - 0.5D0

*     Separate day and fraction.
         F1 = MOD(D1,1D0)
         F2 = MOD(D2,1D0)
         F = MOD(F1+F2,1D0)
         IF ( F .LT. 0D0 ) F = F+1D0
         D = ANINT(D1-F1) + ANINT(D2-F2) + ANINT(F1+F2-F)
         JD = NINT(D) + 1

*     Express day in Gregorian calendar.
         L = JD + 68569
         N = ( 4*L ) / 146097
         L = L - ( 146097*N + 3 ) / 4
         I = ( 4000 * (L+1) ) / 1461001
         L = L - ( 1461*I ) / 4 + 31
         J = ( 80*L ) / 2447
         ID = L - ( 2447*J ) / 80
         L = J / 11
         IM = J + 2 - 12*L
         IY = 100 * ( N-49 ) + I + L

         FD = F
         J = 0
      END IF

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2001
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END

