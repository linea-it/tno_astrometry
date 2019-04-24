c
c
c
c     Program PRAIA_astrometry
c
c
c     Purpose
c
c
c
c     Given a set of images, identifies catalogue stars and targets in the FOV,
c     determinates measurements (x, y) and raw PSF photometry of objects and
c     sky background, outputs (RA, Dec) positions, magnitudes and errors, and
c     many other information regardging the stars and targets. Everything is
c     done automatically.
c
c
c     The objects are automatically and very fastly identified in the CCD frame
c     by an original spiral search procedure. The regions surrounding the identified
c     objects are fitted by a PSF in order to obtain the photocenter, that is, the
c     (x,y) measurement. If the object is round-shaped, the PSF currently used is
c     a 2-D symetrical Gaussian. In this case, only pixels within 1 FWHM (seeing)
c     from the center are fitted in an iterative procedure. If it is a trace-shaped
c     object, a special PSF is fitted (see comments about this PSF further in the
c     text). The (x,y), PSF magnitude and other parameters from the PSF adjustments,
c     errors, etc, are stored and made available in the PRAIA output files.
c     
c
c
c     The catalog star identification is started with a 4-constant adjustment
c     with and without reflection in X. It is only necessary to furnish an
c     approximate pixel scale with an estimate of its error to carry on
c     the identification automatically. The (say N = 30) brightest measured stars
c     and the (say M = 100) brightest catalogue stars are used for the
c     identification. In the process the pixel scale is improved, as well as the
c     tangent plane polynomial model relating catalogue and measured coordinates,
c     from the 4-constant to the 1rst degree, then second and finally third - if
c     there are enough reference stars available, and regardless of the correct
c     or final model furnished by the user. The pixel scale and other parameters
c     are eventually and precisely determined at the end, after the final (RA,Dec)
c     reduction of the FOV with the tangent plane model choosen by the user.
c
c     In the case that all the images come from the same instrument setup, with
c     the same axis orientation and pixel scale, the user has the option of
c     using the adjusted parameters from the first image reduction (orientation
c     and scale) in the catalogue/star identifications of the remaining images,
c     considerably speeding up the process.
c       
c
c     The identification is now done starting with the stars from the GAIA DR1
c     (GAIA1) catalogue. From the (RA,Dec) reduction of the FOV with this catalogue,
c     after obtaining GAIA1-based (RA,Dec)s for all the objects in the FOV, the
c     identification of stars from the other catalogs is made by direct comparison
c     with these (RA,Dec)s.
c
c
c     Only FITS images are treated. Littleendian or bigendian, bitpix 16, 32, -32,
c     64, -64 are all accepted. All standard WCS and FITS header keys apply. No
c     external packages are necessary for reading FITS images. PRAIA_astrometry, as
c     the other tasks of the package, is an auto-sufficient stand-alone FORTRAN 77
c     program with subroutines.      
c
c
c     Bad pixel masks can be furnished for each individual image and/or for all
c     the images being proccessed. The format is the same as the line format of IRAF.
c
c
c     In this version, UCAC4, UCAC5 and 2MASS stars are extracted using accelerator
c     indexes. No accelerator index is available for the GAIA1 catalogue in this
c     version.
c
c
c     Besides the catalogues UCAC4, UCAC5 and GAIA versions (see text ahead), the
c     user can also use a "User Catalogue" for obtaining (RA,Dec) in his/her catalogue
c     system.
c
c
c     The user can externally setup CCD regions for the detection of objects,
c     overcoming the automatic object identification. It is particularly useful when
c     the user notices that, for some reason, a target was not identified in a
c     previous run. The files are in DS9 format. PRAIA also outputs files in DS9
c     format (.reg files) indicating the objects measured in the FOV and the region
c     used for the PSF fitting for getting the (x,y) measurements.
c
c  
c     In this version, trace-images (asteroids, NEAs, artifical satellites, meteors)
c     of any length and orientation are automatically detected and measured among
c     round-shaped, stellar-like images (stars, etc) in the FOV. A special analytical
c     PSF model was devised for this purpose. The user can also externally setup
c     CCD regions for the detection of trace-images of any length and orientation
c     in the CCD field, among those for round-shaped objects, in the same file. 
c
c
c     In this version, (RA, Dec) re-reduction from previous (RA,Dec) reductions can
c     be performed, based on previous PRAIA files containing (x,y) measurements or
c     simply previously reduced (RA,Dec)s (tangent plane technique). This incorporates,
c     with improvements, the old separate PRAIA_redo_astrometry task. 
c
c
c     In this version, more FDPs (Field Distortion Pattern) options are available. For
c     instance, it is now allowed to read (x,y) distortion maps directly from the FITS
c     headers from individual CCD images of the DESCAM (DES). Also, FDP can now be
c     passed in polynomial form or from a file in (x,y) or (RA,Dec) units.
c
c
c     In this version, the are now files with target positions output in MPC and NIMA
c     ingestion formats for orbit fitting.
c
c
c     In this version, the 2MASS catalogue is no longer used for (RA,Dec) reductions.
c     It is only used for extraction of (J, H, K) infrared magnitudes.
c
c
c     In this version, (RA,Dec) reductions are made with the GAIA1 (GAIA DR1 without
c     proper motions) and, for comparison purposes, with the UCAC4 catalogue.
c
c
c     The new UCAC5 catalogue with positions and proper motions in the GAIA system is
c     now also available.
c
c
c     In this version, (RA,Dec) reductions are made with many versions of the GAIA
c     catalogue, including combinations with the UCAC4, in order to overcome the
c     absense of proper motions in the GAIA1. Seven options or GAIA versions are
c     available:
c
c
c
c     - GAIA_1: complete GAIA DR1, without proper motions, except of course for TGAS
c               (Tycho-GAIA) stars;
c
c     - GAIA_2: GAIA_1 stars within a given magnitude range set by the user;
c
c     - GAIA_3: GAIA_1 stars with proper motions borrowed from UCAC4, except for TGAS
c               stars, of course, for which the original proper motions are mantained;
c
c     - GAIA_4: GAIA_1 stars with proper motions for common UCAC4/GAIA stars, except
c               for TGAS stars; different from GAIA_3, here the proper motions are
c               computed using the mean UCAC4 position (first epoch) and the J2015.0
c               GAIA1 position (second epoch);
c
c     - GAIA_5: same as GAIA_3, except that only stars with proper motions are used
c               in the (RA,Dec) reductions;
c
c     - GAIA_6: same as GAIA_4, except that only stars with proper motions are used
c               in the (RA,Dec) reductions;
c
c     - GAIA_7 same as GAIA_5 or GAIA_6, except that no UCAC4-related proper motions
c              are used in the (RA,Dec) reductions, except proper motions of TGAS
c              stars; serves as control for the results obtained with the GAIA_5 and
c              GAIA_6 solutions.
c
c
c
c      Last update: Marcelo Assafin - 07 Sept 2017
c   
c
c


      IMPLICIT REAL *8 (A-H,O-Z)
      parameter (stdin=5,idiobs=150000,ipmax=5001,icofsp=21,ng=10,
     ?idin=150,idin50=50,idin2=20000,iu4z=900,iu4i=1440,iu5z=900,
     ?iu5i=1440,nhist=10,nhisfw=10,jfdp=10000,jjfdp=10,kfund=150000)


      integer*2 imagem(ipmax,ipmax)
      real*4 pixmat(ipmax,ipmax)
      real*4 pixel(ipmax,ipmax)
      integer*2 bitpix,bitpyx,betpix


      dimension contag(idiobs),histo(nhist),ico(nhist),ior(idiobs),
     ?nval(idiobs),ior1(idiobs),nval1(idiobs),val(idiobs)

      dimension xob(idiobs),yob(idiobs),xoob(idiobs),yoob(idiobs),
     ?ilado(idiobs),seng(idiobs),iflag(idiobs),altu(idiobs),
     ?ialtu(idiobs)

      dimension xest(idiobs),yest(idiobs),xp(idiobs),yp(idiobs),
     ?coefx(idiobs)


      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),bcg(idiobs),
     ?bcgs(idiobs),dan(idiobs),exce(idiobs)

      dimension xctra(idiobs),yctra(idiobs),dmalat(idiobs),
     ?dmilat(idiobs),agtra(idiobs),bcgt(idiobs),bcgst(idiobs)


      dimension exgcc(idiobs),eygcc(idiobs),sgcc(idiobs),fgcc(idiobs)

      dimension xold(idiobs),yold(idiobs)


c
c     2MASS-related variables
c

      dimension ra2ma(idiobs),de2ma(idiobs),era2ma(idiobs),
     ?ede2ma(idiobs),dmgj(idiobs),dmgh(idiobs),dmgk(idiobs),
     ?emgj(idiobs),emgh(idiobs),emgk(idiobs),xra2ma(idiobs),
     ?yde2ma(idiobs),id2ma(idiobs),ddj2(idiobs)



c
c     UCAC4-related variables
c

      dimension rauc4(idiobs),deuc4(idiobs),erauc4(idiobs),
     ?edeuc4(idiobs),pmra4(idiobs),pmde4(idiobs),epmra4(idiobs),
     ?epmde4(idiobs),udmgj4(idiobs),udmgh4(idiobs),udmgk4(idiobs),
     ?udmg4(idiobs),cudmg4(idiobs),iduc4(idiobs),xrauc4(idiobs),
     ?ydeuc4(idiobs),era4(idiobs),ede4(idiobs),alfre4(idiobs),
     ?delre4(idiobs),coefx4(icofsp),coefy4(icofsp),ecofx4(icofsp),
     ?ecofy4(icofsp),itira4(idiobs),edmgj4(idiobs),edmgh4(idiobs),
     ?edmgk4(idiobs),rame4(idiobs),deme4(idiobs),eramc4(idiobs),
     ?edemc4(idiobs),epora4(idiobs),epode4(idiobs)


c
c     UCAC5-related variables
c

      dimension rauc5(idiobs),deuc5(idiobs),erauc5(idiobs),
     ?edeuc5(idiobs),pmra5(idiobs),pmde5(idiobs),epmra5(idiobs),
     ?epmde5(idiobs),udmgj5(idiobs),udmgh5(idiobs),udmgk5(idiobs),
     ?udmg5(idiobs),cudmg5(idiobs),iduc5(idiobs),xrauc5(idiobs),
     ?ydeuc5(idiobs),era5(idiobs),ede5(idiobs),alfre5(idiobs),
     ?delre5(idiobs),coefx5(icofsp),coefy5(icofsp),ecofx5(icofsp),
     ?ecofy5(icofsp),itira5(idiobs),edmgj5(idiobs),edmgh5(idiobs),
     ?edmgk5(idiobs)


c
c     User-catalogue-related variables
c



      dimension raucs(idiobs),deucs(idiobs),eraucs(idiobs),
     ?edeucs(idiobs),pmras(idiobs),pmdes(idiobs),epmras(idiobs),
     ?epmdes(idiobs),udmgjs(idiobs),udmghs(idiobs),udmgks(idiobs),
     ?edmgjs(idiobs),edmghs(idiobs),edmgks(idiobs),udmgs(idiobs),
     ?cudmgs(idiobs),iducs(idiobs),itiras(idiobs),eras(idiobs),
     ?edes(idiobs),alfres(idiobs),delres(idiobs),coefxs(icofsp),
     ?coefys(icofsp),ecofxs(icofsp),ecofys(icofsp),xraucs(idiobs),
     ?ydeucs(idiobs)


c
c     GAIA_1-related variables
c


      dimension rag1(idiobs),deg1(idiobs),erag1(idiobs),
     ?edeg1(idiobs),pmag1(idiobs),pmdg1(idiobs),epmag1(idiobs),
     ?epmdg1(idiobs),g1mgj(idiobs),g1mgh(idiobs),g1mgk(idiobs),
     ?g1emgj(idiobs),g1emgh(idiobs),g1emgk(idiobs),ug1mgg(idiobs),
     ?cg1mgg(idiobs),idga1(idiobs),itirg1(idiobs),erasg1(idiobs),
     ?edesg1(idiobs),alfrg1(idiobs),delrg1(idiobs),g1cofx(icofsp),
     ?g1cofy(icofsp),g1ecfx(icofsp),g1ecfy(icofsp),xrag1(idiobs),
     ?ydeg1(idiobs)

   


c
c     GAIA_2-related variables
c


      dimension idga2(idiobs),ug2mgg(idiobs),itirg2(idiobs),
     ?erasg2(idiobs),edesg2(idiobs),alfrg2(idiobs),delrg2(idiobs),
     ?g2cofx(icofsp),g2cofy(icofsp),g2ecfx(icofsp),g2ecfy(icofsp),
     ?xrag2(idiobs),ydeg2(idiobs)



c
c     GAIA_3-related variables
c



      dimension rag3(idiobs),deg3(idiobs),erag3(idiobs),edeg3(idiobs),
     ?pmag3(idiobs),pmdg3(idiobs),epmag3(idiobs),epmdg3(idiobs),
     ?itirg3(idiobs),erasg3(idiobs),edesg3(idiobs),alfrg3(idiobs),
     ?delrg3(idiobs),g3cofx(icofsp),g3cofy(icofsp),g3ecfx(icofsp),
     ?g3ecfy(icofsp),xrag3(idiobs),ydeg3(idiobs)




c
c     GAIA_4-related variables
c



      dimension rag4(idiobs),deg4(idiobs),erag4(idiobs),edeg4(idiobs),
     ?pmag4(idiobs),pmdg4(idiobs),epmag4(idiobs),epmdg4(idiobs),
     ?itirg4(idiobs),erasg4(idiobs),edesg4(idiobs),alfrg4(idiobs),
     ?delrg4(idiobs),g4cofx(icofsp),g4cofy(icofsp),g4ecfx(icofsp),
     ?g4ecfy(icofsp),xrag4(idiobs),ydeg4(idiobs)




c
c     GAIA_5-related variables
c



      dimension itirg5(idiobs),erasg5(idiobs),edesg5(idiobs),
     ?alfrg5(idiobs),delrg5(idiobs),g5cofx(icofsp),g5cofy(icofsp),
     ?g5ecfx(icofsp),g5ecfy(icofsp),xrag5(idiobs),ydeg5(idiobs),
     ?idga5(idiobs),ug5mgg(idiobs)




c
c     GAIA_6-related variables
c



      dimension itirg6(idiobs),erasg6(idiobs),edesg6(idiobs),
     ?alfrg6(idiobs),delrg6(idiobs),g6cofx(icofsp),g6cofy(icofsp),
     ?g6ecfx(icofsp),g6ecfy(icofsp),xrag6(idiobs),ydeg6(idiobs)




c
c     GAIA_7-related variables
c



      dimension itirg7(idiobs),erasg7(idiobs),edesg7(idiobs),
     ?alfrg7(idiobs),delrg7(idiobs),g7cofx(icofsp),g7cofy(icofsp),
     ?g7ecfx(icofsp),g7ecfy(icofsp),xrag7(idiobs),ydeg7(idiobs)


c
c     Proper motion computation variables using (x,y,z) direction vectors
c

      dimension v1(6),v2(6)



c
c    Remake astrometry related variables
c


      dimension cmgu(idiobs),omag(idiobs),xra(idiobs),yde(idiobs)



      dimension coefxr(icofsp),coefyr(icofsp)


c
c     FDP related variables
c


      dimension adx(jfdp),ady(jfdp),coordx(jfdp),coordy(jfdp),cdx(2),
     ?cdy(2),xfdp(idiobs),yfdp(idiobs)


c
c     Catalogue acceleration index related variables
c

      dimension inuu4(iu4z,iu4i),ir1u4(iu4z,iu4i),ir2u4(iu4z,iu4i)

      dimension inuu5(iu5z,iu5i),ir1u5(iu5z,iu5i),ir2u5(iu5z,iu5i)


c
c     (x,y) measuring related variables
c

 
      dimension xcir(idiobs),ycir(idiobs),lacir(idiobs),bcgc(idiobs),
     ?bcgsc(idiobs)

      dimension xtra(idiobs),ytra(idiobs),xlatra(idiobs),ylatra(idiobs),
     ?angtra(idiobs),bcgtr(idiobs),bcgstr(idiobs)
   
      dimension volum(idiobs),angle(idiobs)


c
c     File name related variables
c


      character*150 infits,imfits,names(idin2),ids9,ibadpx,kbadpx,imes,
     ?ires,inxy


      character*200 iredu4,iredu5,iredus,iredg1,iredg2,iredg3,iredg4,
     ?iredg5,iredg6,iredg7


      character*50 centro,ialvos,ialvu4,ialvu5,ialvus,ialvg1,ialvg2,
     ?ialvg3,ialvg4,ialvg5,ialvg6,ialvg7,nalvu4,nalvu5,nalvus,nalvg1,
     ?nalvg2,nalvg3,nalvg4,nalvg5,nalvg6,nalvg7,malvu4,malvu5,malvus,
     ?malvg1,malvg2,malvg3,malvg4,malvg5,malvg6,malvg7,lredu4,lredu5,
     ?lredus,lredg1,lredg2,lredg3,lredg4,lredg5,lredg6,lredg7


c
c     NIMA,MPC output format related variables
c

      character*1  obtipo
      character*1  band
      character*3  iau
      character*1  icat,icato



      character*50 fotrel,redru4,redru5,redrus,redrg1,redrg2,redrg3,
     ?redrg4,redrg5,redrg6,redrg7,ifdp

      character *50 mraiz,u4raiz,u5raiz,raizg1,cpraia
      character *61 u4ind,u5ind

      character*50 subf

      character*1  menos,iver,nome(idin50),isig,ibrac,obtype(idiobs)
      character*69 ichobj,ihname(idin2),mchobj
      character*20 ichfil

      character*200 linha

      character*8 kadx(jjfdp),kady(jjfdp),kcrpx,kcrpy,kcdx(2),kcdy(2)



      data ibrac/' '/
      data menos/'-'/

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      dj2000=2451545.d0

      epog1=2015.d0

      tt0=2.8d-3

      izero=0
      zero=0.d0

      d99=99.999d0

      ireflex=+1
 
c

      um=1.d0
      do i=1,6
      v1(i)=0.d0
      v2(i)=0.d0
      enddo


c

      do i=1,idiobs
      contag(i)=0.d0
      enddo

      do i=1,idiobs
      ior(i)=0
      enddo



c
c     Zeroing imagem, pixmat and matpix (image matrices' variables)
c

      do i=1,ipmax
      do j=1,ipmax
      pixmat(j,i)=0.
      enddo
      enddo

c
c     Defines the correct number of backspaces to safely apply a 1-line backspace
c     in an open file by FORTRAN.
c


      call backsp (1,nbac,91)

   

c
c     Reads input dada
c

c     open (1,file='PRAIA_astrometry_30_04.dat')

      read (*,3) mraiz
      read (*,3) u4raiz
      read (*,3) u5raiz
      read (*,3) raizg1

      read (*,*) aux1
      read (*,*) aux2

      g2mag2=dmax1(aux1,aux2)
      g2mag1=dmin1(aux1,aux2)

      read (*,3) cpraia


c
c     Checks if a user reference catalogue is present.
c
c     If it is present, a (RA,Dec) reduction with the user catalogue
c     will be automatically attempted (iuserc=1), otherwise not
c     (iuserc=2).
c
c

      iuserc=2

      open (77,file=cpraia,status='old',err=705)

      read (77,702,err=705,end=705) iah,iam,sa,isig,idg,idm,sd,ex,ey,dj,
     ?pma,pmd,epma,epmd,cudmg,cudmg2

 702  format(i2,1x,i2,1x,f7.4,2x,a1,i2,1x,i2,1x,f6.3,2x,2f7.3,2x,f16.8,
     ?1x,4(1x,f7.3),2(1x,f6.3))


      iuserc=1


 705  close (77)

c

      read (*,*) iastro

      read (*,3) centro

      read (*,3) ialvos

      read (*,*) mfdp

      read (*,3) ifdp
      read (*,*) kfdp

      read (*,*) lfdp

  
      read (*,3) kbadpx

      read (*,3) fotrel

      read (*,3) redru4
      read (*,3) redru5
      read (*,3) redrg1
      read (*,3) redrg2
      read (*,3) redrg3
      read (*,3) redrg4
      read (*,3) redrg5
      read (*,3) redrg6
      read (*,3) redrg7
      read (*,3) redrus


      read (*,3) ialvu4
      read (*,3) ialvu5
      read (*,3) ialvg1
      read (*,3) ialvg2
      read (*,3) ialvg3
      read (*,3) ialvg4
      read (*,3) ialvg5
      read (*,3) ialvg6
      read (*,3) ialvg7
      read (*,3) ialvus


      read (*,3) malvu4
      read (*,3) malvu5
      read (*,3) malvg1
      read (*,3) malvg2
      read (*,3) malvg3
      read (*,3) malvg4
      read (*,3) malvg5
      read (*,3) malvg6
      read (*,3) malvg7
      read (*,3) malvus


      read (*,3) nalvu4
      read (*,3) nalvu5
      read (*,3) nalvg1
      read (*,3) nalvg2
      read (*,3) nalvg3
      read (*,3) nalvg4
      read (*,3) nalvg5
      read (*,3) nalvg6
      read (*,3) nalvg7
      read (*,3) nalvus


      read (*,'(a1)') obtipo
      read (*,'(a1)') band
      read (*,'(a3)') iau
      read (*,'(a1)') icat



      read (*,3) lredu4
      read (*,3) lredu5
      read (*,3) lredg1
      read (*,3) lredg2
      read (*,3) lredg3
      read (*,3) lredg4
      read (*,3) lredg5
      read (*,3) lredg6
      read (*,3) lredg7
      read (*,3) lredus


      read (*,*) tbox

      read (*,*) box
      read (*,*) scala
      read (*,*) ecala


      read (*,*) mix

      if (mix.ne.1 .and. mix.ne.2) mix=1


  
      read (*,*) vmin
      read (*,*) ipflag
      read (*,*) bscale
      read (*,*) bzero
      read (*,*) bitpyx
      read (*,*) kswap

      read (*,*) nswap

      read (*,*) ifittr

      read (*,*) elimt

      read (*,*) fmin
      read (*,*) fmax

      read (*,*) lperc
      read (*,*) lpert


      read (*,*) fatceu
      read (*,*) fapiro
      read (*,*) dlimro
      read (*,*) plimro

      plimro=plimro/scala

      read (*,*) icorox
      read (*,*) icorot


      read (*,*) fotceu
      read (*,*) fapitr
      read (*,*) dlimtr
      read (*,*) plimtr

      plimtr=plimtr/scala


      read (*,*) icotr


      read (*,*) nbcat
      read (*,*) nbmed

      read (*,*) expfov

      read (*,*) barx
      read (*,*) bary

      read (*,*) erpix
      read (*,*) pcoru4
      read (*,*) pcoru5
      read (*,*) pcorg1
      read (*,*) pcorg2
      read (*,*) pcorg3
      read (*,*) pcorg4
      read (*,*) pcorg5
      read (*,*) pcorg6
      read (*,*) pcorg7
      read (*,*) pcorus
      read (*,*) ngrau
      read (*,*) ngrau3
      read (*,*) ngrau5

      read (*,*) inicio
      read (*,*) iultmo

      read (*,*) itx
      read (*,*) ity


 3    format(a50)


c     close (1)



      eoff=eoff/scala
      erpix=erpix/scala

      fmin=fmin/scala
      fmax=fmax/scala

      carx=grarad*barx/60.d0
      cary=grarad*bary/60.d0



c
c
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '
      write (*,1)
 1    format (23x,'PRAIA - astrometric and photometric setup')
c
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '
 

c
 
      rewind (5)


 2    continue


      read (*,5) linha
 5    format(a200)

      write (*,5) linha

      if (linha(1:1).ne.'*') go to 2


      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '



c
c     Organizing the fields to be treated
c


      open (1,file=centro)


      i=0
 4    read (1,*,end=2028)
      i=i+1
      go to 4

 2028 close (1)

      iul=i

      if (iul.eq.0) then
      write (*,*) ' '
      write (*,*) 'No field data furnished. Exiting program.'
      write (*,*) ' '
      stop
      endif
      

c
      
      if (inicio.eq.0 .and. iultmo.eq.0) then

      inicio=1
      iultmo=iul


      else

      if (inicio.gt.iultmo) then
      write (*,*) ' '
      write (*,*) 'Initial/Final field data file ranks do not match. Exi
     ?ting program.'
      write (*,*) ' '
      stop
      endif


      if (inicio.gt.iul .or. inicio.lt.1) then
      write (*,*) ' '
      write (*,*) 'Initial field data file outside list range. Exiting p
     ?rogram.'
      write (*,*) ' '
      stop
      endif

      if (iultmo.gt.iul .or. iultmo.lt.1) then
      write (*,*) ' '
      write (*,*) 'Final field data file outside list range. Exiting pro
     ?gram.'
      write (*,*) ' '
      stop
      endif

      endif




c
c     Stores file names and object names
c


      open (3,file=centro)


      do i=1,inicio-1
      read(3,*,end=200)
      enddo

      do 190 i=inicio,iultmo

      names(i)=''
      ihname(i)=''


      if (iastro.eq.0) then

      read(3,402,err=194,end=200) iah,iam,sa,isig,idg,idm,ds,iuth,
     ?iutm,sut,iutano,iutmes,iutdia,djm,dj,iexps,ichfil,names(i),mchobj

 402  format(1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,f16.8,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20)


      ihname(i)=mchobj


      else


      read(3,91,err=194,end=200) names(i)

      open(92,file=names(i))

      read (92,470,err=196,end=196) xob(i),yob(i),seng(i),altu(i),
     ?fgcc(i),fumag,fumag2,xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,res2mg,
     ?resmg2,ermgj,ermgh,ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,
     ?delsiu,nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,dj,iexps,ichfil,imfits,mchobj,nx,ny

 470  format(2(1x,f7.2),1x,f5.3,2(f10.2),13(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
     ?2(1x,i5))


      ihname(i)=mchobj

      close (92)


      endif



 190  continue


      go to 200


c

 194  write (*,195) names(i)
 195  format(1x,'Reading error on field data file list: ',a200)
      write (*,*)
      write (*,*)'Exiting program.'

      close (3)

      stop

c

 196  write (*,195) names(i)
      write (*,*)
      write (*,*)'Exiting program.'

      close (92)
      close (3)

      stop

c

 200  continue


      close (3)




c
c     Common root end name of input xy files (remake astrometry mode only)
c
c     It is assumed that the xy file names stand in the form like:
c
c
c       *.cat.red.xy
c
c
c     where "cat" is any catalog (ex>: cat=ucac4, cat=wfi, cat=gaia1, etc).
c    
c


      if (iastro.ne.0) then


      imfits=''

      imfits=names(1)

      k=0
      m=151

      do i=150,1,-1
      if (imfits(i:i).eq.' ') m=i
      if (imfits(i:i).eq.'.') k=k+1
      if (k.eq.3) go to 161
      enddo 

 161  inxy=''

      inxy=imfits(i:m-1)

      knxy=m-i


      endif


c
c     Stores acceleration indexes for UCAC4 reading
c

      u4ind=''
      u4ind=u4raiz


      do l=1,idin50
      if (u4ind(l:l).eq.' ') go to 163
      enddo

 163  u4ind(l:l+10)='u4index.asc'


      open (3,file=u4ind)


      do i=1,iu4z
      do j=1,iu4i
      read(3,*) naz,nsbin
      inuu4(i,j)=nsbin
      ir1u4(i,j)=naz+1
      ir2u4(i,j)=ir1u4(i,j)+nsbin-1
      enddo
      enddo

      close (3)



c
c     Stores acceleration indexes for UCAC5 reading
c

      u5ind=''
      u5ind=u5raiz


      do l=1,idin50
      if (u5ind(l:l).eq.' ') go to 165
      enddo

 165  u5ind(l:l+10)='u5index.asc'



      open (3,file=u5ind)


      do i=1,iu5z
      do j=1,iu5i
      read(3,*) naz,nsbin
      inuu5(i,j)=nsbin
      ir1u5(i,j)=naz+1
      ir2u5(i,j)=ir1u5(i,j)+nsbin-1
      enddo
      enddo

      close (3)



c
c     Initiates the astrometry
c



      scl=0.d0
      scl2=0.d0
      nscl=0
 

      do i=1,icofsp
      coefxr(i)=0.d0
      coefyr(i)=0.d0
      enddo


c
c     Super loop of all-fields astrometry
c


      do 60 lllll=inicio,iultmo


c


      iredu4=''
      iredu5=''
      iredus=''
      iredg1=''
      iredg2=''
      iredg3=''
      iredg4=''
      iredg5=''
      iredg6=''
      iredg7=''


      infits=''
      infits=names(lllll)


c
c     Mounts names of input and output field files
c


      if (iastro.eq.0) then

      do ii=1,idin
      if (infits(ii:ii+5).eq.'.fits ') go to 2000
      if (infits(ii:ii+5).eq.'.FITS ') go to 2000
      if (infits(ii:ii+4).eq.'.fts ') go to 2000
      if (infits(ii:ii+4).eq.'.FTS ') go to 2000
      if (infits(ii:ii+4).eq.'.fit ') go to 2000
      if (infits(ii:ii+4).eq.'.FIT ') go to 2000
      if (infits(ii:ii).eq.ibrac) go to 2000
      enddo

      else

      do ii=1,idin
      if (infits(ii:ii+knxy-1).eq.inxy) go to 2000
      if (infits(ii:ii).eq.ibrac) go to 2000
      enddo

      endif


c


 2000 continue

      ii=ii-1

      kkii=ii

c


      iredu4(1:ii)=infits(1:ii)
      iredu5(1:ii)=infits(1:ii)
      iredus(1:ii)=infits(1:ii)
      iredg1(1:ii)=infits(1:ii)
      iredg2(1:ii)=infits(1:ii)
      iredg3(1:ii)=infits(1:ii)
      iredg4(1:ii)=infits(1:ii)
      iredg5(1:ii)=infits(1:ii)
      iredg6(1:ii)=infits(1:ii)
      iredg7(1:ii)=infits(1:ii)




      iredu4(ii+1:ii+1)='.'
      iredu5(ii+1:ii+1)='.'
      iredus(ii+1:ii+1)='.'
      iredg1(ii+1:ii+1)='.'
      iredg2(ii+1:ii+1)='.'
      iredg3(ii+1:ii+1)='.'
      iredg4(ii+1:ii+1)='.'
      iredg5(ii+1:ii+1)='.'
      iredg6(ii+1:ii+1)='.'
      iredg7(ii+1:ii+1)='.'



      
c
c     GAIA_1
c

      do iii=idin50,1,-1
      if (lredg1(iii:iii).ne.ibrac) go to 2001
      enddo

 2001 continue

      iredg1(ii+2:ii+1+iii)=lredg1



      
c
c     GAIA_2
c

      do iii=idin50,1,-1
      if (lredg2(iii:iii).ne.ibrac) go to 2002
      enddo

 2002 continue

      iredg2(ii+2:ii+1+iii)=lredg2



      
c
c     GAIA_3
c

      do iii=idin50,1,-1
      if (lredg3(iii:iii).ne.ibrac) go to 2003
      enddo

 2003 continue

      iredg3(ii+2:ii+1+iii)=lredg3



c
c     GAIA_4
c

      do iii=idin50,1,-1
      if (lredg4(iii:iii).ne.ibrac) go to 2004
      enddo

 2004 continue

      iredg4(ii+2:ii+1+iii)=lredg4



c
c     GAIA_5
c

      do iii=idin50,1,-1
      if (lredg5(iii:iii).ne.ibrac) go to 2005
      enddo

 2005 continue

      iredg5(ii+2:ii+1+iii)=lredg5



c
c     GAIA_6
c

      do iii=idin50,1,-1
      if (lredg6(iii:iii).ne.ibrac) go to 2006
      enddo

 2006 continue

      iredg6(ii+2:ii+1+iii)=lredg6




c
c     GAIA_7
c

      do iii=idin50,1,-1
      if (lredg7(iii:iii).ne.ibrac) go to 2007
      enddo

 2007 continue

      iredg7(ii+2:ii+1+iii)=lredg7



      
c
c     UCAC4
c

      do iii=idin50,1,-1
      if (lredu4(iii:iii).ne.ibrac) go to 2008
      enddo

 2008 continue

      iredu4(ii+2:ii+1+iii)=lredu4



      
c
c     UCAC5
c

      do iii=idin50,1,-1
      if (lredu5(iii:iii).ne.ibrac) go to 2009
      enddo

 2009 continue

      iredu5(ii+2:ii+1+iii)=lredu5


      
c
c     User reference catalog
c

      do iii=idin50,1,-1
      if (lredus(iii:iii).ne.ibrac) go to 2010
      enddo

 2010 continue

      iredus(ii+2:ii+1+iii)=lredus

c     

 91   format(a150)
 11   format(a50)
 12   format(50a1)
 92   format(150a1)
 93   format(a200)



c
c     Bad pixels file name
c

      ibadpx=''

      ibadpx(1:kkii)=infits(1:kkii)
      ibadpx(kkii+1:kkii+4)='.bpx'



c
c     ds9 regions file name 
c

      ids9=''

      ids9(1:kkii)=infits(1:kkii)
      ids9(kkii+1:kkii+4)='.reg'


c
c     File names of objects and targets (round and trace-shaped)
c     in ds9 format input by the user for (x,y) measurements
c

      imes=''
      imes(1:kkii)=infits(1:kkii)
      imes(kkii+1:kkii+4)='.mes'
      ires=''
      ires=imes
      ires(kkii+1:kkii+4)='.res'



c
c     write (*,91) infits
c     write (*,93) iredu4
c     write (*,93) iredu5
c     write (*,93) iredg1
c     write (*,93) iredg2
c     write (*,93) iredg3
c     write (*,93) iredg4
c     write (*,93) iredg5
c     write (*,93) iredg6
c     write (*,93) iredg7
c     write (*,93) iredus
c     write (*,91) ids9
c     write (*,91) ibadpx
c     stop
c



c

      write (*,*)
      write (*,*)


      if (iastro.eq.0) then

      write (*,14) lllll,iultmo,infits
 14   format (1x,'Proccessing field ',i5,' of ',i5,': file = ',a150)

      else

      write (*,14) lllll,iultmo,infits(1:kkii)

      endif


      write (*,*)

c
c     Estimates processing time consumption
c

      if (lllll.ne.inicio) then

      tt=(iultmo-lllll+1)*(tt+tt0)

      tt=tt/24.d0

      iday=tt

      hour=(tt-iday)*24.d0
      ihour=hour
      minu=(hour-ihour)*60.d0
      seg=((hour-ihour)*60.d0-minu)*60.d0

      write (*,*)      
      write (*,16) iday,ihour,minu,seg
 16   format(1x,'Estimated time left for end of PRAIA reductions: ',
     ?i3,'days ',i2,'hs ',i2,'m ',f4.1,'s')  
      write (*,*)      

      endif

c
c     Initializing execution time routines for time consuption estimates
c     for the reductions with PRAIA
c

      tempoi=0.d0
      call tempo (tempoi,tempot,tempop)

      if (lllll.ne.inicio) then

      seg=tt0*3600.d0

      write (*,*)      
      write (*,17) seg
 17   format(1x,'Estimated time for identification and measuring of all 
     ?objects: ',f5.1,'seconds')  
      write (*,*)      

      endif



c
c     Field Distortion Pattern. Corrects field distortions by applying
c     (dx,dy) offsets for the (x,y) measurements, or (dRA,dDec) offsets
c     for the (RA,Dec)s (remaking astrometry mode 3 tangent plane technique).
c
 
      nfdp=0

c


      kcrpx=''
      kcrpy=''


      do i=1,2
      kcdx(i)=''
      kcdy(i)=''
      cdx(i)=0.d0
      cdy(i)=0.d0
      enddo


      do i=1,jjfdp
      kadx(i)=''
      kady(i)=''
      enddo


      do i=1,jfdp
      adx(i)=0.d0
      ady(i)=0.d0
      coordx(i)=0.d0
      coordy(i)=0.d0
      enddo


      do i=1,idiobs
      xfdp(i)=0.d0
      yfdp(i)=0.d0
      enddo

c

      open (23,file=ifdp,status='old',err=280)


c
c     FDP mode 1.
c
c     Gives the mask with (dx,dy) in pixels as a function of (x,y)s or
c     (dRA,dDec) in arcseconds as a function of (RA,Dec)
c


      if (mfdp.eq.1) then

      do i=1,jfdp
      read (23,*,end=275) adx(i),ady(i),coordx(i),coordy(i)
      enddo

      endif


c
c     FDP mode 2.
c
c     Gives directly the coefficients in (x,y) in pixels or (RA,Dec) in
c     arcseconds describing the 3rd degree bivariate distortion polynomial model.
c
c     Here the 3rd degree distortion model follows the astrometric convention:
c
c
c     Pol(x)=a1+a2x+a3y+a4x**2+a5x*y+a6y**2+a7x**3+a8x**2y+a9x*y**2+a10y**3
c
c     Pol(y)=b1+b2x+b3y+b4x**2+b5x*y+b6y**2+b7x**3+b8x**2y+b9x*y**2+b10y**3
c



      if (mfdp.eq.2) then

      do i=1,jjfdp
      read (23,*,end=275) adx(i),ady(i)
      enddo

      endif


c
c
c     FDP mode 3. The 3rd degree bivariate distortion polynomial model coefficients
c     are furnished through WCS FITS header keys.
c
c     Here the 3rd degree distortion model do not follow the astrometric convention.
c     It follows the WCS FITS header convention:
c
c
c     Pol(x)=a1+a2x+a3y+a4x**2+a5x*y+a6y**2+a7x**3+a8x**2y+a9x*y**2+a10y**3
c
c     Pol(y)=b1+b2y+b3x+b4y**2+b5y*x+b6x**2+b7y**3+b8y**2x+b9y*x**2+b10x**3
c
c
c     It is assumed that Pol describes distortions as a function of (x,y) in pixels.
c
c

      if (mfdp.eq.3) then

      do i=1,jjfdp
      read (23,*,end=275) kadx(i),kady(i)
      enddo

      endif


c
c
c     FDP mode 4. DES DECAM procedure. 
c
c     The 3rd degree bivariate distortion polynomial model coefficients
c     are furnished through WCS FITS header keys. As in mode 3, it does not follow
c     the astrometric convention. It follows the WCS FITS header convention.
c
c     Extra coefficiennts are furnished for characterizing the coordinates of
c     the central pixel and scale and rotation of axex, prior for applying the
c     distortion polynomial corrections.
c
c

      if (mfdp.eq.4) then

      do i=1,jjfdp
      read (23,*,end=275) kadx(i),kady(i)
      enddo

      read (23,*,end=275) kcrpx,kcrpy


      do j=1,2
      read (23,*,end=275) kcdx(j),kcdy(j)
      enddo


      endif

c


 275  nfdp=i-1

 280  close (23)



c
c     Astrometry of fits images
c


      if (iastro.ne.0) go to 150



c
c     Reads fits image and stores it into a (nx,ny) matrix.
c

      bitpix=bitpyx

      write (*,*)

      call refits (ipmax,pixmat,infits,nx,ny,nheads,ichobj,ipflag,
     ?bscale,bzero,kswap,iswap,nswap,bitpix,mfdp,jfdp,jjfdp,nfdp,adx,
     ?ady,kadx,kady,crpx,crpy,cdx,cdy,kcrpx,kcrpy,kcdx,kcdy)

      ichobj=ihname(lllll)


      write (*,*)


c
c     Initializes auxiliary image matrix which marks pixels to be
c     excluded from the object identification proccess.
c


      do i=1,ny
      do j=1,nx
      imagem(j,i)=0
      enddo
      enddo


c
c     Pixel cutoff
c
c     If cutoff is negative, pixel counts are reset to ADS = original - cutoff, so
c     that all new counts are postive.
c


      backgr=0.d0

      if (vmin.lt.0.d0) backgr=-vmin

      do i=1,ny
      do j=1,nx

      if (pixmat(j,i).lt.vmin) then
      imagem(j,i)=-20
      endif

      pixmat(j,i)=pixmat(j,i)+backgr+2.d0

      enddo
      enddo

c
c     Excludes pixels from the matrix marked as bad pixels from the
c     bad pixels file only for the current image (if the file exists)
c

      open (23,file=ibadpx,status='old',err=270)
 
 250  read (23,*,end=270) ix1,ix2,iy1,iy2
 
      do i=iy1,iy2
      do j=ix1,ix2
      imagem(j,i)=-20
      enddo
      enddo
      
      go to 250
 
 270  close (23)


c
c     Excludes pixels from the matrix marked as bad pixels from the
c     bad pixels file for all proccessed images (if the file exists)
c
c


      open (23,file=kbadpx,status='old',err=271)
 
 251  read (23,*,end=271) ix1,ix2,iy1,iy2
 


      do i=iy1,iy2
      do j=ix1,ix2
      imagem(j,i)=-20
      enddo
      enddo
      
      go to 251
 
 271  close (23)


c
c     Initiates object identification and measuring
c


      write (*,*)
      write (*,*) 'Identifing and measuring objects. Please wait ...'
      write (*,*)



c
c     Stores pixel region data (*.mes files) for targets directly furnished by
c     the user. The region data conforms to the ds9 region format.
c
c     Circular regions are associated to Gaussian PSFs.
c
c     Rectangular regions are associated to trace-shaped PSFs
c     (Error-Function-based PSF).
c


      call mesure (idiobs,imes,nmcir,xcir,ycir,lacir,bcgc,bcgsc,nmtra,
     ?xtra,ytra,xlatra,ylatra,angtra,bcgtr,bcgstr)




c
c     Excludes from the identification process the pixels of the circular
c     regions of rounded-shaped targets directly furnished by the user.
c



      do k=1,nmcir

      ix1=xcir(k)-lacir(k)
      ix2=xcir(k)+lacir(k)
      iy1=ycir(k)-lacir(k)
      iy2=ycir(k)+lacir(k)

      if (ix1.lt.1) ix1=1
      if (iy1.lt.1) iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      raio=lacir(k)
      xc=xcir(k)
      yc=ycir(k)

      do i=iy1,iy2
      do j=ix1,ix2

      call circul(raio,xc,yc,j,i,ichave)

      if (ichave.gt.0) imagem(j,i)=-1 

      enddo
      enddo

      enddo



c
c     Excludes from the identification process the pixels of the rectangular
c     regions of trace-shaped targets directly furnished by the user.
c




      do k=1,nmtra


      ix1=xtra(k)-xlatra(k)
      ix2=xtra(k)+xlatra(k)
      iy1=ytra(k)-ylatra(k)
      iy2=ytra(k)+ylatra(k)

      cs=dcos(grarad*angtra(k))
      sn=dsin(grarad*angtra(k))

      do     i=iy1,iy2
      do 272 j=ix1,ix2

      ix=+(j-xtra(k))*cs+(y-ytra(k))*sn
      iy=-(j-xtra(k))*sn+(y-ytra(k))*cs


      if (ix.lt.1)  go to 272
      if (iy.lt.1)  go to 272
      if (ix.gt.nx) go to 272
      if (iy.gt.ny) go to 272

      imagem(ix,iy)=-1 


 272  continue

      enddo

      enddo

c
c
c     Opens output ds9 file in ds9 region format. 
c     Writes ds9 region file heder.
c


      open (20,file=ids9)

      write (20,273)
 273  format('# Region file format: DS9 version 4.1')

      write (20,274) infits
 274  format('# Filename: ',a50)

      write (20,276)
 276  format('global color=green dashlist=8 3 width=1 font="helvetica 10
     ? normal roman" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 de
     ?lete=1 include=1 source=1')

      write (20,277)
 277  format('image')



c
c
c     Identifies candidate objects with PRAIA's circular spiral search
c     algorithm.
c
c     Candidate objects are detected and classified as round-shaped objects or not.
c     If not, they are stored as regions potentially containing part or all of a
c     candidate trace-shaped object.
c
c     Parameters defining the regions' and candidate objects' characteristics are
c     stored for the fitting proccess of rounded-shaped objects, and for the analysis
c     of the regions with candidate traced-shaped objects.
c
c



      call ident (ipmax,idiobs,icofsp,coefx,xest,yest,xp,pixmat,imagem,
     ?nx,ny,contag,fatceu,lperc,iflag,ior,nval,xid,yid,idx1,idx2,idy1,
     ?idy2,idlado,npix,bcg,bcgs,dan,exce,nstar)



c
c
c     Analyses regions with candidate trace-shaped objects identified with "ident".
c
c     Identifies trace-shaped objects.
c
c     Removes rounded-shaped candidates from the initial list that turned out to be
c     part of a trace-shaped object. 
c    
c
c


      call idtrack (ipmax,idiobs,icofsp,coefx,xest,yest,xp,pixmat,
     ?imagem,nx,ny,contag,fatceu,fotceu,lpert,iflag,ior,nval,xid,yid,
     ?idx1,idx2,idy1,idy2,idlado,npix,bcg,bcgs,dan,exce,nstar,elimt,
     ?xctra,yctra,dmalat,dmilat,agtra,bcgt,bcgst,ntrac)


c
c     Determines the (x,y) Gaussian center of candidate rounded-shaped objects
c     automatically identified by PRAIA
c


      nest=0

      do 57 k=1,nstar

      if (npix(k).lt.ng) go to 57

c

      xc=xid(k)
      yc=yid(k)
      lado=idlado(k)

      ix1=idx1(k)
      ix2=idx2(k)
      iy1=idy1(k)
      iy2=idy2(k)

      fc=bcg(k)

      xcj=xc
      ycj=yc
      zlado=2.d0*lado


c
c     The 2D circular Gaussian fit
c

      r=lado


      call gcc (ipmax,idiobs,icofsp,pixmat,imagem,contag,ior,nval,lperc,
     ?fapiro,dlimro,plimro,icorox,icorot,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,
     ?ex,ey,it,sig,hh,fc,ierro)


      lado=r

      if (xc.lt.ix1-0.5d0) go to 57
      if (xc.gt.ix2+0.5d0) go to 57
      if (yc.lt.iy1-0.5d0) go to 57
      if (yc.gt.iy2+0.5d0) go to 57



c
c     Stores results only with FWHM (seeing) allowed by the user
c    (from PRAIA input file)
c


      fwhm=2.d0*sig*1.177410023d0

      if (fwhm.lt.fmin) go to 57
      if (fwhm.gt.fmax) go to 57

      seeing=fwhm*scala

c


      nest=nest+1

      xob(nest)=xc
      yob(nest)=yc
      ilado(nest)=lado
      seng(nest)=seeing

      altu(nest)=hh
      ialtu(nest)=nest

      exgcc(nest)=ex
      eygcc(nest)=ey
      sgcc(nest)=sig
      fgcc(nest)=fc

      angle(nest)=0.d0


      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2


      obtype(nest)='r'
c



 57   continue




c
c     Determines the (x,y) Gaussian center of rounded-shaped objects input by
c     the user
c




      do 59 k=1,nmcir


      xc=xcir(k)
      yc=ycir(k)
      r=lacir(k)

      ix1=xc-r
      ix2=xc+r
      iy1=yc-r
      iy2=yc+r

      fc=bcgc(k)



c
c     The 2D circular Gaussian fit
c


      call gcc (ipmax,idiobs,icofsp,pixmat,imagem,contag,ior,nval,lperc,
     ?fapiro,dlimro,plimro,icorox,icorot,nx,ny,ix1,ix2,iy1,iy2,r,xc,yc,
     ?ex,ey,it,sig,hh,fc,ierro)



      lado=r


      ix1=xc-r
      ix2=xc+r
      iy1=yc-r
      iy2=yc+r


      cfwhm=2.d0*sig*1.177410023d0

      seeing=cfwhm*scala


      nest=nest+1

      xob(nest)=xc
      yob(nest)=yc
      ilado(nest)=lado
      seng(nest)=seeing

      altu(nest)=hh
      ialtu(nest)=nest

      exgcc(nest)=ex
      eygcc(nest)=ey
      sgcc(nest)=sig
      fgcc(nest)=fc

      angle(nest)=0.d0


      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2


      obtype(nest)='r'

c

 59   continue




c
c     Determines the (x,y) center of candidate trace-shaped objects
c     automatically identified by PRAIA by the ERF PSF model
c


      if (ifittr.ne.1)  go to 2107


c
c     Adjustment results for debugging 
c

      open (96,file=ires)

      write(96,*)'altura,bx,by,sigma*scala,seeing,tetha,dlenght*sc
     ?ala,fundo,ex*scala,ey*scala,dlenght*altura'
      write (96,*)

c

      do 117 k=1,ntrac


      xc=xctra(k)
      yc=yctra(k)
      rx=dmalat(k)
      ry=dmilat(k)
      ang=agtra(k)
      fc=bcgt(k)
      sb=bcgst(k)
      



c
c     The Error Function (ERF) PSF model fit of a trace-shaped object
c



      call trace (idiobs,ipmax,icofsp,pixmat,imagem,fapitr,dlimtr,
     ?plimtr,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lpert,contag,ior,nval,
     ?icotr,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,itx,ity)



      tfwhm=2.d0*sigma*1.177410023d0

      seeing=tfwhm*scala


      nest=nest+1

      xob(nest)=bx
      yob(nest)=by
      ilado(nest)=dlenght
      seng(nest)=seeing

      altu(nest)=altura*dlenght
      ialtu(nest)=nest

      exgcc(nest)=ex
      eygcc(nest)=ey
      sgcc(nest)=sigma
      fgcc(nest)=fundo

      angle(nest)=tetha

      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2


      obtype(nest)='t'


 117  continue





c
c     Determines the (x,y) center of trace-shaped objects
c     input by the user, by fitting with the ERF PSF model
c



      do 118 k=1,nmtra


      xc=xtra(k)
      yc=ytra(k)
      rx=xlatra(k)
      ry=ylatra(k)
      ang=angtra(k)
      fc=bcgtr(k)
      sb=bcgstr(k)

    

c
c     The Error Function (ERF) PSF model fit of a trace-shaped object
c


      call trace (idiobs,ipmax,icofsp,pixmat,imagem,fapitr,dlimtr,
     ?plimtr,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lpert,contag,ior,nval,
     ?icotr,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,itx,ity)



      tfwhm=2.d0*sigma*1.177410023d0

      seeing=tfwhm*scala


      nest=nest+1

      xob(nest)=bx
      yob(nest)=by
      ilado(nest)=dlenght
      seng(nest)=seeing

      altu(nest)=altura*dlenght
      ialtu(nest)=nest

      exgcc(nest)=ex
      eygcc(nest)=ey
      sgcc(nest)=sigma
      fgcc(nest)=fundo

      angle(nest)=tetha


      sigg=seng(nest)/(2.d0*1.177410023d0)
      volum(nest)=2.d0*pi*altu(nest)*sigg**2

      obtype(nest)='t'



 118  continue

c


 2107 continue


c
c     Eliminates multiple measurements of the same object (if any) when the
c     (x,y) center of the measurement falls inside 1 FWHM of another measurement.
c     The object with the best (x,y) error fit is preserved.
c


      do ii=1,nest
      iflag(ii)=0
      enddo


      n=0


      do 65 ii=1,nest

      do 63 jj=1,nest

      if (ii.eq.jj) go to 63

      d=dsqrt((xob(jj)-xob(ii))**2+(yob(jj)-yob(ii))**2)

      sjj=seng(jj)/2.d0
      sii=seng(ii)/2.d0

      if (d.gt.sjj.and.d.gt.sii) go to 63

c
c     The center of a measurement falls inside 1 FWHM of the current
c     measurement or vice-versa.
c
c     Stores (x,y) errors of multiple object fittings. 
c


      n=n+1
      ior1(n)=jj
      nval1(n)=1000*(exgcc(jj)**2+eygcc(jj)**2)


 63   continue

      if (n.eq.0) go to 65


      n=n+1
      ior1(n)=ii
      nval1(n)=1000*(exgcc(ii)**2+eygcc(ii)**2)


c
c     Orders common measurements from low to high (x,y) error fits
c

      call ordem (idiobs,n,ior1,nval1)

c
c     Flags all the higher (x,y) error fits measurements.
c     Only the measurement with the best (x,y) error fit is preserved.
c


      do k=2,n
      kk=ior1(k)
      iflag(kk)=1
      enddo

      n=0

 65   continue



      nstar=nest

      nest=0

      do 70 k=1,nstar

      if (iflag(k).ne.0) go to 70

      nest=nest+1


      xob(nest)=xob(k)
      yob(nest)=yob(k)
      ilado(nest)=ilado(k)
      seng(nest)=seng(k)

      altu(nest)=altu(k)
      ialtu(nest)=ialtu(k)

      exgcc(nest)=exgcc(k)
      eygcc(nest)=eygcc(k)
      sgcc(nest)=sgcc(k)
      fgcc(nest)=fgcc(k)

      angle(nest)=angle(k)

      volum(nest)=volum(k)

      obtype(nest)=obtype(k)


      ialtu(nest)=nest
      nval(nest)=1000.d0*volum(nest)


      if (obtype(nest).eq.'r') then
  
      xlado=2.5d0*sgcc(nest)

      write (20,69) xob(nest),yob(nest),xlado
 69   format('circle(',2(f8.2,','),f8.2,')')

      endif


      if (obtype(nest).eq.'t') then

      xlado=ilado(nest)
      ylado=3.d0*sgcc(nest)

      tetha=angle(nest)*radgra

      write (20,116) xob(nest),yob(nest),xlado,ylado,tetha
 116  format('box(',4(f8.2,','),f8.2,')')

      altura=altu(nest)
      bx=xob(nest)
      by=yob(nest)
      sigma=sgcc(nest)*scala
      seeing=seng(nest)
      dlen=ilado(nest)*scala
      fundo=fgcc(nest)
      ex=exgcc(nest)*scala
      ey=eygcc(nest)*scala
      dlena=dlen*altura

      write (96,*) altura,bx,by,sigma,seeing,tetha,
     ?dlen,fundo,ex,ey,dlena


      endif



 70   continue

      close (20)

      close (96)



c
c     Stores original (x,y) measurements without FDP
c


      do i=1,nest

      xoob(i)=xob(i)
      yoob(i)=yob(i)

      enddo


c
c     Ordering from fainter to brightest measured object
c


      call ordem (idiobs,nest,ialtu,nval)


c
c     Minimum number of objects not reached ?
c

c     write (*,*) 'nnn = ',nest

c     stop


c     if (nest.lt.4) go to 60




c
c     Remaking astrometry mode
c



 150  continue

      if (iastro.eq.0) go to 399



c
c     Uploads field data from xy field file (Remaking astrometry modes only)
c


      daj=0.d0

      fumag=0.d0
      fumag2=0.d0

c

      open (33,file=infits)

c

      do j=1,idiobs


      read (33,470,end=151) xob(j),yob(j),seng(j),altu(j),fgcc(j),gumag,
     ?gumag2,cmgu(j),omag(j),xmgu,xmgj,xmgh,xmgk,oemag,oemag,ermgj,
     ?ermgh,ermgk,pma,pmd,epma,epmd,exgcc(j),eygcc(j),era,ede,alfsig,
     ?delsig,nstart,nfinal,alsi2,desi2,ktira,xra(j),yde(j),iuth,iutm,
     ?sut,iutano,iutmes,iutdia,dj,iexps,ichfil,imfits,mchobj,nx,ny



c
c     Stores original (x,y) measurements given in the xy file
c


      xoob(j)=xob(j)
      yoob(j)=yob(j)

c

      xra(j)=xra(j)*15.d0


c
c     For mixed objects from different frames, like in mosaics of
c     fields, finds the average observation epoch of the mosaic
c
c     If it is a single field, the resulting average equals the
c     original fixed value.
c


      cj=j

      daj=(daj*(cj-1.d0)+(dj-dj2000))/cj


c
c     For mixed objects from different frames, like in mosaics of
c     fields, takes the average of the mean and of the dispersion of
c     the magnitude's sky background
c
c     If it is a single field, the resulting averages equal the
c     original fixed values.
c


      fumag=(fumag*(cj-1.d0)+gumag)/cj
      fumag2=(fumag2*(cj-1.d0)+gumag2)/cj


      enddo


 151  close (33)

      nest=j-1

      dj=daj+dj2000

      epoj=2000d0+(dj-dj2000)/365.25d0



c
c     Updates the UTC gregorian instant for matching the averaged JD epoch      
c

      djm=dj-dj2000

      call iau_jd2cal (dj2000,djm,iutano,iutmes,iutdia,fd,jjj)

      hora=fd*24.d0

      iuth=hora
      iutm=(hora-iuth)*60.d0
      sut=((hora-iuth)*60.d0-iutm)*60.d0


c
c     FOV (RA,Dec) center and limits (Remaking astrometry modes 1 and 2 only)
c
c
c     Extracts or estimates the (RA,Dec) limits and the center of the FOV. 
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     Here, the FOV's (RA,Dec) limits are not expanded or contracted by a factor
c     given by the user. The FOV's (RA,Dec) true limits are directly determined
c     from the stars' (RA,Dec)s obtained in the previous (RA,Dec) reduction.
c
c
c



      call fov_12 (idiobs,nest,xra,yde,rac,dec,ramin,ramax,demin,demax,
     ?iah,iam,sa,isig,idg,idm,ds)



c
c
c     Remaking astrometry of xy fields, tangent plane technique option.
c
c
c     Tangent plane technique. Takes (X,Y) standard coordinates from (RA,DEC)
c     as if they were measured (x,y), and proceed with (RA,DEC) reduction with
c     respect to the reference catalogues.
c
c     This reduction procedure is mandatory in the case that the xy files come
c     from a field mosaic reduction with PRAIA, were the stamped (x,y) in the
c     files are meaningless 
c     
c


      if (iastro.eq.2) then


      xxmin=+1.d14
      yymin=+1.d14


      grac=grarad*rac
      gdec=grarad*dec


      do i=1,nest

      bra=xra(i)*grarad
      bde=yde(i)*grarad

      d=dexy(bra,bde,grac,gdec)
      xx=xpad(bra,bde,grac)/d
      yy=ypad(bra,bde,grac,gdec)/d

      xob(i)=xx*radgra*3600.d0
      yob(i)=yy*radgra*3600.d0

   
      if (xob(i).lt.xxmin) xxmin=xob(i)

      if (yob(i).lt.yymin) yymin=yob(i)


      enddo


      do i=1,nest

      xob(i)=xob(i)-xxmin+1.d0
      yob(i)=yob(i)-yymin+1.d0

      enddo


      endif




c
c     All astrometry modes.
c
c
c     Determines typical FWHM, using average and standard deviation of the
c     two middle quartiles (border quartiles cut in 20%) of the FWHM
c     distribution of the objects.
c

 399  continue



      percs=0.2d0

      do i=1,nest
      contag(i)=seng(i)
      enddo

      n=nest


      call quartl (idiobs,ior1,nval1,n,percs,contag)

      call avsdev (idiobs,n,contag,fwhm,sigfwh)




c
c     All astrometry modes.
c
c
c     Field Distortion Pattern correction. 
c
c
c     In astrometry option modes 0 and 1, units are in pixels.
c
c     In astrometry option mode 2 (tangent plane technique),
c     units are in arcseconds.
c
c
c 



      if (nfdp.eq.0) go to 120

      if (kfdp.gt.nfdp) kfdp=nfdp


c
c     FDP mode 1, mask in pixel (astrometry modes 0 or 1) or in
c     arcsec units (astrometry mode 2, tangent plane technique)
c


      if (mfdp.eq.1) then



      do i=1,nest


      if (iastro.ne.2) then

      do j=1,nfdp
      ior1(j)=j
      nval1(j)=1000*((xob(i)-coordx(j))**2+(yob(i)-coordy(j))**2)
      enddo

      else

      do j=1,nfdp
      ior1(j)=j
      nval1(j)=1000*(((xra(i)-coordx(j))*dcos(grarad*yde(i)))**2+
     ?(yde(i)-coordy(j))**2)
      enddo


      endif


      
      call ordem (idiobs,nfdp,ior1,nval1)


      ad0=0.d0
      cadx=0.d0
      cady=0.d0

      do k=1,kfdp

      m=ior1(k)

      if (iastro.ne.2) then

      di=dsqrt((xob(i)-coordx(m))**2+(yob(i)-coordy(m))**2)

      else

      di=dsqrt(((xra(i)-coordx(m))*dcos(grarad*yde(i)))**2+
     ?(yde(i)-coordy(m))**2)

      endif

      ad0=ad0+1.d0/di**2
      cadx=cadx+adx(m)/di**2
      cady=cady+ady(m)/di**2

      enddo

      cadx=cadx/ad0
      cady=cady/ad0

      xob(i)=xob(i)-cadx
      yob(i)=yob(i)-cady

      xfdp(i)=cadx
      yfdp(i)=cady

      enddo


      endif




c
c     FDP correction using 3rd degree polynomial distortion model
c
c     FDP mode 2.
c
c     For all astrometry modes
c
c



      if (mfdp.eq.2) then

      ndgr=3

      do i=1,nest

      if (iastro.eq.2) then

      x=xra(i)
      y=yde(i)

      else

      x=xob(i)
      y=yob(i)

      endif


      dx=pol(jjfdp,x,y,adx,ndgr)
      dy=pol(jjfdp,x,y,ady,ndgr)


      xob(i)=xob(i)-dx
      yob(i)=yob(i)-dy

      xfdp(i)=dx
      yfdp(i)=dy


      enddo

      endif



c
c
c     FDP correction using 3rd degree polynomial distortion model
c
c     FDP mode 3 (FITS header keywords general procedure) or
c     mode 4 (DES-DECAM FITS header keywords procedure)
c
c     Only for astrometry mode 0 (needs FITS images)
c    
c



      if (mfdp.eq.3 .or. mfdp.eq.4) then

      if (iastro.ne.0) go to 120 


      ndgr=3


      if (mfdp.eq.3) then
      sccx=adx(2)
      sccy=ady(2)
      endif


      if (mfdp.eq.4) then
      sccx=adx(2)*dsqrt(cdx(1)**2+cdx(2)**2)
      sccy=ady(2)*dsqrt(cdy(1)**2+cdy(2)**2)
      endif

c

      do i=1,nest


      if (mfdp.eq.3) then

      x=xob(i)
      y=yob(i)

      endif



c
c     Linear transformation in DES procedure
c

      if (mfdp.eq.4) then

      xx=xob(i)-crpx
      yy=yob(i)-crpy


      x=cdx(1)*xx+cdx(2)*yy
      y=cdy(1)*xx+cdy(2)*yy

      endif

c

      if (mfdp.eq.3) then


      dx=pol(jjfdp,x,y,adx,ndgr)/sccx
      dy=pol(jjfdp,y,x,ady,ndgr)/sccy

c     xob(i)=xob(i)-dx
c     yob(i)=yob(i)-dy


      xfdp(i)=xob(i)-dx
      yfdp(i)=yob(i)-dy


      xob(i)=dx
      yob(i)=dy



      endif

c

      if (mfdp.eq.4) then


      dx=pol(jjfdp,x,y,adx,ndgr)
      dy=pol(jjfdp,y,x,ady,ndgr)


      xob(i)=crpx-dy/sccy
      yob(i)=crpy+dx/sccx


c     xob(i)=crpx-dy*3600.d0/scala
c     yob(i)=crpy+dx*3600.d0/scala



      xfdp(i)=xob(i)-xoob(i)
      yfdp(i)=yob(i)-yoob(i)


      endif


c


      enddo

      endif



c



 120  continue



c
c
c     FOV (RA,Dec) center and limits (astrometry of images, mode 0 only).
c
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. Also extracts the instant of observation in JD
c     and Julian year, among other FOV auxiliary information.
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     The FOV's (RA,Dec) limits may be expanded or contracted by a factor
c     given by the user. If the factor is 1 the limits are unchanged.
c
c
c 




      if (iastro.eq.0) then

      call fov_0 (centro,infits,expfov,nx,ny,scala,rac,dec,ramin,ramax,
     ?demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)

      endif






      write (*,*) 'Files searched for catalogue extraction:'
      write (*,*)


c
c
c     Picks up 2MASS stars from a (RA,Dec) region around the CCD
c     center with twice its estimated sizes     
c
c
c     - ra2ma,de2ma in degrees: (RA,Dec) from 2MASS PSC
c     - era2ma,ede2ma in arcsec: (RA,Dec) errors
c     - extracted magnitudes: J, H and K
c
c


      call stmass (idiobs,mraiz,rac,dec,ramin,ramax,demin,demax,ra2ma,
     ?de2ma,era2ma,ede2ma,dmgj,dmgh,dmgk,emgj,emgh,emgk,ddj2,n2mass)




c
c     Picks up UCAC4 stars  
c
c
c     - rauc4,deuc4: RA,Dec in degrees at epoch epoj of the CCD observation
c     - epram4, epdem4: mean JD epoch of (RA,Dec)s
c     - erauc4, edeuc4: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra4: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde4: Dec proper motion in arcsec/year
c     - epmra4, epmde4: proper motion errors in arcsec/year
c     - udmgj4,udmgh4,udmgk4: J, H and K magnitudes from 2MASS in the UCAC4
c     - udmg4:  internal magnitude in the UCAC4 system between V e R
c



      call sucac4 (idiobs,iu4z,iu4i,inuu4,ir1u4,ir2u4,u4raiz,epoj,rac,
     ?dec,ramin,ramax,demin,demax,rauc4,deuc4,erauc4,edeuc4,pmra4,pmde4,
     ?epmra4,epmde4,udmgj4,udmgh4,udmgk4,udmg4,nucac4,rame4,deme4,
     ?eramc4,edemc4,epora4,epode4)


c
c     UCAC4 does not furnish J, H and K magnitude errors
c


      do i=1,nucac4
      edmgj4(i)=d99
      edmgh4(i)=d99
      edmgk4(i)=d99
      enddo



c
c     Picks up UCAC5 stars  
c
c
c     - rauc5,deuc5: RA,Dec in degrees at epoch epoj of the CCD observation
c     - erauc5, edeuc5: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra5: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde5: Dec proper motion in arcsec/year
c     - epmra5, epmde5: proper motion errors in arcsec/year
c     - udmgj5,udmgh5,udmgk5: J, H and K magnitudes from 2MASS in the UCAC5
c     - udmg5: UCAC5 magnitude R Johnston
c


      call sucac5 (idiobs,iu5z,iu5i,inuu5,ir1u5,ir2u5,u5raiz,epoj,rac,
     ?dec,ramin,ramax,demin,demax,rauc5,deuc5,erauc5,edeuc5,pmra5,pmde5,
     ?epmra5,epmde5,udmgj5,udmgh5,udmgk5,udmg5,nucac5)




c
c     UCAC5 does not furnish J, H and K magnitude errors
c


      do i=1,nucac5
      edmgj5(i)=d99
      edmgh5(i)=d99
      edmgk5(i)=d99
      enddo



c
c
c     Picks up stars from the user catalogue (PRAIA format)
c
c
c     - raucs,deucs: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - eraucs, edeucs: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmras: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdes: Dec proper motion in arcsec/year
c     - epmras, epmdes: proper motion errors in arcsec/year
c     - udmg_s ...: J, H and K magnitudes from 2MASS in the user catalogue (code 99 if absent)
c     - udmgs:  star magnitude in the user catalogue
c


      if (iuserc.eq.1) then

      write (*,*)

      call cuser (idiobs,cpraia,epoj,rac,dec,ramin,ramax,demin,demax,
     ?raucs,deucs,eraucs,edeucs,pmras,pmdes,epmras,epmdes,udmgjs,udmghs,
     ?udmgks,udmgs,nucaus)

      endif



c
c     User catalogue does not furnish J, H and K magnitude errors
c


      do i=1,nucaus
      edmgjs(i)=d99
      edmghs(i)=d99
      edmgks(i)=d99
      enddo



c
c
c     Picks up stars from the GAIA_1 catalogue.
c
c
c     GAIA_1 refers to the complete GAIA catalog (GAS) from the release version DR1
c     (14/Sep/2016).
c
c     We use the version of GAS organized by J. Camargo and LIneA team in 0.1 degrees
c     declination zones, with crescent RA within each zone, but otherwise keeping
c     the original line contents in CSV format.
c
c
c     - rag1,deg1: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - erag1, edeg1: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmag1: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdg1: Dec proper motion in arcsec/year
c     - epmag1, epmdg1: proper motion errors in arcsec/year
c     - g1mg_  ...: J, H and K magnitudes from 2MASS in the GAIA catalogue (code 99 if absent)
c     - cg1mgg: GAIA magnitude in the G band
c


      write (*,*)

      call gaia1 (idiobs,raizg1,epoj,rac,dec,ramin,ramax,demin,demax,
     ?rag1,deg1,erag1,edeg1,pmag1,pmdg1,epmag1,epmdg1,cg1mgg,ngaia1)




c
c     Reports the number of measured objects in the FOV
c


      write (*,*)
      write (*,*) 'Total number of measured objects in the FOV = ',nest
      write (*,*)
      write (*,*)


c
c     Identifies common GAIA_1 and 2MASS stars for extracting
c     J, H and K 2MASS magnitudes and their errors.
c

 

      boxerr=(erpix*scala)**2



      do 650 i=1,ngaia1

      g1mgj(i)=d99
      g1mgh(i)=d99
      g1mgk(i)=d99

      g1emgj(i)=d99
      g1emgh(i)=d99
      g1emgk(i)=d99



      do j=1,n2mass

      ior(j)=j

      dx=dcos(deg1(i)*grarad)*(ra2ma(j)-rag1(i))*3600d0
      dy=(de2ma(j)-deg1(i))*3600d0

      val(j)=dx**2+dy**2

      enddo



      call dordem (idiobs,n2mass,ior,val)


      j=ior(1)


      dx=dcos(deg1(i)*grarad)*(ra2ma(j)-rag1(i))*3600d0
      dy=(de2ma(j)-deg1(i))*3600d0

      xx=dx**2+dy**2

      if (xx.gt.boxerr) go to 650


      g1mgj(i)=dmgj(j)
      g1mgh(i)=dmgh(j)
      g1mgk(i)=dmgk(j)

      g1emgj(i)=emgj(j)
      g1emgh(i)=emgh(j)
      g1emgk(i)=emgk(j)



 650  continue



c
c     Initializes time rotines for estimating time consumption in the (RA,Dec)
c     reduction of PRAIA
c

      tempoi=0.d0
      call tempo (tempoi,tempot,tempop)

      tt0=tempop/3600.d0



c
c     Recognizes the GAIA_1 stars among those identified in the FOV
c


      if (iastro.eq.0) go to 755


c
c     Remaking astrometry of xy fields
c


      boxerr=(erpix*scala)**2


      do i=1,idiobs
      idga1(i)=0
      enddo

      nga1=0


      do 750 i=1,ngaia1

      do     j=1,nest

      ior(j)=j

      dx=dcos(deg1(i)*grarad)*(xra(j)-rag1(i))*3600d0
      dy=(yde(j)-deg1(i))*3600d0

      val(j)=dx**2+dy**2


      enddo


      call dordem (idiobs,nest,ior,val)

      j=ior(1)


      dx=dcos(deg1(i)*grarad)*(xra(j)-rag1(i))*3600d0
      dy=(yde(j)-deg1(i))*3600d0


      xx=dx**2+dy**2

      if (xx.gt.boxerr) go to 750

      idga1(j)=i

      nga1=nga1+1


 750  continue



      write (*,*)
      write (*,*) 'GAIA_1: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_1: identified stars in field       = ',nga1
      write (*,*)
      write (*,*)

c

      go to 760


c
c     Astrometry of images.
c
c    Recognizes the GAIA_1 stars among those identified in the FOV
c
c


 755  continue


      if (nscl.eq.0) then

      call idxy2m (idiobs,icofsp,xest,yest,xp,yp,carx,cary,nval,ior,
     ?scala,erpix,rac,dec,nbcat,nbmed,nest,ialtu,xob,yob,ngaia1,idga1,
     ?rag1,deg1,cg1mgg,xold,yold,xrag1,ydeg1,ireflex,ecala,tt,ierro)

      else



      if (mix.eq.2) then

      call idxy2m (idiobs,icofsp,xest,yest,xp,yp,carx,cary,nval,ior,
     ?scala,erpix,rac,dec,nbcat,nbmed,nest,ialtu,xob,yob,ngaia1,idga1,
     ?rag1,deg1,cg1mgg,xold,yold,xrag1,ydeg1,ireflex,ecala,tt,ierro)


      else


      call mdxy2m (idiobs,icofsp,xest,yest,xp,yp,carx,cary,ngrau,coefxr,
     ?coefyr,kreflex,nval,ior,rcala,erpix,rac,dec,nbcat,nbmed,nest,
     ?ialtu,xob,yob,ngaia1,idga1,rag1,deg1,cg1mgg,xold,yold,xrag1,
     ?ydeg1,ireflex,escala,tt,ierro)

      endif


      endif


c


      if (ierro.eq.1) then
      ierro=0
      go to 62
      endif



c
c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_1
c


 760  continue

c

      do i=1,idiobs
      itirg1(i)=0
      enddo


c

      call posred (idiobs,icofsp,ireflex,rac,dec,idga1,ngaia1,rag1,
     ?deg1,nest,xob,yob,xest,yest,xp,yp,pcorg1,ngrau,ngrau3,ngrau5,
     ?nstag1,nfing1,xrag1,ydeg1,erasg1,edesg1,alfsg1,delsg1,alfrg1,
     ?delrg1,g1cofx,g1cofy,g1ecfx,g1ecfy,itirg1,avamg1,dvamg1,ierro)




      if (iastro.ne.0) go to 765


c
c     Astrometry of images.
c
c     Improves pixel scale and error for option mix = 1
c     (images with same pixel scale and orientation)
c




      if (ierro.eq.0) then


      kreflex=ireflex


c
c     Updates coefficient values image after image
c


      do mmmm=2,icofsp
      coefxr(mmmm)=(nscl*coefxr(mmmm)+g1cofx(mmmm))/(nscl+1.d0)
      coefyr(mmmm)=(nscl*coefyr(mmmm)+g1cofy(mmmm))/(nscl+1.d0)
      enddo



c
c     Calculates pixel scale from adjustment in radians per pixel
c

  

      rscala=dsqrt(coefxr(2)**2+coefxr(3)**2)+dsqrt(coefyr(2)**2+
     ?coefyr(3)**2) 
      rscala=rscala/2.d0


      scl=scl+rscala
      scl2=scl2+rscala**2
      nscl=nscl+1



c
c     Estimates pixel scale error in radians per pixel, by the variation of
c     the pixel scales found for the images (valid only for mix=1)
c


      if (nscl.eq.1) then

      rcala=scl


      dnx=nx/2.d0
      dny=ny/2.d0

      escala=3.d0*dsqrt(avam**2+dvam**2)/dsqrt(dnx**2+dny**2)
      escala=escala*grarad/3600.d0


      else


      kscl=nscl
      tscl=scl
      tscl2=scl2
      
      call desvio (kscl,tscl,tscl2)

      rcala=tscl
      escala=tscl2*3.d0


      endif


      endif



c     write (*,*) 'i,num, esc, err = ',lllll,nscl,rcala*radgra*3600.d0,
c    ?escala*radgra*3600.d0


      ierro=0



c
c
c     Astrometry of all modes.
c
c
c     Identifies GAIA_2 catalogue stars in the FOV
c
c     GAIA_2 is comprised of GAIA_1 stars within the magnitude limits
c     imposed by the user are used in the (RA,Dec) reductions.
c
c



 765  continue


      do i=1,idiobs
      idga2(i)=0
      enddo

c

      ngaia2=0
      nga2=0



      do 405 i=1,ngaia1


      if (cg1mgg(i).lt.g2mag1) go to 405
      if (cg1mgg(i).gt.g2mag2) go to 405

      ngaia2=ngaia2+1



      do j=1,nest

      if (idga1(j).eq.i) then

      idga2(j)=i

      nga2=nga2+1

      go to 405

      endif

      enddo


 405  continue


c

      write (*,*) 'GAIA_2: 2x2 field size, extracted stars = ',ngaia2
      write (*,*) 'GAIA_2: identified stars in field       = ',nga2
      write (*,*)
      write (*,*)

c

      nga3=0

      do j=1,nest

      if (idga1(j).ne.0) nga3=nga3+1

      enddo

      nga1=nga3

      nga4=nga3

c

      write (*,*) 'GAIA_3: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_3: identified stars in field       = ',nga3
      write (*,*)
      write (*,*)



      write (*,*) 'GAIA_4: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_4: identified stars in field       = ',nga4
      write (*,*)
      write (*,*)




c
c      Astrometry of all modes.
c
c
c     Identifies UCAC4 catalogue stars in the FOV
c


      do i=1,idiobs
      iduc4(i)=0
      enddo

c


      icouc4=0

      errou=erpix*scala


      do 410 i=1,nucac4
      do     j=1,nest

      ior(j)=j

      dx=dcos(deuc4(i)*grarad)*(xrag1(j)-rauc4(i))*3600.d0
      dy=(ydeg1(j)-deuc4(i))*3600.d0


      val(j)=dx**2+dy**2


      enddo

      call dordem (idiobs,nest,ior,val)

      j=ior(1)


      dx=dcos(deuc4(i)*grarad)*(xrag1(j)-rauc4(i))*3600.d0
      dy=(ydeg1(j)-deuc4(i))*3600.d0



      if (dabs(dx).gt.errou) go to 410 
      if (dabs(dy).gt.errou) go to 410

      iduc4(j)=i


      icouc4=icouc4+1




 410  continue



c
c      Astrometry of all modes.
c
c
c     Identifies UCAC5 catalogue stars in the FOV
c


      do i=1,idiobs
      iduc5(i)=0
      enddo

c


      icouc5=0

      errou=erpix*scala


      do 411 i=1,nucac5
      do     j=1,nest

      ior(j)=j

      dx=dcos(deuc5(i)*grarad)*(xrag1(j)-rauc5(i))*3600.d0
      dy=(ydeg1(j)-deuc5(i))*3600.d0


      val(j)=dx**2+dy**2


      enddo

      call dordem (idiobs,nest,ior,val)

      j=ior(1)


      dx=dcos(deuc5(i)*grarad)*(xrag1(j)-rauc5(i))*3600.d0
      dy=(ydeg1(j)-deuc5(i))*3600.d0


      if (dabs(dx).gt.errou) go to 411 
      if (dabs(dy).gt.errou) go to 411

      iduc5(j)=i


      icouc5=icouc5+1




 411  continue


 


c
c     Astrometry of all modes.
c
c
c     Furnishes proper motions based on UCAC4 data in two ways.
c
c     In one of them, the proper motions (and their errors) from the UCAC4
c     catalogue itself are directly used (GAIA_3 and GAIA_5 catalogues). 
c
c     In the other one, the proper motions are computed by using as first epoch
c     the UCAC4 positions at their central epochs, and by using the 2015.0
c     GAIA_1 positions as second epoch (GAIA_4 and GAIA_6 catalogues). Proper
c     motion errors are also computed based on the contributing (RA,Dec)s and
c     on the time span between the first and second epochs.
c
c     The GAIA_3 and GAIA_4 catalogues comprise of all GAS stars. The GAIA_5
c     and GAIA_6 catalogues comprise of only stars with proper motions, that is,
c     TGAS stars plus GAS stars commom to the UCAC4 catalogue, for which proper
c     motions have been derived by one of the two ways explained above.
c     
c     GAIA_3, GAIA_4, GAIA_5 and GAIA_6 stars with proper motions have their
c     positions corrected for the observation epoch. (RA,Dec) position errors
c     are also updated for the observation epoch.
c
c


      do i=1,idiobs
      idga5(i)=0
      enddo

      nga5=0



      do 412 ii=1,nest

      if (idga1(ii).eq.0) go to 412

      i=idga1(ii)

      pmag3(i)=pmag1(i)
      pmdg3(i)=pmdg1(i)

      epmag3(i)=epmag1(i)
      epmdg3(i)=epmdg1(i)

      rag3(i)=rag1(i)
      deg3(i)=deg1(i)

      erag3(i)=erag1(i)
      edeg3(i)=edeg1(i)



      pmag4(i)=pmag1(i)
      pmdg4(i)=pmdg1(i)

      epmag4(i)=epmag1(i)
      epmdg4(i)=epmdg1(i)

      rag4(i)=rag1(i)
      deg4(i)=deg1(i)

      erag4(i)=erag1(i)
      edeg4(i)=edeg1(i)



      idga5(ii)=i

      nga5=nga5+1


      if (pmag3(i).lt.90.d0) go to 412
      if (pmdg3(i).lt.90.d0) go to 412

      idga5(ii)=0

      nga5=nga5-1

      if (iduc4(ii).eq.0) go to 412

      idga5(ii)=i

      nga5=nga5+1

      j=iduc4(ii)

      pmag3(i)=pmra4(j)
      pmdg3(i)=pmde4(j)

      epmag3(i)=epmra4(j)
      epmdg3(i)=epmde4(j)

      deg3(i)=deg1(i)+pmdg3(i)*(epoj-epog1)/3600.d0
      rag3(i)=rag1(i)+(pmag3(i)*(epoj-epog1)/3600.d0)/dabs(dcos(grarad*
     ?deg3(i)))

      erag3(i)=dsqrt(erag1(i)**2+(epmag3(i)*(epoj-epog1))**2)
      edeg3(i)=dsqrt(edeg1(i)**2+(epmdg3(i)*(epoj-epog1))**2)


c
c     Computes here (RA,Dec) GAIA/UCAC4 proper motions and errors and
c     updates (Ra,Dec) for observation epoch
c

      deme44=(deg1(i)+deme4(j))/2.d0

      pmag4(i)=3600.d0*(rag1(i)-rame4(j))/(epog1-epora4(j))
      pmag4(i)=pmag4(i)*dabs(dcos(grarad*deme44))
      pmdg4(i)=3600.d0*(deg1(i)-deme4(j))/(epog1-epode4(j))


      epmag4(i)=dsqrt(erag1(i)**2+eramc4(j)**2)/(epog1-epora4(j))
      epmdg4(i)=dsqrt(edeg1(i)**2+edemc4(j)**2)/(epog1-epode4(j))

      deg4(i)=deg1(i)+pmdg4(i)*(epoj-epog1)/3600.d0
      rag4(i)=rag1(i)+(pmag4(i)*(epoj-epog1)/3600.d0)/dabs(dcos(grarad*
     ?deg4(i)))

      erag4(i)=dsqrt(erag1(i)**2+(epmag4(i)*(epoj-epog1))**2)
      edeg4(i)=dsqrt(edeg1(i)**2+(epmdg4(i)*(epoj-epog1))**2)


 412  continue



c

      write (*,*) 'GAIA_4: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_4: identified stars in field       = ',nga3
      write (*,*)
      write (*,*)



      write (*,*) 'GAIA_5: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_5: identified stars in field       = ',nga5
      write (*,*)
      write (*,*)



      write (*,*) 'GAIA_6: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_6: identified stars in field       = ',nga5
      write (*,*)
      write (*,*)



      write (*,*) 'GAIA_7: 2x2 field size, extracted stars = ',ngaia1
      write (*,*) 'GAIA_7: identified stars in field       = ',nga5
      write (*,*)
      write (*,*)


c

      write (*,*)
      write (*,*) 'UCAC4: 2x2 field size, extracted stars = ',nucac4
      write (*,*) 'UCAC4: identified stars in field       = ',icouc4
      write (*,*)
      write (*,*)


c

      write (*,*)
      write (*,*) 'UCAC5: 2x2 field size, extracted stars = ',nucac5
      write (*,*) 'UCAC5: identified stars in field       = ',icouc5
      write (*,*)
      write (*,*)


c
c     Astrometry of all modes.
c
c
c     Identifies User catalogue stars in the FOV
c



      if (iuserc.eq.1) then


      do i=1,idiobs
      iducs(i)=0
      enddo

c

      icoucs=0

      errou=erpix*scala


      do 415 i=1,nucaus
      do     j=1,nest

      ior(j)=j

      dx=dcos(deucs(i)*grarad)*(xrag1(j)-raucs(i))*3600d0
      dy=(ydeg1(j)-deucs(i))*3600d0

      val(j)=dx**2+dy**2

      enddo


      call dordem (idiobs,nest,ior,val)

      j=ior(1)


      dx=dcos(deucs(i)*grarad)*(xrag1(j)-raucs(i))*3600d0
      dy=(ydeg1(j)-deucs(i))*3600d0


      if (dabs(dx).gt.errou) go to 415 
      if (dabs(dy).gt.errou) go to 415

      iducs(j)=i

      icoucs=icoucs+1



 415  continue


      write (*,*)
      write (*,*) 'USER: 2x2 field size, extracted stars = ',nucaus
      write (*,*) 'USER: identified stars in field       = ',icoucs
      write (*,*)
      write (*,*)


      endif




c
c     Retrieves J, H, K magnitude errors from GAIA_1 data to UCAC4 stars
c     in the FOV
c
 
      do i=1,nest

      j=iduc4(i)

      k=idga1(i)

      if (j.ne.0 .and. k.ne.0) then

      edmgj4(j)=g1emgj(k)
      edmgh4(j)=g1emgh(k)
      edmgk4(j)=g1emgk(k)

      endif

      enddo




c
c     Retrieves J, H, K magnitude errors from GAIA_1 data to UCAC5 stars
c     in the FOV
c
 
      do i=1,nest

      j=iduc5(i)

      k=idga1(i)

      if (j.ne.0 .and. k.ne.0) then

      edmgj5(j)=g1emgj(k)
      edmgh5(j)=g1emgh(k)
      edmgk5(j)=g1emgk(k)

      endif

      enddo




c
c     Retrieves J, H, K magnitude errors from GAIA_1 data to User catalogue
c     stars in the FOV
c
 
      do i=1,nest

      j=iducs(i)

      k=idga1(i)

      if (j.ne.0 .and. k.ne.0) then

      edmgjs(j)=g1emgj(k)
      edmghs(j)=g1emgh(k)
      edmgks(j)=g1emgk(k)

      endif

      enddo




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_2
c


      do i=1,idiobs
      itirg2(i)=0
      enddo

c

      call posred (idiobs,icofsp,ireflex,rac,dec,idga2,ngaia1,rag1,
     ?deg1,nest,xob,yob,xest,yest,xp,yp,pcorg2,ngrau,ngrau3,ngrau5,
     ?nstag2,nfing2,xrag2,ydeg2,erasg2,edesg2,alfsg2,delsg2,alfrg2,
     ?delrg2,g2cofx,g2cofy,g2ecfx,g2ecfy,itirg2,avamg2,dvamg2,ierro)






c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_3
c


      do i=1,idiobs
      itirg3(i)=0
      enddo




      call posred (idiobs,icofsp,ireflex,rac,dec,idga1,ngaia1,rag3,
     ?deg3,nest,xob,yob,xest,yest,xp,yp,pcorg3,ngrau,ngrau3,ngrau5,
     ?nstag3,nfing3,xrag3,ydeg3,erasg3,edesg3,alfsg3,delsg3,alfrg3,
     ?delrg3,g3cofx,g3cofy,g3ecfx,g3ecfy,itirg3,avamg3,dvamg3,ierro)




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_4
c


      do i=1,idiobs
      itirg4(i)=0
      enddo




      call posred (idiobs,icofsp,ireflex,rac,dec,idga1,ngaia1,rag4,
     ?deg4,nest,xob,yob,xest,yest,xp,yp,pcorg4,ngrau,ngrau3,ngrau5,
     ?nstag4,nfing4,xrag4,ydeg4,erasg4,edesg4,alfsg4,delsg4,alfrg4,
     ?delrg4,g4cofx,g4cofy,g4ecfx,g4ecfy,itirg4,avamg4,dvamg4,ierro)





c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_5
c


      do i=1,idiobs
      itirg5(i)=0
      enddo

c

      call posred (idiobs,icofsp,ireflex,rac,dec,idga5,ngaia1,rag3,
     ?deg3,nest,xob,yob,xest,yest,xp,yp,pcorg5,ngrau,ngrau3,ngrau5,
     ?nstag5,nfing5,xrag5,ydeg5,erasg5,edesg5,alfsg5,delsg5,alfrg5,
     ?delrg5,g5cofx,g5cofy,g5ecfx,g5ecfy,itirg5,avamg5,dvamg5,ierro)




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_6
c


      do i=1,idiobs
      itirg6(i)=0
      enddo

c

      call posred (idiobs,icofsp,ireflex,rac,dec,idga5,ngaia1,rag4,
     ?deg4,nest,xob,yob,xest,yest,xp,yp,pcorg6,ngrau,ngrau3,ngrau5,
     ?nstag6,nfing6,xrag6,ydeg6,erasg6,edesg6,alfsg6,delsg6,alfrg6,
     ?delrg6,g6cofx,g6cofy,g6ecfx,g6ecfy,itirg6,avamg6,dvamg6,ierro)



c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with GAIA_7
c


      do i=1,idiobs
      itirg7(i)=0
      enddo

c

      call posred (idiobs,icofsp,ireflex,rac,dec,idga5,ngaia1,rag1,
     ?deg1,nest,xob,yob,xest,yest,xp,yp,pcorg7,ngrau,ngrau3,ngrau5,
     ?nstag7,nfing7,xrag7,ydeg7,erasg7,edesg7,alfsg7,delsg7,alfrg7,
     ?delrg7,g7cofx,g7cofy,g7ecfx,g7ecfy,itirg7,avamg7,dvamg7,ierro)




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with UCAC4
c


      do i=1,idiobs
      itira4(i)=0
      enddo


      call posred (idiobs,icofsp,ireflex,rac,dec,iduc4,nucac4,rauc4,
     ?deuc4,nest,xob,yob,xest,yest,xp,yp,pcoru4,ngrau,ngrau3,ngrau5,
     ?nstar4,nfina4,xrauc4,ydeuc4,era4,ede4,alfsi4,delsi4,alfre4,delre4,
     ?coefx4,coefy4,ecofx4,ecofy4,itira4,avam4,dvam4,ierro)




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with UCAC5
c


      do i=1,idiobs
      itira5(i)=0
      enddo



      call posred (idiobs,icofsp,ireflex,rac,dec,iduc5,nucac5,rauc5,
     ?deuc5,nest,xob,yob,xest,yest,xp,yp,pcoru5,ngrau,ngrau3,ngrau5,
     ?nstar5,nfina5,xrauc5,ydeuc5,era5,ede5,alfsi5,delsi5,alfre5,delre5,
     ?coefx5,coefy5,ecofx5,ecofy5,itira5,avam5,dvam5,ierro)




c
c     Astrometry of all modes.
c
c
c     (RA,Dec) reduction with User catalogue
c


      if (iuserc.eq.1) then

      do i=1,idiobs
      itiras(i)=0
      enddo



      call posred (idiobs,icofsp,ireflex,rac,dec,iducs,nucaus,raucs,
     ?deucs,nest,xob,yob,xest,yest,xp,yp,pcorus,ngrau,ngrau3,ngrau5,
     ?nstars,nfinas,xraucs,ydeucs,eras,edes,alfsis,delsis,alfres,delres,
     ?coefxs,coefys,ecofxs,ecofys,itiras,avams,dvams,ierro)



      endif


c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_1 catalogue
c


      if (nstag1.eq.0) nstag1=1

      if (nstag1.lt.nfing1) nfing1=nstag1

      percg1=100.d0*(nstag1-nfing1)/nstag1

c
      open (97,file=redrg1)

 450  read (97,*,end=451)
      go to 450

 451  call backsp (2,nbac,97)

c

      write (97,452) alfsg1,delsg1,nstag1,nfing1,percg1,avamg1,dvamg1,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g1cofx(i),i=1,icofsp),(g1cofy(i),i=1,icofsp),
     ?(g1ecfx(i),i=1,icofsp),(g1ecfy(i),i=1,icofsp)



 452  format(2(1x,f6.3),2(1x,i4),1x,f6.2,2(1x,f6.3),1x,f16.8,
     ?1x,i2.2,1x,i2.2,1x,f7.4,1x,a1,i2.2,1x,i2.2,1x,f6.3,2x,i4,2x,a20,
     ?2x,a50,1x,a20,2(1x,i5),1x,4(21(1x,es24.16e3)))


c452  format(2(1x,f6.3),2(1x,i4),1x,f6.2,2(1x,f6.3),1x,f16.8,
c    ?1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i4,2x,a20,2x,a50,
c    ?1x,a20,2(1x,i5),1x,4(21(1x,f16.13)))


      close (97)





c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_2 catalogue
c


      if (nstag2.eq.0) nstag2=1

      if (nstag2.lt.nfing2) nfing2=nstag2

      percg2=100.d0*(nstag2-nfing2)/nstag2

c
      open (97,file=redrg2)

 453  read (97,*,end=454)
      go to 453

 454  call backsp (2,nbac,97)

c

      write (97,452) alfsg2,delsg2,nstag2,nfing2,percg2,avamg2,dvamg2,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g2cofx(i),i=1,icofsp),(g2cofy(i),i=1,icofsp),
     ?(g2ecfx(i),i=1,icofsp),(g2ecfy(i),i=1,icofsp)


      close (97)




c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_3 catalogue
c


      if (nstag3.eq.0) nstag3=1

      if (nstag3.lt.nfing3) nfing3=nstag3

      percg3=100.d0*(nstag3-nfing3)/nstag3

c
      open (97,file=redrg3)

 455  read (97,*,end=456)
      go to 455

 456  call backsp (2,nbac,97)

c

      write (97,452) alfsg3,delsg3,nstag3,nfing3,percg3,avamg3,dvamg3,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g3cofx(i),i=1,icofsp),(g3cofy(i),i=1,icofsp),
     ?(g3ecfx(i),i=1,icofsp),(g3ecfy(i),i=1,icofsp)


      close (97)




c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_4 catalogue
c


      if (nstag4.eq.0) nstag4=1

      if (nstag4.lt.nfing4) nfing4=nstag4

      percg4=100.d0*(nstag4-nfing4)/nstag4

c
      open (97,file=redrg4)

 457  read (97,*,end=458)
      go to 457

 458  call backsp (2,nbac,97)

c

      write (97,452) alfsg4,delsg4,nstag4,nfing4,percg4,avamg4,dvamg4,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g4cofx(i),i=1,icofsp),(g4cofy(i),i=1,icofsp),
     ?(g4ecfx(i),i=1,icofsp),(g4ecfy(i),i=1,icofsp)


      close (97)




c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_5 catalogue
c


      if (nstag5.eq.0) nstag5=1

      if (nstag5.lt.nfing5) nfing5=nstag5

      percg5=100.d0*(nstag5-nfing5)/nstag5

c
      open (97,file=redrg5)

 459  read (97,*,end=460)
      go to 459

 460  call backsp (2,nbac,97)

c

      write (97,452) alfsg5,delsg5,nstag5,nfing5,percg5,avamg5,dvamg5,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g5cofx(i),i=1,icofsp),(g5cofy(i),i=1,icofsp),
     ?(g5ecfx(i),i=1,icofsp),(g5ecfy(i),i=1,icofsp)


      close (97)





c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_6 catalogue
c


      if (nstag6.eq.0) nstag6=1

      if (nstag6.lt.nfing6) nfing6=nstag6

      percg6=100.d0*(nstag6-nfing6)/nstag6

c
      open (97,file=redrg6)

 461  read (97,*,end=462)
      go to 461

 462  call backsp (2,nbac,97)

c

      write (97,452) alfsg6,delsg6,nstag6,nfing6,percg6,avamg6,dvamg6,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g6cofx(i),i=1,icofsp),(g6cofy(i),i=1,icofsp),
     ?(g6ecfx(i),i=1,icofsp),(g6ecfy(i),i=1,icofsp)


      close (97)



c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with GAIA_7 catalogue
c


      if (nstag7.eq.0) nstag7=1

      if (nstag7.lt.nfing7) nfing7=nstag7

      percg7=100.d0*(nstag7-nfing7)/nstag7

c
      open (97,file=redrg7)

 463  read (97,*,end=464)
      go to 463

 464  call backsp (2,nbac,97)

c

      write (97,452) alfsg7,delsg7,nstag7,nfing7,percg7,avamg7,dvamg7,
     ?dj,iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(g7cofx(i),i=1,icofsp),(g7cofy(i),i=1,icofsp),
     ?(g7ecfx(i),i=1,icofsp),(g7ecfy(i),i=1,icofsp)


      close (97)





c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with User catalogue
c


      if (iuserc.eq.1) then


      if (nstars.eq.0) nstars=1

      if (nstars.lt.nfinas) nfinas=nstars

      percus=100.d0*(nstars-nfinas)/nstars

c
      open (97,file=redrus)

 465  read (97,*,end=466)
      go to 465

 466  call backsp (2,nbac,97)

c

      write (97,452) alfsis,delsis,nstars,nfinas,percus,avams,dvams,dj,
     ?iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(coefxs(i),i=1,icofsp),(coefys(i),i=1,icofsp),
     ?(ecofxs(i),i=1,icofsp),(ecofys(i),i=1,icofsp)


      close (97)


      endif




c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with UCAC4 catalogue
c


      if (nstar4.eq.0) nstar4=1

      if (nstar4.lt.nfina4) nfina4=nstar4

      perc4=100.d0*(nstar4-nfina4)/nstar4

c
      open (97,file=redru4)

 467  read (97,*,end=468)
      go to 467

 468  call backsp (2,nbac,97)

c

      write (97,452) alfsi4,delsi4,nstar4,nfina4,perc4,avam4,dvam4,dj,
     ?iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(coefx4(i),i=1,icofsp),(coefy4(i),i=1,icofsp),
     ?(ecofx4(i),i=1,icofsp),(ecofy4(i),i=1,icofsp)


      close (97)





c
c     Astrometry of all modes.
c
c
c     Writes statistcs of (RA,Dec) reduction with UCAC5 catalogue
c


      if (nstar5.eq.0) nstar5=1

      if (nstar5.lt.nfina5) nfina5=nstar5

      perc5=100.d0*(nstar5-nfina5)/nstar5

c
      open (97,file=redru5)

 469  read (97,*,end=471)
      go to 469

 471  call backsp (2,nbac,97)

c

      write (97,452) alfsi5,delsi5,nstar5,nfina5,perc5,avam5,dvam5,dj,
     ?iah,iam,sa,isig,idg,idm,ds,iexps,ichfil,infits,mchobj,nx,ny,
     ?(coefx5(i),i=1,icofsp),(coefy5(i),i=1,icofsp),
     ?(ecofx5(i),i=1,icofsp),(ecofy5(i),i=1,icofsp)


      close (97)



c
c     Astrometry of all modes.
c
c
c     Average sky background counts
c


      percs=0.2d0
      ncut=5

      volcor=0.d0

      do i=1,nest
      contag(i)=fgcc(i)
      enddo

      n=nest

      if (n.ge.ncut) call quartl (idiobs,ior1,nval1,n,percs,contag)

      call avsdev (idiobs,n,contag,fundo,fundo2)


      ceu2=fundo2*pi*(fwhm/(2.d0*1.177410023d0))**2


c
c     Negative average sky background in ADUs.
c
c     In this case, add a volume correction to all objects. The
c     volume correction corresponds to the amount necessary
c     to get a positive volume for the sky background, if the average
c     sky background was 1 ADU.  
c


      if (fundo.le.0.d0) then

      volcor=(1.d0-fundo)*pi*(fwhm/(2.d0*1.177410023d0))**2

      ceu=pi*(fwhm/(2.d0*1.177410023d0))**2
      
      else

      ceu=fundo*pi*(fwhm/(2.d0*1.177410023d0))**2

      endif



c
c     PSF volumes for astrometry mode 0 - sky background volume correction 
c


      if (iastro.eq.0) then

      do i=1,nest

      volum(i)=volum(i)+volcor

      enddo

      endif


c
c     Retrieves PSF volumes for astrometry modes 1 and 2 
c


      if (iastro.ne.0) then


      do i=1,nest

      sigg=seng(i)/(2.d0*1.177410023d0)

      volum(i)=2.d0*pi*altu(i)*sigg**2+volcor

      enddo

      endif





c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the UCAC4 system
c


      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,iduc4,udmg4,cudmg4,r4s2mg,f4mag,f4mags)




c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the UCAC5 system
c


      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,iduc5,udmg5,cudmg5,r5s2mg,f5mag,f5mags)




c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the User catalogue system
c

      fsmag=d99
      fsmags=d99
      rss2mg=d99

      do i=1,nest
      cudmgs(i)=d99
      enddo


      if (iuserc.eq.1) then

      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,iducs,udmgs,cudmgs,rss2mg,fsmag,fsmags)

      endif





c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the GAIA_1 G catalogue system (GAIA_3 system is the same thing).
c



      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,idga1,cg1mgg,ug1mgg,g1smg,g1fmg,g1fmgs)






c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the GAIA_2 G catalogue system
c



      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,idga2,cg1mgg,ug2mgg,g2smg,g2fmg,g2fmgs)




c
c     Astrometry of all modes.
c
c
c     PSF magnitudes and error, and sky background magnitude
c     in the GAIA_5 G catalogue system (GAIA_6 annd GAIA_7 systems are
c     the same thing).
c



      call magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,contag,ceu,
     ?ceu2,volum,idga5,cg1mgg,ug5mgg,g5smg,g5fmg,g5fmgs)





c
c     Astrometry of all modes.
c
c
c     Updates photometry report
c


c


      open (99,file=fotrel)

 500  read (99,*,end=501)
      go to 500

 501  call backsp (2,nbac,99)
    



c
c
c
c     - fundo,fundo2: average and sigma of sky background in ADUs
c
c     - f4mag,f4mags,r4s2mg: average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the UCAC4 system
c
c     - f5mag,f5mags,r5s2mg: average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the UCAC5 system
c
c     - g1fmg,g1fmgs,g1smg : average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the GAIA_1 system
c                            (GAIA_3 magnitude system is the same thing)
c
c     - g2fmg,g2fmgs,g2smg : average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the GAIA_2 system
c
c     - g5fmg,g5fmgs,g5smg : average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the GAIA_5 system
c                            (GAIA_6 and GAIA_7 magnitude systems are the same thing)
c
c     - fsmag,fsmags,rss2mg: average and sigma magnitude of sky background and sigma of star
c                            catalogue magnitude fitting based on the User catalogue system
c
c     - fwhm,sigfwh : average seeing and sigma about this average
c


      write (99,502) fwhm,sigfwh,fundo,fundo2,f4mag,f4mags,r4s2mg,f5mag,
     ?f5mags,r5s2mg,g1fmg,g1fmgs,g1smg,g2fmg,g2fmgs,g2smg,g5fmg,g5fmgs,
     ?g5smg,fsmag,fsmags,rss2mg,nest,iexps,ichfil,infits,ichobj,nx,ny

 502  format(2(1x,f5.3),2(1x,f7.1),18(1x,f6.3),1x,i5,2x,i4,2x,a20,2x,
     ?a50,2x,a20,2(1x,i5))

      close (99)




c
c     Outputs astrometric results of individual objects for each field
c     for each catalogue reduction (creates the so called xy PRAIA files)
c 


      if (iastro.eq.0) then

      do i=1,nest
      exgcc(i)=scala*exgcc(i)
      eygcc(i)=scala*eygcc(i)
      enddo

      endif



c
c     Astrometry of all modes.
c


      if (lfdp.eq.1) then

      do i=1,nest

      xob(i)=xoob(i)
      yob(i)=yoob(i)

      enddo

      endif


c
c     Outputs PRAIA xy results for GAIA_1 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg1,idga1,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g1fmg,g1fmgs,cg1mgg,ug1mgg,g1mgj,g1mgh,g1mgk,g1smg,
     ?g1emgj,g1emgh,g1emgk,pmag1,pmdg1,epmag1,epmdg1,exgcc,eygcc,erasg1,
     ?edesg1,alfsg1,delsg1,nstag1,nfing1,alfrg1,delrg1,itirg1,xrag1,
     ?ydeg1,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)





c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_2 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg2,idga2,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g2fmg,g2fmgs,cg1mgg,ug2mgg,g1mgj,g1mgh,g1mgk,g2smg,
     ?g1emgj,g1emgh,g1emgk,pmag1,pmdg1,epmag1,epmdg1,exgcc,eygcc,erasg2,
     ?edesg2,alfsg2,delsg2,nstag2,nfing2,alfrg2,delrg2,itirg2,xrag2,
     ?ydeg2,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)





c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_3 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg3,idga1,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g1fmg,g1fmgs,cg1mgg,ug1mgg,g1mgj,g1mgh,g1mgk,g1smg,
     ?g1emgj,g1emgh,g1emgk,pmag3,pmdg3,epmag3,epmdg3,exgcc,eygcc,erasg3,
     ?edesg3,alfsg3,delsg3,nstag3,nfing3,alfrg3,delrg3,itirg3,xrag3,
     ?ydeg3,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)




c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_4 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg4,idga1,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g1fmg,g1fmgs,cg1mgg,ug1mgg,g1mgj,g1mgh,g1mgk,g1smg,
     ?g1emgj,g1emgh,g1emgk,pmag4,pmdg4,epmag4,epmdg4,exgcc,eygcc,erasg4,
     ?edesg4,alfsg4,delsg4,nstag4,nfing4,alfrg4,delrg4,itirg4,xrag4,
     ?ydeg4,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)






c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_5 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg5,idga5,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g5fmg,g5fmgs,cg1mgg,ug5mgg,g1mgj,g1mgh,g1mgk,g5smg,
     ?g1emgj,g1emgh,g1emgk,pmag3,pmdg3,epmag3,epmdg3,exgcc,eygcc,erasg5,
     ?edesg5,alfsg5,delsg5,nstag5,nfing5,alfrg5,delrg5,itirg5,xrag5,
     ?ydeg5,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)




c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_6 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg6,idga5,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g5fmg,g5fmgs,cg1mgg,ug5mgg,g1mgj,g1mgh,g1mgk,g5smg,
     ?g1emgj,g1emgh,g1emgk,pmag4,pmdg4,epmag4,epmdg4,exgcc,eygcc,erasg6,
     ?edesg6,alfsg6,delsg6,nstag6,nfing6,alfrg6,delrg6,itirg6,xrag6,
     ?ydeg6,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)







c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for GAIA_7 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredg7,idga5,idga1,idga1,xob,yob,
     ?seng,altu,fgcc,g5fmg,g5fmgs,cg1mgg,ug5mgg,g1mgj,g1mgh,g1mgk,g5smg,
     ?g1emgj,g1emgh,g1emgk,pmag1,pmdg1,epmag1,epmdg1,exgcc,eygcc,erasg7,
     ?edesg7,alfsg7,delsg7,nstag7,nfing7,alfrg7,delrg7,itirg7,xrag7,
     ?ydeg7,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)





c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for UCAC4 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredu4,iduc4,iduc4,idga1,xob,yob,
     ?seng,altu,fgcc,f4mag,f4mags,udmg4,cudmg4,g1mgj,g1mgh,g1mgk,
     ?r4s2mg,g1emgj,g1emgh,g1emgk,pmra4,pmde4,epmra4,epmde4,exgcc,eygcc,
     ?era4,ede4,alfsi4,delsi4,nstar4,nfina4,alfre4,delre4,itira4,xrauc4,
     ?ydeuc4,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)




c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for UCAC5 (RA,Dec) reductions
c


      call results (d99,idiobs,nest,iredu5,iduc5,iduc5,idga1,xob,yob,
     ?seng,altu,fgcc,f5mag,f5mags,udmg5,cudmg5,g1mgj,g1mgh,g1mgk,
     ?r5s2mg,g1emgj,g1emgh,g1emgk,pmra5,pmde5,epmra5,epmde5,exgcc,eygcc,
     ?era5,ede5,alfsi5,delsi5,nstar5,nfina5,alfre5,delre5,itira5,xrauc5,
     ?ydeuc5,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)





c
c     Astrometry of all modes.
c
c
c     Outputs PRAIA xy results for User catalogue (RA,Dec) reductions
c

      if (iuserc.eq.1) then

      call results (d99,idiobs,nest,iredus,iducs,iducs,idga1,xob,yob,
     ?seng,altu,fgcc,fsmag,fsmags,udmgs,cudmgs,g1mgj,g1mgh,g1mgk,
     ?rss2mg,g1emgj,g1emgh,g1emgk,pmras,pmdes,epmras,epmdes,exgcc,eygcc,
     ?eras,edes,alfsis,delsis,nstars,nfinas,alfres,delres,itiras,xraucs,
     ?ydeucs,iuth,iutm,sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,
     ?mchobj,nx,ny,xfdp,yfdp)

      endif



c
c     Astrometry of all modes.
c
c
c     Provides observed minus reference position statistics for targets
c     for each of the (RA,Dec) catalogue reductions 
c
c



c
c     Astrometry of all modes.
c
c
c     UCAC4
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** UCAC4: Results **** '
      write (*,*) 

      icato='q'

      call estat (box,tbox,ialvos,iredu4,ialvu4,malvu4,nalvu4,obtipo,
     ?band,iau,icato)



c
c     Astrometry of all modes.
c
c
c     UCAC5
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** UCAC5: Results **** '
      write (*,*) 

      icato='U'

      call estat (box,tbox,ialvos,iredu5,ialvu5,malvu5,nalvu5,obtipo,
     ?band,iau,icato)



c
c     Astrometry of all modes.
c
c
c     User catalogue
c


      if (iuserc.eq.1) then


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** User Catalogue: Results **** '
      write (*,*) 

      call estat (box,tbox,ialvos,iredus,ialvus,malvus,nalvus,obtipo,
     ?band,iau,icat)

      endif



c
c     Astrometry of all modes.
c
c
c     GAIA_1 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_1 (all stars): Results **** '
      write (*,*) 

      icato='U'

      call estat (box,tbox,ialvos,iredg1,ialvg1,malvg1,nalvg1,obtipo,
     ?band,iau,icato)




c
c     Astrometry of all modes.
c
c
c     GAIA_2 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_2 (stars within magnitude range): Results
     ? **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg2,ialvg2,malvg2,nalvg2,obtipo,
     ?band,iau,icato)






c
c     Astrometry of all modes.
c
c
c     GAIA_3 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_3 (all stars): Results **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg3,ialvg3,malvg3,nalvg3,obtipo,
     ?band,iau,icato)





c
c     Astrometry of all modes.
c
c
c     GAIA_4 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_4 (all stars): Results **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg4,ialvg4,malvg4,nalvg4,obtipo,
     ?band,iau,icato)




c
c     Astrometry of all modes.
c
c
c     GAIA_5 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_5 (only stars with proper motions): Resul
     ?ts **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg5,ialvg5,malvg5,nalvg5,obtipo,
     ?band,iau,icato)





c
c     Astrometry of all modes.
c
c
c     GAIA_6 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_6 (only stars with proper motions): Resul
     ?ts **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg6,ialvg6,malvg6,nalvg6,obtipo,
     ?band,iau,icato)




c
c     Astrometry of all modes.
c
c
c     GAIA_7 catalogue
c


      write (*,*) 
      write (*,*) 
      write (*,*) 
      write (*,*) '  **** GAIA_7 (only GAIA_5/6 stars, but without UCAC-
     ?based proper motions): Results **** '
      write (*,*) 


      icato='U'

      call estat (box,tbox,ialvos,iredg7,ialvg7,malvg7,nalvg7,obtipo,
     ?band,iau,icato)





c
c     Astrometry of all modes.
c
c
c     Finalizing the astrometry of the field
c



 62   continue



c
c     Stores flattened image in fits format for debug purposes
c
c
c     do i=1,ny
c     do j=1,nx
c     pixel(j,i)=pixmat(j,i)
c     enddo
c     enddo
c      
c
c     subf='teste.fits'
c     if=83
c     betpix=-32
c     mswap=2
c     scale=1.d0
c     zero=0.d0
c
c
c     call wfits (ipmax,if,subf,betpix,mswap,nx,ny,pixel,scale,zero)
c
c



 60   continue

c


      write (*,*)
      write (*,*)
      write (*,*)
      write (*,*)
      write (*,61) 
 61   format (23x,'Processing terminated.')
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '

      end



c
c
c     Subrotine refits
c
c
c     Reads integer or floating point FITS images
c
c
c      Last modification:  M. Assafin  10/Dec/2016
c
c


      subroutine refits (ipmax,pixmat,infits,nx,ny,nheads,ichobj,ipflag,
     ?bscale,bzero,kswap,iswap,nswap,bitpix,mfdp,jfdp,jjfdp,nfdp,adx,
     ?ady,kadx,kady,crpx,crpy,cdx,cdy,kcrpx,kcrpy,kcdx,kcdy)



      implicit real*8 (a-h,o-z)
      parameter (ihead=100)

      integer*2 bitpix

      real*4 pixmat
      integer*2 iwork2
      integer*4 iwork4
      real*4 work4
      integer*8 iwork8
      real*8 work8

      integer*1 swork,iby8


      dimension iwork2(1440),swork(2880),iby8(8)
      dimension work4(720),iwork4(720)
      dimension work8(360),iwork8(360)

      dimension pixmat(ipmax,ipmax)


      dimension adx(jfdp),ady(jfdp),cdx(2),cdy(2)


      character*150 infits
      character*69 ichobj
      character*9  naxisx,naxisy,bitpx,ibscal,ibzero
      character*1  ler(2880),ibr
      character*9 iend,kend
      character*50 erro
      character*20 imaux
      character*4 jmaux
      character*9 sista
      character*29 systa

      character*8 kadx(jjfdp),kady(jjfdp),kcrpx,kcrpy,kcdx(2),kcdy(2)

      character*2880 header
      character*(ihead*2880) head

c

      data naxisx /'NAXIS1  ='/
      data naxisy /'NAXIS2  ='/
      data bitpx  /'BITPIX  ='/
      data ibscal /'BSCALE  ='/
      data ibzero /'BZERO   ='/
      data iend   /'END      '/
      data ibr    /' '/

c

      nbytes=2880

      if=1

c
c     Abre arquivo fits
c

      

      open(if,file=infits,access='direct',form='unformatted',recl=2880)


c
c     Abre arquivo auxiliar
c

      sista='rm -f -r '

      imaux=''

      imaux(1:16)='PRAIA_refits.aux'


      do 1 i=1,9999

      write (jmaux,'(i4.4)') i

      imaux(17:20)=jmaux(1:4)

      open(99,file=imaux,access='direct',form='unformatted',
     ?recl=2880,status='old',err=2)
      close (99)
 1    continue

 2    close (99)

      open(99,file=imaux,access='direct',form='unformatted',
     ?recl=2880)

      systa=sista//imaux

c
c     Determina quantos headers existem na imagem
c


      do 20 i=1,ihead

      header='' 

      read (1,rec=i,err=32) header

      head(2880*(i-1)+1:2880*i)=header(1:2880)
 

      do k = 1,2880,80
      if (header(k:k+3).eq.'END ') iendh=i
      enddo

 
c
 20   continue

c

 30   nheads=iendh

c

      if (nheads.gt.ihead) then
      write (*,31) ihead
 31   format('Header size exceeded. More than ',i5,' pages. Exiting.')
      ierro=1
      close (1)
      close (99)
      return
      endif 

c
      go to 34

c

 32   continue

      write (*,33)
 33   format('Reached end of fits file. Exiting.')
      ierro=1
      close (1)
      close (99)
      return


c


 34   continue

c     write (*,*)
c     write (*,*) 'nheads ',nheads
c     stop



c
c     Field Distortion Pattern
c
c     Modes 3, 4 (DES): 3rd degree (x,y) polynomial coefficients
c


      if (mfdp.eq.3 .or. mfdp.eq.4) then

      do j=1,nfdp

      do ii=1,nheads

      header='' 
      read(1,rec=ii) header

      do i=1,2880,80

c
c     x nth-coefficient
c

      if (header(i:i+7).eq.kadx(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 100
      enddo

 100  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 101
      enddo

 101  k2=k-1

      read (header(k1:k2),*) adx(j)

      endif



c
c     y nth-coefficient
c

      if (header(i:i+7).eq.kady(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 102
      enddo

 102  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 103
      enddo

 103  k2=k-1

      read (header(k1:k2),*) ady(j)

      endif


      enddo

      enddo

      enddo

      endif




c
c     Mode 4 (DES, DECAM): 
c

      if (mfdp.eq.4 .and. nfdp.gt.3) then



c
c     CRPIXi values for DES
c


      do ii=1,nheads

      header='' 
      read(1,rec=ii) header


      do i=1,2880,80


      if (header(i:i+7).eq.kcrpx) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 104
      enddo

 104  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 105
      enddo

 105  k2=k-1

      read (header(k1:k2),*) crpx

      endif

c

      if (header(i:i+7).eq.kcrpy) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 106
      enddo

 106  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 107
      enddo

 107  k2=k-1

      read (header(k1:k2),*) crpy

      endif


      enddo

      enddo




c
c     CD_ji linear matrix transformation
c


      do j=1,2

      do ii=1,nheads

      header='' 
      read(1,rec=ii) header

      do i=1,2880,80



      if (header(i:i+7).eq.kcdx(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 108
      enddo

 108  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 109
      enddo

 109  k2=k-1

      read (header(k1:k2),*) cdx(j)

      endif

c

      if (header(i:i+7).eq.kcdy(j)) then

      do k=i+9,i+79
      if (header(k:k).ne.' ') go to 110
      enddo

 110  k1=k

      do k=k1+1,i+79
      if (header(k:k).eq.' ') go to 111
      enddo

 111  k2=k-1

      read (header(k1:k2),*) cdy(j)

      endif


      enddo
 
      enddo
 
      enddo



      endif





c
c     Debug
c
c
c     do i=1,nfdp
c     write (*,*) 'coef, x = ',kadx(i),adx(i)
c     enddo
c     write (*,*)
c     do i=1,nfdp
c     write (*,*) 'coef, y = ',kady(i),ady(i)
c     enddo
c     stop
c
c
c     write (*,*) 'crpx = ',crpx
c     write (*,*) 'crpy = ',crpy
c
c     do i=1,2
c     write (*,*) 'cdx, cdy = ',cdx(i),cdy(i)
c     enddo
c
c     stop
c


c
c     Determina dimensao nx da matriz
c

      do i=1,nheads

      read(1,rec=i) ler


      call find (ler,naxisx,key,dnx)

      if (key.gt.0) nx=dnx


      enddo



c
c     Determina dimensao ny da matriz
c

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,naxisy,key,dny)

      if (key.gt.0) ny=dny

      enddo



c
c     Determina bitpix (imagem integer ou real?)
c

      if (bitpix.eq.-99) then

c     bitpix=16

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,bitpx,key,bpix)

      if (key.gt.0) bitpix=bpix

      enddo

      endif


      i=bitpix
      if (i.lt.0) i=-i

      
      ibytes=i/8+0.1
      kwork=nbytes/ibytes+0.1


c
c     Determina bscale 
c

      if (ipflag.ne.1) then

      bscale=1.d0

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,ibscal,key,bsc)

      if (key.gt.0) bscale=bsc

      enddo

      endif


c
c     Determina bzero
c


      if (ipflag.ne.1) then

      bzero=0.d0

      do i=1,nheads

      read(1,rec=i) ler

      call find (ler,ibzero,key,bzr)

      if (key.gt.0) bzero=bzr

      enddo

      endif


c
c     Le a matriz de pixels
c


c
c     Checa byte-swap (litteendian ou bigendian)
c
c     kswap : chave do usuario
c
c          kswap = 0 determinacao automatica de byte-swap
c          kswap = 1 sem byte-swap (definido pelo usuario)
c          kswap = 2 com byte-swap (definido pelo usuario)
c
c     Determinacao automatica (kswap=0):
c
c     iswap=1  sem byte-swap
c     iswap=2  com byte-swap
c
c


      if (kswap.ne.0) then

      iswap=kswap

      go to 50

      endif


      irec=nheads+((nx*ny)/2.d0)*ibytes/nbytes 

c

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) iwork2
      if (ibytes.eq.4) read (1,rec=irec) iwork4
      if (ibytes.eq.8) read (1,rec=irec) iwork8

      else

      if (ibytes.eq.4) read (1,rec=irec) work4
      if (ibytes.eq.8) read (1,rec=irec) work8

      endif     


c
c     Media do valor absoluto sem swap
c

      c1=0.d0


      if (bitpix.gt.0) then

      do i=1,kwork
      if (ibytes.eq.2) c=bscale*iwork2(i)+bzero
      if (ibytes.eq.4) c=bscale*iwork4(i)+bzero
      if (ibytes.eq.8) c=bscale*iwork8(i)+bzero
      c1=c1+dabs(c)
      enddo

      c1=c1/kwork

      else

      do i=1,kwork
      if (ibytes.eq.4) c=bscale*work4(i)+bzero
      if (ibytes.eq.8) c=bscale*work8(i)+bzero
      c1=c1+dabs(c)
      enddo

      c1=c1/kwork

      endif



c
c     Testa com swap
c


      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)




c
c     Media do valor absoluto com swap
c

      c2=0.d0


      if (bitpix.gt.0) then

      do i=1,kwork
      if (ibytes.eq.2) c=bscale*iwork2(i)+bzero
      if (ibytes.eq.4) c=bscale*iwork4(i)+bzero
      if (ibytes.eq.8) c=bscale*iwork8(i)+bzero
      c2=c2+dabs(c)
      enddo

      c2=c2/kwork

      else

      do i=1,kwork
      if (ibytes.eq.4) c=bscale*work4(i)+bzero
      if (ibytes.eq.8) c=bscale*work8(i)+bzero
      c2=c2+dabs(c)
      enddo

      c2=c2/kwork

      endif

c
c     Define swap
c

      erro=''

      write (erro,*) c1

      do i=1,50
      if (ichar(erro(i:i)).ge.48 .and. ichar(erro(i:i)).le.57) go to 35
      enddo

      c1=1.d14

 35   continue

      if (c1.lt.1.d-10) c1=1.d14 

c

      erro=''
     
      write (erro,*) c2

      do i=1,50
      if (ichar(erro(i:i)).ge.48 .and. ichar(erro(i:i)).le.57) go to 40
      enddo

      c2=1.d14

 40   continue    

      if (c2.lt.1.d-10) c2=1.d14 


c

      if (c2.lt.c1) then
      iswap=2
      else
      iswap=1
      endif     


c     write (*,*) 'c1 c2 ',c1, c2

c
c     Lendo matriz
c

 50   continue


c

      block=nx*ny*ibytes 
      block=block/nbytes 
      nblock=block
      iresto=(block-nblock)*nbytes
      iresto=iresto/ibytes+0.2

c     write (*,*) 'nx,ny,ibytes,nbytes,block,nblock,iresto = ',nx,ny,
c    ?ibytes,nbytes,block,nblock,iresto

      j=0
      i=1

      do m=1,nblock

      irec=m+nheads

c


      if (iswap.eq.1) then

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) iwork2
      if (ibytes.eq.4) read (1,rec=irec) iwork4
      if (ibytes.eq.8) read (1,rec=irec) iwork8

      else

      if (ibytes.eq.4) read (1,rec=irec) work4
      if (ibytes.eq.8) read (1,rec=irec) work8


      endif

      else

      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)

      endif

c

      if (bitpix.gt.0) then

      do mm=1,kwork

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.2) pixmat(j,i)=bscale*iwork2(mm)+bzero
      if (ibytes.eq.4) pixmat(j,i)=bscale*iwork4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*iwork8(mm)+bzero

      enddo

      else

      do mm=1,kwork

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif


      if (ibytes.eq.4) pixmat(j,i)=bscale*work4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*work8(mm)+bzero



      enddo      
 

      endif

      enddo

c
c     Ultimo pedaco de bloco da matriz (se existente)
c


      if (iresto.ne.0) THEN



      irec=irec+1

      if (iswap.eq.1) then

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (1,rec=irec) (iwork2(m),m=1,iresto)
      if (ibytes.eq.4) read (1,rec=irec) (iwork4(m),m=1,iresto)
      if (ibytes.eq.8) read (1,rec=irec) (iwork8(m),m=1,iresto)

      else

      if (ibytes.eq.4) read (1,rec=irec) (work4(m),m=1,iresto)
      if (ibytes.eq.8) read (1,rec=irec) (work8(m),m=1,iresto)
     
      endif

      else

      nbytes=iresto*ibytes

      call swap (if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)

      endif

c
      if (bitpix.gt.0) then

      do mm=1,iresto

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.2) pixmat(j,i)=bscale*iwork2(mm)+bzero
      if (ibytes.eq.4) pixmat(j,i)=bscale*iwork4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*iwork8(mm)+bzero

      enddo      

      else

      do mm=1,iresto

      j=j+1
      if (j.gt.nx) then
      j=1
      i=i+1
      endif

      if (ibytes.eq.4) pixmat(j,i)=bscale*work4(mm)+bzero
      if (ibytes.eq.8) pixmat(j,i)=bscale*work8(mm)+bzero

      enddo      

      endif
c

      ENDIF

c

      close (1)
      close (99)


c
c     Sample a pixel to check image reading configurations
c


      if (nswap.eq.1) then

      mmm1=nx/2
      mmm2=ny/2

      write (*,*)
      write (*,*)


      write (*,*) 'Pixel sample to check for image reading configuration
     ?s:'
      write (*,*)

      write (*,60) mmm1,mmm2,pixmat(mmm1,mmm2)
 60   format(' Image (',i5.5,',',i5.5') = ',f18.6)


      write (*,*)
      write (*,*)

      endif



c
c     Debug
c
c
c     read (*,*) j,i
c     j=100
c     i=100
c     write (*,*) 'ix iy = ',j,i
c     write (*,*) 'pixmat = ',pixmat(j,i)
c     write (*,*) 'nx ny ',nx,ny
c     write (*,*) 'swap ',iswap
c     write (*,*) 'c1 c2 = ',c1,c2
c     write (*,*) 'nheads = ',nheads
c     write (*,*) 'bitpix = ',bitpix
c     write (*,*) 'bscale = ',bscale
c     write (*,*) 'bzero  = ',bzero 
c     stop
 


      call system (systa)

c

      return
      end




c
c
c     subroutine find
c
c
c     Acha o valor numerico correspondente a palavra chave
c     do cabecalho fits
c
c     ler     = extracao do header 
c     word    = a palavra do header
c     key     = +1 achou
c             = -1 nao achou
c     valor   = valor numerico encontrado
c
c


      subroutine find (ler,word,key,valor)

      IMPLICIT REAL *8 (A-H,O-Z)
      

      character*1  ler(2880),iplic,ibr,ibar
      character*9  word,kend
      character*1 ivalor(71)




      data iplic /"'"/
      data ibar  /"/"/
      data ibr   /' '/

c

      key=-1
      valor=-1.d14

      icol=1
      id=71

c

      do j=0,35

      do i=1,9
      kend(i:i)=ler(j*80+i)
      enddo

      if (kend.eq.word) then
      key=+1
      go to 20
      endif

      enddo

      go to 50

c

 20   j=j*80
      do i=10,80
      if (ler(j+i).ne.ibr .and. ler(j+i).ne.iplic) go to 30
      enddo
 30   i1=i
      do i=i1+1,80
      if (ler(j+i).eq.ibr .or. ler(j+i).eq.ibar) go to 40
      enddo

 40   i2=i-1

c
      do i=1,id
      ivalor(i)=ibr
      enddo
c

      n=0

      do i=j+i1,j+i2
      n=n+1
      ivalor(n)=ler(i)
      enddo

c

      call chanum (icol,id,ivalor,valor)


 50   continue
      return

      end




c
c     subrotina chanum
c
c     Pega uma string (character) de numeros e extrai o numero da
c     coluna, sem abrir arquivos temporarios para isso.
c
c     string  = contem a string completa
c     palavra = variavel de trabalho contendo a string completa
c     b2 = contem o numero extraido correspondente a coluna dada
c     
c
c     Ultima modificacao: M. Assafin  27/Agosto/2009
c
c
c
      subroutine chanum (icol,id,string,valor)

      implicit real *8 (a-h,o-z)

      integer*8 n

      dimension ni(id+2),nf(id+2)

      character*1 string(id),palavra(id+2),ibra

c

      ibra=' '

c

      do i=1,id+2
      palavra(i)=ibra
      enddo

      do i=1,id
      palavra(i+1)=string(i)
      enddo

c
c     Checando colunas pelos espacos em branco
c

      do i=1,id
      ni(id)=0
      nf(id)=0
      enddo

      ki=0
      kf=0

c
c     Onde estah o comeco do numero
c

      do i=2,id+2

      if (palavra(i-1).eq.ibra .and. palavra(i).ne.ibra) then
      ki=ki+1
      ni(ki)=i
      endif

      enddo

c
c     Onde estah o fim do numero
c

      do i=2,id+2

      if (palavra(i-1).ne.ibra .and. palavra(i).eq.ibra) then
      kf=kf+1
      nf(kf)=i-1
      endif
      
      enddo

c
c     Checa se numero eh positivo ou negativo
c

      isig=+1

      i=ni(icol)
      if (palavra(i).eq.'-') isig=-1
  

c
c     Checa se numero estah escrito em notacao "E" ou "D"
c

      iep=0
      ie=0
      isige=+1


      do i=ni(icol),nf(icol)
      if (palavra(i).eq.'e'.or.palavra(i).eq.'E'.or.palavra(i).eq.'d'
     ?.or.palavra(i).eq.'D') ie=i
      enddo

      if (ie.ne.0) then

      iee=ie

      if (palavra(ie+1).eq.'-') then
      isige=-1
      endif

      if (palavra(ie+1).eq.'-'.or.palavra(ie+1).eq.'+') then
      ie=ie+2
      else
      ie=ie+1
      endif


      iep=0
      j=0
      k=nf(icol)-ie+1
      do i=ie,nf(icol)
      icomp=ichar(palavra(i))
      j=j+1
      iep=iep+(icomp-48)*10.d0**(k-j)
      enddo

      nf(icol)=iee-1

      endif

      expo=10.d0**(isige*iep)


c
c     Determina onde o ponto decimal estah, se nao for mumero inteiro
c

      m=0
      do i=nf(icol),ni(icol),-1
      if (palavra(i).eq.'.') m=i-nf(icol)
      enddo

c
c     Determina os algarismos do numero
c

      k=0
      do i=ni(icol),nf(icol)
      if (palavra(i).ne.'.' .and. palavra(i).ne.'+' .and. palavra(i).
     ?ne.'-') k=k+1
      enddo

      n=0
      j=0
      do i=ni(icol),nf(icol)
      icomp=ichar(palavra(i))
      if (icomp.ge.48 .and. icomp.le.57) then
      j=j+1
      n=n+(icomp-48)*10.d0**(k-j)
      endif
      enddo


      valor=expo*isig*n*10.d0**m

      return
      end




c
c
c     subroutine swap
c
c
c     Swap dos bytes da imagem fits.
c
c     Imagem pode ser integer ou floating point
c
c 
c     Ultima modificacao: M. Assafin 15/08/2015
c
c


      subroutine swap(if,bitpix,ibytes,nbytes,irec,iwork2,iwork4,iwork8,
     ?work4,work8,swork)


      IMPLICIT REAL *8 (A-H,O-Z)



      dimension iwork2(1440),swork(2880),iby8(8)
      dimension work4(720),iwork4(720)
      dimension work8(360),iwork8(360)

      integer*2 bitpix

c     real*4 pixmat
      integer*2 iwork2
      integer*4 iwork4
      real*4 work4
      integer*8 iwork8
      real*8 work8

      integer*1 swork,iby8

c

      read (if,rec=irec) swork


      do k=ibytes,nbytes,ibytes

      do m=1,ibytes
      iby8(m)=swork(k-m+1)
      enddo

      do m=1,ibytes
      swork(k-ibytes+m)=iby8(m)
      enddo

      enddo

      write (99,rec=1) swork

      if (bitpix.gt.0) then

      if (ibytes.eq.2) read (99,rec=1) iwork2
      if (ibytes.eq.4) read (99,rec=1) iwork4
      if (ibytes.eq.8) read (99,rec=1) iwork8

      else

      if (ibytes.eq.4) read (99,rec=1) work4
      if (ibytes.eq.8) read (99,rec=1) work8

      endif     

c

      return
      end





c
c
c      Subroutine gcc
c
c
c      Fits a 2D symmetric Gaussian PSF to the detected rounded-shaped objects.
c      The pixels are fitted within a circular region of diameter 2 times the FWHM
c      in a iterative procedure.
c
c      Routines are based in the non-linear least squares procedures by Bevington (1969),
c      extended to a 2 dimension Gaussian.
c
c
c
c      Last modification: M. Assafin 18/Sep/2015
c 
c 

      subroutine gcc (ipmax,idiobs,icofsp,pixmat,imagem,contag,ior,nval,
     ?lpe,fac,dlimit,plimit,kcontx,kcontt,nx,ny,ix1,ix2,iy1,iy2,raio,bx,
     ?by,sigdex,sigdey,icontt,sigx,alturx,fundox,ierro)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(ipmax,ipmax)

      integer*2 imagem(ipmax,ipmax)

      dimension deltax(5),xsigma(5),param(5),contag(idiobs),ior(idiobs),
     ?nval(idiobs)

 
c
c     Initial data
c

      perc=0.2d0

      ramax=raio

      lado=raio

      iaaux=bx
      iaauy=by


      centrx = 0.d0
      centry = 0.d0

c

      icontt=0
      icontx=1
      transx=0.d0
      transy=0.d0
      sigx=0.d0
      sigy=0.d0

c

      dinc=0.05d0
      xlamda=0.001d0
      ntermx=5
      xresid=-1.d14
      residx=0.d0
      xcent=1.d14
      ycent=1.d14


c
c     Initial parameters for non-linear L.S. fit
c

      param(5)=fundox

      param(1)=bx
      param(2)=by

      param(4)=pixmat(iaaux,iaauy)-fundox


c
c     Initial sigma of Gaussian for non-linear L.S. fit
c

      sigx=param(4)/2.d0

c     lpe=1

      do m=1,lado


      call sperim (ipmax,idiobs,nx,ny,iaaux,iaauy,m,m,lpe,pixmat,
     ?imagem,contag,n,jx1,jx2,jy1,jy2)


      call quartl (idiobs,ior,nval,n,perc,contag)

      call  avsdev (idiobs,n,contag,adum,adus)

      adum=adum-fundox

      if (adum.lt.sigx) go to 10

      enddo


 10   param(3)=(m+0.5d0)/2.35d0



c
c     Increments of parameters
c

      do i=1,ntermx
      xsigma(i)=0.d0
      enddo

 
      do i=1,ntermx
      deltax(i)=dinc*param(i)
      enddo


c
c     Gaussian fitting
c


   14 call gausic (ipmax,icofsp,pixmat,imagem,icontt,raio,ix1,iy1,ix2,
     ?iy2,ntermx,param,deltax,xsigma,xlamda,residx,ierro)


c
c     Checks for errors
c

      if (residx.lt.0.d0) then
      bx=0.d0
      by=0.d0
      return
      endif

      if (ierro.ne.0) then
      bx=0.d0
      by=0.d0
      return
      endif
  

c
c     Checks convergence of internal L.S. iterations
c


      residx = dsqrt(residx)
      centx  = param(1)
      centy  = param(2)
      conver = dabs(residx*dlimit)
      diferd = dabs(residx-xresid)
      difpox = dabs(centx-xcent)
      difpoy = dabs(centy-ycent)

      if ((diferd.lt.conver).and.(difpox.lt.plimit).and.(difpoy.lt.
     ?plimit)) go to 150

      xresid = residx
      xcent=centx
      ycent=centy

      icontx = icontx + 1

      if (icontx.gt.kcontx) go to 150

      go to 14

c

  150 continue


c
c     Eliminates pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting, one by one, until no pixel is above the threshold
c

      teto=fac*residx

      difmax=-1.d14

      n=0

      do 30 i=iy1,iy2
      do 20 j=ix1,ix2

      if (imagem(j,i).lt.-9) go to 20

      call circul (raio,param(1),param(2),j,i,ichave)
      if (ichave.lt.0) go to 20

      difcon=dabs(pixmat(j,i)-fgausi(j,i,param,ierro))

      if (difcon.gt.teto) then

c     write (*,*) 'fac,x,y ',difcon/residx,centx,centy

      if (difcon.gt.difmax) then
      difmax=difcon
      n=n+1
      jxx=j
      iyy=i
      endif

      endif

 20   continue
 30   continue

    
      if (n.gt.0) then
      imagem(jxx,iyy)=-10
      icontx=1
      go to 14
      endif


c
c     Resets eliminated pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting of the previous iteration
c

      do i=iy1,iy2
      do j=ix1,ix2
 
      if (imagem(j,i).eq.-10) imagem(j,i)=0
 
      enddo
      enddo




c
c     Circular trimming with region diameter = 2 FHHM, or more precisely,
c     with radius = 2.5 sigma of the Gaussian
c
c     Region fitting area convergence is reached if radius converges within
c     1% between external L.S. iterations 
c
c

      sigdex = param(3)
      consix = 0.01d0*sigdex
      aux=dabs(sigdex-transx)

      if (aux.lt.consix) go to 154

      icontt = icontt + 1

      if (icontt.eq.kcontt) go to 154

      icontx=0

      raio=2.5d0*sigdex

      if (raio.gt.ramax) raio=ramax

      transx = sigdex
      xlamda=0.001d0
      xresid= -10.d10
      residx=0.d0
      xcent=1.d14
      ycent=1.d14


      go to 14

c

  154 continue

      sigdex = xsigma(1)*residx
      sigdey = xsigma(2)*residx
      alturx = param(4)
      centrx = param(1)
      centry = param(2)
      sigx   = param(3)
      fundox = param(5)

      bx=centrx
      by=centry


      return
      end




c
c
c      Subroutine gausic
c
c
c      Performs the non-linear least-squares fitting of the 2-D Gaussian PSF.
c
c
c      Last modification: M. Assafin 03/Sep/2015
c
c

      subroutine gausic (ipmax,icofsp,pixmat,imagem,key,raio,nx,ny,mx,
     ?my,nterms,a,deltaa,sigmaa,flamda,chisqr,ierro)

      implicit real *8 (a-h,o-z)

      real*4 pixmat(ipmax,ipmax),maximo
      integer*2 imagem(ipmax,ipmax)

      dimension a(5),deltaa(5),sigmaa(5),b(5),alpha(icofsp,icofsp),
     ?beta(icofsp),deriv(5),array(icofsp,icofsp)


      ierro=0
      det=1.d0
      icont=0
      ixc=a(1)
      iconv=20
      jconv=0

c
      chisqr = 0.d0

c
c     Evaluates alpha and beta matrices
c

      if (key.gt.0) go to 31


c


      do 134 j=1,nterms
      beta(j) = 0.d0

      do 134 k=1,j
  134 alpha(j,k) = 0.d0

      do 150 i=ny,my
      do 5001 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5001

      call gderiv (l,i,a,deltaa,deriv,ierro)

      if (ierro.eq.1) go to 107

      icont=icont+1

      do 146 j=1,nterms
      beta(j)= beta(j) + (pixmat(l,i)-fgausi(l,i,a,ierro))*deriv(j)

      if (ierro.eq.1) go to 107

      do 146 k=1,j
  146 alpha(j,k)=alpha(j,k)+deriv(j)*deriv(k)

 5001 continue
  150 continue

c
c     Number of degrees of freedom
c

      free=icont-nterms

c

      if (free.le.0.d0) go to 107


      go to 51

c
 
   31 do 34 j=1,nterms
      beta(j) = 0.d0

      do 34 k=1,j
   34 alpha(j,k) = 0.d0

      do 50 i=ny,my
      do 5000 l=nx,mx

      if (imagem(l,i).lt.-9) go to 5000

      call circul (raio,a(1),a(2),l,i,ichave)
      if (ichave.lt.0) go to 5000

      call gderiv (l,i,a,deltaa,deriv,ierro)

      if (ierro.eq.1) go to 107

      icont=icont+1

      do 46 j=1,nterms
      beta(j)= beta(j)+(pixmat(l,i)-fgausi(l,i,a,ierro))*deriv(j)

      if (ierro.eq.1) go to 107

      do 46 k=1,j
   46 alpha(j,k) =alpha(j,k)+deriv(j)*deriv(k)

 5000 continue
   50 continue
 
 
c
c     Number of degrees of freedom
c

      free=icont-nterms

C

      if (free.le.0.d0) go to 107

c

 
   51 continue

c

      do 53 j=1,nterms
      do 53 k=1,j
   53 alpha(k,j)=alpha(j,k)


c
c     Evaluates chi square at starting point
c


      chisq1=qiquad(ipmax,pixmat,imagem,key,raio,nx,ny,mx,my,free,a,
     ?ierro)

c
      if (ierro.eq.1) go to 107



c				 
c        Invert modified curvature matrix to find new parameters
c


   71 do 74 j=1,nterms
      do 73 k=1,nterms

      aux=alpha(j,j)*alpha(k,k)

      if (aux.lt.0.d0) go to 107

   73 array(j,k)=alpha(j,k)/dsqrt(aux)
   74 array(j,j)=1.d0+flamda

   80 call matinv (nterms,icofsp,array,det,ierro)

      if (ierro.eq.1) go to 107

c

      do 84 j=1,nterms
      b(j)=a(j)

      do 84 k=1,nterms

      aux=alpha(j,j)*alpha(k,k)

      if (aux.lt.0.d0) go to 107

   84 b(j) = b(j) + beta(k)*array(j,k)/dsqrt(aux)


c
c        If chi-square increased, increase flamda and try again
c


      chisqr = qiquad(ipmax,pixmat,imagem,key,raio,nx,ny,mx,my,free,b,
     ?ierro)

c

      if (ierro.eq.1) go to 107

c

      jconv=jconv+1
      if (jconv.gt.iconv) go to 107

      if (chisq1 - chisqr) 95, 101, 101

   95 flamda = 10.d0*flamda

      go to 71

c
c        Evaluate parameters and uncertainties
c


  101 do 104 j=1,nterms
      a(j)=b(j)

      aux=array(j,j)/alpha(j,j)

      if (aux.lt.0.d0) go to 107

      sigmaa(j)=dsqrt(aux)

  104 continue

c

      flamda = flamda/10.d0

      go to 110

c

  107 chisqr = -1.d0
      ierro=1

  110 continue

      return
      end



c
c
c     Subroutine gderiv
c
c     Evaluate the derivatives of the 2D circular Gaussian function
c     given by function FGAUSI
c
c
c     Last modified: M. Assafin  16/Sep/2015
c


      subroutine gderiv (j,i,a,deltaa,deriv,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(5),deltaa(5),deriv(5)

c

      if ((a(3).eq.0.d0).or.(a(4).eq.0.d0)) then
      ierro=1
      return
      endif

c

      x=j
      y=i
      zx=(x-a(1))/a(3)
      zy=(y-a(2))/a(3)
      zx2=zx*zx
      zy2=zy*zy

c
c     Analytical expressions for the derivatives
c

      funcao=fgausi(j,i,a,ierro)

      if (ierro.eq.1) return

      deriv(1)=(x-a(1))*(funcao-a(5))/a(3)**2
      deriv(2)=(y-a(2))*(funcao-a(5))/a(3)**2
      deriv(3)=(zx2+zy2)*(funcao-a(5))/a(3)
      deriv(4)=(funcao-a(5))/a(4)
      deriv(5)=1.d0

c

      return
      end




c
c     Function FGAUSI
c
c     2D circular Gaussian PSF function
c
c
c     Last modified:  M. Assafin   16/Sep/2015
c


      double precision function fgausi (j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(5)

c

      if ((a(3).eq.0.d0).or.(a(4).eq.0.d0)) then
      ierro=1
      return
      endif

c


      x=j
      y=i

      fgausi=a(5)

      zx=(x-a(1))/a(3)
      zy=(y-a(2))/a(3)
      zx2=zx*zx
      zy2=zy*zy

      fgausi=fgausi+a(4)*dexp(-(zx2+zy2)/2.d0)

      return
      end




c
c
c      Function QIQUAD
c
c      Evaluates reduced chi-square of fit to the data
c
c
c      Last modification: M. Assafin 16/Sep/2015
c
c


      double precision function qiquad (ipmax,pixmat,imagem,key,raio,nx,
     ?ny,mx,my,free,a,ierro)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(ipmax,ipmax),maximo
      integer*2 imagem(ipmax,ipmax)
 

      dimension a(5)

c

      chisq=0.d0
      qiquad=0.d0

c

      if (free.le.0.d0) then
      ierro=1
      return
      endif

c

      if (key.gt.0) go to 30

c

      do 38 i=ny,my
      do 37 j=nx,mx

      if (imagem(j,i).lt.-9) go to 37

      chisq=chisq+(pixmat(j,i)-fgausi(j,i,a,ierro))**2

      if (ierro.eq.1) return

 37   continue
 38   continue


      go to 39

c

   30 ixc=a(1)

      do 35 i=ny,my

      call circul (raio,a(1),a(2),ixc,i,ichave)
      if (ichave.lt.0) go to 35

      do 350 j=nx,mx

      if (imagem(j,i).lt.-9) go to 350

      call circul (raio,a(1),a(2),j,i,ichave)
      if (ichave.lt.0) go to 350

      chisq=chisq+(pixmat(j,i)-fgausi(j,i,a,ierro))**2

      if (ierro.eq.1) return

  350 continue
   35 continue

c
c        Divide by the number of degrees of freedom
c


   39 qiquad=chisq/free

      return
      end


c
c
c     Subroutine CIRCUL
c
c
c     Verifies if a pixel is within a circle of radius "raio" and center (xc,yc).
c
c     Output key is "ichave":  +1 : pixel within  the circle
c                              -1 : pixel outside the circle
c
c
c    Last modified:  M. Assafin  16/Sep/2015 
c
c

 
      subroutine circul (raio,xc,yc,ix,iy,ichave)

      implicit real*8 (a-h,o-z)

c

      raio2=raio*raio
      radius=(ix-xc)**2+(iy-yc)**2
      if (radius.le.raio2) then
      ichave=1
      else
      ichave=-1
      endif

      return
      end


C
C     SUBROUTINE MATINV
C
C     PURPOSE
C       INVERT A SYMMETRIC MATRIX AND CALCULATE ITS DETERMINANT
C
C     USAGE
C       CALL MATINV ( NORDER, DET)
C
C     DESCRIPTION OF PARAMETERS
C       ARRAY  - INPUT MATRIX WICH IS REPLACED BY ITS INVERSE
C       NORDER - DEGREE OF MATRIX (ORDER OF DETERMINANT)
C       DET    - DETERMINANT OF INPUT MATRIX
C
C     SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C       NONE
C
C     MODIFICATIONS FOR FORTRAN II
C       OMIT DOUBLE PRECISION SPECIFICATIONS
C       CHANGE DABS TO ABSF IN STATEMENT 23
C
C     COMMENTS
C       DIMENSION STATEMANTS VALID FOR NORDER UP TO 10
C
      SUBROUTINE MATINV (NORDER, icofsp, array, DET,ierro)
      IMPLICIT REAL *8 (A-H,O-Z)
      DIMENSION ARRAY (icofsp,icofsp), IK(icofsp), JK(icofsp)
C
   10 DET = 1.D0
   11 DO 100 K=1, NORDER
C
C        FIND LARGEST ELEMENT ARRAY(I,J) IN REST OF MATRIX
C
      AMAX= 0.D0
   21 DO 30 I=K, NORDER
      DO 30 J=K, NORDER
   23 IF (DABS(AMAX) - DABS(ARRAY(I,J))) 24, 24, 30
   24 AMAX = ARRAY(I,J)
      IK(K) = I
      JK(K) = J
   30 CONTINUE
C
C        INTERCHANGE ROWS AND COLUMNS TO PUT AMAX IN ARRAY(K,K)
C
   31 IF (AMAX) 41, 32, 41
   32 DET = 0.D0
      GO TO 140
   41 I = IK(K)
      IF (I-K) 21, 51, 43
   43 DO 50 J=1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = ARRAY(I,J)
   50 ARRAY(I,J) = -SAVE
   51 J = JK(K)
      IF (J-K) 21, 61, 53
   53 DO 60 I=1, NORDER
      SAVE = ARRAY(I,K)
      ARRAY (I,K) = ARRAY(I,J)
   60 ARRAY (I,J) = -SAVE
C
C        ACCUMULATE ELEMENTS OF INVERSE MATRIX
C
   61 DO 70 I=1, NORDER
      IF (I-K) 63, 70, 63
   63 IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(I,K) = -ARRAY(I,K) / AMAX
   70 CONTINUE
   71 DO 80 I=1, NORDER
      DO 80 J=1, NORDER
      IF (I-K) 74, 80, 74
   74 IF (J-K) 75, 80, 75
   75 ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K)*ARRAY(K,J)
   80 CONTINUE
   81 DO 90 J=1, NORDER
      IF (J-K) 83, 90, 83
   83 IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(K,J) = ARRAY(K,J) / AMAX
   90 CONTINUE
      IF (AMAX.EQ.0.D0) THEN
      IERRO=1
      RETURN
      ENDIF
      ARRAY(K,K) = 1.D0 / AMAX
  100 DET = DET * AMAX
C
C        RESTORE ORDERING OF MATRIX
C
  101 DO 130 L=1, NORDER
      K = NORDER - L + 1
      J = IK(K)
      IF (J-K) 111, 111, 105
  105 DO 110 I=1, NORDER
      SAVE = ARRAY(I,K)
      ARRAY(I,K) = -ARRAY(I,J)
  110 ARRAY(I,J) = SAVE
  111 I = JK(K)
      IF (I-K) 130, 130, 113
  113 DO 120 J=1, NORDER
      SAVE = ARRAY(K,J)
      ARRAY(K,J) = -ARRAY(I,J)
  120 ARRAY(I,J) = SAVE
  130 CONTINUE
  140 CONTINUE
      RETURN
      END








C
C
C     Subroutine ordem
C
C
C
C     Purpose
C
C       Orders data vectors in crescent value order.
C
C
C     Use
C
C     SUBROUTINE ORDEM (IDIM,N,IORDEM,NVAL)
C
C
C     Description of parameters
C
C       IDIM   - vector dimension
C	N      - number of points to be ordered
C       IORDEM - increasing order numbering of NVAL
C       NVAL   - data vector to be ordered
C
C
C     Subroutines and subprograms required
C
C
C
C     Comments
C
C
C
      SUBROUTINE ORDEM (idiobs,N,IORDEM,NVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IORDEM(idiobs),NVAL(idiobs)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=NVAL(L)
          IRA=IORDEM(L)
        ELSE
          RRA=NVAL(IR)
          IRA=IORDEM(IR)
          NVAL(IR)=NVAL(1)
          IORDEM(IR)=IORDEM(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            NVAL(1)=RRA
            IORDEM(1)=IRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(NVAL(J).LT.NVAL(J+1))J=J+1
          ENDIF
          IF(RRA.LT.NVAL(J))THEN
            NVAL(I)=NVAL(J)
            IORDEM(I)=IORDEM(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        NVAL(I)=RRA
        IORDEM(I)=IRA
      GO TO 10
      END








c
c
c     Subroutine dordem
c
c
c
c     Purpose
c
c       Orders data vectors in crescent value order.
c
c
c     Use
c
c     subroutine dordem (idim,n,ior,val)
c
c
c     Description of parameters
c
c       idim   - dimension of vectors
c       n      - number of points to be ordered
c       indx   - increasing order numbering of array "arr"
c       arr    - data array itself, NOT ORDERED
c
c
c     Subroutines and subprograms required
c
c
c
c     Comments
c
c
c     Last modification:  M. Assafin  09/Jan/2017
c
c

      subroutine dordem(idim,n,indx,arr)

      implicit real*8 (a-h,o-z)

      integer idim
      integer n,indx(idim),m,nstack
      real*8 arr(idim)
      parameter (m=7,nstack=50)
      integer i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
      real*8 a
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.m)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
c       if(jstack.gt.nstack)pause 'NSTACK too small in ordem'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1

      return
      end




c
c
c
c     Subroutine gaia1
c
c
c
c     Extracts stars from the GAIA_1 catalogue.
c
c
c     GAIA_1 refers to the complete GAIA catalog (GAS) from the release version DR1
c     (14/Sep/2016).
c
c     We use the version of GAS organized by J. Camargo and LIneA team in 0.1 degrees
c     declination zones, with crescent RA within each zone, but otherwise keeping
c     the original line contents in CSV format.
c
c
c     - rag1,deg1: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - erag1, edeg1: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmag1: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdg1: Dec proper motion in arcsec/year
c     - epmag1, epmdg1: proper motion errors in arcsec/year
c     - g1mg_  ...: J, H and K magnitudes from 2MASS in the GAIA catalogue (code 99 if absent)
c     - cg1mgg: GAIA magnitude in the G band
c
c
c
c      Last modification: M. Assafin 07/Sep/2017
c
c

      subroutine gaia1 (idiobs,raizg1,epoj,rac,dec,ramin,ramax,demin,
     ?demax,rag1,deg1,erag1,edeg1,pmag1,pmdg1,epmag1,epmdg1,cg1mgg,
     ?nest)


      implicit real *8 (a-h,o-z)


      dimension rag1(idiobs),deg1(idiobs),erag1(idiobs),edeg1(idiobs),
     ?pmag1(idiobs),pmdg1(idiobs),epmag1(idiobs),epmdg1(idiobs),
     ?cg1mgg(idiobs)



      dimension inic(48),ifim(48),jxmin(2),jxmax(2),cxmin(2),cxmax(2)


c     character *1 isig


      character*1  ip,norsul(2)
      character*65 ifaixa,kfaixa
      character*8  iarq,karq
      character*50 raizg1


      data iarq/'_dec.csv'/
      data karq/'_dec.acc'/

      data norsul/'s','n'/
 

c
c     Auxiliary data
c


      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c


      iun=95

      idin50=50
      izon=1800

      sfac=1.d0
      bin=7.5d0
      nbin=48


c
c     Determines pointers for declination zone reading given
c     the furnished (min,max) RA limits
c



      if (ramax.ge.ramin) then


      inde=1

      jxmin(1)=ramin/bin+1
      jxmax(1)=ramax/bin+1

      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin



      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2

      jxmin(1)=ramin/bin+1
      jxmax(1)=nbin

      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac


      jxmin(2)=1
      jxmax(2)=ramax/bin+1


      if (jxmin(2).lt.1) jxmin(2)=1
      if (jxmax(2).lt.1) jxmax(2)=1

      if (jxmin(2).gt.nbin) jxmin(2)=nbin
      if (jxmax(2).gt.nbin) jxmax(2)=nbin


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif



c
c     Reads GAIA_1 declination zones by 0.1 degrees steps
c


      dfaixa=demin-0.2d0

      nest=0

c

      do 30 k=1,izon

      dfaixa=dfaixa+0.1d0


      if (dfaixa-demax.gt.0.1d0) go to 35

      if (dfaixa.lt.-90.d0) dfaixa=-90.d0

      j1=dabs(dfaixa)*10.d0
      
      j2=j1+1


      if (dfaixa.lt.0.d0) then

      ip=norsul(1)

      else

      ip=norsul(2)

      endif


c
c     Mounts current declination zone file name
c


      ifaixa=''
      ifaixa=raizg1



      do l=1,idin50
      if (ifaixa(l:l).eq.' ') go to 10
      enddo

 10   continue

      if (dfaixa.ge.0.d0) then

      write(ifaixa(l:l+14),'(a1,2(i3.3),a8)') ip,j1,j2,iarq

      else

      write(ifaixa(l:l+14),'(a1,2(i3.3),a8)') ip,j2,j1,iarq

      endif

c

      write (*,14) ifaixa
 14   format(1x,'GAIA_DR1: ',a65)


c
c     Mounts current declination zone's acceleration reading index
c     file name
c

      kfaixa=ifaixa

      do j=1,idin50+15
      if (ifaixa(j:j).eq.' ') go to 15
      enddo

 15   kfaixa(j-3:j-1)='acc'




c
c     Loads acceleration indexes for the current declination zone file
c
c     GAIA Catalogue RA indexed by 7.5 degree steps in each declination zone file
c
c     inic - rec pointer for the 1rst star in the n-th RA range
c     ifim - rec pointer for the last star in the n-th RA range
c
c

      open (iun,file=kfaixa)


      do j=1,nbin
      read (iun,*) inic(j),ifim(j)
      enddo

      close (iun)




c
c     Opens current declination zone GAIA file
c

      open (iun,file=ifaixa)


c


      do 20 in=1,inde

      do 19 nn=jxmin(in),jxmax(in)


      i1=inic(nn)
      i2=ifim(nn)

      kk=i2-i1+1

      rewind (iun)


      do i=1,i1-1

      read (iun,*,err=19,end=19)

      enddo


      do 18 n=1,kk



c
c     GAIA_1 (RA,Dec)s in degrees
c
c     GAIA_1 (RA,Dec) errors in arcseconds
c
c     GAIA_1 proper motions and errors (if any) in arcseconds/year (Julian years)
c
c



      call rgas (iun,epoj,cxmin(in),cxmax(in),demin,demax,rag,deg,erag,
     ?edeg,pmag,pmdg,epmag,epmdg,cgmgg,mark)





c
c     Checks if star falls within FOV
c


      if (mark.eq.2) go to 19

      if (mark.eq.1) go to 18


c
c     A GAIA star in the FOV
c



      nest=nest+1

      if (nest.gt.idiobs) then
      close (iun)
      go to 35
      endif



      rag1(nest)=rag
      deg1(nest)=deg
      erag1(nest)=erag
      edeg1(nest)=edeg
      pmag1(nest)=pmag
      pmdg1(nest)=pmdg
      epmag1(nest)=epmag
      epmdg1(nest)=epmdg
      cg1mgg(nest)=cgmgg




c
c     debug alfa delta
c
c
c     ra=rag1(nest)/15.d0
c     de=deg1(nest)
c     dmag=cg1mgg(nest)
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,dmag,erag1(nest),
c    ?edeg1(nest),pmag1(nest),pmdg1(nest),epmag1(nest),epmdg1(nest)
c



 18   continue

 19   continue

 20   continue


c

      close (iun)

c

 30   continue

 35   continue

 
      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)'Attention: overflow in the number of GAIA stars.' 
      write (*,*)'Using only the GAIA data stored before the overflow.'
      write (*,*)
      write (*,*)

      endif

c

      write (*,*)
      write (*,*)


c


      return
      end


c
c
c
c     Subroutine rgas
c
c
c
c     Reads star data one by one from the GAIA_1 catalogue.
c
c
c     GAIA_1 refers to the complete GAIA catalog (GAS) from the release version DR1
c     (14/Sep/2016).
c
c     We use the version of GAS organized by J. Camargo and LIneA team in 0.1 degrees
c     declination zones, with crescent RA within each zone, but otherwise keeping
c     the original line contents in CSV format.
c
c
c     - rag1,deg1: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - erag1, edeg1: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmag1: RA proper motion (multiplied by the cosD factor) in arcsec/year (Julian year)
c     - pmdg1: Dec proper motion in arcsec/year (Julian year)
c     - epmag1, epmdg1: proper motion errors in arcsec/year (Julian year)
c     - g1mg_  ...: J, H and K magnitudes from 2MASS in the GAIA catalogue (code 99 if absent)
c     - cg1mgg: GAIA magnitude in the G band
c
c     - epoj: epoch of observation, used for computing (RA,Dec) and related errors
c             for observation epoch in case of star with proper motions (currently
c             TGAS stars mixed in GAS stars)
c
c     - mark: indicates file reading status
c
c 
c
c      Last modification: M. Assafin 15/Jun/2017
c
c
c
c

      subroutine rgas (iun,epoj,ramin,ramax,demin,demax,rag1,deg1,erag1,
     ?edeg1,pmag1,pmdg1,epmag1,epmdg1,cg1mgg,mark)


      implicit none


      double precision ramin,ramax,demin,demax,rag1,deg1,erag1,edeg1,
     ?pmag1,pmdg1,epmag1,epmdg1,cg1mgg,epoj,epgaia,d99,d98,pi,grarad,
     ?radgra


      double precision Epoch,RAdeg,e_RAdeg,DEdeg,e_DEdeg,Plx,e_Plx,
     +                 pmRA,e_pmRA,pmDE,e_pmDE,RADEcor,RAPlxcor,
     +                 RApmRAcor,RApmDEcor,DEPlxcor,DEpmRAcor,DEpmDEcor,
     +                 PlxpmRAcor,PlxpmDEcor,pmRApmDEcor,DQ,epsi,sepsi,
     +                 ARF,WAL,WAC,sK1,sK2,sK3,sK4,mK1,mK2,mK3,mK4,FG,
     +                 e_FG,Gmag,GLON,GLAT,ELON,ELAT


      INTEGER*8 SolID,Source,RandomI,NAL,NAC,NgAL,NgAC,NbAL,NbAc,
     +          Apr,MatchObs,o_Gmag


      integer i,j,mark,iun
      integer commas(2000)

      character*2000 gas

      character*5 APF,Dup
      character*13 Var


c
c     Auxiliary data
c


      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      epgaia=2015.d0

      d99=99.999d3
      d98=98.999d3

c
c     Reads GAIA data, one by one star
c


      mark=0


c
c     Variable mark marks reading as follows:
c
c
c     mark=0 -> star within  FOV
c     mark=1 -> star outside FOV
c     mark=2 -> reading error or end of file (end of file should not happen,
c               since we follow the acceleration indexes)
c
c

 
 1    continue


      if (mark.ne.0) then
      mark=2
      return
      endif


c
c     Reads declination zone file
c


      read(iun,'(a2000)',err=1,end=1) gas

      j=1

      do i=1,2000
       if (gas(i:i).eq.',') then
        commas(j)=i
        j=j+1
       endif
      enddo

      j=j-1


c
c      Extracts data from CSV format
c


      read(gas(1:commas(1)-1),*)SolID
      read(gas(commas(1)+1:commas(2)-1),*)Source
      read(gas(commas(2)+1:commas(3)-1),*)RandomI
      read(gas(commas(3)+1:commas(4)-1),*)Epoch
      read(gas(commas(4)+1:commas(5)-1),*)RAdeg
      read(gas(commas(5)+1:commas(6)-1),*)e_RAdeg
      read(gas(commas(6)+1:commas(7)-1),*)DEdeg
      read(gas(commas(7)+1:commas(8)-1),*)e_DEdeg


c
c     Checks if star is within FOV
c


      mark=1



      if (RAdeg.lt.ramin) return
      if (RAdeg.gt.ramax) return

      if (DEdeg.lt.demin) return
      if (DEdeg.gt.demax) return



c
c     Star is within FOV: continue data extraction
c



      mark=0


c


      if (commas(8).eq.commas(9)-1) then
       Plx=d99
      else
       read(gas(commas(8)+1:commas(9)-1),*)Plx
      endif
      if (commas(9).eq.commas(10)-1) then
       e_Plx=d99
      else
       read(gas(commas(9)+1:commas(10)-1),*)e_Plx
      endif
      if (commas(10).eq.commas(11)-1) then
       pmRA=d99
      else
       read(gas(commas(10)+1:commas(11)-1),*)pmRA
      endif
      if (commas(11).eq.commas(12)-1) then
       e_pmRA=d99
      else
       read(gas(commas(11)+1:commas(12)-1),*)e_pmRA
      endif
      if (commas(12).eq.commas(13)-1) then
       pmDE=d99
      else
       read(gas(commas(12)+1:commas(13)-1),*)pmDE
      endif
      if (commas(13).eq.commas(14)-1) then
       e_pmDE=d99
      else
       read(gas(commas(13)+1:commas(14)-1),*)e_pmDE
      endif
      read(gas(commas(14)+1:commas(15)-1),*)RADEcor
      if (commas(15).eq.commas(16)-1) then
       RAPlxcor=d99
      else
       read(gas(commas(15)+1:commas(16)-1),*)RAPlxcor
      endif
      if (commas(16).eq.commas(17)-1) then
       RApmRAcor=d99
      else
       read(gas(commas(16)+1:commas(17)-1),*)RApmRAcor
      endif
       if (commas(17).eq.commas(18)-1) then
        RApmDEcor=d99
      else
       read(gas(commas(17)+1:commas(18)-1),*)RApmDEcor
      endif
      if (commas(18).eq.commas(19)-1) then
       DEPlxcor=d99
      else
       read(gas(commas(18)+1:commas(29)-1),*)DEPlxcor
      endif
      if (commas(19).eq.commas(20)-1) then
       DEpmRAcor=d99
      else
       read(gas(commas(19)+1:commas(20)-1),*)DEpmRAcor
      endif
      if (commas(20).eq.commas(21)-1) then
       DEpmDEcor=d99
      else
       read(gas(commas(20)+1:commas(21)-1),*)DEpmDEcor
      endif
      if (commas(21).eq.commas(22)-1) then
       PlxpmRAcor=d99
      else
       read(gas(commas(21)+1:commas(22)-1),*)PlxpmRAcor
      endif
      if (commas(22).eq.commas(23)-1) then
       PlxpmDEcor=d99
      else
       read(gas(commas(22)+1:commas(23)-1),*)PlxpmDEcor
      endif
      if (commas(23).eq.commas(24)-1) then
       pmRApmDEcor=d99
      else
       read(gas(commas(23)+1:commas(24)-1),*)pmRApmDEcor
      endif
      read(gas(commas(24)+1:commas(25)-1),*)NAL
      read(gas(commas(25)+1:commas(26)-1),*)NAC
      read(gas(commas(26)+1:commas(27)-1),*)NgAL
      read(gas(commas(27)+1:commas(28)-1),*)NgAC
      read(gas(commas(28)+1:commas(29)-1),*)NbAL
      read(gas(commas(29)+1:commas(30)-1),*)NbAC
      if (commas(30).eq.commas(31)-1) then
       DQ=d99
      else
       read(gas(commas(30)+1:commas(31)-1),*)DQ
      endif
      read(gas(commas(31)+1:commas(32)-1),*)epsi
      read(gas(commas(32)+1:commas(33)-1),*)sepsi
      read(gas(commas(33)+1:commas(34)-1),*)APF
      read(gas(commas(34)+1:commas(35)-1),*)ARF
      read(gas(commas(35)+1:commas(36)-1),*)WAL
      if (commas(36).eq.commas(37)-1) then
       WAC=d99
      else
       read(gas(commas(36)+1:commas(37)-1),*)WAC
      endif
      read(gas(commas(37)+1:commas(38)-1),*)Apr
      read(gas(commas(38)+1:commas(39)-1),*)MatchObs
      read(gas(commas(39)+1:commas(40)-1),*)Dup
      read(gas(commas(40)+1:commas(41)-1),*)sK1
      read(gas(commas(41)+1:commas(42)-1),*)sK2
      read(gas(commas(42)+1:commas(43)-1),*)sK3
      read(gas(commas(43)+1:commas(44)-1),*)sK4
      read(gas(commas(44)+1:commas(45)-1),*)mK1
      read(gas(commas(45)+1:commas(46)-1),*)mK2
      read(gas(commas(46)+1:commas(47)-1),*)mK3
      read(gas(commas(47)+1:commas(48)-1),*)mK4
      read(gas(commas(48)+1:commas(49)-1),*)o_Gmag
      read(gas(commas(49)+1:commas(50)-1),*)FG
      read(gas(commas(50)+1:commas(51)-1),*)e_FG
      read(gas(commas(51)+1:commas(52)-1),*)Gmag
      read(gas(commas(52)+1:commas(53)-1),*)Var
      read(gas(commas(53)+1:commas(54)-1),*)GLON
      read(gas(commas(54)+1:commas(55)-1),*)GLAT
      read(gas(commas(55)+1:commas(56)-1),*)ELON
      read(gas(commas(56)+1:LEN_TRIM(gas)),*)ELAT


c
c      Filters only currently usable data for PRAIA
c
c      Applies proper motion for TGAS stars.
c
c      No parallax correction in this version.
c
c      No radial velocity correction in this version.
c



      cg1mgg=Gmag


      erag1=e_RAdeg
      edeg1=e_DEdeg

c

      rag1=RAdeg
      deg1=DEdeg


c
c     If present, applies GAIA's Dec proper motion
c


      if (pmDE.lt.d98) then

      deg1=deg1+pmDE*(epoj-epgaia)/3600.d3

      endif

      pmdg1=pmDE/1.d3



c
c     If present, applies GAIA's RA proper motion. It is multiplied
c     by the cos(delta) factor by default.
c


      if (pmRA.lt.d98) then

      rag1=rag1+(pmRA*(epoj-epgaia)/3600.d3)/dabs(dcos(grarad*deg1))

      endif

      pmag1=pmRA/1.d3




c
c     If RA proper motion error is present, computes total RA error to observation
c     date 
c

      if (e_pmRA.lt.d98) then

      erag1=dsqrt(erag1**2+(e_pmRA*(epoj-epgaia))**2)

      endif

      epmag1=e_pmRA/1.d3


c
c     If Dec proper motion error is present, computes total Dec error to observation
c     date 
c

      if (e_pmDE.lt.d98) then

      edeg1=dsqrt(edeg1**2+(e_pmDE*(epoj-epgaia))**2)

      endif


      epmdg1=e_pmDE/1.d3


c
c     RA, Dec errors in arcsec
c


      erag1=erag1/1.d3
      edeg1=edeg1/1.d3

c


      return

      end






c
c
c     Subrotine stmass
c
c
c
c     Picks up 2MASS stars from a (RA,Dec) region around the field
c     center     
c
c
c     - ra2ma,de2ma in degrees: (RA,Dec) from 2MASS PSC
c     - era2ma,ede2ma in arcsec: (RA,Dec) errors
c     - extracted magnitudes: J, H and K
c
c
c
c     Last modification: M. Assafin  26/Jul/2017
c
c


      subroutine stmass (idiobs,mraiz,rac,dec,ramin,ramax,demin,demax,
     ?ra2ma,de2ma,era2ma,ede2ma,dmgj,dmgh,dmgk,emgj,emgh,emgk,ddj2,nest)


      implicit real*8 (a-h,o-z)


      integer*4 CO1,CO2,JJD
      integer*2 MAG1,MAG2,MAG3,ERCO
      integer*1 ERMG1,ERMG2,ERMG3


      dimension ra2ma(idiobs),de2ma(idiobs),era2ma(idiobs),
     ?ede2ma(idiobs),dmgj(idiobs),dmgh(idiobs),dmgk(idiobs),
     ?emgj(idiobs),emgh(idiobs),emgk(idiobs),ddj2(idiobs)

      dimension inic(360),ifim(360),inum(360),jxmin(2),jxmax(2),
     ?cxmin(2),cxmax(2)


c     character *1 isig

      character *1 ip,norsul(2)
      character *63 ifaixa,kfaixa
      character *9  iarq,karq
      character *50 mraiz


      data iarq/'TMASS.ast'/
      data karq/'TMASS.acc'/
      data norsul/'m','p'/
 


c
c     Auxiliary data
c


      idin50=50
      izon=1800

      sfac=1.d0
      bin=1.d0
      nbin=360



c
c     Determines pointers for declination zone reading given
c     the furnished (min,max) RA limits
c



      if (ramax.ge.ramin) then


      inde=1

      jxmin(1)=ramin/bin+1
      jxmax(1)=ramax/bin+1

      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2

      jxmin(1)=ramin/bin+1
      jxmax(1)=nbin

      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac


      jxmin(2)=1
      jxmax(2)=ramax/bin+1


      if (jxmin(2).lt.1) jxmin(2)=1
      if (jxmax(2).lt.1) jxmax(2)=1

      if (jxmin(2).gt.nbin) jxmin(2)=nbin
      if (jxmax(2).gt.nbin) jxmax(2)=nbin


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif






c
c     Reads PRAIA-formatted 2MASS PSC declination zones
c     by 0.1 degrees steps
c


      dfaixa=demin-0.2d0

      nest=0

c

      do 30 k=1,izon

      dfaixa=dfaixa+0.1d0


      if (dfaixa-demax.gt.0.1d0) go to 35

      if (dfaixa.lt.-90.d0) dfaixa=-90.d0

      if (dfaixa.lt.0.d0) then
      j=dabs(dfaixa)*10.d0
      if (j.eq.900) go to 30
      ip=norsul(1)
      else
      j=dabs(dfaixa)*10.d0
      if (j.eq.900) go to 35
      ip=norsul(2)
      endif

      jaca=j

c
c     Mounts current declination zone file name
c


      ifaixa=''
      ifaixa=mraiz



      do l=1,idin50
      if (ifaixa(l:l).eq.' ') go to 10
      enddo

 10   write(ifaixa(l:l+12),'(a1,i3.3,a9)') ip,j,iarq



      write (*,14) ifaixa
 14   format(1x,'2MASS: ',a63)


c
c     Mounts current declination zone's acceleration reading index
c     file name
c

      kfaixa=ifaixa

      do j=1,idin50+13
      if (ifaixa(j:j).eq.' ') go to 15
      enddo

 15   kfaixa(j-3:j-1)='acc'



c
c     Loads acceleration indexes for the current declination zone file
c
c     Catalogue RA indexed by 1 degree steps in each declination zone file
c
c     inic - rec pointer for the 1rst star in the n-th RA range
c     ifim - rec pointer for the last star in the n-th RA range
c     inu  - number of stars in the n-th RA range (it can be zero)
c
c

      open (95,file=kfaixa)

      do j=1,nbin
      read (95,16) inic(j),ifim(j),inum(j)
 16   format(16x,3i7)
      enddo

      close (95)




c
c     Reads current declination zone file
c

      open (95,file=ifaixa,access="direct",form="unformatted",recl=23)


c


      do 20 in=1,inde

      do 19 nn=jxmin(in),jxmax(in)

      if(inum(nn).eq.0) go to 19

      i1=inic(nn)
      i2=ifim(nn)


      do 18 n=i1,i2



c
c     2MASS (RA,Dec)s in degrees
c
c     (RA,Dec) errors in arcseconds
c



    
      read (95,rec=n) CO1,CO2,ERCO,MAG1,ERMG1,MAG2,
     ?ERMG2,MAG3,ERMG3,JJD

      RA=DBLE(CO1)/1.0D6

c
c     Checks if star falls inside RA limits
c


      if (ra.lt.cxmin(in)) go to 18
      if (ra.gt.cxmax(in)) go to 18

c


      DE=DBLE(CO2)/1.0D6


c
c     Checks if star falls inside Dec limits
c



      if (de.lt.demin) go to 18
      if (de.gt.demax) go to 18




      nest=nest+1

      if (nest.gt.idiobs) then
      close (95)
      go to 35
      endif






      ERDE=dble(MOD(ERCO,100))/100.0d0
      ERRA=(dble(ERCO)-ERDE)/10000.0d0
      ZMGJ=dble(MAG1)/1000.0d0
      EEMGJ=dble(ERMG1)/100.0d0
      ZMGH=dble(MAG2)/1000.0d0
      EEMGH=dble(ERMG2)/100.0d0
      ZMGK=dble(MAG3)/1000.0d0
      EEMGK=dble(ERMG3)/100.0d0
      DDJ=DBLE(JJD)/1.0D4+2451.0D3



      ra2ma(nest)=ra
      de2ma(nest)=de
      era2ma(nest)=erra
      ede2ma(nest)=erde
      dmgj(nest)=zmgj
      dmgh(nest)=zmgh
      dmgk(nest)=zmgk
      emgj(nest)=eemgj
      emgh(nest)=eemgh
      emgk(nest)=eemgk
      ddj2(nest)=ddj



c
c     debug alfa delta
c

c     ra=ra/15.d0
c     dmag=mag/100.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0

c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,dmgj(nest),dmgh(nest),
c    ?dmgk(nest)


 18   continue

 19   continue

 20   continue


c

      close (95)

c

 30   continue

 35   continue

 
      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)'Attention: overflow in the number of 2MASS stars.' 
      write (*,*)'Using only the 2MASS data stored before the overflow.'
      write (*,*)
      write (*,*)
      endif


      return
      end







c
c
c     Subrotine sucac4
c
c
c     Picks up UCAC4 stars  
c
c
c     - rauc4,deuc4: RA,Dec in degrees at epoch epoj of the CCD observation
c     - epram4, epdem4: mean JD epoch of (RA,Dec)s
c     - erauc4, edeuc4: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra4: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde4: Dec proper motion in arcsec/year
c     - epmra4, epmde4: proper motion errors in arcsec/year
c     - udmgj4,udmgh4,udmgk4: J, H and K magnitudes from 2MASS in the UCAC4
c     - udmg4:  internal magnitude in the UCAC4 system between V e R
c
c     - rame4,deme4  : ra,de at respective true UCAC4 mean epochs 
c     - eramc4,edemc4: ra,de errors at respective true UCAC4 mean epochs
c     - epora4,epode4: respective ra,de true UCAC4 mean epochs
c 
c
c
c     Last modification: M. Assafin 26/Jul/2017
c
c


      subroutine sucac4 (idiobs,iu4z,iu4i,inuu4,ir1u4,ir2u4,u4raiz,epoj,
     ?rac,dec,ramin,ramax,demin,demax,rauc4,deuc4,erauc4,edeuc4,pmra4,
     ?pmde4,epmra4,epmde4,udmgj4,udmgh4,udmgk4,udmg4,nest,rame4,deme4,
     ?eramc4,edemc4,epora4,epode4)

      implicit real*8 (a-h,o-z)

      integer*2 un,ierra,reclen


      LOGICAL  bf, eozf
      INTEGER*4 ran,spdn, id2m,rnm, mcf, rnz
      INTEGER*2 magm,maga, cepra,cepdc, pmra2,pmdc2
     .         ,jmag,hmag,kmag, apasm(5), zn2
      INTEGER*1 sigmag, sigra,sigdc, sigpmr,sigpmd
      INTEGER*1 ojt,dsf, na1,nu1,us1, apase(5), gcflg
      INTEGER*1 icqflg(3), q2mflg(3), leda,x2m


      dimension rauc4(idiobs),deuc4(idiobs),erauc4(idiobs),
     ?edeuc4(idiobs),pmra4(idiobs),pmde4(idiobs),epmra4(idiobs),
     ?epmde4(idiobs),udmgj4(idiobs),udmgh4(idiobs),udmgk4(idiobs),
     ?udmg4(idiobs),rame4(idiobs),deme4(idiobs),eramc4(idiobs),
     ?edemc4(idiobs),epora4(idiobs),epode4(idiobs)

      dimension irnm(25),ipmrc(25),ipmd(25)

      dimension inuu4(iu4z,iu4i),ir1u4(iu4z,iu4i),ir2u4(iu4z,iu4i)

      dimension jxmin(2),jxmax(2),cxmin(2),cxmax(2)


c     character*1 isig

      character*1 ip

      character *54 ifaixa
      character *50 u4raiz

      data ip/'z'/
 
      data irnm /1,2,200137,200168,200169,200229,200400,200503,200530,
     ?200895,201050,201349,201526,201550,201567,201633,201803,249921,
     ?249984,268357,80118783,93157181,106363470,110589580,113038183/ 

      data ipmrc /41087,41558,-37758,-36004,-36782,39624,65051,56347,
     ?67682,-22401,-7986,22819,-5803,40033,41683,-44099,34222,-10015,
     ?-9994,5713,32962,-22393,-37060,-38420,10990/

      data ipmd /31413,32586,7655,9521,4818,-25374,-57308,-23377,13275,
     ?-34203,103281,53694,-47659,-58151,32691,9416,-15989,-35427,-35419,
     ?-36943,5639,-34199,-11490,-27250,-51230/




c
c     Auxiliary data
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      idin50=50

      un=95
      izon=iu4z
      reclen=78

      sfac=1.d0
      bin=0.25d0
      nbin=iu4i



c
c     Determines pointers for declination zone reading, given
c     the furnished (min,max) RA limits
c




      if (ramax.ge.ramin) then


      inde=1

      jxmin(1)=ramin/bin+1
      jxmax(1)=ramax/bin+1


      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2

      jxmin(1)=ramin/bin+1
      jxmax(1)=nbin


      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac



      jxmin(2)=1
      jxmax(2)=ramax/bin+1


      if (jxmin(2).lt.1) jxmin(2)=1
      if (jxmax(2).lt.1) jxmax(2)=1

      if (jxmin(2).gt.nbin) jxmin(2)=nbin
      if (jxmax(2).gt.nbin) jxmax(2)=nbin


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif




c
c     Reads UCAC4 declination zones by 0.2 degrees steps
c


      dfaixa=demin-0.4d0

      nest=0

c

      do 30 k=1,izon

      dfaixa=dfaixa+0.2d0


      if (dfaixa-demax.gt.0.2d0) go to 35

      if (dfaixa.lt.-90.d0) dfaixa=-90.d0

      j=(dfaixa+90.d0)/0.2d0+1.0d0

      jaca=j



c
c     Checks if zone file number is not in error 
c


      if (j.gt.izon) go to 30


c
c     Mounts current declination zone file name
c


      ifaixa=''
      ifaixa=u4raiz


      do l=1,idin50
      if (ifaixa(l:l).eq.' ') go to 10
      enddo

 10   write(ifaixa(l:l+3),'(a1,i3.3)') ip,j


      write (*,14) ifaixa
 14   format(1x,'UCAC4: ',a54)




c
c     Reads current declination zone file
c



      open (95,file=ifaixa,access="direct",recl=reclen)



      do 20 in=1,inde

      do 19 nn=jxmin(in),jxmax(in)



c
c     Loads acceleration indexes for the current declination zone file
c
c     Catalogue RA indexed by 0.25 degree steps in each declination zone file
c
c     i1     - rec pointer for the 1rst star in the n-th RA range
c     i2     - rec pointer for the last star in the n-th RA range
c     inuu4  - number of stars in the n-th RA range (it can be zero)
c
c



      if(inuu4(jaca,nn).eq.0) go to 19

      i1=ir1u4(jaca,nn)
      i2=ir2u4(jaca,nn)



      do 18 n=i1,i2


c
c     RA,Dec converted to degrees.
c
c     Error in RA,Dec converted to arcseconds.
c
c     Proper motions converted to arcseconds per Julian Year.
c    




      READ (un,REC=n)                             !          sum = 78
     .     ran,spdn, magm,maga, sigmag,ojt,dsf    !  8 + 4 +  3  = 15
     .    ,sigra,sigdc, na1,nu1,us1               !  2 + 3       =  5
     .    ,cepra,cepdc, pmra2,pmdc2,sigpmr,sigpmd !  4 + 4 +  2  = 10
     .    ,id2m, jmag,hmag,kmag, icqflg, q2mflg   !  4 + 6 +  6  = 16
     .    ,apasm, apase, gcflg                    ! 10 + 5 +  1  = 16
     .    ,mcf, leda,x2m, rnm                     !  4 + 2 +  4  = 10
     .    ,zn2, rnz                               !  2 + 4       =  6


c


      ra=ran


      de=spdn

      if (pmra2.eq.32767 .and. pmdc2.eq.32767) then

      do m=1,25 
      if (rnm.eq.irnm(m)) go to 16
      enddo

 16   continue

      pmx =ipmrc(m)
      pmy =ipmd(m)


      else

      pmx =pmra2
      pmy =pmdc2

      endif

c

      de=de+pmy*(epoj-2000d0)/10d0
      de=de/3.6d6
      de=de-90.d0


      ra=ra+(pmx*(epoj-2000d0)/10d0)/dabs(dcos(grarad*de))

      ra=ra/3.6d6


c
c     Checks if star falls inside (RA,Dec) limits
c



      if (ra.lt.cxmin(in)) go to 18
      if (ra.gt.cxmax(in)) go to 18

      if (de.lt.demin) go to 18
      if (de.gt.demax) go to 18



c
c       Stores data of UCAC4 stars  
c
c
c     - ra,de in degrees at epoch epoj of the CCD observation
c     - epram4, epdem4: mean JD epoch of (RA,Dec)s
c     - erauc4, edeuc4: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra4: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde4: Dec proper motion in arcsec/year
c     - epmra4, epmde4: proper motion errors in arcsec/year
c     - udmg_4 ...: J, H and K magnitudes from 2MASS in the UCAC4
c     - udmg4:  internal magnitude in the UCAC4 system between V e R
c
c     - rame4,deme4  : ra,de at respective true UCAC4 mean epochs
c     - eramc4,edemc4: ra,de errors at respective true UCAC4 mean epochs
c     - epora4,epode4: respective ra,de true UCAC4 mean epochs
c



      nest=nest+1

      if (nest.gt.idiobs) then
      close (un)
      go to 35
      endif


c
c     (RA,Dec) at UCAC4 reference epoch J2000.0 
c


      ramed=ran/3.6d6
      demed=spdn/3.6d6
      demed=demed-90.d0


c

      epram4=cepra/1.d2+1900d0
      epdem4=cepdc/1.d2+1900d0


      esigra=sigra+128
      esigdc=sigdc+128

      era4=esigra/1.d3
      ede4=esigdc/1.d3


      sipmr=sigpmr+128.d0
      sipmd=sigpmd+128.d0


      if (sipmr.eq.251) sipmr=275
      if (sipmr.eq.252) sipmr=325 
      if (sipmr.eq.253) sipmr=375 
      if (sipmr.eq.254) sipmr=450 
      if (sipmr.eq.255) sipmr=500 


      if (sipmd.eq.251) sipmr=275
      if (sipmd.eq.252) sipmr=325 
      if (sipmd.eq.253) sipmr=375 
      if (sipmd.eq.254) sipmr=450 
      if (sipmd.eq.255) sipmr=500 



      epmx4=sipmr/10.d3
      epmy4=sipmd/10.d3

      erra4=dsqrt(era4**2+(epmx4*(epoj-epram4))**2)
      erde4=dsqrt(ede4**2+(epmy4*(epoj-epdem4))**2)

      zmgj4=jmag/1.d3
      zmgh4=hmag/1.d3
      zmgk4=kmag/1.d3

      dmg4=maga/1.d3



      rauc4(nest)=ra
      deuc4(nest)=de
      erauc4(nest)=erra4
      edeuc4(nest)=erde4

      pmde4(nest)=pmy/10d3
      pmra4(nest)=pmx/10d3
      epmra4(nest)=epmx4
      epmde4(nest)=epmy4


      udmgj4(nest)=zmgj4
      udmgh4(nest)=zmgh4
      udmgk4(nest)=zmgk4
      udmg4(nest)=dmg4




c
c     (RA,Dec) at respective true UCAC4 mean epochs 
c


      demed=demed+(pmde4(nest)*(epdem4-2000.d0))/3.6d3
      ramed=ramed+pmra4(nest)*(epram4-2000.d0)/(3.6d3*dabs(dcos(grarad*
     ?demed)))


      rame4(nest)=ramed
      deme4(nest)=demed
      eramc4(nest)=era4
      edemc4(nest)=ede4
      epora4(nest)=epram4
      epode4(nest)=epdem4


c
c     debug alfa delta
c
c
c     ra=ra/15.d0
c     dmag=dmg4
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF 
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,'  ',erra4,erde4,
c    ?pmra4(nest),pmde4(nest),epmx4,epmy4,udmgj4(nest),
c    ?udmgh4(nest),udmgk4(nest),udmg4(nest)
c



 18   continue

 19   continue

 20   continue


      close (un)


 30   continue

 35   continue

c

      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)'Attention: overflow in the number of UCAC4 stars.' 
      write (*,*)'Using only the UCAC4 data stored before the overflow.'
      write (*,*)
      write (*,*)
      endif


      return
      end





c
c
c     Subrotine sucac5
c
c
c     Picks up UCAC5 stars  
c
c
c     - rauc5,deuc5: RA,Dec in degrees at epoch epoj of the CCD observation
c     - erauc5, edeuc5: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra5: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde5: Dec proper motion in arcsec/year
c     - epmra5, epmde5: proper motion errors in arcsec/year
c     - udmgj5,udmgh5,udmgk5: J, H and K magnitudes from 2MASS in the UCAC5
c     - udmg5: UCAC5 magnitude R Johnston
c 
c
c
c     Last modification: M. Assafin 26/Jul/2017
c
c


      subroutine sucac5 (idiobs,iu5z,iu5i,inuu5,ir1u5,ir2u5,u5raiz,epoj,
     ?rac,dec,ramin,ramax,demin,demax,rauc5,deuc5,erauc5,edeuc5,pmra5,
     ?pmde5,epmra5,epmde5,udmgj5,udmgh5,udmgk5,udmg5,nest)


      implicit real*8 (a-h,o-z)

      integer*2 un,ierra,reclen


      INTEGER*8 srcid
      INTEGER*4 ira,idc, rag,dcg
      INTEGER*2 epi, pmir,pmid,pmer,pmed, phgm,im1,rmag,jmag,hmag,kmag
     .         ,erg, edg
      INTEGER*1 flg,nu1



      dimension rauc5(idiobs),deuc5(idiobs),erauc5(idiobs),
     ?edeuc5(idiobs),pmra5(idiobs),pmde5(idiobs),epmra5(idiobs),
     ?epmde5(idiobs),udmgj5(idiobs),udmgh5(idiobs),udmgk5(idiobs),
     ?udmg5(idiobs)


      dimension inuu5(iu5z,iu5i),ir1u5(iu5z,iu5i),ir2u5(iu5z,iu5i)

      dimension jxmin(2),jxmax(2),cxmin(2),cxmax(2)


c     character*1 isig

      character*1 ip

      character *54 ifaixa
      character *50 u5raiz

      data ip/'z'/
 

c
c     Auxiliary data
c


      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      idin50=50

      un=95
      izon=iu5z
      reclen=52

      sfac=1.d0
      bin=0.25d0
      nbin=iu5i

      epog1=2015.d0


c
c     Determines pointers for declination zone reading, given
c     the furnished (min,max) RA limits
c


      if (ramax.ge.ramin) then


      inde=1

      jxmin(1)=ramin/bin+1
      jxmax(1)=ramax/bin+1


      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2

      jxmin(1)=ramin/bin+1
      jxmax(1)=nbin


      if (jxmin(1).lt.1) jxmin(1)=1
      if (jxmax(1).lt.1) jxmax(1)=1

      if (jxmin(1).gt.nbin) jxmin(1)=nbin
      if (jxmax(1).gt.nbin) jxmax(1)=nbin


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac



      jxmin(2)=1
      jxmax(2)=ramax/bin+1


      if (jxmin(2).lt.1) jxmin(2)=1
      if (jxmax(2).lt.1) jxmax(2)=1

      if (jxmin(2).gt.nbin) jxmin(2)=nbin
      if (jxmax(2).gt.nbin) jxmax(2)=nbin


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif




c
c     Reads UCAC5 declination zones by 0.2 degrees steps
c


      dfaixa=demin-0.4d0

      nest=0

c

      do 30 k=1,izon

      dfaixa=dfaixa+0.2d0


      if (dfaixa-demax.gt.0.2d0) go to 35

      if (dfaixa.lt.-90.d0) dfaixa=-90.d0

      j=(dfaixa+90.d0)/0.2d0+1.0d0

      jaca=j



c
c     Checks if zone file number is not in error 
c


      if (j.gt.izon) go to 30


c
c     Mounts current declination zone file name
c


      ifaixa=''
      ifaixa=u5raiz


      do l=1,idin50
      if (ifaixa(l:l).eq.' ') go to 10
      enddo

 10   write(ifaixa(l:l+3),'(a1,i3.3)') ip,j


      write (*,14) ifaixa
 14   format(1x,'UCAC5: ',a54)




c
c     Reads current declination zone file
c


      open (un,file=ifaixa,access="direct",recl=reclen)



      do 20 in=1,inde

      do 19 nn=jxmin(in),jxmax(in)



c
c     Loads acceleration indexes for the current declination zone file
c
c     Catalogue RA indexed by 0.25 degree steps in each declination zone file
c
c     i1     - rec pointer for the 1rst star in the n-th RA range
c     i2     - rec pointer for the last star in the n-th RA range
c     inuu5  - number of stars in the n-th RA range (it can be zero)
c
c



      if(inuu5(jaca,nn).eq.0) go to 19

      i1=ir1u5(jaca,nn)
      i2=ir2u5(jaca,nn)



      do 18 n=i1,i2


c
c     RA,Dec converted to degrees.
c
c     Error in RA,Dec converted to arcseconds.
c
c     Proper motions converted to arcseconds per Julian Year.
c    


      READ (un,REC=n)                    !     total = 52 b
     .    srcid, rag,dcg, erg,edg        ! 8 + 8 + 4 = 20 b
     .   ,flg,nu1,epi                    ! 4         =  4 b
     .   ,ira,idc, pmir,pmid, pmer,pmed  ! 8 + 4 + 4 = 16 b
     .   ,phgm,im1,rmag,jmag,hmag,kmag   ! 6 * 2     = 12 b


c

      ra=rag
      de=dcg

      pmx=pmir
      pmy=pmid


      de=de+pmy*(epoj-epog1)/10d0
      de=de/3.6d6

      ra=ra+pmx*(epoj-epog1)/(10d0*dabs(dcos(de*grarad)))

      ra=ra/3.6d6



c
c     Checks if star falls inside (RA,Dec) limits
c



      if (ra.lt.cxmin(in)) go to 18
      if (ra.gt.cxmax(in)) go to 18

      if (de.lt.demin) go to 18
      if (de.gt.demax) go to 18



c
c       Stores data of UCAC5 stars  
c
c
c     - rauc5,deuc5: RA,Dec in degrees at epoch epoj of the CCD observation
c     - erauc5, edeuc5: (RA,Dec) position error in arcsec for JD observation epoch
c     - pmra5: RA proper motion (multiplied by dcosD factor) in arcsec/year
c     - pmde5: Dec proper motion in arcsec/year
c     - epmra5, epmde5: proper motion errors in arcsec/year
c     - udmgj5,udmgh5,udmgk5: J, H and K magnitudes from 2MASS in the UCAC5
c     - udmg5: UCAC5 magnitude R Johnston
c
c



      nest=nest+1

      if (nest.gt.idiobs) then
      close (un)
      go to 35
      endif

c

      era5=erg/10d3
      ede5=edg/10d3


      epmx5=pmer/10.d3
      epmy5=pmed/10.d3


      erra5=dsqrt(era5**2+(epmx5*(epoj-epog1))**2)
      erde5=dsqrt(ede5**2+(epmy5*(epoj-epog1))**2)


      zmgj5=jmag/1.d3
      zmgh5=hmag/1.d3
      zmgk5=kmag/1.d3

      dmg5=rmag/1.d3

c

      rauc5(nest)=ra
      deuc5(nest)=de
      erauc5(nest)=erra5
      edeuc5(nest)=erde5

      pmde5(nest)=pmy/10d3
      pmra5(nest)=pmx/10d3

      epmra5(nest)=epmx5
      epmde5(nest)=epmy5


      udmgj5(nest)=zmgj5
      udmgh5(nest)=zmgh5
      udmgk5(nest)=zmgk5
      udmg5(nest)=dmg5



c
c     debug alfa delta
c
c
c
c     ra=ra/15.d0
c     dmag=dmg5
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF 
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,'  ',erra5,erde5,
c    ?pmra5(nest),pmde5(nest),epmx5,epmy5,udmgj5(nest),
c    ?udmgh5(nest),udmgk5(nest),udmg5(nest)
c



 18   continue

 19   continue

 20   continue


      close (un)


 30   continue

 35   continue

c

      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)'Attention: overflow in the number of UCAC5 stars.' 
      write (*,*)'Using only the UCAC5 data stored before the overflow.'
      write (*,*)
      write (*,*)
      endif

 

      return
      end





c
c
c     Subrotina cuser
c
c
c
c
c     Picks up stars from the user catalogue (PRAIA format)
c
c
c     - raucs,deucs: (RA,Dec) in degrees at epoch epoj of the CCD observation
c     - eraucs, edeucs: (RA,Dec) position error in arcsec for JD CCD observation epoch
c     - pmras: RA proper motion (multiplied by the cosD factor) in arcsec/year
c     - pmdes: Dec proper motion in arcsec/year
c     - epmras, epmdes: proper motion errors in arcsec/year
c     - udmg_s ...: J, H and K magnitudes from 2MASS in the user catalogue (code 99 if absent)
c     - udmgs:  star magnitude in the user catalogue
c
c
c
c     Last modification:   M. Assafin 14/Nov/2016
c
c



      subroutine cuser (idiobs,ifaixa,epoj,rac,dec,ramin,ramax,demin,
     ?demax,raucs,deucs,eraucs,edeucs,pmras,pmdes,epmras,epmdes,udmgjs,
     ?udmghs,udmgks,udmgs,nest)



      implicit real*8 (a-h,o-z)


      dimension raucs(idiobs),deucs(idiobs),eraucs(idiobs),
     ?edeucs(idiobs),pmras(idiobs),pmdes(idiobs),epmras(idiobs),
     ?epmdes(idiobs),udmgjs(idiobs),udmghs(idiobs),udmgks(idiobs),
     ?udmgs(idiobs)


      dimension cxmin(2),cxmax(2)



      character*1 isig
      character*50 ifaixa

      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0
  


c
c     Auxiliary data
c



      idin50=50
      iun=95


      sfac=1.d0



c
c     Determines pointers for user catalogue reading, given the
c     furnished (min,max) RA limits (no acceleration indexes)
c




      if (ramax.ge.ramin) then


      inde=1


      cxmin(1)=ramin*sfac
      cxmax(1)=ramax*sfac


      else



      inde=2


      cxmin(1)=ramin*sfac
      cxmax(1)=360.d0*sfac


      cxmin(2)=0.d0
      cxmax(2)=ramax*sfac


      endif



c

      write (*,14) ifaixa
 14   format(1x,'User:  ',a50)



c
c     Reads catalogue in PRAIA's user catalogue format
c


      open (iun,file=ifaixa)


      nest=0



      do 40 i=1,inde

      rewind (iun)

      racmin=cxmin(i)
      racmax=cxmax(i)



 20   continue

      read (iun,21,err=20,end=40) iah,iam,sa,isig,idg,idm,sd,ex,ey,codj,
     ?copma,copmd,erpma,erpmd,codmg,res2mg

 21   format(i2,1x,i2,1x,f7.4,2x,a1,i2,1x,i2,1x,f6.3,2x,2f7.3,2x,f16.8,
     ?1x,4(1x,f7.3),2(1x,f6.3))

c

      ra=15.d0*hmsgms(iah,iam,sa)

      de=hmsgms(idg,idm,sd)
      if (isig.eq.'-') de=-de



c

      if (copma.lt.90.d0 .and. copmd.lt.90.d0) then

      cop=2000d0+(codj-2451545d0)/365.25d0
      dt=epoj-cop

      de=de+(copmd*dt)/3600.d0

      ra=ra+(copma*dt/dcos(grarad*dabs(de)))/3600.d0

c     ra=ra+(copma*dt)/3600.d0

      endif



c
c     Checks if star falls inside (RA,Dec) limits
c


      if (ra.lt.racmin) go to 20
      if (ra.gt.racmax) go to 20

      if (de.lt.demin) go to 20
      if (de.gt.demax) go to 20




c
c     Stores star data
c


      nest=nest+1

      if (nest.gt.idiobs) go to 45


      raucs(nest)=ra
      deucs(nest)=de
      eraucs(nest)=ex
      edeucs(nest)=ey

      pmras(nest)=copma
      pmdes(nest)=copmd
      epmras(nest)=erpma
      epmdes(nest)=erpmd


      udmgjs(nest)=99.9d0
      udmghs(nest)=99.9d0
      udmgks(nest)=99.9d0
      udmgs(nest)=codmg



c
c     debug alfa delta
c
c
c     ra=ra/15.d0
c     dmag=dmg4
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIG='-'
c     de=-de
c     ELSE
c     ISIG='+'
c     ENDIF 
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0
c
c     write(*,*) IAH,IAM,SA,'  ',ISIG,IDG,IDM,DS,'  ',ex,ey,
c    ?pmras(nest),pmdes(nest),erpma,erpmd,udmgjs(nest),
c    ?udmghs(nest),udmgks(nest),udmgs(nest)
c


      go to 20

c

 40   continue

 45   close (iun)

c 


      if (nest.gt.idiobs) then

      nest=idiobs

      write (*,*)
      write (*,*)
      write (*,*)'Attention: overflow in the number of UCAC4 stars.' 
      write (*,*)'Using only catalogue data stored before the overflow.'
      write (*,*)
      write (*,*)
      endif



c


      return
      end




c
c
c
c     Subrotine idxy2m
c
c
c     Identifies main reference catalogue stars in the FOV.
c
c     Here, the main reference star catalogue is the GAIA DR1 (GAS), here
c     referred to as GAIA_1. It is the complete GAIA catalog (GAS) from the
c     release version DR1 (14/Sep/2016).
c
c     We use the version of GAS organized by J. Camargo and LIneA team in
c     0.1 degrees declination zones, with crescent RA within each zone, but
c     otherwise keeping the original line contents in CSV format.
c
c
c
c     - id2ma: pointer of each FOV star with the n-th main reference catalogue
c              star
c
c     - ialtu: instrumental star brightness pointer in crescent brightness order
c
c     - ra2ma, de2ma: (RA,Dec)s of main reference catalogue stars
c
c
c     - xob,yob: measured (x,y)s of FOV stars
c
c
c     - xold,yold: (RA,Dec)s of main reference catalogue stars projected in
c                  the tangent plane (sky plane)
c
c     - xra2ma, yde2ma: provisional (RA,Dec)s of all FOV stars in the reference
c                       frame of the main reference catalogue; used in the
c                       identification of FOV stars with other catalogues, for
c                       the (RA,Dec) reductions with these other catalogues
c
c
c
c
c     Last modification:   M. Assafin  29/Out/2012
c
c
c

      subroutine idxy2m (idiobs,icofsp,xest,yest,xp,yp,carx,cary,nval,
     ?ior,scala,erpix,rac,dec,nbcat,nbmed,nest,ialtu,xob,yob,n2mass,
     ?id2ma,ra2ma,de2ma,dmgj,xold,yold,xra2ma,yde2ma,ireflex,ecala,tt,
     ?ierro)


      implicit real*8 (a-h,o-z)

      dimension ialtu(idiobs),xob(idiobs),yob(idiobs),id2ma(idiobs),
     ?ra2ma(idiobs),de2ma(idiobs),dmgj(idiobs),xra2ma(idiobs),
     ?yde2ma(idiobs),nval(idiobs),ior(idiobs),xold(idiobs),yold(idiobs)

      dimension coefx(icofsp),coefy(icofsp),xcof(icofsp),ycof(icofsp),
     ?xest(idiobs),yest(idiobs),xp(idiobs),yp(idiobs)

      character*1 isiga


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))




c
c     Auxiliary data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

      ierro=0

      pcala=grarad*ecala/3600.d0

      epixra=grarad*erpix*scala/3600.d0
      rscala=grarad*scala/3600.d0


c
c     Projects the main reference catalogue (RA,Dec)s
c     in the tangent plane
c

      grac=grarad*rac
      gdec=grarad*dec

      n=0

      do 5 i=1,n2mass
      bra=grarad*ra2ma(i)
      bde=grarad*de2ma(i)
      d=dexy(bra,bde,grac,gdec)

      xold(i)=xpad(bra,bde,grac)/d
      yold(i)=ypad(bra,bde,grac,gdec)/d

      if (dabs(xold(i)).gt.carx) go to 5
      if (dabs(yold(i)).gt.cary) go to 5


      n=n+1
      ior(n)=i
      nval(n)=dmgj(i)*100

 5    continue



c
c     Picks up the N brightest main reference catalogue stars
c
c     (N first positions in vector ior)
c



      call ordem (idiobs,n,ior,nval)

c

      nmed=nbmed
      if (nbmed.gt.nest) nmed=nest


      ncat=nbcat
      if (nbcat.gt.n2mass) ncat=n2mass

      if (nbcat.gt.n) ncat=n


c
c
c     Pair identification with and without reflection
c
c
c     k=1 : no reflexion in x
c     k=-1: with reflex in x
c


      do i=1,icofsp
      xcof(i)=0.d0
      ycof(i)=0.d0
      coefx(i)=0.d0
      coefy(i)=0.d0
      enddo


      ireflex=0
      ncomum=0

      nptos=2
      ngrau=0
      ngraup=1


c
c     Execution time initialization for star identification
c


      ntempo=ncat*0.1d0

      tempoi=0.d0
      call tempo (tempoi,tempot,tempop)

c



      do iii=1,ncat-1

      if (k.eq.-1 .and. iii.eq.ntempo) then

      call tempo (tempoi,tempot,tempop)

      tt=2.d0*ncat*tempop/ntempo
      tt=tt/3600.d0
      
      ihour=tt
      minu=(tt-ihour)*60.d0
      seg=((tt-ihour)*60.d0-minu)*60.d0

      write (*,*)      
      write (*,1) ihour,minu,seg
 1    format(1x,'Time consuming per field for reference catalog star ide
     ?ntification: ',i3,'hs ',i2,'m ',f4.1,'s')  
      write (*,*)      

      endif

c

      xp(1)=xold(ior(iii))
      yp(1)=yold(ior(iii))

      do ii =iii+1,ncat


      xp(2)=xold(ior(ii))     
      yp(2)=yold(ior(ii))     


      do jjj=1,nmed

      xest(1)=xob(ialtu(nest-jjj+1))
      yest(1)=yob(ialtu(nest-jjj+1))

      do 20 jj =1,nmed

      if (jj.eq.jjj) go to 20

      xest(2)=xob(ialtu(nest-jj+1))     
      yest(2)=yob(ialtu(nest-jj+1))     



c
c     Eliminates star pair which distance is not compatible
c     with the pixel scale within the pixel scale error
c


      dp=dsqrt((xp(2)-xp(1))**2+(yp(2)-yp(1))**2)
      dest=dsqrt((xest(2)-xest(1))**2+(yest(2)-yest(1))**2)
      d=dp/dest


      if (dabs(d-rscala).gt.pcala) go to 20


c
c     Computes 4 constant coefficients for the measured
c     and catalogue star pairs
c




      do k=+1,-1,-2

      xest(1)=k*xest(1)
      xest(2)=k*xest(2)

      call isol (idiobs,icofsp,ngrau,nptos,xest,yest,xp,yp,coefx,coefy,
     ?ierro)

      if (ierro.eq.1) then
      ierro=0
      go to 20

      endif


c
c     Computes the number of identifications for these 2 pairs
c


      icont=0

      do 10 j=1,nmed

      xx=k*xob(ialtu(nest-j+1))
      yy=yob(ialtu(nest-j+1))

      x=pol(icofsp,xx,yy,coefx,ngraup)
      y=pol(icofsp,xx,yy,coefy,ngraup)

      do  9 i=1,ncat
      
      dx=dabs(xold(ior(i))-x)
      dy=dabs(yold(ior(i))-y)
      

      if (dx.gt.epixra) go to 9
      if (dy.gt.epixra) go to 9

      icont=icont+1
      go to 10



 9    continue
 10   continue


      if (icont.gt.ncomum) then

      ncomum=icont

      do l=1,3
      xcof(l)=coefx(l)
      ycof(l)=coefy(l)
      enddo

      ireflex=k
      endif

      enddo


 20   continue
      enddo


      enddo
      enddo



c
c     debug
c

      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) cross-identification'
      write (*,*) '1rst step: 4 constant adjustment'
      write (*,*) 

      write (*,*) 'GAIA_1 stars extracted in 2x2 field  = ',n2mass
      write (*,*) 'Extracted field stars with (x,y)     = ',nest
      write (*,*) 'Common GAIA_1 vs. bright field stars = ',ncomum
      write (*,*) 'system reflection flag               = ',ireflex
      write (*,*)
      write (*,*)

c

      x1=xcof(1)*radgra*3600.d0
      x2=xcof(2)*radgra*3600.d0
      x3=xcof(3)*radgra*3600.d0

      y1=ycof(1)*radgra*3600.d0
      y2=ycof(2)*radgra*3600.d0
      y3=ycof(3)*radgra*3600.d0

c
      write (*,*) 'Coeficients solution'
      write (*,*)
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D - Cx + By '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)


c
c     Refines the polinomial fit with the use of more common
c     measured/catalogue stars, after the first identification
c     step with star pairs; now we use a complete polinomial of
c     degree 1, 2 or 3 if the commom number of reference stars
c     allows for that.
c



      ncomum=0

      do i=1,idiobs
      id2ma(i)=0
      enddo


      do 30 j=1,nest
      
      xx=ireflex*xob(j)
      yy=yob(j)

      x=pol(icofsp,xx,yy,xcof,ngraup)
      y=pol(icofsp,xx,yy,ycof,ngraup)

      do i=1,n2mass

      dx=(xold(i)-x)*radgra*3600d0
      dy=(yold(i)-y)*radgra*3600d0

      nval(i)=dsqrt(dx**2+dy**2)

      ior(i)=i
      enddo

      call ordem (idiobs,n2mass,ior,nval)

      ii=ior(1)

      dx=dabs(xold(ii)-x)
      dy=dabs(yold(ii)-y)


      if (dx.gt.epixra) go to 30
      if (dy.gt.epixra) go to 30



c     x1=dx*radgra*3600.d0
c     y1=dy*radgra*3600.d0

c     write (*,*) 'x y dx dy ',xob(j),yob(j),x1,y1



      ncomum=ncomum+1

      id2ma(j)=ii
c     nval(ncomum)=ii
      xp(ncomum)=xold(ii)
      yp(ncomum)=yold(ii)

      xest(ncomum)=ireflex*xob(j)
      yest(ncomum)=yob(j)


 30   continue



c     write (*,*) 'ncomum final = ',ncomum


      if (ncomum.lt.3) then
      ierro=1
      return
      endif


c
c     Chooses higher possible degree for polinomial, given the
c     available number of commom measured/catalogue stars
c



      ngraup=2
      if (ncomum.le.11) ngraup=1
      if (ncomum.ge.21) ngraup=3


      call isol (idiobs,icofsp,ngraup,ncomum,xest,yest,xp,yp,coefx,
     ?coefy,ierro)

      if (ierro.eq.1) then

      return
      endif

c

      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) cross-identification'
      write (*,*) '2nd final step:'
      write (*,*) 
      write (*,*) 'Complete polynomial model, degree     = ',ngraup
      write (*,*)  

      write (*,*) 'GAIA_1 stars extracted in 2x2 field   = ',n2mass
      write (*,*) 'Extracted field stars with (x,y)      = ',nest
      write (*,*) 'Common GAIA_1 vs. field stars (final) = ',ncomum
      write (*,*) 'system reflection flag                = ',ireflex
      write (*,*)
      write (*,*)

c


      x1=coefx(1)*radgra*3600.d0
      x2=coefx(2)*radgra*3600.d0
      x3=coefx(3)*radgra*3600.d0

      y1=coefy(1)*radgra*3600.d0
      y2=coefy(2)*radgra*3600.d0
      y3=coefy(3)*radgra*3600.d0



c
      write (*,*) 'Solution. Linear coeficients:'
      write (*,*) 
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D + Ex + Fy '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)



c
c     Computes provisional (RA,Dec)s of all FOV stars in the reference
c     frame of the main reference catalogue, for posterior identification
c     with other catalogue stars for (RA,Dec) reductions with these
c     other catalogues
c



      do i=1,nest


      xx=ireflex*xob(i)
      yy=yob(i)

      x=pol(icofsp,xx,yy,coefx,ngraup)
      y=pol(icofsp,xx,yy,coefy,ngraup)

      xra2ma(i)=alff(x,y,grac,gdec)
      yde2ma(i)=deltt(xra2ma(i),y,grac,gdec)


      xra2ma(i)=xra2ma(i)*radgra
      yde2ma(i)=yde2ma(i)*radgra


c
c     debug alfa e delta
c

c     ra=xra2ma(i)
c     de=yde2ma(i)


c     ra=ra/15.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIGA='-'
c     de=-de
c     ELSE
c     ISIGA='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0 

c     write(*,*) IAH,IAM,SA,'  ',ISIGA,IDG,IDM,DS



      enddo


      return

      end






c
c
c
c     Subrotine mdxy2m
c
c
c
c
c     Identifies main reference catalogue stars in the FOV.
c
c     Here, the main reference star catalogue is the GAIA DR1 (GAS), here
c     referred to as GAIA_1. It is the complete GAIA catalog (GAS) from the
c     release version DR1 (14/Sep/2016).
c
c     We use the version of GAS organized by J. Camargo and LIneA team in
c     0.1 degrees declination zones, with crescent RA within each zone, but
c     otherwise keeping the original line contents in CSV format.
c
c
c
c     - id2ma: pointer of each FOV star with the n-th main reference catalogue
c              star
c
c     - ialtu: instrumental star brightness pointer in crescent brightness order
c
c     - ra2ma, de2ma: (RA,Dec)s of main reference catalogue stars
c
c
c     - xob,yob: measured (x,y)s of FOV stars
c
c
c     - xold,yold: (RA,Dec)s of main reference catalogue stars projected in
c                  the tangent plane (sky plane)
c
c     - xra2ma, yde2ma: provisional (RA,Dec)s of all FOV stars in the reference
c                       frame of the main reference catalogue; used in the
c                       identification of FOV stars with other catalogues, for
c                       the (RA,Dec) reductions with these other catalogues
c
c     - rcala: better estimate of pixel scale  (rad/pixel)
c
c     - ecala: better estimate of pixel scale error  (rad/pixel)
c
c
c       Here, the scale and orientation angle are furnished from the (RA,Dec)
c     reduction of the first image in the list, speeding up the proccessing of
c     the other images.
c
c
c
c
c      Last modification:  M. Assafin  21/Out/2012
c
c



      subroutine mdxy2m (idiobs,icofsp,xest,yest,xp,yp,carx,cary,ngrau,
     ?coefxr,coefyr,kreflex,nval,ior,rscala,erpix,rac,dec,nbcat,nbmed,
     ?nest,ialtu,xob,yob,n2mass,id2ma,ra2ma,de2ma,dmgj,xold,yold,xra2ma,
     ?yde2ma,ireflex,pcala,tt,ierro)



      implicit real*8 (a-h,o-z)

      dimension ialtu(idiobs),xob(idiobs),yob(idiobs),id2ma(idiobs),
     ?ra2ma(idiobs),de2ma(idiobs),dmgj(idiobs),xra2ma(idiobs),
     ?yde2ma(idiobs),nval(idiobs),ior(idiobs),xold(idiobs),yold(idiobs)

      dimension coefx(icofsp),coefy(icofsp),coefxr(icofsp),
     ?coefyr(icofsp),xcof(icofsp),ycof(icofsp),xest(idiobs),
     ?yest(idiobs),xp(idiobs),yp(idiobs)

      character*1 isiga


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)

      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))



c
c     Auxiliary data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      ierro=0

      ncof=icofsp
      
      ireflex=kreflex

c

      epixra=erpix*rscala

c

      do i=1,ncof
      xcof(i)=0.d0
      ycof(i)=0.d0
      coefx(i)=0.d0
      coefy(i)=0.d0
      enddo



c
c     Projects the main reference catalogue (RA,Dec)s
c     in the tangent plane
c



      grac=grarad*rac
      gdec=grarad*dec

      n=0

      do 5 i=1,n2mass
      bra=grarad*ra2ma(i)
      bde=grarad*de2ma(i)
      d=dexy(bra,bde,grac,gdec)

      xold(i)=xpad(bra,bde,grac)/d
      yold(i)=ypad(bra,bde,grac,gdec)/d
 
      if (dabs(xold(i)).gt.carx) go to 5
      if (dabs(yold(i)).gt.cary) go to 5

      n=n+1
      ior(n)=i
      nval(n)=dmgj(i)*100

 5    continue



c
c     Picks up the N brightest main reference catalogue stars
c
c     (N first positions in vector ior)
c


      call ordem (idiobs,n,ior,nval)

c

      nmed=nbmed
      if (nbmed.gt.nest) nmed=nest


      ncat=nbcat
      if (nbcat.gt.n2mass) ncat=n2mass


      if (nbcat.gt.n) ncat=n



c
c     Execution time initialization for star identification
c


      tempoi=0.d0
      call tempo (tempoi,tempot,tempop)





c
c
c     Pair identification with scale, orientation and reflection
c     already determined in the first image (RA,Dec) reduction
c
c     k=1 : no reflexion in x
c     k=-1: with reflex in x
c


      ncomum=0


c

      k=kreflex

c

      do iii=1,ncat-1


      xp(1)=xold(ior(iii))
      yp(1)=yold(ior(iii))


      do ii =iii+1,ncat


      xp(2)=xold(ior(ii))     
      yp(2)=yold(ior(ii))     


      do jjj=1,nmed

      xest(1)=k*xob(ialtu(nest-jjj+1))
      yest(1)=yob(ialtu(nest-jjj+1))

      do 20 jj =1,nmed

      if (jj.eq.jjj) go to 20

      xest(2)=k*xob(ialtu(nest-jj+1))     
      yest(2)=yob(ialtu(nest-jj+1))     




c
c     Eliminates star pair which distance is not compatible
c     with the pixel scale within the pixel scale error
c


      dp=dsqrt((xp(2)-xp(1))**2+(yp(2)-yp(1))**2)
      dest=dsqrt((xest(2)-xest(1))**2+(yest(2)-yest(1))**2)
      d=dp/dest


      if (dabs(d-rscala).gt.pcala) go to 20



c
c     Adopts the coefficients from the (RA,Dec) reduction of
c     the first image; only the independent term is computed here. 
c



      coefxr(1)=0.d0
      coefyr(1)=0.d0

      cx1=xp(1)-pol(icofsp,xest(1),yest(1),coefxr,ngrau)
      cy1=yp(1)-pol(icofsp,xest(1),yest(1),coefyr,ngrau)
      cx2=xp(2)-pol(icofsp,xest(2),yest(2),coefxr,ngrau)
      cy2=yp(2)-pol(icofsp,xest(2),yest(2),coefyr,ngrau)


      coefxr(1)=(cx1+cx2)/2.d0
      coefyr(1)=(cy1+cy2)/2.d0



c
c     Computes the number of identifications for these 2 pairs
c

      icont=0

      do 10 j=1,nmed

      xx=k*xob(ialtu(nest-j+1))
      yy=yob(ialtu(nest-j+1))

      x=pol(icofsp,xx,yy,coefxr,ngrau)
      y=pol(icofsp,xx,yy,coefyr,ngrau)

      do  9 i=1,ncat
      
      dx=dabs(xold(ior(i))-x)
      dy=dabs(yold(ior(i))-y)
      

      if (dx.gt.epixra) go to 9
      if (dy.gt.epixra) go to 9

      icont=icont+1
      go to 10



 9    continue
 10   continue


      if (icont.gt.ncomum) then

      ncomum=icont
      do l=1,ncof
      xcof(l)=coefxr(l)
      ycof(l)=coefyr(l)
      enddo

      endif


 20   continue
      enddo


      enddo
      enddo


c     stop

c

      call tempo (tempoi,tempot,tempop)

      tt=tempop
      tt=tt/3600.d0
      
      ihour=tt
      minu=(tt-ihour)*60.d0
      seg=((tt-ihour)*60.d0-minu)*60.d0

      write (*,*)      
      write (*,1) ihour,minu,seg
 1    format(1x,'Time consuming per field for reference catalog star ide
     ?ntification: ',i3,'hs ',i2,'m ',f4.1,'s')  
      write (*,*)      




c
c     debug
c

      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) cross-identification'
      write (*,*) 
      write (*,*) '1rst step: coefficients from 1rst image solution'
      write (*,*) 

      write (*,*) 'GAIA_1 stars extracted in 2x2 field  = ',n2mass
      write (*,*) 'Extracted field stars with (x,y)     = ',nest
      write (*,*) 'Common GAIA_1 vs. bright field stars = ',ncomum
      write (*,*) 'system reflection flag               = ',kreflex
      write (*,*)
      write (*,*)

c

      x1=xcof(1)*radgra*3600.d0
      x2=xcof(2)*radgra*3600.d0
      x3=xcof(3)*radgra*3600.d0

      y1=ycof(1)*radgra*3600.d0
      y2=ycof(2)*radgra*3600.d0
      y3=ycof(3)*radgra*3600.d0

c
      write (*,*) 'Solution. Linear coeficients:'
      write (*,*)
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D + Ex + Fy '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)



c
c     Refines the polinomial fit with the use of more common
c     measured/catalogue stars, after the first identification
c     step with star pairs; now the degree of the polinomial is
c     the same degree (1, 2 or 3) used for the first image
c     reduction. 


      ncomum=0

      do i=1,idiobs
      id2ma(i)=0
      enddo


      do 30 j=1,nest
      
      xx=ireflex*xob(j)
      yy=yob(j)

      x=pol(icofsp,xx,yy,xcof,ngrau)
      y=pol(icofsp,xx,yy,ycof,ngrau)

      do i=1,n2mass

      dx=(xold(i)-x)*radgra*3600d0
      dy=(yold(i)-y)*radgra*3600d0

      nval(i)=dsqrt(dx**2+dy**2)

      ior(i)=i
      enddo

      call ordem (idiobs,n2mass,ior,nval)

      ii=ior(1)

      dx=dabs(xold(ii)-x)
      dy=dabs(yold(ii)-y)


      if (dx.gt.epixra) go to 30
      if (dy.gt.epixra) go to 30




      ncomum=ncomum+1

      id2ma(j)=ii
      xp(ncomum)=xold(ii)
      yp(ncomum)=yold(ii)

      xest(ncomum)=ireflex*xob(j)
      yest(ncomum)=yob(j)


 30   continue



      if (ncomum.lt.3) then
      ierro=1
      return
      endif


c
c     Chooses higher possible degree for polinomial, given the
c     available number of commom measured/catalogue stars
c


      call isol (idiobs,icofsp,ngrau,ncomum,xest,yest,xp,yp,coefx,coefy,
     ?ierro)

      if (ierro.eq.1) then
      return
      endif

c

      write (*,*) 
      write (*,*) '(RA,DE) vs. (x,y) cross-identification'
      write (*,*) '2nd final step:'
      write (*,*) 
      write (*,*) 'Complete polynomial model, degree = ',ngrau
      write (*,*)  

      write (*,*) 'GAIA_1 stars extracted in 2x2 field   = ',n2mass
      write (*,*) 'Extracted field stars with (x,y)      = ',nest
      write (*,*) 'Common GAIA_1 vs. field stars (final) = ',ncomum
      write (*,*) 'system reflection flag                = ',ireflex
      write (*,*)
      write (*,*)

c


      x1=coefx(1)*radgra*3600.d0
      x2=coefx(2)*radgra*3600.d0
      x3=coefx(3)*radgra*3600.d0

      y1=coefy(1)*radgra*3600.d0
      y2=coefy(2)*radgra*3600.d0
      y3=coefy(3)*radgra*3600.d0



c
      write (*,*) 'Solution. Linear coeficients:'
      write (*,*)
      write (*,*) 'X = A + Bx + Cy '
      write (*,*) 'Y = D + Ex + Fy '
      write (*,*)
      write (*,*) 'X: ',x1,x2,x3
      write (*,*) 'Y: ',y1,y2,y3
      write (*,*)


c
c     Computes provisional (RA,Dec)s of all FOV stars in the reference
c     frame of the main reference catalogue, for posterior identification
c     with other catalogue stars for (RA,Dec) reductions with these
c     other catalogues
c


      do i=1,nest


      xx=ireflex*xob(i)
      yy=yob(i)

      x=pol(icofsp,xx,yy,coefx,ngrau)
      y=pol(icofsp,xx,yy,coefy,ngrau)

      xra2ma(i)=alff(x,y,grac,gdec)
      yde2ma(i)=deltt(xra2ma(i),y,grac,gdec)


      xra2ma(i)=xra2ma(i)*radgra
      yde2ma(i)=yde2ma(i)*radgra


c
c     debug alfa e delta
c

c     ra=xra2ma(i)
c     de=yde2ma(i)


c     ra=ra/15.d0
c     IAH=ra
c     AM=(ra-IAH)*60.D0
c     IAM=AM
c     SA =(AM-IAM)*60.D0
c     IF (de.LT.0.D0) THEN
c     ISIGA='-'
c     de=-de
c     ELSE
c     ISIGA='+'
c     ENDIF
c     IDG=de
c     DM=(de-IDG)*60.D0
c     IDM=DM
c     DS=(DM-IDM)*60.D0 

c     write(*,*) IAH,IAM,SA,'  ',ISIGA,IDG,IDM,DS



      enddo


      return

      end






C
C
C
C
C     SUBROTINA ISOL
C
C     PROPOSITO
C
C     Ajustar polinomio bivariado P=P(x,y) de grau n
C		    
C       onde  x  -  medida x da estrela
C             y  -  medida y da estrela
C							     
C
C     USO
C
C     SUBROUTINE ISOL (IENTRA,NUMEST,IVER,XPIXEL,YPIXEL,NGRAU,
C       NGRAU3,NGRAU5,NCOMUM,XSAO,YSAO,XEST1,YEST1,XP,YPNTIRA,ITIRA,
C       COEFX,COEFY,XSIG,YSIG,XRRAY,YRRAY)
C
C     DESCRICAO DOS PARAMETROS
C
C       NGRAU  - GRAU DO POLINOMIO DE AJUSTE
C                0 = 4ctes
C                1 = 1o grau completo
C                2 = 2o grau completo
C                3 = 3o grau completo
C
C       NCOMUM - NUMERO DE ESTRELAS COMUNS PARA AJUSTE
C       NUMEST - NUMERO TOTAL DE ESTRELAS DO L-ESIMO CCD
C       NTIRA  - NUMERO DE ESTRELAS RETIRADAS DO AJUSTE
C       ITIRA  - ESTRELAS RETIRADAS DO AJUSTE
C       XSAO   - MEDIDA X DO L-ESIMO CCD NA TELA SAOIMAGE
C       YSAO   - MEDIDA Y DO L-ESIMO CCD NA TELA SAOIMAGE
C       XEST1  - MEDIDA X NO L-ESIMO CCD
C       YEST1  - MEDIDA Y NO L-ESIMO CCD
C       XP     - X DE REFERENCIA DO MOSAICO CENTRAL
C       YP     - Y DE REFERENCIA DO MOSAICO CENTRAL
C       COEFX
C       COEFY  - COEFICIENTES DO POLINOMIO
C       ITIRA  - ESTRELAS RETIRADAS COM MAIOR (O-C)
C       VAX    - ERRO PADRAO X DA RADIOESTRELA
C       VAY    - ERRO PADRAO Y DA RADIOESTRELA
C       XSIG   - DESVIO-PADRAO X DA RADIOESTRELA
C       YSIG   - DESVIO-PADRAO Y DA RADIOESTRELA
C       SIGMA  - TRUNCAMENTO DE VARIACAO DE COORDENADA (X,Y) EM ("),
C                NO CASO DE ELIMINACAO AUTOMATICA
C
C     SUBROTINAS E SUBPROGRAMAS REQUERIDOS
C
C     MATINV (NTERMS,DET)
C
C     COMENTARIOS
C
C     A ROTINA NAO SUPORTA TERMOS RADIAS
C
c     Ela ajusta 4ctes e polinomios completos de graus 1 a 3
c
c
c
      SUBROUTINE ISOL (idiobs,icofsp,NGRAU,NCOMUM,XEST,YEST,XP,YP,COEFX,
     ?COEFY,ierro)
      IMPLICIT REAL *8 (A-H,O-Z)
      DIMENSION COEFX(icofsp),COEFY(icofsp),ALPHA(icofsp,icofsp),
     ?ARRAY(icofsp,icofsp),
     ?BETA(icofsp),BETAX(icofsp),TERMX(icofsp)
      DIMENSION XEST(idiobs),YEST(idiobs),XP(idiobs),YP(idiobs)

C
C     INICIALIZACAO DE DADOS
C
      PI=3.141592653589793D0
      GRARAD=PI/180.D0
      RADGRA=180.D0/PI

      IERRO=0
      DET=1.d0
C
C     Zera vetores
C

      DO 8 I=1,icofsp
      BETA(I) =0.D0
      BETAX(I) =0.D0
      COEFX(I) =0.D0
      COEFY(I) =0.D0
      TERMX(I) =0.D0
      DO 8 J=I,icofsp
      array(i,j)=0.d0
 8    ALPHA(I,J)=0.D0

c
c     4 ctes ou 1o grau?
c

      if (ngrau.ge.1) go to 200

C
C     AJUSTE POR 4 CONSTANTES
C
      NTERMS=4
      IGRAU=2
C

      ITERMS=NTERMS

C
C     MONTANDO AS EQUACOES DE CONDICAO PARA O AJUSTE POR M.Q. DE Ax=B,
C     NO CASO DE "AJUSTE DE 4 CTES"
C
      DO 10 I=1,NCOMUM
      X=XEST(I)
      Y=YEST(I)
      XG=XP(I)
      YG=YP(I)
C
C     CALCULA OS TERMOS DOS COEFICIENTES E AtB
C
      TERMX(1)=TERMX(1)+X**2+Y**2
      TERMX(2)=TERMX(2)+X
      TERMX(3)=TERMX(3)+Y
      TERMX(4)=TERMX(4)+1.D0
      BETAX(1)=BETAX(1)+X*XG+Y*YG
      BETAX(2)=BETAX(2)+Y*XG-X*YG
      BETAX(3)=BETAX(3)+XG
      BETAX(4)=BETAX(4)+YG
   10 CONTINUE
C
C     PREENCHENDO AtA
C
      ALPHA(1,1)=TERMX(1)
      ALPHA(1,2)=0.D0
      ALPHA(1,3)=TERMX(2)
      ALPHA(1,4)=TERMX(3)
      ALPHA(2,2)=TERMX(1)
      ALPHA(2,3)=TERMX(3)
      ALPHA(2,4)=-TERMX(2)
      ALPHA(3,3)=TERMX(4)
      ALPHA(3,4)=0.D0
      ALPHA(4,4)=TERMX(4)
      DO 15 L=1,ITERMS
      DO 15 K=L,ITERMS
   15 ALPHA(K,L) =ALPHA(L,K)
C
C     PREENCHENDO MATRIZ ARRAY=AtA PARA INVERSAO (elementos normalizados
C     pela diagonal) PARA X
C
      DO 80 L=1,ITERMS
      DO 80 K=1,ITERMS
   80 ARRAY(L,K)=ALPHA(L,K)/DSQRT(ALPHA(L,L)*ALPHA(K,K))
C
C     INVERTENDO AtA PARA "X"
C
      CALL MATINV (ITERMS,icofsp,array,DET,ierro)
      IF (IERRO.EQ.1) RETURN
C
C     CALCULANDO OS COEFICIENTES DO POLINOMIO PARA "X"
C
      DO 90 L=1,ITERMS
      DO 90 K=1,ITERMS
   90 ARRAY(L,K)=ARRAY(L,K)/DSQRT(ALPHA(K,K)*ALPHA(L,L))
C
C      OBTEM COEFICIENTES PARA AJUSTE DE 4 CTES
C
      DO 100 L=1,ITERMS
      DO 100 K=1,ITERMS
  100 COEFX(L)=COEFX(L)+ARRAY(L,K)*BETAX(K)
C
      DO  105 L=1,ITERMS
 105  TERMX(L)=COEFX(L)
      COEFX(1)=TERMX(3)
      COEFX(2)=TERMX(1)
      COEFX(3)=TERMX(2)
      COEFY(1)=TERMX(4)
      COEFY(2)=-TERMX(2)
      COEFY(3)=TERMX(1)

      RETURN


c
c     Ajuste de 1o, 2o ou 3o grau
c

 200  continue


C
C     CALCULA NUMERO DE TERMOS DO POL. DE GRAU=NGRAU
C

      NTERMS=1
      DO I=1,NGRAU
      NTERMS=NTERMS+I+1
      ENDDO
C
      IGRAU=NGRAU+1

      ITERMS=NTERMS
C
C


      DO 265 I=1,NCOMUM
      X=XEST(I)
      Y=YEST(I)
      XG=XP(I)
      YG=YP(I)
C
C     Computando os termos para o polinomio em X
C
      ICONT=0
      DO 240 N=1,IGRAU
      DO 240 L=1,N
      K=N-L
      ICONT=ICONT+1
 240  TERMX(ICONT)=(X**K)*(Y**(L-1))

C
C     Computando AtA e AtB (para "X" e "Y")
C
      DO 260 L=1,ITERMS
      BETAX(L)=BETAX(L)+XG*TERMX(L)
      BETA(L)=BETA(L)+YG*TERMX(L)
      DO 260 K=L,ITERMS
  260 ALPHA(L,K)=ALPHA(L,K)+TERMX(K)*TERMX(L)
C
 265  CONTINUE

C
C     Preenchendo a parte triangular inferior da matriz simetrica AtA
c     para "X" e "Y"
C

      DO 270 L=1,ITERMS
      DO 270 K=L,ITERMS
  270 ALPHA(K,L)=ALPHA(L,K)
c
c
c     Preenchendo ARRAY=AtA para inversao (elementos normalizados
c     pela diagonal) para X ou Y
c
c

      DO 280 L=1,ITERMS
      DO 280 K=1,ITERMS
  280 ARRAY(L,K)=ALPHA(L,K)/DSQRT(ALPHA(L,L)*ALPHA(K,K))
C
C     Invertendo AtA para "X" ou " Y" 
C
      CALL MATINV (ITERMS,icofsp,array,DET,ierro)
      IF (IERRO.EQ.1) RETURN
C
C     Computando os coeficientes do polinomio para "X"
C
      DO 290 L=1,ITERMS
      DO 290 K=1,ITERMS
  290 ARRAY(L,K)=ARRAY(L,K)/DSQRT(ALPHA(K,K)*ALPHA(L,L))

C
C     Guarda coeficiente X
C
      DO 300 L=1,ITERMS
      DO 300 K=1,ITERMS
  300 COEFX(L)=COEFX(L)+ARRAY(L,K)*BETAX(K)

C
C     Guarda coeficiente Y
C
      DO 310 L=1,ITERMS
      DO 310 K=1,ITERMS
  310 COEFY(L)=COEFY(L)+ARRAY(L,K)*BETA(K)

      RETURN

      END





c
c     Function pol
c
c     Calcula o valor de um polinomio bivariado em (x,y). Grau N
c     0 < N < 3
c
c

      double precision function pol (icofsp, x, y, coefis, ngrau)


      implicit real *8 (a-h,o-z)

      dimension coefis(icofsp)

c
      mgrau=ngrau+1
      icont=0
      pol=0.d0
      do 1 n=1,mgrau
      do 1 l=1,n
      icont=icont+1
      k=n-l
    1 pol=pol+coefis(icont)*(x**k)*(y**(l-1))


      return
      end






c
c
c
c
c     Subroutine isolo
c
c               
c
c     Fits bivariate polinoms P=P(x,y) of degree n with weights
c		    
c
c
c



      subroutine isolo (idiobs,icofsp,ngrau,ncomum,xest,yest,xp,contag,
     ?coefx,ierro)

      implicit real *8 (a-h,o-z)

      dimension coefx(icofsp),alpha(icofsp,icofsp),array(icofsp,icofsp),
     ?betax(icofsp),termx(icofsp)

      dimension xest(idiobs),yest(idiobs),xp(idiobs),contag(idiobs)


c
c     Initial data
c
      pi=3.141592653589793d0
      grarad=pi/180.d0
      radgra=180.d0/pi

      ierro=0
      det=1.d0

c
c     Initializes vectors
c

      do 8 i=1,icofsp
      betax(i) =0.d0
      coefx(i) =0.d0
      termx(i) =0.d0
      do 8 j=i,icofsp
      array(i,j)=0.d0
 8    alpha(i,j)=0.d0

c
c     Computing the terms
c

      nterms=1
      do i=1,ngrau
      nterms=nterms+i+1
      enddo
 
      igrau=ngrau+1

      iterms=nterms
 
 
      do 265 i=1,ncomum
      x=xest(i)
      y=yest(i)
      xg=xp(i)
      w=contag(i)

c
c     The terms of the polynom
c

      icont=0
      do 240 n=1,igrau
      do 240 l=1,n
      k=n-l
      icont=icont+1
 240  termx(icont)=w*(x**k)*(y**(l-1))


c
c     Computing AtA and AtB 
c

      do 260 l=1,iterms
      betax(l)=betax(l)+xg*termx(l)
      do 260 k=l,iterms
  260 alpha(l,k)=alpha(l,k)+termx(k)*termx(l)
 
  265 continue


c
c     Filling up the inferior triangular part of the symetric AtA matrix
c   
 

      do 270 l=1,iterms
      do 270 k=l,iterms
  270 alpha(k,l)=alpha(l,k)

c
c
c     Filling up ARRAY=AtA for inversion (elements normalized by the
c     diagonal)
c
c

      do 280 l=1,iterms
      do 280 k=1,iterms
  280 array(l,k)=alpha(l,k)/dsqrt(alpha(l,l)*alpha(k,k))

c
c     Inverting AtA  
c

      call matinv (iterms,icofsp,array,det,ierro)

      if (ierro.eq.1) return

c
c     Computing the polynomial coefficients 
c

      do 290 l=1,iterms
      do 290 k=1,iterms
  290 array(l,k)=array(l,k)/dsqrt(alpha(k,k)*alpha(l,l))

c
c     Stores coefficients
c

      do 300 l=1,iterms
      do 300 k=1,iterms
  300 coefx(l)=coefx(l)+array(l,k)*betax(k)


      return

      end





c 
c
c     Subrotina posred
c
c
c     Reducao alfa e delta das posicoes (x,y) medidas em relacao a um
c     catalogo de referencia
c
c
c     Atualmente aplicada para reducao com o catalogo 2MASS e com o 
c     catalogo UCAC2
c
c     Assume-se que as poosicoes (RA,DEC) de catalogo estao na epoca
c     da observacao, no sistema ICRS (J2000)
c
c     Nessa versao nao ha peso.
c     Nessa versao nao ha termos de magnitude ou cor no polinomio de ajuste.
c
c     Ajustes possiveis: 4ctes, 1o, 2o, 3o graus completos, 2o+3dr,
c     2o+3dr+5dr, 3o+5dr.
c
c
c     - (RA,DEC) entram em graus, saem em graus
c     - (x,y)s entram em pixels
c     - erros e sigmas saem em segundos de arco (")
c     - coeficientes (e seus erros) saem em radianos por pixel,
c       rad por pixel**2 etc
c
c
c
c
c
c     Atualizacao: M. Assafin  22/Dez/2005
c
c
c



      subroutine posred (idiobs,icofsp,ireflex,rac,dec,id,ncat,racat,
     ?decat,nest,xob,yob,xest,yest,xp,yp,corte,ngrau,ngrau3,ngrau5,
     ?nstart,nfinal,ra,de,era,ede,alfsig,delsig,alfres,delres,coefx,
     ?coefy,ecoefx,ecoefy,itira,avam,dvam,ierro)


      implicit real*8 (a-h,o-z)

      dimension id(idiobs),racat(idiobs),decat(idiobs),xob(idiobs),
     ?yob(idiobs),xp(idiobs),yp(idiobs),xest(idiobs),yest(idiobs)

      dimension ra(idiobs),de(idiobs),era(idiobs),ede(idiobs),
     ?coefx(icofsp),coefy(icofsp),ecoefx(icofsp),ecoefy(icofsp),
     ?alfres(idiobs),delres(idiobs),itira(idiobs),xsao(icofsp),
     ?ysao(icofsp),xrray(icofsp,icofsp),yrray(icofsp,icofsp),
     ?array(icofsp,icofsp)



      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)


      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))


c
c     Auxiliary data
c


      ncof=icofsp

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi
      ierro=0
      dnove=9.999d0
      d99=99.99d0

      equin=2000.d0
      izero=0
      zero=0.d0
      pma=0.d0
      pmd=0.d0

c
c     Zerando vetores
c

      do i=1,idiobs

      xest(i)=0.d0
      yest(i)=0.d0
      xp(i)=0.d0
      yp(i)=0.d0
      ra(i)=0.d0
      de(i)=0.d0
      era(i)=0.d0
      ede(i)=0.d0
      alfres(i)=0.d0
      delres(i)=0.d0

      enddo


      do i=1,ncof

      coefx(i)=0.d0
      coefy(i)=0.d0
      ecoefx(i)=0.d0
      ecoefy(i)=0.d0
      xsao(i)=0.d0
      ysao(i)=0.d0
      do j=1,ncof
      xrray(j,i)=0.d0
      yrray(j,i)=0.d0
      array(j,i)=0.d0
      enddo

      enddo



c
c     Recolhendo estrelas medidas comuns ao catalogo de referencia
c


      nstart=0
      grac=grarad*rac
      gdec=grarad*dec

      do 10 i=1,nest

      if (id(i).eq.0) go to 10

      nstart=nstart+1

      xest(nstart)=ireflex*xob(i)
      yest(nstart)=yob(i)

c
c     Projecao do catalogo de referencia no plano tangente
c


      bra=grarad*racat(id(i))
      bde=grarad*decat(id(i))
      d=dexy(bra,bde,grac,gdec)

      xp(nstart)=xpad(bra,bde,grac)/d
      yp(nstart)=ypad(bra,bde,grac,gdec)/d


 10   continue



c
c     Ajuste do modelo polinomial entre (x,y) e (X,Y)
c


      call solucao (idiobs,icofsp,ngrau,ngrau3,ngrau5,nstart,xest,yest,
     ?xp,yp,ntira,coefx,coefy,alfsig,delsig,grac,gdec,alfres,delres,
     ?itira,corte,xrray,yrray,ierro)


      if (ierro.eq.1) then

      write (*,*) 'L.S. solution crashed.'

c
c     Zerando vetores no erro
c

      do i=1,idiobs

      xest(i)=0.d0
      yest(i)=0.d0
      xp(i)=0.d0
      yp(i)=0.d0
      ra(i)=0.d0
      de(i)=0.d0
      era(i)=0.d0
      ede(i)=0.d0
      alfres(i)=0.d0
      delres(i)=0.d0
      itira(i)=0

      enddo


      do i=1,ncof

      coefx(i)=0.d0
      coefy(i)=0.d0
      ecoefx(i)=0.d0
      ecoefy(i)=0.d0
      xsao(i)=0.d0
      ysao(i)=0.d0
      do j=1,ncof
      xrray(j,i)=0.d0
      yrray(j,i)=0.d0
      array(j,i)=0.d0
      enddo

      enddo

      return
      endif

c

      nfinal=nstart-ntira

c
c     Determinacao do alfa e delta observado de cada estrela do campo
c
c     (RA,DEC) guardados em graus

c
c     Calculo do erro padrao em alfa e delta para cada estrela de campo
c

      XVAM=0.D0
      YVAM=0.D0
      XVAS=0.D0
      YVAS=0.D0


C
C     NGRAU=0 --> 4 ctes
C
      nterms=1
      do i=1,ngrau
      nterms=nterms+i+1
      enddo
C
      igrau=ngrau+1
      if (ngrau.eq.0) igrau=2
C
      if (ngrau.eq.0) then
      nterms=4
c     ngrau3=0
c     ngrau5=0
      endif
C
      iterms=nterms
      if (ngrau3.eq.3) iterms=iterms+1
      if (ngrau5.eq.5) iterms=iterms+1
C

      do 130 i=1,nest
      x=xob(i)*ireflex
      y=yob(i)

      ICONT=0
      POLX=0.D0
      POLY=0.D0
      DO 20 N=1,IGRAU
      DO 20 LL=1,N
      ICONT=ICONT+1
      K=N-LL
      POLX=POLX+COEFX(ICONT)*(X**K)*(Y**(LL-1))
   20 POLY=POLY+COEFY(ICONT)*(X**K)*(Y**(LL-1))
C
      IF (NGRAU3.EQ.3) THEN
      ICONT=ICONT+1
      POLX=POLX+COEFX(ICONT)*X*(X**2+Y**2)
      POLY=POLY+COEFY(ICONT)*Y*(X**2+Y**2)
      ENDIF

      IF (NGRAU5.EQ.5) THEN
      ICONT=ICONT+1
      POLX=POLX+COEFX(ICONT)*X*(X**2+Y**2)*(X**2+Y**2)
      POLY=POLY+COEFY(ICONT)*Y*(X**2+Y**2)*(X**2+Y**2)
      ENDIF

      RA(I)=POLX
      DE(I)=POLY
C
C     Calcula o erro padrao em alfa para cada estrela
C
      ICONT=0
      if (ngrau.ne.0) then
      DO  30 N=1,IGRAU
      DO  30 LL=1,N
      K=N-LL
      ICONT=ICONT+1
   30 XSAO(ICONT)=(X**K)*(Y**(LL-1))
      IF (NGRAU3.EQ.3) THEN
       ICONT=ICONT+1
       XSAO(ICONT)=X*(X**2+Y**2)
      ENDIF
      IF (NGRAU5.EQ.5) THEN
       ICONT=ICONT+1
       XSAO(ICONT)=X*(X**2+Y**2)*(X**2+Y**2)
      ENDIF
c
      else
      xsao(1)=x
      xsao(2)=y
      xsao(3)=1.d0
      xsao(4)=0.d0
      endif
c
      DO  40 K=1,ITERMS
   40 YSAO(K)=0.D0
C
      DO  50 LL=1,ITERMS
      DO  50 K=1,ITERMS
   50 YSAO(LL)=YSAO(LL)+XRRAY(LL,K)*XSAO(K)

      DO  60 K=1,ITERMS
   60 era(I)=era(I)+XSAO(K)*YSAO(K)

      era(I)=alfsig*DSQRT(era(I))
      XVAM=XVAM+era(I)
      XVAS=XVAS+era(I)**2

C
C     Calcula o erro padrao em delta para cada estrela
C

      ICONT=0
      if (ngrau.ne.0) then
      DO  70 N=1,IGRAU
      DO  70 LL=1,N
      K=N-LL
      ICONT=ICONT+1
   70 YSAO(ICONT)=(X**K)*(Y**(LL-1))
      IF (NGRAU3.EQ.3) THEN
       ICONT=ICONT+1
       YSAO(ICONT)=Y*(X**2+Y**2)
      ENDIF
      IF (NGRAU5.EQ.5) THEN
       ICONT=ICONT+1
       YSAO(ICONT)=Y*(X**2+Y**2)*(X**2+Y**2)
      ENDIF
c
      else
      ysao(1)=y
      ysao(2)=-x
      ysao(3)=0.d0
      ysao(4)=1.d0
      endif
c
      DO  80 K=1,ITERMS
   80 XSAO(K)=0.D0
C
      DO  90 LL=1,ITERMS
      DO  90 K=1,ITERMS
   90 XSAO(LL)=XSAO(LL)+YRRAY(LL,K)*YSAO(K)

      DO 100 K=1,ITERMS
  100 ede(I)=ede(I)+YSAO(K)*XSAO(K)

      ede(I)=delsig*DSQRT(ede(I))
      YVAM=YVAM+ede(I)
      YVAS=YVAS+ede(I)**2

C
C     Calcula erro dos coeficientes para a solucao X
C

      IF (NGRAU.NE.0) THEN
      DO 110 K=1,ITERMS
 110  ECOEFX(K)=ALFSIG*DSQRT(XRRAY(K,K))
      ELSE
      ECOEFX(1)=ALFSIG*DSQRT(XRRAY(3,3))
      ECOEFX(2)=ALFSIG*DSQRT(XRRAY(1,1))
      ECOEFX(3)=ALFSIG*DSQRT(XRRAY(2,2))
      ENDIF
C
C     Calcula erro dos coeficientes para a solucao Y
C

      IF (NGRAU.NE.0) THEN
      DO 120 K=1,ITERMS
 120  ECOEFY(K)=DELSIG*DSQRT(YRRAY(K,K))
      ELSE
      ECOEFY(1)=DELSIG*DSQRT(YRRAY(4,4))
      ECOEFY(2)=DELSIG*DSQRT(YRRAY(2,2))
      ECOEFY(3)=DELSIG*DSQRT(YRRAY(1,1))
      ENDIF
C
C
 130  CONTINUE
C
C
      if (ngrau.eq.0) then
      iterms=3
      endif

C
C     Media dos erros padrao de todas as estrelas medidas
C

      EXMED=XVAM/NEST
      EYMED=YVAM/NEST
      XVAS=DSQRT((XVAS-2.D0*EXMED*XVAM+NEST*EXMED**2)/(NEST-1.D0))
      YVAS=DSQRT((YVAS-2.D0*EYMED*YVAM+NEST*EYMED**2)/(NEST-1.D0))
      AVAM=EXMED
      DVAM=EYMED

c
c     RA e DEC em graus
c


      j=0

      do 140 i=1,nest

      x=ra(i)
      y=de(i)

      ra(i)=alff(x,y,grac,gdec)
      de(i)=deltt(ra(i),y,grac,gdec)

      ra(i)=ra(i)*radgra
      de(i)=de(i)*radgra

c     if (id(i).eq.0) go to 140

c     j=j+1

c     write(*,*) 'alfres delres itira = ',j,alfres(j),delres(j),itira(j) 



 140  continue


c
c     Debug
c


c     perc=100.d0*ntira/nstart
c     write (*,141) alfsig,delsig,nstart,nfinal,perc,avam,dvam
c141  format(1x,'alfsig delsig NI NF = ',2(1x,f6.3),2(1x,i4),1x,f6.2,
c    ?'%',2(1x,f6.3))




      return
      end



C
C     Subrotina solucao
C
C
C     Ajuste polinomial bivariado P=P(x,y) ate grau 3 e distorcao
C     radial a grau 5
C		    
C
C							    

      subroutine solucao (idiobs,icofsp,ngrau,ngrau3,ngrau5,nstart,xest,
     ?yest,xp,yp,ntira,coefx,coefy,alfsig,delsig,grac,gdec,alfres,
     ?delres,itira,corte,xrray,yrray,ierro)




      IMPLICIT REAL *8 (A-H,O-Z)
      DIMENSION COEFX(icofsp),COEFY(icofsp),ALPHAX(icofsp,icofsp),
     ?ALPHAY(icofsp,icofsp),ARRAY(icofsp,icofsp),BETAX(icofsp),
     ?BETAY(icofsp),TERMX(icofsp),TERMY(icofsp),ITIRA(idiobs)

      DIMENSION XEST(idiobs),YEST(idiobs),XP(idiobs),YP(idiobs),
     ?XRRAY(icofsp,icofsp),YRRAY(icofsp,icofsp),ALFRES(idiobs),
     ?DELRES(idiobs)



      ALFF (XX,YY,ZZ,WW)=ZZ+DATAN2(XX,DCOS(WW)-YY*DSIN(WW))
      DELTT(XX,YY,ZZ,WW)=DATAN2((YY*DCOS(WW)+DSIN(WW))*DCOS(XX-ZZ),
     ?DCOS(WW)-YY*DSIN(WW))

C
C     Initializing data
C
      PI=3.141592653589793D0
      GRARAD=PI/180.D0
      RADGRA=180.D0/PI
      DET  =1.D0
      IERRO=0

      alfsig=0.d0
      delsig=0.d0

c

      sigma=grarad*corte/3600d0

C
      NCOMUM=nstart
C
c
c     Computa no. de termos do polinomio
c     NGRAU=0 --> 4 constantes
c

      NTERMS=1
      DO I=1,NGRAU
      NTERMS=NTERMS+I+1
      ENDDO

C
      IGRAU=NGRAU+1
      IF (NGRAU.EQ.0) IGRAU=2
      NTIRA=0
C


 5    LCONT=0

C
      RESMX=0.D0
      RES2X=0.D0
      RESMY=0.D0
      RES2Y=0.D0
C
      IF (NGRAU.EQ.0) THEN
      NTERMS=4
      NGRAU3=0
      NGRAU5=0
      ENDIF
C
      ITERMS=NTERMS
      IF (NGRAU3.EQ.3) ITERMS=ITERMS+1
      IF (NGRAU5.EQ.5) ITERMS=ITERMS+1

C
C     Checa No. de estrelas versus no. de coeficientes a ajustar
C

      IF (NGRAU.EQ.0) THEN
      iequa=2.d0*ncomum
      mtira=2.d0*ntira
      if ((iequa-mtira).LT.ITERMS) then
      IERRO=1
      RETURN
      endif
      else
      if ((ncomum-ntira).LT.ITERMS) then
      IERRO=1
      RETURN
      endif
      ENDIF

c
      DO 9 I=1,ITERMS
      BETAX(I) =0.D0
      BETAY(I) =0.D0
      COEFX(I) =0.D0
      COEFY(I) =0.D0
      TERMX(I) =0.D0
      TERMY(I) =0.D0
      DO 9 J=I,ITERMS
      ALPHAX(I,J)=0.D0
 9    ALPHAY(I,J)=0.D0


C
C     Montando equacoes de condicao para ajuste com 4 ctes
C

      IF (NGRAU.EQ.0) THEN

      DO 10 I=1,NCOMUM
      IF (ITIRA(I).NE.0) GO TO 10
      X=XEST(I)
      Y=YEST(I)
      XG=XP(I)
      YG=YP(I)
C
C     Computa  termos dos coeficientes para AtB
C

      TERMX(1)=TERMX(1)+X**2+Y**2
      TERMX(2)=TERMX(2)+X
      TERMX(3)=TERMX(3)+Y
      TERMX(4)=TERMX(4)+1.D0
      BETAX(1)=BETAX(1)+X*XG+Y*YG
      BETAX(2)=BETAX(2)+Y*XG-X*YG
      BETAX(3)=BETAX(3)+XG
      BETAX(4)=BETAX(4)+YG
   10 CONTINUE

C
C     Preenchendo  AtA para ajuste com 4 ctes
C

      ALPHAX(1,1)=TERMX(1)
      ALPHAX(1,2)=0.D0
      ALPHAX(1,3)=TERMX(2)
      ALPHAX(1,4)=TERMX(3)
      ALPHAX(2,2)=TERMX(1)
      ALPHAX(2,3)=TERMX(3)
      ALPHAX(2,4)=-TERMX(2)
      ALPHAX(3,3)=TERMX(4)
      ALPHAX(3,4)=0.D0
      ALPHAX(4,4)=TERMX(4)
      DO 15 L=1,ITERMS
      DO 15 K=L,ITERMS
   15 ALPHAX(K,L) =ALPHAX(L,K)
C
      GO TO 75

      ENDIF


C
C     Montando equacoes de condicao para ajustes que NAO o de 4 ctes
C     para polinomio X


      DO 65 I=1,NCOMUM
      IF (ITIRA(I).NE.0) GO TO 65
      X=XEST(I)
      Y=YEST(I)
      XG=XP(I)
      YG=YP(I)
C
C     Computando termos para o polinomio em X
C
      ICONT=0
      DO 40 N=1,IGRAU
      DO 40 L=1,N
      K=N-L
      ICONT=ICONT+1
      TERMX(ICONT)=(X**K)*(Y**(L-1))
   40 TERMY(ICONT)=(X**K)*(Y**(L-1))

      IF (NGRAU3.EQ.3) THEN
      ICONT=ICONT+1
      TERMX(ICONT)=X*(X**2+Y**2)
      TERMY(ICONT)=Y*(X**2+Y**2)
      ENDIF

      IF (NGRAU5.EQ.5) THEN
      ICONT=ICONT+1
      TERMX(ICONT)=X*(X**2+Y**2)*(X**2+Y**2)
      TERMY(ICONT)=Y*(X**2+Y**2)*(X**2+Y**2)
      ENDIF
C
C     Computando AtA and AtB (para "X" e "Y")
C

      DO 60 L=1,ITERMS
      BETAX(L)=BETAX(L)+XG*TERMX(L)
      BETAY(L)=BETAY(L)+YG*TERMY(L)
      DO 60 K=L,ITERMS
      ALPHAX(L,K)=ALPHAX(L,K)+TERMX(K)*TERMX(L)
   60 ALPHAY(L,K)=ALPHAY(L,K)+TERMY(K)*TERMY(L)
C
   65 CONTINUE

C
C     Preenchendo parte triangular inferior da matriz simetrica AtA
c     (para "X" e "Y")
C

      DO 70 L=1,ITERMS
      DO 70 K=L,ITERMS
      ALPHAX(K,L)=ALPHAX(L,K)
   70 ALPHAY(K,L)=ALPHAY(L,K)

C
C     Preenchendo ARRAY=AtA para inversao (elementos normalizados pela
C     diagonal) para "X"
C

 75   CONTINUE
C
      DO 80 L=1,ITERMS
      DO 80 K=1,ITERMS
   80 ARRAY(L,K)=ALPHAX(L,K)/DSQRT(ALPHAX(L,L)*ALPHAX(K,K))
C
C     Invertendo AtA para "X"
C
      CALL MATINV (ITERMS,icofsp,array,DET,ierro)
      IF (IERRO.EQ.1) RETURN
C
C     Computando coeficientes do polinomio para "X"
C
      DO 90 L=1,ITERMS
      DO 90 K=1,ITERMS
   90 ARRAY(L,K)=ARRAY(L,K)/DSQRT(ALPHAX(K,K)*ALPHAX(L,L))
C
C     Backup do array para "X"
C
      IF (NGRAU.EQ.0) GO TO 97
      DO 95 L=1,ITERMS
      DO 95 K=1,ITERMS
   95 XRRAY(L,K)=ARRAY(L,K)
C
 97   DO 100 L=1,ITERMS
      DO 100 K=1,ITERMS
  100 COEFX(L)=COEFX(L)+ARRAY(L,K)*BETAX(K)
C
C     Obtem coeficientes e backup do array para modelo de 4 Constantes
C
      IF (NGRAU.EQ.0) THEN
      DO  105 L=1,ITERMS
 105  TERMX(L)=COEFX(L)
      COEFX(1)=TERMX(3)
      COEFX(2)=TERMX(1)
      COEFX(3)=TERMX(2)
      COEFY(1)=TERMX(4)
      COEFY(2)=-TERMX(2)
      COEFY(3)=TERMX(1)
      do 107 l=1,iterms
      do 107 k=1,iterms
      xrray(l,k)=array(l,k)
 107  yrray(l,k)=array(l,k)
      GO TO 133
      ENDIF

C
C     Preenchendo ARRAY=AtA para inversao (elementos normalizados pela
C     diagonal) para "Y"
C

      DO 110 L=1,ITERMS
      DO 110 K=1,ITERMS
  110 ARRAY(L,K)=ALPHAY(L,K)/DSQRT(ALPHAY(L,L)*ALPHAY(K,K))
C
C     Invertendo AtA para "Y"
C

      CALL MATINV (ITERMS,icofsp,array,DET,ierro)
      IF (IERRO.EQ.1) RETURN
C
C     Computando coeficientes do polinomio para "Y"
C


      DO 120 L=1,ITERMS
      DO 120 K=1,ITERMS
  120 ARRAY(L,K)=ARRAY(L,K)/DSQRT(ALPHAY(K,K)*ALPHAY(L,L))
C
C     Backup do array para "Y"
C

      DO 125 L=1,ITERMS
      DO 125 K=1,ITERMS
  125 YRRAY(L,K)=ARRAY(L,K)
C
      DO 130 L=1,ITERMS
      DO 130 K=1,ITERMS
  130 COEFY(L)=COEFY(L)+ARRAY(L,K)*BETAY(K)

c
c
c     Computa residuos. Estrelas com residuo em alfa ou delta mais alto
c     sao eliminadas uma a uma, ate que nenhuma possua (O-C) maior
c     que o valor definido pela variavel "corte"
c

 133  CONTINUE
C


      REMAXI=-1.D14

      DO 160 I=1,NCOMUM
      X=XEST(I)
      Y=YEST(I)
      xg=xp(i)
      yg=yp(i)
      ICONT=0
      POLX=0.D0
      POLY=0.D0
      DO 140 N=1,IGRAU
      DO 140 L=1,N
      ICONT=ICONT+1
      K=N-L
      POLX=POLX+COEFX(ICONT)*(X**K)*(Y**(L-1))
  140 POLY=POLY+COEFY(ICONT)*(X**K)*(Y**(L-1))
C
      IF (NGRAU3.EQ.3) THEN
      ICONT=ICONT+1
      POLX=POLX+COEFX(ICONT)*X*(X**2+Y**2)
      POLY=POLY+COEFY(ICONT)*Y*(X**2+Y**2)
      ENDIF
      IF (NGRAU5.EQ.5) THEN
      ICONT=ICONT+1
      POLX=POLX+COEFX(ICONT)*X*(X**2+Y**2)*(X**2+Y**2)
      POLY=POLY+COEFY(ICONT)*Y*(X**2+Y**2)*(X**2+Y**2)
      ENDIF

C
C     Computa (O-C) para R.A. e  Dec
C

      xx=alff(polx,poly,grac,gdec)
      yy=deltt(xx,poly,grac,gdec)

      xxr=alff(xg,yg,grac,gdec)
      yyr=deltt(xxr,yg,grac,gdec)


      AUX=(XX-XXR)*DCOS(YYR)
      AUY=YY-YYR
      ALFRES(I)=AUX*radgra*3600d0
      DELRES(I)=AUY*radgra*3600d0


C
      IF (ITIRA(I).NE.0) GO TO 160
      IF ((DABS(AUX).GT.REMAXI).OR.(DABS(AUY).GT.REMAXI)) THEN
      IFORA =I
      REMAXI=DMAX1(DABS(AUX),DABS(AUY))
      ENDIF

 157  RESMX=RESMX+ALFRES(I)
      RES2X=RES2X+ALFRES(I)**2
      RESMY=RESMY+DELRES(I)
      RES2Y=RES2Y+DELRES(I)**2

      LCONT=LCONT+1

C
  160 CONTINUE
C
C     Atingido numero minimo de estrelas!
C
      iwar=0
      IF (NGRAU.EQ.0) THEN
      iequa=2.d0*LCONT
      if (iequa.eq.4) iwar=1
      ELSE
      if (LCONT.eq.ITERMS) iwar=1
      ENDIF
C
      IF (IWAR.EQ.1) THEN
      alfsig=0.D0
      delsig=0.D0
      RETURN
      ENDIF


C
C     Computa media e desvio padrao dos (O-C)s
C

      XMED=RESMX/LCONT
      YMED=RESMY/LCONT
C
      IF (NGRAU.EQ.0) THEN
      alfsig=DSQRT((RES2X-2.D0*XMED*RESMX+LCONT*XMED**2)/(LCONT-2.D0))
      delsig=DSQRT((RES2Y-2.D0*YMED*RESMY+LCONT*YMED**2)/(LCONT-2.D0))
      ELSE
      alfsig=DSQRT((RES2X-2.D0*XMED*RESMX+LCONT*XMED**2)/(LCONT-ITERMS))
      delsig=DSQRT((RES2Y-2.D0*YMED*RESMY+LCONT*YMED**2)/(LCONT-ITERMS))
      ENDIF
C
c
c     Atingido o corte !
c

      if (remaxi.lt.sigma) return

c
c     Corte nao atingido, prosseguir com eliminacao de estrelas
c

      NTIRA=NTIRA+1
      ITIRA(IFORA)=1
      GO TO 5


      RETURN
      END





c
c
c
c     Subroutine results
c
c
c     Writes astrometric results of individual objects for each field
c     for each catalogue (the so called xy PRAIA files)
c
c
c     idcat1 - index for (RA,Dec) reduction information for reference catalogue stars
c
c     idcat2 - index for a given catalogue's original magnitudes and proper motion
c              information (values, errors)
c
c     idcat3 - index for 2MASS-based (J,H,K) magnitudes and errors for all identified
c              2MASS stars in GAIA_1 catalogue
c
c 
c
c      Last modification: M. Assafin 09/Jan/2017
c
c
c
c


      subroutine results (d99,idiobs,nest,ired,idcat1,idcat2,idcat3,xob,
     ?yob,seng,altu,fgcc,fumag,fumag2,cmag,omag,cmgj,cmgh,cmgk,oemag,
     ?ecmgj,ecmgh,ecmgk,cpma,cpmd,cepma,cepmd,exgcc,eygcc,era,ede,
     ?alfsig,delsig,nstart,nfinal,alfres,decres,itira,ra,de,iuth,iutm,
     ?sut,iutano,iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny,xfdp,
     ?yfdp)


      implicit real *8 (a-h,o-z)


      dimension idcat1(idiobs),idcat2(idiobs),idcat3(idiobs),
     ?xob(idiobs),yob(idiobs),seng(idiobs),altu(idiobs),fgcc(idiobs),
     ?cmag(idiobs),omag(idiobs),cmgj(idiobs),cmgh(idiobs),cmgk(idiobs),
     ?ecmgj(idiobs),ecmgh(idiobs),ecmgk(idiobs),cpma(idiobs),
     ?cpmd(idiobs),cepma(idiobs),cepmd(idiobs),exgcc(idiobs),
     ?eygcc(idiobs),era(idiobs),ede(idiobs),alfres(idiobs),
     ?decres(idiobs),itira(idiobs),ra(idiobs),de(idiobs),xfdp(idiobs),
     ?yfdp(idiobs)


      character*200 ired
      character*20 ichfil
      character*150 infits
      character*69 mchobj


c
c     Opens  PRAIA xy file
c


      open (64,file=ired)

c
      jj=0

c

      
      do i=1,nest

      xmgu=d99

      pma=d99
      pmd=d99
      epma=d99
      epmd=d99

      xmgj=d99
      xmgh=d99
      xmgk=d99
      ermgj=d99
      ermgh=d99
      ermgk=d99

      ktira=99

      alsi2=d99
      desi2=d99


      xfd=xfdp(i)
      yfd=yfdp(i)

      if (xfd.le.-100.d0 .or. xfd.ge.1000.d0) xfd=d99 
      if (yfd.le.-100.d0 .or. yfd.ge.1000.d0) yfd=d99 




c
c     Retrieves (J,H,K) magnitudes and errors for all 2MASS catalogue stars
c


      if (idcat3(i).ne.0) then

      j=idcat3(i)

      xmgj=cmgj(j)
      xmgh=cmgh(j)
      xmgk=cmgk(j)
      ermgj=ecmgj(j)
      ermgh=ecmgh(j)
      ermgk=ecmgk(j)

      endif


c
c     Retrieves a given catalogue's original magnitude and proper motions
c     information (usually from the GAIA_1 catalogue)
c


      if (idcat2(i).ne.0) then

      j=idcat2(i)

      xmgu=cmag(j)

      pma=cpma(j)
      pmd=cpmd(j)
      epma=cepma(j)
      epmd=cepmd(j)

      endif


c
c     Retrieves (RA,Dec) reduction information for reference catalogue stars
c

      if (idcat1(i).ne.0) then

      jj=jj+1

      j=idcat1(i)

      xmgu=cmag(j)


      pma=cpma(j)
      pmd=cpmd(j)
      epma=cepma(j)
      epmd=cepmd(j)


      ktira=itira(jj)
      alsi2=alfres(jj)
      desi2=decres(jj)

      endif

c


      ras=ra(i)/15.d0

c

      write (64,470) xob(i),yob(i),seng(i),altu(i),fgcc(i),xfd,yfd,xmgu,
     ?omag(i),oemag,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,ermgk,
     ?pma,pmd,epma,epmd,exgcc(i),eygcc(i),era(i),ede(i),alfsig,delsig,
     ?nstart,nfinal,alsi2,desi2,ktira,ras,de(i),iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny


 470  format(2(1x,f7.2),1x,f5.3,2f10.2,2f7.3,11(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
     ?2(1x,i5))



c
c470  format(2(1x,f7.2),1x,f5.3,2(f10.2),13(1x,f6.3),4(1x,f7.3),
c    ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
c    ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
c    ?2(1x,i5))


c
c470  format(2(1x,f7.2),1x,f5.3,2(1x,f9.2),13(1x,f6.3),4(1x,f7.3),
c    ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
c    ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
c    ?2(1x,i5))


      enddo


      close (64)

      return
      end



c   
c     Subrotine estat
c
c
c     Outputs target statistics and metadata (mag, epoch, measuring and position
c     errors, etc) on screen and in PRAIA, MPC and NIMA file formats.
c
c     input - target file with (RA,Dec)s and instants of targets.
c           - PRAIA xy file with (RA,Dec) astrometry 
c
c
c     box    -> position error box for target identification in arcseconds.
c
c     tbox   -> epoch error box for target identification in UTC seconds
c
c
c
c     Last modification:  M. Assafin   10/Jan/2017
c
c

      subroutine estat (box,tbox,ialvos,input,ipraia,impc,inima,obtipo,
     ?band,iau,icat)


      IMPLICIT REAL *8 (A-H,O-Z)

      character*150 infits
      character*200 input
      character*50 ialvos,ipraia,impc,inima
      character*20 ichfil,mchobj,iobalv
      character*1 isig


      character*1  obtipo
      character*1  band
      character*3  iau
      character*1  icat

      character*300  ler


      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi


c
c     Error box for instant of time (UTC seconds)
c

      boxepo=tbox

c
c     Converts time error box for Julian Date (fraction of Julian day)
c

      boxepo=boxepo/(3600.d0*24.d0)

c

      ipegou=0

c
c     Sets up backspace of files (g77, gfortran, etc)
c

      call backsp (1,nbac,7)

c

 40   format(a50)


c

      write (*,*)

      write (*,*) ' offsets (RA,DE), Sigma(RA,DE),    Ncat,   (x, y) err
     ?or,  UTC  instant and date, exptime,  target                    RA
     ?             DEC                mag'

      write (*,*) '       (arcsec)        (arcsec)                (arcse
     ?c)    h  m   s   year mo dy   (s)                           h  m  
     ? s       dg am  as'

      write (*,*)



c
c     Opens target file in PRAIA format (ephemeris, etc) with target data
c


      open (7,file=ialvos,status='old',err=200)


c
c     Opens output target files in PRAIA, MPC and NIMA formats
c

      open (2,file=ipraia)
      open (3,file=impc)
      open (9,file=inima)

c

 3    read (2,5,end=6) isig
 5    format(a1)
      go to 3
 6    call backsp (2,nbac,2)

c


 7    read (3,5,end=8) isig
      go to 7
 8    call backsp (2,nbac,3)

c



 9    read (9,5,end=13) isig
      go to 9
 13   call backsp (2,nbac,9)


c
c     Reads target data file
c


 100  read (7,101,end=200) iah,iam,as,isig,idg,idm,ds,datalv,
     ?iobalv
 101  format(1x,i2,1x,i2,1x,f9.6,1x,a1,i2,1x,i2,1x,f8.5,1x,f16.8,
     ?1x,a20)

      rafat=hmsgms(iah,iam,as)
      defat=hmsgms(idg,idm,ds)
      if (isig.eq.'-') defat=-defat


 30   continue


c
c     Opens PRAIA xy astrometry file
c


      open (1,file=input)


c
c     Checks Julian Date of target and of xy astrometry for target identification
c


 18   continue


c
c     Reads PRAIA xy astrometry file
c



      read (1,10,err=18,end=12) xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny



c
c     Targets marked with "negative" JD are not checked against time
c     error box (usually "position-fixed" targets like QSOS, stars
c     with unknown or small proper motions, etc).
c
c     These "fixed" targets are searched throughout all FOVs/images and
c     are selected if they fall inside the position error box for
c     (RA,Dec).
c
c     
c     Targets with valid JDs are set as time-dependent as well as
c     position-dependent targets. They are only selected if they fall
c     inside the position error box for (RA,Dec) and also in the
c     time error box according to the mean JD observation instant.
c
c


      if (datalv.gt.0.d0) then

      dtemp=dabs(datalv-dj)

      if (dtemp.gt.boxepo) go to 12

      endif

      rewind (1)


c
c     So far the target ("fixed" or not) passed the time filter  
c




 19   continue
      read (1,10,err=19,end=12) xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,mchobj,nx,ny


 10   format(2(1x,f7.2),1x,f5.3,2f10.2,2f7.3,11(1x,f6.3),4(1x,f7.3),
     ?6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,1x,a20,
     ?2(1x,i5))


c
c     Position error box filter
c



      dx=(ra-rafat)*dcos(defat*grarad)*3600.d0*15.d0
      dy=(de-defat)*3600.d0

      if (dabs(dx).gt.box) go to 20
      if (dabs(dy).gt.box) go to 20



c
c     Object in the xy PRAIA file is identified with a target.
c

      ipegou=1


c
c     Measured (RA,Dec) of target in hexasegimal notation
c

      raco=ra
      deco=de
      iah=raco
      am=(raco-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0
      if (deco.lt.0.d0) then
      isig='-'  
      deco=-deco
      else
      isig='+' 
      endif
      idg=deco
      dm=(deco-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0


c
c     Day and fraction of day
c

      dia=hmsgms(iuth,iutm,sut)

      dia=dia/24.d0

      dia=dia+iutdia


c
c     Outputs object/target data on screen
c


      write (*,16) dx,dy,alfsiu,delsiu,nfinau,ex,ey,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,iexps,iobalv,iah,iam,sa,isig,idg,idm,
     ?ds,cudmg,band

 16   format(4(1x,f7.3),2x,i5,2x,2(1x,f7.3),2x,i2.2,1x,i2.2,1x,f5.2,1x,
     ?i4,1x,i2.2,1x,i2.2,2x,i4,5x,a20,1x,i2.2,1x,i2.2,1x,f7.4,2x,a1,
     ?i2.2,1x,i2.2,1x,f6.3,9x,f5.2,1x,a1)


c
c     Outputs object/target data on PRAIA output target format
c

      write (2,11) dx,dy,xob,yob,seng,altu,fgcc,xfdp,yfdp,
     ?xmgu,cudmg,cudmg2,xmgj,xmgh,xmgk,fumag,fumag2,ermgj,ermgh,
     ?ermgk,pma,pmd,epma,epmd,ex,ey,erau,edeu,alfsiu,delsiu,
     ?nstaru,nfinau,alsiu,desiu,ktirau,ra,de,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,iexps,ichfil,infits,iobalv,nx,ny


 11   format(2(1x,f7.3),2(1x,f7.2),1x,f5.3,2f10.2,2f7.3,11(1x,f6.3),
     ?4(1x,f7.3),6(1x,f6.3),2(1x,i4),2(1x,f6.3),1x,i4,2(1x,f13.9),1x,
     ?i2,1x,i2,1x,f5.2,1x,i4,1x,i2,1x,i2,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20,2(1x,i5))

c


c
c     Outputs object/target data on MPC output target format
c



      ler=''


c     write (ler,26) obname,obtipo,iutano,iutmes,dia,iah,iam,sa,
c    ?isig,idg,idm,ds,cudmg,band,iau,icat

      write (ler,26) iobalv,obtipo,iutano,iutmes,dia,iah,iam,sa,
     ?isig,idg,idm,ds,cudmg,band,iau,icat

 26   format(a14,a1,i4.4,1x,i2.2,1x,f8.5,1x,i2.2,1x,i2.2,1x,f6.3,a1,
     ?i2.2,1x,i2.2,1x,f5.2,9x,f5.2,a1,6x,a3,1x,a1)


      if (ler(24:24).eq.' ') ler(24:24)='0'

      if (ler(39:39).eq.' ') ler(39:39)='0'

      if (ler(52:52).eq.' ') ler(52:52)='0'

      if (ler(66:66).eq.' ') ler(66:66)='0'

c

      write (3,27) ler
 27   format(a82)




c
c     Outputs object/target data on NIMA output target format
c



      ler='' 

      write (ler,28) iah,iam,sa,isig,idg,idm,ds,cudmg,dj,iau,icat
 28   format(i2.2,1x,i2.2,1x,f7.4,2x,a1,i2.2,1x,i2.2,1x,f6.3,3x,f6.3,2x,
     ?f16.8,2x,a3,2x,a1)



      if (ler(01:01).eq.' ') ler(01:01)='0'

      if (ler(04:04).eq.' ') ler(04:04)='0'

      if (ler(07:07).eq.' ') ler(07:07)='0'

      if (ler(17:17).eq.' ') ler(17:17)='0'

      if (ler(20:20).eq.' ') ler(20:20)='0'

      if (ler(23:23).eq.' ') ler(23:23)='0'
c

      write (9,29) ler,iobalv
 29   format(a63,3x,a20)




c
c     The instant of time of the FOV/image does not match with the associated
c     JD of the target.
c
c     Or there are no more objects in the xy PRAIA astrometry file to search
c     for this target.
c
c     Or an object in the xy FOV/image was identified with a target.
c
c
c     In any of these cases, restarts the search in the FOV/imae for other targets 
c
c


 12   close (1)

      go to 100



c
c     Object outside position error box.
c
c     Continue target search with other object
c


 20   go to 19


c
c     Finish up things
c


 200  continue

      close (2)
      close (3)
      close (9)
      close (7)

      if (ipegou.eq.0) then
      write (*,*) ' Target not identified. '
      endif

      return
      end



c   
c     Subrotina tempo
c
c     Devolve o tempo total decorrido desde o inicio da execucao e o tempo
c     parcial decorrido entre a ultima e a chamada atual, em segundos.
c     
c     O tempo eh a soma do tempo gasto pela CPU no programa mais o tempo
c     gasto pelo sistema na execucao do programa
c
c
c     tempoi = tempo total decorrido ate a ultima chamada, anterior a atual 
c     tempot = tempo total decorrido ate esta chamada
c     tempop = tempo parcial decorrido entre a ultima e a atual chamada
c

      subroutine tempo (tempoi,tempot,tempop)

      implicit real*8 (a-h,o-z)

      real  time(2)

c

      tempop=dtime(time)

      tempot=tempoi+tempop

      return

      end



c   
c     Subrotina backsp
c
c     Efetua o numero correto de backspaces no arquivo aberto "L", para
c     "recuar uma linha" no arquivo "L".
c
c     O numero de backspaces depende do fortran utilizado (gfortran,
c     g77, etc ...)
c
c
c     key=1 : determina o numero correto de backspaces a ser executado
c
c     key=2 : executa o numero correto de backspaces no arquivo aberto "L"
c
c     nbac = numero correto de backspaces a ser executado, para recuar
c            uma linha no arquivo
c
c     L    = unidade do arquivo aberto para execucao de backspace
c

      subroutine backsp (key,nbac,L)

      implicit real*8 (a-h,o-z)

      character*20 imaux
      character*4 jmaux
      character*9 sista
      character*29 systa


c
c     Key=2, executar backspace
c


      if (key.eq.2) then

      do i=1,nbac
      backspace L
      enddo

      return

      endif

c
c     Key=1, determinar numero correto de backspaces a ser executado,
c     para recuar uma linha em um arquivo aberto
c


      sista='rm -f -r '

      imaux=''

      imaux(1:16)='PRAIA_backsp.aux'


      do 1 i=1,9999

      write (jmaux,'(i4.4)') i

      imaux(17:20)=jmaux(1:4)

      open(L,file=imaux,status='old',err=2)
      close (L)
 1    continue

 2    close (L)

      open(L,file=imaux)

      systa=sista//imaux

c

      do i=1,10
      write (L,*) i
      enddo

      close (L)

      open(L,file=imaux)
      
      do i=1,15
      read (L,*,end=10)
      enddo

 10   do i=1,2
      backspace L
      enddo
      
 
      read (L,*) nbac

      close (L)

      nbac=nbac-9

c


      call system (systa)

      return

      end







c
c
c     Subroutine wfits
c
c     Writes a fits image given a matriz of pixels.
c
c
c     bitpix = 16  integer*2 data
c              +32 integer*4 data
c              -32 real*4 data
c              +64 integer*8 data
c              -64 real*8 data
c
c
c     iswap = 1 (do not swap image)
c             2 (swap image)
c
c
c
c
c     Last modified:   M. Assafin  15/Aug/2015
c

      subroutine wfits (ipmax,if,file,bitpix,iswap,nx,ny,pixel,bscale,
     ?bzero)

      implicit real*8 (a-h,o-z)


      integer*2 iwork2(1440)
      integer*4 iwork4(720)
      integer*8 iwork8(360)
      real*4    rwork4(720)
      real*8    rwork8(360)

      integer*1 swork(2880),iby8(8)
      integer*2 bitpix

      real*4 pixel(ipmax,ipmax)

      character*50 file
      character*2880 header

      icab(il,ic)=(il-1)*80+ic


c
c     Initial data
c

      nbytes=2880

      if (bitpix.eq.16)  ibytes=2
      if (bitpix.eq.32)  ibytes=4
      if (bitpix.eq.-32) ibytes=4
      if (bitpix.eq.64)  ibytes=8
      if (bitpix.eq.-64) ibytes=8
      

c

      kwork=nbytes/ibytes


c
c     Opens fits file
c 


      open (if,file=file,access='direct',form='unformatted',
     ?recl=2880)



c
c     Writes fits header
c

      header=''

      l=1
      ic1=1
      ic2=46
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='SIMPLE  =                    T / Fits Standard'

      l=2
      ic1=1
      ic2=47
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BITPIX  =                      / Bits per pixel'

      l=3
      ic1=1
      ic2=47
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS   =                    2 / Number of axes'


      l=4
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS1  =                      / Axis Length'


      l=5
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='NAXIS2  =                      / Axis Length'


      l=6
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BSCALE  =                      / Data scale '


      l=7
      ic1=1
      ic2=44
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='BZERO   =                      / Zero point '


      l=2
      ic1=28
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i3)') bitpix


      l=4
      ic1=26
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i5)') nx

      l=5
      ic1=26
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(i5)') ny


      l=6
      ic1=11 
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(f20.10)') bscale


      l=7
      ic1=11 
      ic2=30
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      write (header(ip1:ip2),'(f20.10)') bzero 



      do l=8,35
      ic1=1
      ic2=41
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='COMMENTS=                      / Comments'
      enddo

      l=36
      ic1=1
      ic2=3
      ip1=icab(l,ic1)
      ip2=icab(l,ic2)
      header(ip1:ip2)='END'

      irec=1

      write (if,rec=irec) header

c
c     Now writes the data
c


      m=0

      do i=1,ny
      do j=1,nx

      m=m+1

c
      if (bitpix.eq.16) then
      iwork2(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork2
      endif
      endif
c
      if (bitpix.eq.32) then
      iwork4(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork4
      endif
      endif
c
      if (bitpix.eq.64) then
      iwork8(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) iwork8
      endif
      endif
c
      if (bitpix.eq.-32) then
      rwork4(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) rwork4
      endif
      endif
c
      if (bitpix.eq.-64) then
      rwork8(m)=pixel(j,i)
      if (m.eq.kwork) then
      irec=irec+1
      write (if,rec=irec) rwork8
      endif
      endif
c


      if (m.eq.kwork) then
      m=0
      if (iswap.eq.2) then
      call swapo (if,ibytes,nbytes,irec,swork)
      write (if,rec=irec) swork
      endif
      endif

      enddo
      enddo


      if (m.eq.0) go to 50

      irec=irec+1

      if (bitpix.eq.16) write (if,rec=irec) (iwork2(ii),ii=1,m)
      if (bitpix.eq.32) write (if,rec=irec) (iwork4(ii),ii=1,m)
      if (bitpix.eq.64) write (if,rec=irec) (iwork8(ii),ii=1,m)
      if (bitpix.eq.-32) write (if,rec=irec) (rwork4(ii),ii=1,m)
      if (bitpix.eq.-64) write (if,rec=irec) (rwork8(ii),ii=1,m)

      if (iswap.eq.2) then
      nbytes=m*ibytes
      call swapo (if,ibytes,nbytes,irec,swork)
      write (if,rec=irec) (swork(ii),ii=1,nbytes)
      endif

 50   continue


      close (if)


      return
      end



c
c
c     subroutine swapo
c
c
c     Swap bytes of pixel data
c
c     Image can by integer or floating point
c
c
c     Last modified: M. Assafin   15/Aug/2015
c


      subroutine swapo (if,ibytes,nbytes,irec,swork)

      IMPLICIT REAL *8 (A-H,O-Z)
      
      integer*1 swork(2880),iby8(8)

c

      read (if,rec=irec) swork


      do k=ibytes,nbytes,ibytes

      do m=1,ibytes
      iby8(m)=swork(k-m+1)
      enddo

      do m=1,ibytes
      swork(k-ibytes+m)=iby8(m)
      enddo

      enddo


c

      return
      end





c   
c
c     subroutine desvio
c
c
c     Mean and standard deviation
c
c
c     entry:
c
c        xvam  = sum of individual values
c        xvas  = sum of square of individual values
c
c     output:
c
c        xvam  = mean
c        xvas  = standard deviation about that mean
c

      subroutine desvio (nest,xvam,xvas)

      implicit real *8 (a-h,o-z)

c

      zero=0.d0
      dnove=99.999d0
      dneg=-1.d0

c

      if (nest.le.0) then
      xvam=zero
      xvas=dneg
      return
      endif


c

      exmed=xvam/nest

c

      if (nest.eq.1) then

      xvas=dneg

      return

      endif

c

      if (nest.eq.2) then


      xvas=dsqrt(dabs(2.d0*xvas-xvam*xvam))/2.d0

      xvam=exmed


      return

      endif

c

      raiz=xvas-2.d0*exmed*xvam+nest*exmed*exmed

      if (raiz.lt.0.d0) then

      xvas=dneg

      else

      xvas=dsqrt(raiz/(nest-1.d0))

      endif
c

      xvam=exmed

      return

      end








c
c
c
c     Subroutine magnitudes
c
c
c     Computes PSF-based or aperture-based object magnitudes, based on
c     the furnished volume (PSF) or flux (aperture) of objects.
c
c 
c
c      Last modification: M. Assafin 13/Nov/2016
c
c
c
c

      subroutine magnitudes (d99,idiobs,nest,percs,ncut,ior1,nval1,
     ?contag,ceu,ceu2,volum,idga1,cg1mgg,ug1mgg,g1smg,g1fmg,g1fmgs)


      implicit real *8 (a-h,o-z)


      dimension ior1(idiobs),nval1(idiobs),contag(idiobs),volum(idiobs),
     ?idga1(idiobs),cg1mgg(idiobs),ug1mgg(idiobs)


c
c     Magnitudes and error in the catalogue system
c

      g1fmg=d99
      g1fmgs=d99
      g1smg=d99

 
      do i=1,nest
      ug1mgg(i)=d99
      enddo

      
c

      zgromg=0.d0
      n=0

      do 10 i=1,nest
      if (idga1(i).eq.0) go to 10
      if (volum(i).le.0.d0) go to 10
      j=idga1(i)
      if (cg1mgg(j).gt.24.0d0) go to 10
      n=n+1
      zgromg=zgromg+cg1mgg(j)+2.5d0*dlog10(volum(i))
 10   continue


      if (n.ge.1) then
      zgromg=zgromg/n
      else
      go to 20
      endif


      n=0

      do 15 i=1,nest

      if (volum(i).le.0.d0) go to 15

      ug1mgg(i)=zgromg-2.5d0*dlog10(volum(i))

      if (idga1(i).eq.0) go to 15
      j=idga1(i)
      if (cg1mgg(j).gt.24.0d0) go to 15
      n=n+1
      contag(n)=ug1mgg(i)-cg1mgg(j)
 15   continue


      if (n.ge.ncut) call quartl (idiobs,ior1,nval1,n,percs,contag)

      call avsdev (idiobs,n,contag,g1amg,g1smg)



c
c     Sky background magnitude in the catalogue system
c


      if (ceu.le.0.d0) then

      g1fmg=d99
      g1fmgs=d99

      else

      g1fmg=zgromg-2.5d0*dlog10(ceu)
      g1fmgs=zgromg-2.5d0*dlog10(ceu+ceu2)
      g1fmgs=g1fmg-g1fmgs

      endif


 20   continue

      return
      end









c
c
c     Subroutine mesure
c
c
c     Purpose
c
c
c     User furnishes regions for measurement of specific targets, for each
c     individual image.
c
c    
c
c     Comments
c
c
c     There are two types of targets. The two types are identified by
c     the type of reagion: circle or box.
c
c     In the case of circles, targets have stellar-like PSFs, and are fitted
c     by the circular Gaussian model. 
c
c     In the case of boxes, the targets are trace images, and are fitted
c     by the ERF PSF model.
c    
c
c     These user's regions have higher priority over the automatic object
c     search. This means that no automatic search is applied over the marked
c     regions. The fits are made directly on the pixels inside the furnished
c     regions.
c
c     The format of the regions follow the ds9 package standards. The user
c     should use the cursor facilities of ds9 to produce the regions. 
c
c
c     Last update: M. Assafin   15 Feb 2015
c
c




      subroutine mesure (idiobs,imes,nmcir,xcir,ycir,lacir,bcgc,bcgsc,
     ?nmtra,xtra,ytra,xlatra,ylatra,angtra,bcgtr,bcgstr)

      implicit real*8 (a-h,o-z)
      parameter(idim=150)

      dimension xcir(idiobs),ycir(idiobs),lacir(idiobs),bcgc(idiobs),
     ?bcgsc(idiobs)

      dimension xtra(idiobs),ytra(idiobs),xlatra(idiobs),ylatra(idiobs),
     ?angtra(idiobs),bcgtr(idiobs),bcgstr(idiobs)


      character*150 imes,linha

c

      open (25,file=imes)

c
c     Searches for targets
c

      nmcir=0
      nmtra=0

      do k=1,idiobs

      linha=''
      read (25,5,end=10) linha
 5    format(a150)

c
c     Searches for circle targets
c

      if (linha(1:6).eq.'circle') then
      nmcir=nmcir+1
      do i=1,idim
      if (linha(i:i).ne.'.') then
      icomp=ichar(linha(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) linha(i:i)=' '
      endif
      enddo
      read (linha,*) xcir(nmcir),ycir(nmcir),raio
      lacir(nmcir)=raio
      endif

c
c     Searches for trace-image targets
c

      if (linha(1:3).eq.'box') then
      nmtra=nmtra+1
      do i=1,idim
      if (linha(i:i).ne.'.') then
      icomp=ichar(linha(i:i))
      if (icomp.lt.48 .or. icomp.gt.57) linha(i:i)=' '
      endif
      enddo
      read (linha,*) xtra(nmtra),ytra(nmtra),dx,dy,ang
      xlatra(nmtra)=dx
      ylatra(nmtra)=dy
      angtra(nmtra)=ang
      endif


      enddo

c

 10   close (25)




      return

      end




c
c
c     Subroutine irot
c
c
c
c     Purpose
c
c
c     Given the internal coordinates (jj,ii) within a rectangle of internal
c     origin zero (0,0) centered at CCD coordinates (ixc,iyc) and rotated by
c     an angle ango in this CCD frame, irot converts the internal coordinates
c     (jj,ii) to the corresponding external CCD coordinates (k,m).
c
c     jj,ii - input internal rectangle coordinates referred to the external
c             rectangle center with CCD coordinates (ixc,iyc), so that in the
c             internal rectangle frame, the origin (jj=0,ii=0) corresponds to
c             (ixc,iyc) in the CCD frame 
c
c     k,m   - output rotated absolute coordinates referred to the external CCD
c             frame  (not referred to the center). They (k,m) coordinates are
c             the actual (x,y) coordinates of the pixel in the CCD frame.
c
c
c                                                               
c                                                       ^        
c        |---------------------------------|          m |     
c        |      .(jj,ii)                   |            |    
c     ii |              .(ixc,iyc)         |   -->      |       .(ixc,iyc)
c        |                                 |            |  
c        |---------------------------------|            |   .(k,m)
c                        jj                             |------------->
c                                                                    k   
c          rectangle associated to the                     CCD
c           trace-shaped object                            frame
c
c
c
c
c     Last update: M. Assafin   03 Nov 2016
c
c
c
      subroutine  irot (jj,ii,ixc,iyc,ango,nx,ny,k,m,iout)

      implicit real*8 (a-h,o-z)

c

      iout=0

c

      ang=-ango

c


      k= jj*dcos(ang)+ii*dsin(ang)
      m=-jj*dsin(ang)+ii*dcos(ang)

      k=k+ixc
      m=m+iyc

      if (k.lt.1) then
      k=1
      iout=1
      endif

      if (m.lt.1) then
      m=1
      iout=1
      endif

      if (k.gt.nx) then
      k=nx
      iout=1
      endif

      if (m.gt.ny) then
      m=ny
      iout=1
      endif

      return
      end






c
c     Subroutine trace 
c
c
c     Purpose
c
c
c     Fits a trace-shaped object with the Error Function PSF model, and finds
c     the (x,y) centroid, among the other parameters of the model, and their errors.
c
c
c     Comments
c
c     The 7 parameters of the ERF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace lenght (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   05 Sep 2015
c
c
c
c



      subroutine trace (idiobs,ipmax,icofsp,pixmat,imagem,fapitr,dlimit,
     ?plimit,nx,ny,xc,yc,rx,ry,ang,fc,sb,fotceu,lper,contag,ior,nval,
     ?icomax,altura,bx,by,sigma,tetha,dlenght,fundo,ex,ey,ierro,itx,ity)


      implicit real*8 (a-h,o-z)
      parameter(nterms=7)

      real*4 pixmat(ipmax,ipmax)

      real*4 pixel1(ipmax,ipmax),pixel2(ipmax,ipmax),pixel3(ipmax,ipmax)

      integer*2 betpix,imagem(ipmax,ipmax)


      dimension deltax(nterms),xsigma(nterms),param(nterms),
     ?contag(idiobs),ior(idiobs),nval(idiobs)


      character*50 subf


c
c     Initial data
c

      pi    = 0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

      perc=0.2d0


c
c     Parameter increment and lambda factor in Maquard non-linear LS method
c

      dinc=0.05d0
      xlamda=0.001

c
c     Convergence limits
c

      icontx=0

      xresid=-1.d14
      xcent=1.d14
      ycent=1.d14

      residx=0.d0


c
c     Trace center, sides and orientation angle
c


      ixc=xc
      iyc=yc
      irx=rx/2.d0
      iry=ry/2.d0

      rang=ang

c
c     Estimated initial values for the parameters
c


c
c     Sky background
c 


      param(7)=fc


c
c     (x, y) center
c


      param(2)=xc
      param(3)=yc



c
c     Tetha
c

      param(5)=rang



c
c     Amplitude at center (auxiliary parameters)
c
c     Not the height parameter A of the model
c



      kk=lper

c     if (kk.lt.5) kk=5

      n=0

      ii=0

      do 05 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 05

      if (imagem(j,i).lt.-19) go to 05

      n=n+1
      contag(n)=pixmat(j,i)


  05  continue



      call quartl (idiobs,ior,nval,n,perc,contag)


      call avsdev (idiobs,n,contag,h,aux)


      hh=h-fc

      h2=hh/2.d0



c
c     Sigma
c
c     Estimated from the FWHM along the smaller width of the trace at the trace
c     center
c
c


      i1=0
      i2=0


      do 11 ii=0,-iry,-1

      n=0

      do 10 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 10

      if (imagem(j,i).lt.-19) go to 10

      n=n+1
      contag(n)=pixmat(j,i)

  10  continue


      call quartl (idiobs,ior,nval,n,perc,contag)

      if (n.lt.1) go to 11

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      i1=-ii
      go to 12
      endif


  11  continue

c

  12  continue


      do 14 ii=0,iry

      n=0

      do 13 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 13

      if (imagem(j,i).lt.-19) go to 13

      n=n+1
      contag(n)=pixmat(j,i)

  13  continue


      call quartl (idiobs,ior,nval,n,perc,contag)

      if (n.lt.1) go to 14

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      i2=ii
      go to 15
      endif


  14  continue

c

  15  continue


      fwhms=i1+i2+1
      sigma=fwhms/(2.d0*1.177410023d0)

      param(4)=sigma





c
c     Lenght parameter d
c     
c     Estimated from the FWHM along the trace lenght
c
c


      j1=0
      j2=0

      ii=0

      do 30 jjj=0,-irx,-kk-1

      n=0

      do 20 jj=jjj,jjj-kk,-1

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 20

      if (imagem(j,i).lt.-19) go to 20

      n=n+1
      contag(n)=pixmat(j,i)

  20  continue


      call quartl (idiobs,ior,nval,n,perc,contag)

      if (n.lt.1) go to 30

      call avsdev (idiobs,n,contag,hhh,aux)


      if ((hhh-fc).lt.h2) then
      j1=-jjj+kk/2
      go to 35
      endif


 30   continue

c

 35   continue


      do 50 jjj=0,irx,kk+1

      n=0

      do 40 jj=jjj,jjj+kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 40

      if (imagem(j,i).lt.-19) go to 40

      n=n+1
      contag(n)=pixmat(j,i)

  40  continue


      call quartl (idiobs,ior,nval,n,perc,contag)

      if (n.lt.1) go to 50

      call avsdev (idiobs,n,contag,hhh,aux)



      if ((hhh-fc).lt.h2) then
      j2=jjj-kk/2
      go to 55
      endif


 50   continue

c

 55   continue

      fwhml=j2+j1+1

      param(6)=fwhml

c     param(6)=214
c     param(1)=3156
c     param(7)=100 



c
c     Height parameter of the model
c


      param(1)=1.d0


      n=0

      ii=0

      do 60 jj=-kk,kk

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 60

      if (imagem(j,i).lt.-19) go to 60

      n=n+1
      contag(n)=(pixmat(j,i)-param(7))/(track(nterms,j,i,param,ierro)-
     ?param(7))


  60  continue


      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,h,aux)


      param(1)=h




c

      write (*,*)

c     write (*,*) 'irx, iry (pixels) = ',irx, iry
c     write (*,*)

      write (*,*) 'Initial guess:'
      write (*,*) 'height (A) (ADUs) = ',param(1)
      write (*,*) 'xc (pixels)       = ',param(2)
      write (*,*) 'yc (pixels)       = ',param(3)
      write (*,*) 'sigma (pixels)    = ',param(4)
      write (*,*) 'theta (rad)       = ',param(5)
      write (*,*) 'lenght (d) pixels = ',param(6)
      write (*,*) 'sky counts (ADUs) = ',param(7)
      write (*,*)
c     stop



c
c     Initial increments for the parameters
c

c140  continue

      do i=1,nterms
      deltax(i)=dinc*param(i)
      enddo

c
c     Initializing error parameters            
c

      do i=1,nterms
      xsigma(i)=0.d0
      enddo

      
c
c     Trace fitting
c

c     go to 150


 140  call traco (ipmax,icofsp,pixmat,imagem,nx,ny,nterms,param,
     ?deltax,xsigma,xlamda,ixc,iyc,irx,iry,rang,residx,ierro)

c     write (*,*) 'ierro = ',ierro

      if (ierro.eq.1) then
      icontx = icontx + 1
      if (icontx.eq.icomax) go to 150
      xlamda=0.001
      go to 140
      endif      

c
c     Solution convergence
c

      residx = dsqrt(residx)
      centx  = param(2) 
      centy  = param(3) 
      conver = dabs(residx*dlimit)
      diferd = dabs(residx-xresid)
      difpox = dabs(centx-xcent)
      difpoy = dabs(centy-ycent)


c     if ((diferd.lt.conver).and.(difpox.lt.plimit).and.(difpoy.lt.
c    ?plimit)) go to 150

      xresid = residx
      xcent=centx
      ycent=centy
      icontx = icontx + 1
      if (icontx.eq.icomax) go to 150

      go to 140


 150  continue




c
c     Eliminates pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting, one by one, until no pixel is above the threshold
c

      ixc=centx
      iyc=centy
      rang=param(5)

      teto=fapitr*residx

      difmax=-1.d14

      n=0

      do 170 ii=-iry,iry
      do 160 jj=-irx,irx

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 160

      if (imagem(j,i).lt.-9) go to 160

      difcon=dabs(pixmat(j,i)-track(nterms,j,i,param,ierro))

      if (difcon.gt.teto) then

c     write (*,*) 'fapitr,x,y ',difcon/residx,centx,centy

      if (difcon.gt.difmax) then
      difmax=difcon
      n=n+1
      jxx=j
      iyy=i
      endif

      endif

 160  continue
 170  continue

    
      if (n.gt.0) then
      imagem(jxx,iyy)=-10
      icontx=1
      go to 140
      endif



c
c     Resets eliminated pixels with discrepant counts by a factor of the sigma
c     of the PSF fitting of the last iteration
c

 

      do 190 ii=-iry,iry
      do 180 jj=-irx,irx

      call irot (jj,ii,ixc,iyc,rang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 180

      if (imagem(j,i).eq.-10) imagem(j,i)=0

 180  continue
 190  continue



c
c     Storing parameters for the main program
c

      residx = residx
      altura = param(1)
      bx     = param(2) 
      by     = param(3) 
      sigma  = param(4)
      tetha  = param(5)
      dlenght= param(6) 
      fundo  = param(7)


      ex=residx*xsigma(2)
      ey=residx*xsigma(3)



      write (*,*)
      write (*,*) 'Fitted values:'
      write (*,*) 'height (A) (ADUs) = ',param(1)
      write (*,*) 'xc (pixels)       = ',param(2)
      write (*,*) 'yc (pixels)       = ',param(3)
      write (*,*) 'sigma (pixels)    = ',param(4)
      write (*,*) 'theta (rad)       = ',param(5)
      write (*,*) 'lenght (d) pixels = ',param(6)
      write (*,*) 'sky counts (ADUs) = ',param(7)
      write (*,*)
c     stop




c
c     Stores trace images in sub-frames in fits format for debug purposes
c

      if (ixc.ne.itx .and. iyc.ne.ity) return

c

      do i=1,ny
      do j=1,nx
      pixel1(j,i)=0
      pixel2(j,i)=0
      pixel3(j,i)=0
      enddo
      enddo

      ang=tetha

      if=83
      betpix=-32
      mswap=2
      scale=1.d0
      zero=0.d0


      irx=irx+100

      idimx=max0(irx,iry)
 
      idimx=2*idimx

      idimx=idimx+1


      idimy=idimx


      ixcc=idimx/2
      iycc=idimy/2


      do  ii=-iry,+iry
      do  jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      p=pixmat(j,i)

      tt=track(nterms,j,i,param,ierro)

      dd=pixmat(j,i)-tt

c     jjj=j-ixc+ixcc
c     iii=i-iyc+iycc


      call irot (jj,ii,ixcc,iycc,ang,idimx,idimy,j,i,iout)

      if (iout.eq.0) then
      pixel1(j,i)=p
      pixel2(j,i)=tt
      pixel3(j,i)=dd
      endif
 
      enddo
      enddo
 
c
c     Original sub-frame
c

      subf='t_original.fits'

      call wfits (ipmax,if,subf,betpix,mswap,idimx,idimy,pixel1,scale,
     ?zero)


c
c     ERF PSF Model sub-frame
c

      subf='t_traco.fits'


      call wfits (ipmax,if,subf,betpix,mswap,idimx,idimy,pixel2,scale,
     ?zero)


c
c     Original minus ERF PSF Model sub-frame
c

      subf='t_diferenca.fits'

      call wfits (ipmax,if,subf,betpix,mswap,idimx,idimy,pixel3,scale,
     ?zero)


c     stop

      return
      end





c
c     Subroutine traco
c
c
c     Purpose
c
c
c     The trace fit itself by the ERF PSF model, following the Marquardt
c     method of non-linear LS.
c
c
c
c     Comments
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c    Subroutines required;
c
c
c    track
c    tqquad
c    tderiv
c    matinv
c
c
c
c     Last update: M. Assafin   21 Jan 2013
c
c
c
c

      subroutine traco (ipmax,icofsp,pixmat,imagem,nx,ny,nterms,a,
     ?deltaa,sigmaa,flamda,ixc,iyc,irx,iry,ang,chisqr,ierro)


      implicit real *8 (a-h,o-z)

      dimension a(nterms),deltaa(nterms),sigmaa(nterms),b(nterms),
     ?alpha(nterms,nterms),beta(nterms),deriv(nterms),
     ?array(icofsp,icofsp)

      real*4 pixmat(ipmax,ipmax)

      integer*2 imagem(ipmax,ipmax)


c
c     Initial values
c

      crash=1.d-14

      ierro=0
      det=1.d0
      icont=0
      iconv=20
      jconv=0

c

c
c        Evaluate alpha and beta matrices
c

      do 134 j=1, nterms
      beta(j) = 0.d0
      do 134 k=1, j
  134 alpha(j,k) = 0.d0

      do 150  ii=-iry,+iry
      do 5001 jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,l,i,iout)

      if (imagem(l,i).lt.-19) go to 5001
      call tderiv (nterms, l, i, a, deltaa, deriv,ierro)
      if (ierro.eq.1) go to 107
      icont=icont+1

      do 146 j=1,nterms
      beta(j)=beta(j)+(pixmat(l,i)-track(nterms,l,i,a,ierro))*deriv(j)
      if (ierro.eq.1) go to 107
      do 146 k=1, j
  146 alpha(j,k) = alpha(j,k) + deriv(j)*deriv(k)

 5001 continue
  150 continue



c
c     Number of degrees of freedom
c

      free=icont-nterms

c

      if (free.le.0.d0) go to 107


c

      do 53 j=1,nterms
      do 53 k=1, j
   53 alpha(k,j)=alpha(j,k)


c
c        Evaluates chi square at starting point
c

      chisq1=tqquad(ipmax,nterms,pixmat,imagem,nx,ny,ixc,iyc,irx,iry,
     ?ang,free,a,ierro)


      if (ierro.eq.1) go to 107




c 				 
c         Invert modified curvature matrix to find the new parameters
c


 71   do 74 j=1, nterms
      do 73 k=1, nterms
      aux = alpha(j,j)*alpha(k,k)
      if (dabs(aux).lt.crash) go to 107
   73 array(j,k)= alpha(j,k) / dsqrt (aux)
   74 array(j,j) = 1.d0 + flamda
   80 call matinv (nterms, icofsp, array, det,ierro)
c

      if (ierro.eq.1) go to 107


C
      do 84 j=1, nterms
      b(j) = a(j)
      do 84 k=1, nterms
      aux = alpha(j,j)*alpha(k,k)
      if (dabs(aux).lt.crash) go to 107
   84 b(j) = b(j) + beta(k)*array(j,k)/dsqrt(aux)



c
c        If chi square increased, increase flamda and try again
c

      chisqr=tqquad(ipmax,nterms,pixmat,imagem,nx,ny,ixc,iyc,irx,iry,
     ?ang,free,b,ierro)


      if (ierro.eq.1) go to 107

c
c     Convergence to minimum is not being reached
c

      jconv=jconv+1

      if (jconv.gt.iconv) go to 107


c



      if (chisq1 - chisqr) 95, 101, 101

   95 flamda = 10.d0*flamda

      go to 71


c
c        Evaluate parameters and uncertainties
c

  101 do 104 j=1, nterms
      a(j) = b(j)

      if (dabs(alpha(j,j)).lt.crash) go to 107

      aux = array(j,j)/alpha(j,j)
  104 sigmaa(j) = dsqrt(aux)

      flamda = flamda/10.d0

      go to 110

  107 chisqr = -1.d0
      ierro=1

  110 continue



      return
      end


c
c
c     Subroutine tderiv
c
c
c     Evaluates derivatives of the ERF PSF model function for non-linear LS.
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   09 Dec 2012
c
c
c
c

      subroutine tderiv (nterms, j, i, a, deltaa, deriv, ierro)

      implicit real*8 (a-h,o-z)

      dimension a(nterms), deltaa(nterms), deriv(nterms)


c

      if (a(4).le.0.d0) then
      ierro=1
      return
      endif 

c

      pi=0.3141592653589793d1

      x=j
      y=i



c
c     Auxiliary terms
c

      s=dsin(a(5))
      c=dcos(a(5))

      z=(-(x-a(2))*s+(y-a(3))*c)/(a(4)*dsqrt(2.d0))
      z2=z*z
 

      w1=((x-a(2))*c+(y-a(3))*s+(a(6)/2.d0))/(a(4)*dsqrt(2.d0))
      w2=((x-a(2))*c+(y-a(3))*s-(a(6)/2.d0))/(a(4)*dsqrt(2.d0))

      w12=w1*w1
      w22=w2*w2

      dk=((x-a(2))*c+(y-a(3))*s)/(a(4)*dsqrt(2.d0))

      efw1=erf(w1)
      efw2=erf(w2)

      dew12=dexp(-w12)
      dew22=dexp(-w22)

      dez2=dexp(-z2)

      sqpi=dsqrt(pi)
      sqpi2=dsqrt(pi/2.d0)


c
c    The derivatives of the ERF PSF model
c


      deriv(1)=sqpi2*a(4)*dez2*(efw1-efw2)

      deriv(2)=-a(1)*dez2*(sqpi*z*s*(efw1-efw2)+c*(dew12-dew22))

      deriv(3)=-a(1)*dez2*(-sqpi*z*c*(efw1-efw2)+s*(dew12-dew22))

      deriv(4)=a(1)*sqpi2*dez2*((1.d0+2.d0*z2)*(efw1-efw2)+(2.d0/sqpi)*
     ?(w1*dew12-w2*dew22))

      deriv(5)=a(1)*2.d0*sqpi2*a(4)*z*dez2*(dk*(efw1-efw2)+(2.d0/sqpi)*
     ?(dew12-dew22))

      deriv(6)=-0.5d0*a(1)*dez2*(dew12+dew22)

      deriv(7)=1.d0


      return

      end




c
c
c     Function  track
c
c
c     The ERF PSF model function for fitting trace-shaped images.
c
c
c     The 7 parameters of the ERF PSF model: 
c
c
c    1 - height (counts)
c    2 - x0 (pixels)
c    3 - y0 (pixels)
c    4 - sigma (related to seeing) in pixels
c    5 - tetha angle (in radians)
c    6 - trace width (pixels)
c    7 - background (counts)
c
c
c
c     Last update: M. Assafin   09 Dec 2012
c
c
c
c

      double precision function track (nterms,j,i,a,ierro)

      implicit real*8 (a-h,o-z)

      dimension a(nterms)


c

      if (a(1).le.0.d0) then
      ierro=1
      return
      endif 

      if (a(4).le.0.d0) then
      ierro=1
      return
      endif 

      if (a(6).lt.0.d0) then
      ierro=1
      return
      endif 





      ierro=0

c

      pi=0.3141592653589793d1

c



      x=j
      y=i



c
c     Auxiliary terms
c

      s=dsin(a(5))
      c=dcos(a(5))

      z=(-(x-a(2))*s+(y-a(3))*c)/(a(4)*dsqrt(2.d0))
      z2=z*z
      dez2=dexp(-z2)
 

      w1=((x-a(2))*c+(y-a(3))*s+(a(6)/2.d0))/(a(4)*dsqrt(2.d0))
      w2=((x-a(2))*c+(y-a(3))*s-(a(6)/2.d0))/(a(4)*dsqrt(2.d0))


      efw1=erf(w1)
      efw2=erf(w2)


      sqpi2=dsqrt(pi/2.d0)


      track=a(1)*sqpi2*a(4)*dez2*(efw1-efw2)+a(7)

c     write (*,*) 'j,i = ',j,i
c     write (*,*) 'z,w1,w2 = ',z,w1,w2
c     write (*,*) 'sqpi2,dez2,efw1,efw2 = ',sqpi2,dez2,efw1,efw2
c     write (*,*) 'track = ',track
c     stop




      return
      end






c
c
c     Function tqquad
c
c
c     Evaluate the reduced chi square for fit to the data
c
c
c
c     Last update: M. Assafin   21 Jan 2013
c
c
c
c

      double precision function tqquad (ipmax,nterms,pixmat,imagem,
     ?nx,ny,ixc,iyc,irx,iry,ang,free,a,ierro)

      implicit real*8 (a-h,o-z)

      real*4 pixmat(ipmax,ipmax)

      integer*2 imagem(ipmax,ipmax)

      dimension a(nterms)



c
      chisq = 0.d0
      tqquad=0.d0
c
      if (free.le.0.d0) then
      ierro=1
      return
      endif
c


      do 39 ii=-iry,+iry
      do 38 jj=-irx,+irx

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      if (imagem(j,i).lt.-19) go to 38
      chisq = chisq + (pixmat(j,i)-track(nterms,j,i,a,ierro))**2
      if (ierro.eq.1) return
 38   continue
 39   continue


c
c     Reduced chi square: divide by the number of degrees of freedom
c

      tqquad = chisq / free

      return
      end



c
c     Error function 
c


      double precision function erf(x)
      implicit real*8 (a-h,o-z)
c     real*8 erf,x
c     real*8 gammp
      if(x.lt.0.d0)then
        erf=-gammp(.5d0,x**2)
      else
        erf=gammp(.5d0,x**2)
      endif
      return
      end



      double precision function gammp(a,x)
      implicit real*8 (a-h,o-z)
c     real*8 a,gammp,x
c     real*8 gammcf,gamser,gln
c     if(x.lt.0.d0.or.a.le.0.d0) stop 'bad arguments in gammp'
      if(x.lt.a+1.d0)then
        call gser(gamser,a,x,gln)
        gammp=gamser
      else
        call gcf(gammcf,a,x,gln)
        gammp=1.d0-gammcf
      endif
      return
      end




      subroutine gcf(gammcf,a,x,gln)
      implicit real*8 (a-h,o-z)
c     integer*4 ITMAX
c     real*8 a,gammcf,gln,x,EPS,FPMIN
      parameter (ITMAX=100,EPS=3.d-7,FPMIN=1.d-30)
c     integer*4 i
c     real*8 an,b,c,d,del,h,gammln
      gln=gammln(a)
      b=x+1.d0-a
      c=1.d0/FPMIN
      d=1.d0/b
      h=d
      do 11 i=1,ITMAX
        an=-i*(i-a)
        b=b+2.d0
        d=an*d+b
        if(dabs(d).lt.FPMIN)d=FPMIN
        c=b+an/c
        if(dabs(c).lt.FPMIN)c=FPMIN
        d=1.d0/d
        del=d*c
        h=h*del
        if(dabs(del-1.d0).lt.EPS)goto 1
11    continue
c     stop 'a too large, ITMAX too small in gcf'
1     gammcf=dexp(-x+a*dlog(x)-gln)*h
      return
      end





      subroutine gser(gamser,a,x,gln)
      implicit real*8 (a-h,o-z)
c     integer*4 ITMAX
c     real*8 a,gamser,gln,x,EPS
      parameter (ITMAX=100,EPS=3.d-7)
c     integer*4 n
c     real*8 ap,del,sum,gammln
      gln=gammln(a)
      if(x.le.0.d0)then
        if(x.lt.0.d0)stop 'x < 0 in gser'
        gamser=0.d0
        return
      endif
      ap=a
      sum=1.d0/a
      del=sum
      do 11 n=1,ITMAX
        ap=ap+1.d0
        del=del*x/ap
        sum=sum+del
        if(dabs(del).lt.dabs(sum)*EPS)goto 1
11    continue
c     stop 'a too large, ITMAX too small in gser'
1     gamser=sum*dexp(-x+a*dlog(x)-gln)
      return
      end



      double precision function gammln(xx)
      implicit real*8 (a-h,o-z)
c     real*8 gammln,xx
c     integer*4 j
c     double precision ser,stp,tmp,x,y,cof(6)
      dimension cof(6)
      SAVE cof,stp
      data cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+dlog(stp*ser/x)
      return
      end





c
c
c     Subroutine ident 
c
c
c     Identify candidate objects in the FOV above the object-local sky background.
c
c
c     Identifies candidate rounded-shaped objects in the FOV.
c
c     If candidate trace-shaped objects are present, all the trace or parts of it are
c     also detected. 
c
c     If blended objects are present, they are also detected (not functional in this
c     vesion).
c
c     The candidate source regions are analysed in terms of the associated
c     excentricity and orientation angle. The results are stored for posterior
c     usage.
c
c     The excentricity indicates if the candidate source region is a single
c     rounded shaped object, blended sources, or part (or all) of a trace
c     shaped object.
c
c
c
c     Last modified: 10 Oct 2014
c
c
c


      subroutine ident (ipmax,idiobs,icofsp,coefx,xest,yest,xp,pixmat,
     ?imagem,nx,ny,contag,fatceu,lper,iflag,ior,nval,xid,yid,idx1,idx2,
     ?idy1,idy2,idlado,npix,bcg,bcgs,dan,exce,nest)

      implicit real *8 (a-h,o-z)

      integer*2 imagem(ipmax,ipmax)
      real*4 pixmat(ipmax,ipmax)

      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),bcg(idiobs),
     ?bcgs(idiobs),dan(idiobs),exce(idiobs)

      dimension iflag(idiobs),contag(idiobs),vcount(idiobs),ior(idiobs),
     ?nval(idiobs)

      dimension coefx(icofsp),xest(idiobs),yest(idiobs),xp(idiobs)

      character*30 nov

      exc(a,b,c,d,e,f,n)=dsqrt((2.d0*dsqrt((a-c)**2+b**2))/(n*(a+c)+
     ?dsqrt((a-c)**2+b**2)))

      det33(a11,a12,a13,a21,a22,a23,a31,a32,a33)=a11*((a22*a33)-(a23*a32
     ?))-a12*((a21*a33)-(a23*a31))+a13*((a21*a32)-(a22*a31))




c
c     Initial values
c

      pi=3.141592653589793D0
      grarad=pi/180.d0
      radgra=180.d0/pi

      perc=0.20d0

      pcort=0.7d0

c

      do i=1,idiobs
      iflag(i)=0
      enddo

c


      iob=0
      nstar=0


c
c     Search loop of entire pixel matrix
c

      do 50 i=1,ny
      do 45 j=1,nx

      if (imagem(j,i).lt.0) go to 45

      sb=-1.d14
      ilax=0
      ilay=0
      cmax=-1.d14
      jm=j
      im=i

      value=-1.d14
      kx=jm
      ky=im

c
c     Square spiral search around a candidate source
c



 20   ilax=ilax+lper
      ilay=ilay+lper

      
 21   ix1=jm-ilax
      ix2=jm+ilax
      iy1=im-ilay
      iy2=im+ilay


      if (ix1.lt.1)  ix1=1
      if (iy1.lt.1)  iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny




c
c     Square spiral search: center in the brightest pixel and
c     expand the perimeter
c




      do    ii=iy1,iy2
      do 22 jj=ix1,ix2

      if (imagem(jj,ii).lt.0) go to 22

      if (pixmat(jj,ii).gt.value) then
      value=pixmat(jj,ii)
      kx=jj
      ky=ii
      endif

 22   continue
      enddo


      if (kx.ne.jm .or. ky.ne.im) then
      jm=kx
      im=ky
      go to 21
      endif 


c
c     Circular spiral search: pins down center, expand the perimeter until
c     local sky background is reached
c


c
c     Sets the counts of the inner ring perimeter of width lper
c

c     write (*,*) '1'


c     write (*,*) 'j,i,jm,im,ilax,ilay,lper= ',j,i,jm,im,ilax,ilay,lper


      call sperim (ipmax,idiobs,nx,ny,jm,im,ilax,ilay,lper,pixmat,
     ?imagem,contag,n,jx1,jx2,jy1,jy2)


      if (n.eq.0) then

      do ii=jy1,jy2
      do jj=jx1,jx2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif


c     write (*,*) '2'


c
c     Eliminates possible outlier counts (lower and higher counts)
c

c     write (*,*) '3'

c     write (*,*) 'j,i,jm,im,n = ',j,i,jm,im,n



      call quartl (idiobs,ior,nval,n,perc,contag)

      if (n.eq.0) then

      do ii=jy1,jy2
      do jj=jx1,jx2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif

c     write (*,*) '4'


c
c     Computes the typical count value of the inner perimeter by 2/4 inner quartyle
c     statistics of counts of the perimeter; computes average
c     and standard deviation values.
c


c     write (*,*) '5'


      call  avsdev (idiobs,n,contag,xm1,xs1)

c     write (*,*) '6'


c
c     Sets the counts of the outer perimeter of the box with lper
c     width
c



      jlax=ilax+lper
      jlay=ilay+lper

c     write (*,*) '7'


      call sperim (ipmax,idiobs,nx,ny,jm,im,jlax,jlay,lper,pixmat,
     ?imagem,contag,n,jx1,jx2,jy1,jy2)


      if (n.eq.0) then

      do ii=jy1,jy2
      do jj=jx1,jx2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif

c     write (*,*) '8'


c
c     Eliminates possible outlier counts (lower and higher counts)
c

c     write (*,*) '9'

      call quartl (idiobs,ior,nval,n,perc,contag)


      if (n.eq.0) then

      do ii=jy1,jy2
      do jj=jx1,jx2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif

c     write (*,*) '10'

c
c     Computes the typical count value of the outer perimeter by the
c     2/4 inner quartyle statistics of counts of the extended perimeter;
c     computes average and standard deviation values.
c

c     write (*,*) '11'

      call avsdev (idiobs,n,contag,xm2,xs2)


c     write (*,*) '12'


c
c     Did the counts of the perimeters converge within a factor fatceu of
c     the sigma of the sky background?
c
c     Yes. The perimeter reached the sky background. Stop the square
c     spiral search.
c
c     No. Keep expanding the box area and searching for a brighter pixel
c
c
        



      sb=dabs(xm2-xm1)

      sg3=fatceu*xs2

      if (sb.gt.sg3) go to 20


c
c     Computes candidate source counts
c


      n=0


      jx1=jm-lper
      jx2=jm+lper
      jy1=im-lper
      jy2=im+lper


      if (jx1.lt.1)  jx1=1
      if (jy1.lt.1)  jy1=1
      if (jx2.gt.nx) jx2=nx
      if (jy2.gt.ny) jy2=ny


      iraio2=lper**2

      do    ii=jy1,jy2
      do 24 jj=jx1,jx2

      if (imagem(jj,ii).lt.-19) go to 24

      ira=(jj-jm)**2+(ii-im)**2
      if (ira.gt.iraio2) go to 24

      n=n+1
      contag(n)=pixmat(jj,ii)

 24   continue
      enddo


      if (n.eq.0) then

      do ii=iy1,iy2
      do jj=ix1,ix2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif



c
c     Eliminates possible outlier counts (lower and higher counts)
c

c     write (*,*) '13'

      call quartl (idiobs,ior,nval,n,perc,contag)

c     write (*,*) '14'


      ncort=pcort*n
      cort=xm2+fatceu*xs2

      mcon=0
      do ii=1,n
      if (contag(ii).gt.cort) mcon=mcon+1
      enddo

c
c     Within the circular region with n valid non-outlier pixels, is
c     the number of pixels above the theshold sky background greater
c     than a percentage pcort of the n pixels?
c
c     If yes, we have a true candidate source.
c
c     If not, mark the not useful pixels and go back to search another
c     candidates.
c


      if (mcon.lt.ncort) then
 
      do ii=iy1,iy2
      do jj=ix1,ix2
      imagem(jj,ii)=-2
      enddo
      enddo
 
      go to 45
      endif


      
c
c     A candidate source was found.
c
c     Refines the object center, by computing the baricenter
c     until (xc, yc) converges within a pixel
c


      cortq=contag(n)


      iraio2=ilax**2+ilay**2


 25   xc=0.d0
      yc=0.d0
      cont=0.d0
      np=0

      do    ii=iy1,iy2
      do 27 jj=ix1,ix2

      if (imagem(jj,ii).lt.-19) go to 27

      ira=(jj-jm)**2+(ii-im)**2
      if (ira.gt.iraio2) go to 27


      if (pixmat(jj,ii).lt.cort) go to 27
      
      xc=xc+jj*pixmat(jj,ii)
      yc=yc+ii*pixmat(jj,ii)
      cont=cont+pixmat(jj,ii)
      np=np+1

 27   continue
      enddo
 
      if (cont.gt.0.d0) then
      xc=xc/cont
      yc=yc/cont
      else
      xc=jm
      yc=im
      endif


      kx=xc
      ky=yc


      if (kx.ne.jm .or. ky.ne.im) then

      jm=kx
      im=ky


      ix1=jm-ilax
      ix2=jm+ilax
      iy1=im-ilay
      iy2=im+ilay


      if (ix1.lt.1)  ix1=1
      if (iy1.lt.1)  iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny

      go to 25

      endif 



c
c     Marks the candidate source pixels
c


      ilax=ilax+lper
      ilay=ilay+lper
 
      iraio2=ilax**2+ilay**2

      rai2=iraio2
      lado=dsqrt(rai2)

      iob=iob+1


      ix1=jm-ilax
      ix2=jm+ilax
      iy1=im-ilay
      iy2=im+ilay
 
      if (ix1.lt.1)  ix1=1
      if (iy1.lt.1)  iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny



      npixel=0

      do    ii=iy1,iy2
      do 30 jj=ix1,ix2

      if (imagem(jj,ii).lt.-19) go to 30

      ira=(jj-jm)**2+(ii-im)**2
      if (ira.gt.iraio2) go to 30

      imagem(jj,ii)=-2

      npixel=npixel+1


 30   continue
      enddo



c
c     Store candidate source data
c


      nstar=nstar+1

      xid(nstar)=xc
      yid(nstar)=yc
      idlado(nstar)=lado

      idx1(nstar)=ix1
      idx2(nstar)=ix2
      idy1(nstar)=iy1
      idy2(nstar)=iy2

      npix(nstar)=npixel

      vcount(nstar)=np

      bcg(nstar)=xm2
      bcgs(nstar)=xs2

 
c

c     write (*,*) 'PASSOU j i = ',j,i


 45   continue


 50   continue




c
c     Eliminates multiple candidate object regions when the centers of one
c     or more regions falls inside another region.
c
c     The region with the highest counts is preserved.
c

c     write (*,*) '15'



      n=0


      do 53 ii=1,nstar

      do 52 jj=1,nstar

      if (ii.eq.jj) go to 52

      d=dsqrt((xid(jj)-xid(ii))**2+(yid(jj)-yid(ii))**2)

      if (d.gt.idlado(jj).and.d.gt.idlado(ii)) go to 52

c
c     The center of a region falls inside the current region or
c     vice-versa.
c     Stores common region counts. 
c



      n=n+1
      ior(n)=jj
c     nval(n)=100000*vcount(jj)
c     nval(n)=npix(jj)
      nval(n)=vcount(jj)


 52   continue

      if (n.eq.0) go to 53


      n=n+1
      ior(n)=ii
c     nval(n)=10000*vcount(ii)
c     nval(n)=npix(ii)
      nval(n)=vcount(ii)


c
c     Orders common regions from low to high counts
c

      call ordem (idiobs,n,ior,nval)

c
c     Flags all the fainter common regions for elimination.
c     Only the region with the highest counts is preserved
c


      do j=1,n-1
      i=ior(j)
      iflag(i)=1
      enddo

      n=0

 53   continue




c     write (*,*) '16'



      nest=0

      do 60 k=1,nstar

      if (iflag(k).ne.0) go to 60

      nest=nest+1

      xid(nest)=xid(k)
      yid(nest)=yid(k)
      idlado(nest)=idlado(k)

      idx1(nest)=idx1(k)
      idx2(nest)=idx2(k)
      idy1(nest)=idy1(k)
      idy2(nest)=idy2(k)

      npix(nest)=npix(k)

      bcg(nest)=bcg(k)
      bcgs(nest)=bcgs(k)

      xlado=idlado(nest)
      write (77,55) xid(nest),yid(nest),xlado
 55   format('circle(',2(f8.2,','),f8.2,')')



 60   continue



c
c     What is the nature of the candidate object?
c
c     To answer that, estimate the excentricity associated to the
c     count distribution of the source.
c
c     To do that:
c
c
c     1) Fits an unweighted general 3D cartesian conic function:
c
c      ADU counts = matrix(x,y) = Ax^2+Bxy+Cy^2+Dx+Ey+F
c
c
c     2) The excentricity e is given by:
c
c
c      e = dsqrt ( 2*dsqrt((A-C)^2+B^2) / ( n*(A+C) + dsqrt((A-C)^2+B^2) ) )
c
c      with n=+/-1 for a negative/positive determinant for the 3x3 matrix:
c
c
c                  |    A   B/2  D/2  |
c                  |   B/2   C   E/2  |
c                  |   D/2  E/2   F   |
c
c
c      3)  There are 3 possibilities:
c
c          a) if the excentricity e is smaller than e1, it is a single rounded
c             shaped object.
c
c          b) if the excentricity e is between  e1 <  e  < e2, it is possible that
c             we have two nearby rounded sources together. A specific procedure
c             must be called to measure the two blended (but rounded) objects. 
c
c          c) if the excentricity e is higher than e2, a trace-shaped object, or
c             part of the trace, is likely to have been detected. A specific
c             procedure must be called to finish identifying the rectangular
c             region around the trace shaped object.
c
c
c      For posterior analysis, the excentricity and the orientation angle are stored.
c
c
c      The angle theta is computed from the 3D conic fitting by using:
c
c
c                     theta = 0.5 arctg [B/(A-C)]
c
c
c





      do 80 k=1,nest



      iraio2=idlado(k)**2

      pcort=bcg(k)+fatceu*bcgs(k)

      n=0


      do    i=idy1(k),idy2(k)
      do 70 j=idx1(k),idx2(k)

      if (imagem(j,i).lt.-19) go to 70

      ira=(j-xid(k))**2+(i-yid(k))**2
      if (ira.gt.iraio2) go to 70

      if (pixmat(j,i).lt.pcort) go to 70

      n=n+1

      xest(n)=j
      yest(n)=i

c     contag(n)=pixmat(j,i)

c     xp(n)=1.d0

      contag(n)=1.d0

      xp(n)=pixmat(j,i)


 70   continue
      enddo


c
c     Fits the 3D unweighted conic
c

      ngrau=2

      call isolo (idiobs,icofsp,ngrau,n,xest,yest,xp,contag,coefx,
     ?ierro)


      f=coefx(1)
      d=coefx(2)
      e=coefx(3)
      a=coefx(4)
      b=coefx(5)
      c=coefx(6)


c
c     Computes the excentricity
c


      a11=a
      a12=b/2.d0
      a13=d/2.d0

      a22=c
      a23=e/2.d0
      a33=f

      a21=a12
      a31=a13
      a32=a23


      det=det33(a11,a12,a13,a21,a22,a23,a31,a32,a33)

      if (det.lt.0.d0) then

      neta=1

      else

      neta=-1

      endif

      ex=exc(a,b,c,d,e,f,neta)

      nov=''
      write(nov,*) ex
      do mm=1,28
      if (nov(mm:mm+2).eq.'NaN') ex=0.d0
      enddo



c
c     Computes the angle
c
c



      sn=b
      cs=a-c


      ayy=dabs(sn)
      axx=dabs(cs)

      if (axx.gt.1.d-14) then


      aux=dabs(datan2(ayy,axx))


      if (cs.ge.0.d0) then
       if (sn.ge.0.d0) then
        tet=aux
       else
        tet=2.d0*pi-aux
       endif
      else
       if (sn.ge.0.d0) then
        tet=pi-aux
       else
        tet=pi+aux
       endif
      endif

      else

      if (sn.ge.0.d0) then
      tet=pi/2.d0
      else
      tet=pi+pi/2.d0
      endif

      endif


      tet=0.5d0*tet



c

      exce(k)=ex
      dan(k)=tet

      write (88,*) 'xc,yc,exc,ang,idlado = ',xid(k),yid(k),ex,
     ?tet*radgra,idlado(k)

 80   continue


c


      return

      end




c
c     Subroutine avsdev
c
c
c     Computes the average and standard deviation of the furnished data
c
c    
c     Comments
c
c
c     contag = data points
c     xm     = average of data points
c     xs     = standard deviation of the data points
c
c
c     Last modified: 27 Sep 2014
c
c
c


      subroutine avsdev (idiobs,n,contag,xm,xs)

      implicit real *8 (a-h,o-z)


      dimension contag(idiobs)


c
c     The simple statistics
c

      
      xm=0.d0
      xs=0.d0
      

      do i=1,n
      xm=xm+contag(i)
      xs=xs+contag(i)**2
      enddo

      call desvio (n,xm,xs)

      if (n.eq.1) xs=0.d0
 

      return
      end




c
c     Subroutine sperim
c
c
c     Loads the counts at a given ring perimeter of width lper. 
c
c     The ring is centered at the furnished (x,y) coordindates
c     with inner radius defined by ilax & ilay.
c
c     
c     Comments
c
c
c     jx1,jx2,jy1,jy2 = coordinates of the square box corners containing
c                       the ring
c     ilax, ilay      = inner ring radius   
c     lper            = width of the ring perimeter
c     pixmat          = pixel matrix
c     imagem          = auxiliary pixel matrix with marked pixels for elimination
c     contag          = the output; counts inside the ring perimeter
c     n               = number of points inside the ring perimeter
c
c
c     Last modified: 27 Sep 2014
c
c
c



      subroutine sperim (ipmax,idiobs,nx,ny,jm,im,ilax,ilay,lper,pixmat,
     ?imagem,contag,n,jx1,jx2,jy1,jy2)

      implicit real *8 (a-h,o-z)

      integer*2 imagem(ipmax,ipmax)
      real*4 pixmat(ipmax,ipmax),mazi


      dimension contag(idiobs)

c

      n=0

      nlimo=idiobs

c

      irmin2=ilax**2+ilay**2
      irmax2=(ilax+lper)**2+(ilay+lper)**2


c
c     Sets each valid perimeter
c

      ix1=jm-ilax-lper
      ix2=jm+ilax+lper
      iy1=im-ilay-lper
      iy2=im+ilay+lper


      if (ix1.lt.1)  ix1=1
      if (iy1.lt.1)  iy1=1
      if (ix2.gt.nx) ix2=nx
      if (iy2.gt.ny) iy2=ny


c
c     Picks up counts within the ring of radius sqrt(ilax**2+ilay**2) and
c     width lper
c
      

      do    ii=iy1,iy2
      do 10 jj=ix1,ix2

      if (imagem(jj,ii).lt.-19) go to 10

      ir2=(jj-jm)**2+(ii-im)**2
      if (ir2.lt.irmin2) go to 10
      if (ir2.gt.irmax2) go to 10

      n=n+1
      contag(n)=pixmat(jj,ii)
      if (n.eq.nlimo) go to 50

 10   continue
      enddo

c

 50   continue

c

      jx1=ix1
      jx2=ix2
      jy1=iy1
      jy2=iy2


      return
      end




c
c     Subroutine quartl
c
c
c     Eliminates possible outlier counts (lower and higher counts)
c
c     
c     Comments
c
c
c     contag = the input/output of counts
c     n      = the input/output number of counts
c
c
c     Last modified: 17 Oct 2014
c
c
c



      subroutine quartl (idiobs,ior,nval,n,perc,contag)


      implicit real *8 (a-h,o-z)

      dimension contag(idiobs),contbo(idiobs),ior(idiobs),nval(idiobs)


c
c     Stores data
c

      if (n.eq.1) return

      do i=1,n
      contbo(i)=contag(i)
      enddo

c
c     Prepares data
c

      do i=1,n
      ior(i)=i
      nval(i)=10000*dabs(contbo(i))
      enddo

c
c     Orders data from low to high counts
c

      call ordem (idiobs,n,ior,nval)


c
c     Sets lower and higher count limits
c

      n1=perc*n
      n2=n-n1

      if (n1.lt.1) n1=1
      if (n2.lt.1) n2=1



c
c     Fills in selected counts
c


      m=0

      do i=n1,n2
      k=ior(i)
      m=m+1
      contag(m)=contbo(k)
      enddo

      n=m

      return

      end




c
c     Subroutine subtss
c
c
c     Gets the pixel counts of the right/letf/top/botom section of the current
c     rectangle of the current trace-shaped structure
c
c
c
c     Last modified: M. Assafin, 16 Oct 2014
c
c


      subroutine subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,
     ?j1,j2,i1,i2,n,contag)


      implicit real *8 (a-h,o-z)


      integer*2 imagem(ipmax,ipmax)
      real*4 pixmat(ipmax,ipmax),mazi


      dimension contag(idiobs)




c
c     Left/right/top/botom counts of ideal rectangle
c



      n=0

      do    ii=i1,i2
      do 20 jj=j1,j2


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)

      if (iout.ne.0) go to 20

      if (imagem(j,i).lt.-19) go to 20

      n=n+1
      contag(n)=pixmat(j,i)

 20   continue
      enddo


      return

      end

  





c
c
c     Subroutine idtrack
c
c
c     Identifies trace-shaped objects.
c
c     Input comes from candidates detected by the ident subroutine. The candidate
c     objects are filtered and those with potential to be parts of, or a trace,
c     are dropped from the initial list (rounded-shaped objects) and processed
c     for the identification of the trace.  
c
c
c
c     Last modified: M. Assafin, 16 Nov 2014
c
c


      subroutine idtrack (ipmax,idiobs,icofsp,coefx,xest,yest,xp,pixmat,
     ?imagem,nx,ny,contag,fatceu,fotceu,lper,iflag,ior,nval,xid,yid,
     ?idx1,idx2,idy1,idy2,idlado,npix,bcg,bcgs,dan,exce,nstar,elimt,
     ?xctra,yctra,dmalat,dmilat,agtra,bcgt,bcgst,ntrac)



      implicit real *8 (a-h,o-z)

      integer*2 imagem(ipmax,ipmax)
      real*4 pixmat(ipmax,ipmax)

      dimension xid(idiobs),yid(idiobs),idlado(idiobs),idx1(idiobs),
     ?idx2(idiobs),idy1(idiobs),idy2(idiobs),npix(idiobs),bcg(idiobs),
     ?bcgs(idiobs),dan(idiobs),exce(idiobs),iflag(idiobs)

      dimension xctra(idiobs),yctra(idiobs),dmalat(idiobs),
     ?dmilat(idiobs),agtra(idiobs),bcgt(idiobs),bcgst(idiobs),
     ?npixt(idiobs),kflag(idiobs)

      dimension coefx(icofsp),xest(idiobs),yest(idiobs),xp(idiobs),
     ?contag(idiobs),ior(idiobs),nval(idiobs),iora(idiobs)

      character*30 nov

      exc(a,b,c,d,e,f,n)=dsqrt((2.d0*dsqrt((a-c)**2+b**2))/(n*(a+c)+
     ?dsqrt((a-c)**2+b**2)))

      det33(a11,a12,a13,a21,a22,a23,a31,a32,a33)=a11*((a22*a33)-(a23*a32
     ?))-a12*((a21*a33)-(a23*a31))+a13*((a21*a32)-(a22*a31))




c
c     Initial values
c

      pi=3.141592653589793D0
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      perc=0.2d0

      dong=0.10d0

      loops=500

c

      do i=1,idiobs
      iflag(i)=0
      kflag(i)=0
      enddo

c
c     Orders the regions by the number of pixels above the sky background theshold
c     (priority is to process regions of brighter objects first) 
c


      do k=1,nstar
      iora(k)=k
      nval(k)=npix(k)
      enddo



      call ordem (idiobs,nstar,iora,nval)
       


c
c     Loops the rounded-shaped candidate object regions
c

      ntrac=0



      write (66,274) 
 274  format('# Filename: traco')

      write (66,276)
 276  format('global color=green dashlist=8 3 width=1 font="helvetica 10
     ? normal roman" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 de
     ?lete=1 include=1 source=1')

      write (66,277)
 277  format('image')


      write (666,274)
      write (666,276)
      write (666,277)



c


      do 40 lll=nstar,1,-1

      kkk=iora(lll)

c


      if (exce(kkk).lt.elimt) go to 40

      if (iflag(kkk).ne.0) go to 40

c
c
c     A part or all of a trace-shaped object, or trace-shaped structure,
c     or tss for simplicity, was found.
c
c     Now it comes the procedure to find the (rotated) rectangle which contains
c     the entire trace-shaped object. This is made in an interactive procedure.
c
c     First, using the center and rotation angle from the input list, the tss is
c     expanded along each semi-width by lper steps until the sky background is
c     reached on each side (here, care is taken with possible nearby tss). 
c
c     Next, the trace lenght is searched in lper steps until it reaches the
c     sky background too. The rectangle is further improved by repeating the steps
c     above N times (limited to 500 loops) until convergence in (x,y) center,
c     rotation angle and sizes (width and length) are reached.
c     
c     The center, sides and rotation angle of the rectangle are stored, as well as
c     the sky background (average and sigma) and the number of pixels above the sky
c     background threshold.
c
c
c



 5    continue



c
c     Initial values from the input list
c

      npx=npix(kkk)

      ang=dan(kkk)


      xc=xid(kkk)
      yc=yid(kkk)

      ixc=xc
      iyc=yc


      xm=bcg(kkk)
      sb=bcgs(kkk)


      ftsb=xm+fotceu*sb




c
c     More initial values
c


      ipass=0

      dang=1.d14

      lax=-100
      lay=-100

      ixcen=-100
      iycen=-100
 
c
     

 7    continue



      ipass=ipass+1


      nle1=0
      nle2=0

      nwi1=0
      nwi2=0

      itouch=0


c
c     Width estimation of the rectangle
c


 10   continue


      j1=-lper/2.d0-0.2
      j2=+lper/2.d0+0.2

c     j1=-lper/2.d0+0.2
c     j2=+lper/2.d0-0.2




c
c     Rectangle counts at internal bottom
c

 12   continue

      i1=-lper/2.d0-lper*nwi1-0.2
      i2=+lper/2.d0-lper*nwi1+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the internal bottom of rectangle
c


      if (n.lt.2) go to 15

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,ym1i,ys1i)


c
c     Rectangle counts at external bottom of rectangle
c


      i1=-lper/2.d0-lper*(nwi1+1)-0.2
      i2=+lper/2.d0-lper*(nwi1+1)+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the external bottom of rectangle
c


      if (n.lt.2) go to 15

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,ym1e,ys1e)

c
c     Touched another nearby tss ?
c

      dif=ym1e-ym1i
      
      if (dif.gt.0.d0) then 
      itouch=1
      go to 15
      endif


c
c     Sky background reached at bottom of the rectangle?
c


c     ftys1e=fotceu*ys1e

c     dif=dabs(dif)

c     if (dif.lt.ftys1e) go to 15

c     if (dif.lt.ftsb) go to 15


      if (ym1i.le.ftsb) go to 15


      nwi1=nwi1+1

      go to 12



c
c     Rectangle counts at internal top
c

 15   continue


      i1=-lper/2.d0+lper*nwi2-0.2
      i2=+lper/2.d0+lper*nwi2+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the internal top of rectangle
c


      if (n.lt.2) go to 17

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,ym2i,ys2i)


c
c     Rectangle counts at external top of rectangle
c


      i1=-lper/2.d0+lper*(nwi2+1)-0.2
      i2=+lper/2.d0+lper*(nwi2+1)+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the external top of rectangle
c


      if (n.lt.2) go to 17

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,ym2e,ys2e)


c
c     Touched another nearby tss ?
c

      dif=ym2e-ym2i

      if (dif.gt.0.d0) then 
      itouch=1
      go to 17
      endif

c
c     Sky background reached at top of the rectangle?
c


c     ftys2e=fotceu*ys2e

c     dif=dabs(ym2e-ym2i)

c     if (dif.lt.ftys2e) go to 17

c     if (dif.lt.ftsb) go to 17

      if (ym2i.le.ftsb) go to 17



      nwi2=nwi2+1

      go to 15


c
c     Local sky background threshold from the more trusty width counts statistics
c
c
c     Picks up the smaller of the semi-widths to avoid contamination by
c     other nearby tss
c



 17   continue



c     if (ym1e.le.ym2e) then
c     xm=ym1e
c     sb=ys1e
c     else
c     xm=ym2e
c     sb=ys2e
c     endif
c
c     ftsb=xm+fotceu*sb
c
c     nwi=min0(nwi1,nwi2)



c     if (ym1e.ge.ym2e) then
c     xm=ym1e
c     sb=ys1e
c     else
c     xm=ym2e
c     sb=ys2e
c     endif
c
c     nwi=max0(nwi1,nwi2)


      if (itouch.ne.0) then
c     write (*,*)'itouch 1, xc, yc = ',ixc,iyc
      endif

      if (itouch.ne.0) then

      nwi=min0(nwi1,nwi2)

      else

      nwi=max0(nwi1,nwi2)

      endif

c     nwi=max0(nwi1,nwi2)


c
c     Length estimation of the rectangle
c




 20   continue




      i1=-lper/2.d0-0.2
      i2=+lper/2.d0+0.2


c     i1=-lper/2.d0+0.2
c     i2=+lper/2.d0-0.2



c
c     Rectangle counts at left of the rectangle
c


 22   continue

      j1=-lper/2.d0-lper*nle1-0.2
      j2=+lper/2.d0-lper*nle1+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the left of rectangle
c


      if (n.lt.2) go to 25

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,xm1,xs1)



c
c     Sky background reached at left of the rectangle?
c



      if (xm1.le.ftsb) go to 25




      nle1=nle1+1

      go to 22



c
c     Rectangle counts at right of the rectangle
c

 25   continue


      j1=-lper/2.d0+lper*nle2-0.2
      j2=+lper/2.d0+lper*nle2+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the right of rectangle
c


      if (n.lt.2) go to 30

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,xm2,xs2)



c
c     Sky background reached at right of the rectangle?
c




      if (xm2.le.ftsb) go to 30



      nle2=nle2+1


      go to 25



c


 30   continue



c
c     Touched another nearby tss ?
c

      itouch=0


c
c     Rectangle counts at left of the rectangle's end
c


      j1=-lper/2.d0-lper*(nle1+1)-0.2
      j2=+lper/2.d0-lper*(nle1+1)+0.2


      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)


c
c     Statistics of the external left of rectangle's end
c


      if (n.lt.2) go to 31

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,xm1e,xs1e)


      dif=xm1e-xm1

      if (dif.gt.0.d0) then 
      itouch=1
      endif



c
c     Rectangle counts at right of the rectangle's end
c



 31   continue


      j1=-lper/2.d0+lper*(nle2+1)-0.2
      j2=+lper/2.d0+lper*(nle2+1)+0.2

      call subtss (ipmax,idiobs,pixmat,imagem,nx,ny,ixc,iyc,ang,j1,j2,
     ?i1,i2,n,contag)



c
c     Statistics of the external right of rectangle's end
c




      if (n.lt.2) go to 32

      call quartl (idiobs,ior,nval,n,perc,contag)

      call avsdev (idiobs,n,contag,xm2e,xs2e)


      dif=xm2e-xm2

      if (dif.gt.0.d0) then 
      itouch=1
      endif


c

 32   continue


      if (itouch.ne.0) then

      nle=min0(nle1,nle2)

      else

      nle=max0(nle1,nle2)

      endif

c



      j1=-lper*nle
      j2=+lper*nle


      i1=-lper*nwi 
      i2=+lper*nwi 


      ladx=j2-j1+1
      lady=i2-i1+1




c
c     Convergence reached? 
c


      deng=dabs(ang-dang)*radgra


      if (ladx.eq.lax.and.lady.eq.lay.and.ixc.eq.ixcen.and.iyc.eq.iycen.
     ?and.deng.le.dong)  go to 34





      if (ipass.eq.loops) go to 34

      lax=ladx
      lay=lady

      ixcen=ixc
      iycen=iyc

      dang=ang



c
c
c     Computes new center and new rotation angle using
c     the pixels within the current rectangle region.
c
c     The angle theta is computed from the fitting of an unweighted
c     3D cartesian conic function:
c
c
c      ADU counts = matrix(x,y) = Ax^2+Bxy+Cy^2+Dx+Ey+F
c
c
c     by using:
c
c
c                     theta = 0.5 arctg [B/(A-C)]
c
c
c
c     The center is computed by the baricenter of weighted pixels
c     above the sky background threshold.
c
c





      n=0
      cont=0.d0
      xc=0.d0
      yc=0.d0


c     write (*,*) 'lll,ftsb = ',lll,ftsb


      do    ii=i1,i2
      do 33 jj=j1,j2

      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 33

      if (imagem(j,i).lt.-19) go to 33

      if (pixmat(j,i).lt.ftsb) go to 33


      n=n+1

      xest(n)=j
      yest(n)=i

c     contag(n)=pixmat(j,i)

c     xp(n)=1.d0


      contag(n)=1.d0

      xp(n)=pixmat(j,i)

      xc=xc+j*pixmat(j,i)
      yc=yc+i*pixmat(j,i)
      cont=cont+pixmat(j,i)

 

 33   continue
      enddo

c     if (n.eq.0) write (*,*) 'n=0 lll,kkk = ',lll,kkk

      xc=xc/cont
      yc=yc/cont

      npx=n

      ixc=xc
      iyc=yc



c
c     Fits the 3D unweighted conic
c


      ngrau=2

      call isolo (idiobs,icofsp,ngrau,n,xest,yest,xp,contag,coefx,
     ?ierro)

      a=coefx(4)
      b=coefx(5)
      c=coefx(6)



c
c     Computes the angle
c
c


      sn=b
      cs=a-c


      ayy=dabs(sn)
      axx=dabs(cs)

      if (axx.gt.1.d-14) then


      aux=dabs(datan2(ayy,axx))


      if (cs.ge.0.d0) then
       if (sn.ge.0.d0) then
        tet=aux
       else
        tet=2.d0*pi-aux
       endif
      else
       if (sn.ge.0.d0) then
        tet=pi-aux
       else
        tet=pi+aux
       endif
      endif

      else

      if (sn.ge.0.d0) then
      tet=pi/2.d0
      else
      tet=pi+pi/2.d0
      endif

      endif


      ang=0.5d0*tet



c


      go to 7

c
 
   
 34   continue


c
c     Stores trace-shaped object rectangle data
c



      nov=''
      write(nov,*) xc
      do mm=1,28
      if (nov(mm:mm+2).eq.'NaN') then
      iflag(kkk)=1
      go to 40
      endif
      enddo

      nov=''
      write(nov,*) yc
      do mm=1,28
      if (nov(mm:mm+2).eq.'NaN') then
      iflag(kkk)=1
      go to 40
      endif
      enddo

c


      ixc=xc
      iyc=yc

      ntrac=ntrac+1

      xctra(ntrac)=xc
      yctra(ntrac)=yc
      dmalat(ntrac)=ladx
      dmilat(ntrac)=lady
      agtra(ntrac)=ang
      bcgt(ntrac)=xm
      bcgst(ntrac)=sb
      npixt(ntrac)=npx





      write (666,116) xctra(ntrac),yctra(ntrac),dmalat(ntrac),
     ?dmilat(ntrac),agtra(ntrac)*radgra




c     write (*,*) 'kkk,n,dan,ang = ',kkk,ntrac,dan(kkk)*radgra,
c    ?ang*radgra,ixc,iyc,xc,yc,ipass



c
c     Checks and marks other circular candidate object regions of the input
c     list that fall inside the rectangle region of the identified traced-shape
c     object. It mainly avoids that other circular regions are processed over
c     and over again against the same rectangular region.
c


      iflag(kkk)=1


      ang=-ang

      ddma=dmalat(ntrac)/2.d0
      ddmi=dmilat(ntrac)/2.d0


      do 35 k=1,nstar

      if (iflag(k).ne.0) go to 35


      jj=xid(k)-ixc
      ii=yid(k)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 35

      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      if (dx.le.ddma .and. dy.le.ddmi) iflag(k)=1

     

 35   continue

c

 40   continue   



c
c     Eliminates trace-shaped rectangles that fall inside larger
c     detected trace-shaped rectangular regions
c


      do  k=1,ntrac

      ixc=xctra(k)
      iyc=yctra(k)
      ang=-agtra(k)
      ddma=dmalat(k)/2.d0
      ddmi=dmilat(k)/2.d0
      npx=npixt(k)

      do 42 m=1,ntrac

      if (k.eq.m) go to 42

      jj=xctra(m)-ixc
      ii=yctra(m)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 42


      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      if (dx.le.ddma .and. dy.le.ddmi) then

      if (npx.lt.npixt(m)) then
      kflag(k)=1
      else
      kflag(m)=1
      endif

      endif

 42   continue

      enddo

c


      nt=ntrac
      ntrac=0


      do 43 k=1,nt


      if (kflag(k).ne.0) go to 43

      ntrac=ntrac+1

      xctra(ntrac)=xctra(k)
      yctra(ntrac)=yctra(k)
      dmalat(ntrac)=dmalat(k)
      dmilat(ntrac)=dmilat(k)
      agtra(ntrac)=agtra(k)
      bcgt(ntrac)=bcgt(k)
      bcgst(ntrac)=bcgst(k)
      npixt(ntrac)=npixt(k)



      write (66,116) xctra(ntrac),yctra(ntrac),dmalat(ntrac),
     ?dmilat(ntrac),agtra(ntrac)*radgra
 116  format('box(',4(f8.2,','),f8.2,')')



 43   continue



c
c     Eliminates rounded-shaped candidates that eventually still falls
c     inside the detected trace-shaped rectangular regions
c


      nest=nstar

      nstar=0


      do 50 k=1,nest


      if (iflag(k).ne.0) go to 50


      do 45 m=1,ntrac


      ixc=xctra(m)
      iyc=yctra(m)
      ang=-agtra(m)


      jj=xid(k)-ixc
      ii=yid(k)-iyc


      call irot (jj,ii,ixc,iyc,ang,nx,ny,j,i,iout)


      if (iout.ne.0) go to 45


      dx=j-ixc
      dy=i-iyc

      dx=dabs(dx)
      dy=dabs(dy)


      ddma=dmalat(m)/2.d0
      ddmi=dmilat(m)/2.d0


      if (dx.le.ddma .and. dy.le.ddmi) go to 50


 45   continue



      nstar=nstar+1

      xid(nstar)=xid(k)
      yid(nstar)=yid(k)
      idlado(nstar)=idlado(k)

      idx1(nstar)=idx1(k)
      idx2(nstar)=idx2(k)
      idy1(nstar)=idy1(k)
      idy2(nstar)=idy2(k)

      npix(nstar)=npix(k)

      bcg(nstar)=bcg(k)
      bcgs(nstar)=bcgs(k)



 50   continue


c     stop

      return

      end









c
c     Subroutine iau_jd2cal
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









c
c
c
c     Subroutine fov_0
c
c
c     Astrometry of images, only. (Astrometry option 0).
c
c
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. Also extracts the instant of observation in JD
c     and Julian year, among other FOV auxiliary information.
c
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     The FOV's (RA,Dec) limits may be expanded or contracted by a factor
c     given by the user. If the factor is 1 the limits are unchanged.
c
c
c 
c
c      Last modification: M. Assafin 13/Nov/2016
c
c
c
c


      subroutine fov_0 (centro,infits,expfov,nx,ny,scala,rac,dec,ramin,
     ?ramax,demin,demax,iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,iutano,
     ?iutmes,iutdia,dj,epoj,iexps,ichfil,mchobj)




      implicit real *8 (a-h,o-z)


      character*50 centro
      character*69 mchobj
      character*20 ichfil
      character*1  isig
      character*150 infits,imfits


      hmsgms(i,j,a)=i+j/60.d0+a/3600.d0

      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)



c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi

c

      dj2000=2451545.d0



c
c
c     Opens field file data with extracted information from fits images' headers
c     using the PRAIA_header_extraction task.
c
c

      open (77,file=centro)

c

      k=0

 400  continue

      imfits=''

      read(77,402,end=403) iah,iam,sa,isig,idg,idm,ds,iuth,iutm,sut,
     ?iutano,iutmes,iutdia,djm,dj,iexps,ichfil,imfits,mchobj

 402  format(1x,i2,1x,i2,1x,f7.4,1x,a1,i2,1x,i2,1x,f6.3,2x,i2,1x,i2,
     ?1x,f5.2,1x,i4,1x,i2,1x,i2,f16.8,1x,f16.8,2x,i4,2x,a20,2x,a50,
     ?1x,a20)



      if (imfits.ne.infits) go to 400

      k=1

 403  close (77)


c
c     No input data found
c


      if (k.ne.1) then
      write (*,*) 'No field data found. Exiting program'
      stop
      endif


c
c     Julian year of observation
c


      epoj=2000d0+(dj-dj2000)/365.25d0


c
c     FOV (RA,Dec) center in degrees
c


      rac=hmsgms(iah,iam,sa)*15.d0
      dec=hmsgms(idg,idm,ds)
      if (isig.eq.'-') dec=-dec




c
c     Estimated (x,y) field sizes in degrees
c


      dx=nx*scala/3600.d0
      dy=ny*scala/3600.d0


c
c     We don't know what is the (RA,Dec) association with
c     (x,y) due to possible field rotations.
c
c     So, picks up the largest value and associate this
c     size to both RA and Dec.
c


      dd=dmax1(dx,dy)


c
c     Applies the user's expansion/contraction factor
c


      dd=dd*expfov


c
c     Checks if pole (north or south) falls in the FOV
c
c
c     krcat = 1  -->  pole is not in the FOV 
c     krcat = 2  -->  pole is inside the FOV 
c
c


      krcat=1

      grac=grarad*rac
      gdec=grarad*dec

      bra=0.d0

      if (dec.lt.0.d0) then
      bde=-90.d0*grarad
      else      
      bde=+90.d0*grarad
      endif

      d=dexy(bra,bde,grac,gdec)
      xx=xpad(bra,bde,grac)/d
      yy=ypad(bra,bde,grac,gdec)/d

      xx=radgra*xx
      yy=radgra*yy


      if (dabs(xx).le.dd/2.d0 .and. dabs(yy).le.dd/2.d0) krcat=2


c
c     (RA,Dec) FOV limits for pole inside the FOV
c


      if (krcat.eq.2) then

      ramin=0.d0
      ramax=360.d0


      if (dec.ge.0.d0) then

      demax=+90.d0
      demin=dec-dd/2.d0
      if (demin.lt.-90.d0) demin=-90.d0

      else

      demin=-90.d0
      demax=dec+dd/2.d0
      if (demax.gt.+90.d0) demax=+90.d0

      endif

      return

      endif



c
c     (RA,Dec) FOV limits for pole not inside the FOV
c


      if (krcat.eq.1) then

      ramax=rac+dd/2.d0
      if (ramax.gt.360.d0) ramax=ramax-360.d0

      ramin=rac-dd/2.d0
      if (ramin.lt.0.d0) ramin=ramin+360.d0



      demax=dec+dd/2.d0
      if (demax.gt.+90.d0) demax=+90.d0

      demin=dec-dd/2.d0
      if (demin.lt.-90.d0) demin=-90.d0


      return

      endif

c

      return

      end




c
c
c
c     Subroutine fov_12
c
c
c     Remaking astrometry of xy PRAIA file, only (Astrometry options 1,2).
c
c     Applies to remaking astrometry based on both (x,y)s or (RA,Dec)s (tangent
c     plane technique). 
c
c
c     Extracts or estimates the (RA,Dec) limits and tangent point (the center
c     so to speak) of the FOV. 
c
c
c     xra = RA  of stars in degrees from former reduction
c     yde = Dec of stars in degrees from former reduction
c
c     rac = RA  center in degrees
c     dec = Dec center in degrees
c     
c     ramax = maximum RA in the FOV in degrees
c     ramin = minimum RA in the FOV in degrees
c
c     demax = maximum Dec in the FOV in degrees
c     demin = minimum Dec in the FOV in degrees
c
c
c
c     Here, the FOV's (RA,Dec) limits are not expanded or contracted by a factor
c     given by the user. The FOV's (RA,Dec) true limits are directly determined
c     from the stars' (RA,Dec)s obtained in the previous (RA,Dec) reduction.
c
c
c 
c
c      Last modification: M. Assafin 26/Jul/2017
c
c
c
c


      subroutine fov_12 (idiobs,nest,xra,yde,rac,dec,ramin,ramax,demin,
     ?demax,iah,iam,sa,isig,idg,idm,ds)


      implicit real *8 (a-h,o-z)


      dimension xra(idiobs),yde(idiobs)

      character*1 isig


      dexy(xx,yy,zz,ww)=dsin(yy)*dsin(ww)+dcos(yy)*dcos(ww)*dcos(xx-zz)
      xpad(xx,yy,zz)=dcos(yy)*dsin(xx-zz)
      ypad(xx,yy,zz,ww)=dsin(yy)*dcos(ww)-dcos(yy)*dsin(ww)*dcos(xx-zz)


      alff (xx,yy,zz,ww)=zz+datan2(xx,dcos(ww)-yy*dsin(ww))
      deltt(xx,yy,zz,ww)=datan2((yy*dcos(ww)+dsin(ww))*dcos(xx-zz),
     ?dcos(ww)-yy*dsin(ww))



c
c     Initial data
c
c

      pi=0.3141592653589793d1
      grarad=pi/180.d0
      radgra=180.d0/pi


      safe=5.d0/3600.d0

c
c     Gets some parameters from the FOV stars
c


      ramin=361.d0
      ramax=-1.d0

      demin=+91.d0
      demax=-91.d0
      


      do j=1,nest

      if (yde(j).lt.demin) demin=yde(j)
      if (yde(j).gt.demax) demax=yde(j)

      if (xra(j).lt.ramin) ramin=xra(j)
      if (xra(j).gt.ramax) ramax=xra(j)


      enddo


c
c     Provisional (RA,Dec) FOV center
c


      ram=(ramax+ramin)/2.d0
      dem=(demax+demin)/2.d0



c
c     Detemines final (RA,Dec) FOV center:
c
c
c     1) gnomonic projection of all star's (RA,Dec)s w.r.t the
c        provivional center
c
c     2) computes (X,Y)'s baricenter
c
c     3) anti-gnomonic projection of (X,Y) baricenter to get
c        the (RA,Dec) baricenter of the FOV
c
c     4) Use this as the final (RA,Dec) FOV center 
c
c
c     Note: here, it is irrelevant if the pole is or is not inside 
c           the FOV
c 
c

      gram=grarad*ram
      gdem=grarad*dem


      x=0.d0
      y=0.d0

      do i=1,nest

      bra=xra(i)*grarad
      bde=yde(i)*grarad

      d=dexy(bra,bde,gram,gdem)
      xx=xpad(bra,bde,gram)/d
      yy=ypad(bra,bde,gram,gdem)/d

      x=x+xx
      y=y+yy

      enddo


      x=x/nest
      y=y/nest


      grac=alff(x,y,gram,gdem)
      gdec=deltt(grac,y,gram,gdem)


      rac=grac*radgra
      dec=gdec*radgra


    
c
c     (RA,Dec) of FOV in hexasegimal format
c 


      ra=rac/15.d0
      de=dec

      iah=ra
      am=(ra-iah)*60.d0
      iam=am
      sa =(am-iam)*60.d0
      if (de.lt.0.d0) then
      isig='-'
      de=-de
      else
      isig='+'
      endif
      idg=de
      dm=(de-idg)*60.d0
      idm=dm
      ds=(dm-idm)*60.d0



c
c     Adds a safe limit of 5 arcsec for finding stars in
c    (RA,Dec) extremes
c


      demax=demax+safe
      demin=demin-safe

      if (demax.gt.+90.d0) demax=+90.d0
      if (demin.lt.-90.d0) demin=-90.d0

c

      remax=remax+safe/dabs(dcos(grarad*dec))
      remin=remin-safe/dabs(dcos(grarad*dec))

      if (remax.gt.360.d0) remax=remax-360.d0
      if (remin.lt.0.d0) remin=remin+360.d0

c


      return

      end








